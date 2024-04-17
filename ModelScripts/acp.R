library(acp)
library(tidyverse)
library(glmnet)

source("ModelScripts/CVFunctions.R")

std <- read_csv("Data/FE/crime_num_fe.csv")
pca <- read_csv("Data/FE/crime_num_dr.csv")[,-1]
vs <- read_csv("Data/FE/crime_num_varselect.csv")[,-1]


# Functions to make ACP work ----------------------------------------------

predict_acp_with_intervals <- function(object, newydata = NULL, newxdata = NULL, family = "acp", alpha = 0.05) {
  if (is.null(newydata)) {
    y <- fitted(object)
  }
  n <- length(newydata)
  k <- ifelse(is.null(newxdata), 0, ncol(newxdata))
  if (k > 0) {
    x <- newxdata
  }
  if (family == "acp") {
    q <- object$q
    p <- object$p
    lambda <- matrix(NA, n, 1)
    lambda[1] <- 1
    vylag <- Lag(newydata, 1)
    vparlag <- c(coef(object)[k + 2])
    if (p > 1) {
      for (i in 1:(p - 1)) {
        vparlag <- append(vparlag, coef(object)[k + 2 + i])
        vylag <- cbind(vylag, Lag(newydata, i + 1))
        lambda[i + 1] <- 1
      }
    }
    vfilt <- c(coef(object)[k + 2 + p])
    vinit <- c(lambda[1])
    if (q > 1) {
      for (i in 1:(q - 1)) {
        vfilt <- append(vfilt, coef(object)[k + 2 + p + i])
        vinit <- append(vinit, lambda[1])
      }
    }
    lambda[(p + 1):n] <- stats::filter(as.ts(coef(object)[k + 1] + na.omit(vylag %*% vparlag)), 
                                       filter = vfilt, init = vinit, method = "recursive")
    if (k == 0) {
      xb <- matrix(1, n, 1)
    } else {
      xb <- exp(as.matrix(x) %*% as.matrix(coef(object)[1:k]))
    }
    y <- as.vector(xb * lambda)
    
    # Calculate prediction intervals
    se <- sqrt(y)  # Standard error of the predicted counts
    z <- qnorm(1 - alpha / 2)  # Z-score for the desired confidence level
    lower <- y - z * se  # Lower bound of prediction interval
    upper <- y + z * se  # Upper bound of prediction interval
    
    # Combine predictions and intervals into a data frame
    prediction_with_intervals <- data.frame(Predicted = y, Lower = lower, Upper = upper)
  } else {
    y <- as.vector(exp(as.matrix(x) %*% coef(object)[1:k]))
    prediction_with_intervals <- y
  }
  return(prediction_with_intervals)
}

# ACP - auto regressive conditional poisson
y <- std$Num_Crimes
X_std <- model.matrix(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year, data = std)
acp_std <- acp(X_std, y, p = 1, q = 1, startval = NULL, varopt=TRUE)
X_pca <- model.matrix(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year, data = pca)
acp_pca <- acp(X_pca, y, p = 1, q = 1, startval = NULL, varopt=TRUE)
X_vs <- model.matrix(Num_Crimes ~ Time_Interval + . -Date-TS_interval-Day-Month-Year, data = vs)
acp_vs <- acp(X_vs, y, p = 1, q = 1, startval = NULL, varopt=TRUE)

# inference
summary(acp_std)
summary(acp_pca)
summary(acp_vs)

# predictions
preds = predict_acp_with_intervals(object = acp_std, newydata = y, newxdata = X_std)$Predicted
rmse(y, preds)
var_red(y, preds)
preds = predict_acp_with_intervals(object = acp_pca, newydata = y, newxdata = X_pca)$Predicted
rmse(y, preds)
var_red(y, preds)
preds = predict_acp_with_intervals(object = acp_vs, newydata = y, newxdata = X_vs)$Predicted
rmse(y, preds)
var_red(y, preds)

# these metrics on just the model fit were so bad that we ultimately scrapped doing it at this point. 

# graph for best fit
preds = predict_acp_with_intervals(object = acp_vs, newydata = y, newxdata = X_vs)$Predicted
p_rmse <- rmse(y, preds) %>% round(digits = 2)
p_var_red <- (var_red(y, preds)*100) %>% round(digits = 2)
st = paste("RMSE: ", p_rmse, " Var Red: ", p_var_red, "%", sep = "")
cap = "Used Lasso Variable Selection"

out_plt <- ggplot(data = std, mapping = aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = "Truth")) +
  geom_line(mapping = aes(y = preds, color = "ACP")) +
  labs(title = "ACP Fit on Crime Rates", color = "Key", y = "Crime Count",
       subtitle = st, caption = cap) +
  scale_color_manual(values = c("Truth" = "black", "ACP" = "red3")) +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(file = "FitGraphs/acp_plot.png", plot = out_plt, width = 7, height = 5, units = 'in')





