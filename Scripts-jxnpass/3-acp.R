library(acp)
library(tidyverse)

predict_acp <- function(object, newydata = NULL, newxdata = NULL, family = "acp") {
  if (is.null(newydata)) {
    y <- fitted(object)
  }
  n <- length(newydata)
  k <- ifelse(is.null(newxdata), 0, ncol(newxdata))
  if (k > 0) {
    x <- newxdata
  }
  if (family == "acp") {
    q = object$q
    p = object$p
    lambda <- matrix(NA, n, 1)
    lambda[1] <- 1
    vylag = Lag(newydata, 1)
    vparlag = c(coef(object)[k + 2])
    if (p > 1) {
      for (i in 1:(p - 1)) {
        vparlag <- append(vparlag, coef(object)[k + 2 + 
                                                  i])
        vylag <- cbind(vylag, Lag(newydata, i + 1))
        lambda[i + 1] <- 1
      }
    }
    vfilt = c(coef(object)[k + 2 + p])
    vinit = c(lambda[1])
    if (q > 1) {
      for (i in 1:(q - 1)) {
        vfilt <- append(vfilt, coef(object)[k + 2 + p + 
                                              i])
        vinit <- append(vinit, lambda[1])
      }
    }
    lambda[(p + 1):n] <- stats::filter(as.ts(coef(object)[k + 1] + na.omit(vylag %*% vparlag)), 
                                       filter = vfilt, init = vinit, method = "recursive")
    if (k == 0) {
      xb = matrix(1, n, 1)
    }
    else {
      xb = exp(as.matrix(x) %*% as.matrix(coef(object)[1:k]))
    }
    y <- as.vector(xb * lambda)
  }
  else {
    y <- as.vector(exp(as.matrix(x) %*% coef(object)[1:k]))
  }
  return(y)
}

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

num_features <- c("temp","feelslike", "dew","humidity","precip","snow","windgust",
                  "winddir","cloudcover","severerisk","is_FullMoon_BLOCK","suntime",
                  'Holiday', "Time_Interval")
crime_mod <- read_csv("Data/crime_num.csv")[,-1]

X <- model.matrix(~ ., data=crime_mod %>% select(all_of(num_features)))[,-11]
y <- crime_mod$Num_Crimes

# ACP - auto regressive conditional poisson
ar_pois <- acp(X, y, p = 4, q = 2, startval = NULL, varopt=TRUE)

ar_pois_summary <- summary(ar_pois)
ar_pois_summary$coefficients[,c(1,4)] %>% round(digits = 4)

preds <- predict_acp(ar_pois, newydata = y, newxdata = X)
preds_fv <- ar_pois$fitted.values

acf(ar_pois$residuals, lag.max = nrow(crime_mod))
acf(y, lag.max = nrow(crime_mod))

ggplot(data = crime_mod) +
  geom_line(mapping = aes(x = Date, y = Num_Crimes, color = Time_Interval)) + 
  geom_line(mapping = aes(x = Date, y = preds), color = "black") + 
  facet_wrap(~ Time_Interval, labeller = "label_both") 

ggplot(data = crime_mod) +
  geom_line(mapping = aes(x = Date, y = Num_Crimes)) +
  #  geom_point(mapping = aes(x = TS_interval, y = Num_Crimes)) +
  geom_line(mapping = aes(x = Date, y = preds), color = "blue") 

(preds - y)^2 %>% mean() %>% sqrt()
sd(y)
cor(preds, y)
evaluation(y, preds)
par(mfrow = c(1,1))
# Cross-Validate on ACP????

# scores for the first observation

# logarithmic (equivalent to deviance score up to a constant) 
dev_score = -log(dpois(y_test, lambda=preds))

ggplot() +
  geom_line(mapping = aes(x = TS_interval, y = y), col = "black") +
  geom_line(mapping = aes(x = TS_interval, y = preds), col = "blue") +
  geom_line(mapping = aes(x = TS_interval, y = dev_score), color = "red")

# cross-val
# create train set
final_year = max(crime_mod$Year)-1
crime_train <- crime_mod %>% filter(Year < final_year)
crime_test <- crime_mod %>% filter(Year >= final_year)

y_train <- crime_train$Num_Crimes
y_test <- crime_test$Num_Crimes
X_train <- model.matrix(~ ., data=crime_train %>% select(all_of(num_features)))[,-11]
X_test <- model.matrix(~ ., data=crime_test %>% select(all_of(num_features)))[,-11]

# fit model
ar_pois_train <- acp(X_train, y_train, p = 4, q = 2, startval = NULL, varopt=TRUE)
length(coef(ar_pois_train))
dim(X_test)

# generate predictions
y_pred <- predict_acp(ar_pois_train, newxdata = X_test, newydata = y_test)
y_pred_int <- predict_acp_with_intervals(ar_pois_train, newxdata = X_test, newydata = y_test, alpha = .05)
# RPMSE, COVERAGE, WIDTH
(y_pred - y_test)^2 %>% mean() %>% sqrt()
sd(y_test)
cvg = mean(
  (y_test < y_pred_int[,3]) & (y_test > y_pred_int[,2])
)
wid = mean(y_pred_int[,3] - y_pred_int[,2])

# plot to verify
ggplot() +
  geom_line(data = crime_mod, aes(x = Date, y = Num_Crimes)) +
  geom_line(data = crime_test, aes(x = Date, y = y_pred), color = "blue")

ggplot() +
  geom_line(data = crime_test, aes(x = Date, y = Num_Crimes)) +
  geom_line(data = crime_test, aes(x = Date, y = y_pred), color = "blue") +
  geom_smooth(data = crime_test, aes(x = Date, y = y_pred_int$Upper), color = "blue", lty = 2) +
  geom_smooth(data = crime_test, aes(x = Date, y = y_pred_int$Lower), color = "blue", lty = 2) 
  
