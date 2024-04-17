library(tidyverse)
library(forecast)

source("ModelScripts/CVFunctions.R")

std <- read_csv("Data/FE/crime_num_fe.csv") 
pca <- read_csv("Data/FE/crime_num_dr.csv")[,-1]
vs <- read_csv("Data/FE/crime_num_varselect.csv")[,-1]
y <- std$Num_Crimes

holidays <- std$Holiday %>% unique()
holidays <- c(holidays[2], holidays[-2])

std <- std %>% 
  mutate(Holiday = factor(Holiday, levels = holidays))
pca <- pca %>% 
  mutate(Holiday = factor(Holiday, levels = holidays))

# SARIMA model ------------------------------------------------------------

minYear <- std$Year %>% min()
minInt <- std$TS_interval %>% min()

tms <- ts(data = y, frequency = 1, start = c(minInt, 1))

X_std <- model.matrix(Num_Crimes ~ -1 + Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = std)
X_std <- X_std[,c(-1)]
sarima_std <- auto.arima(tms, xreg = X_std, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, start.p = 1, start.q = 1, start.P = 1, start.Q = 1)
sarima_std <- Arima(tms, xreg = X_std, order = c(2,1,1), seasonal = c(0,1,0))
# best was c(2,1,1) and c(0,1,0)
X_pca <- model.matrix(Num_Crimes ~ -1 + Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = pca)
X_pca <- X_pca[,c(-1)]
sarima_pca <- auto.arima(tms, xreg = X_pca, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, start.p = 1, start.q = 1, start.P = 1, start.Q = 1)
sarima_pca <- Arima(tms, xreg = X_pca, order = c(0,1,5), seasonal = c(0,1,0))
# best was c(0,1,5) and c(0,1,0)
X_vs <- model.matrix(Num_Crimes ~ -1 + Time_Interval + . -Date-TS_interval-Day-Month-Year, data = vs)
X_vs <- X_vs[,c(-1)]
sarima_vs <- auto.arima(tms, xreg = X_vs, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, start.p = 1, start.q = 1, start.P = 1, start.Q = 1)
sarima_vs <- Arima(tms, xreg = X_vs, order = c(2,1,1), seasonal = c(0,1,0))
# best was c(2,1,1) and c(0,1,0)

summary(sarima_std)
summary(sarima_pca)
summary(sarima_vs)


# on dataset validation
preds <- sarima_std$fitted
rmse(y, preds)
var_red(y, preds)
preds <- sarima_pca$fitted
rmse(y, preds)
var_red(y, preds)
preds <- sarima_vs$fitted
rmse(y, preds)
var_red(y, preds)

# graph for best fit
preds <- sarima_pca$fitted
p_rmse <- rmse(y, preds) %>% round(digits = 2)
p_var_red <- (var_red(y, preds)*100) %>% round(digits = 2)
st = paste("RMSE: ", p_rmse, " Var Red: ", p_var_red, "%", sep = "")
cap = "Used PCA Dimension Reduction"

sarima_plt <- ggplot(data = std, mapping = aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = "Truth")) +
  geom_line(mapping = aes(y = preds, color = "SARIMA")) +
  labs(title = "SARIMA Fit on Crime Rates", color = "Key", y = "Crime Count",
       subtitle = st, caption = cap) +
  scale_color_manual(values = c("Truth" = "black", "SARIMA" = "green4")) +
  theme_light() +
  theme(legend.position = 'bottom')
  
ggsave(file = "FitGraphs/sarima_plot.png", plot = sarima_plt, width = 7, height = 5, units = 'in')

# validation
split_year <- 2022
idx <- std$TS_interval[std$Year <= split_year]

y_train <- y[idx]
y_test <- y[-idx]
std_train <- std[idx,]
std_test <- std[-idx,]
pca_train <- pca[idx,]
pca_test <- pca[-idx,]
vs_train <- vs[idx,]
vs_test <- vs[-idx,]

tms <- ts(data = y_train, frequency = 1, start = c(minInt, 1))

X_std <- model.matrix(Num_Crimes ~ -1 + Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = std_train)[,-1]
Xt_std <- model.matrix(Num_Crimes ~ -1 + Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = std_test)[,-1]
sarima_std_t <- Arima(tms, xreg = X_std, order = c(2,1,1), seasonal = c(0,1,0))
X_pca <- model.matrix(Num_Crimes ~ -1 + Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = pca_train)[,-1]
Xt_pca <- model.matrix(Num_Crimes ~ -1 + Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = pca_test)[,-1]
sarima_pca_t <- Arima(tms, xreg = X_pca, order = c(0,1,5), seasonal = c(0,1,0))
X_vs <- model.matrix(Num_Crimes ~ -1 + Time_Interval + . -Date-TS_interval-Day-Month-Year, data = vs_train)[,-1]
Xt_vs <- model.matrix(Num_Crimes ~ -1 + Time_Interval + . -Date-TS_interval-Day-Month-Year, data = vs_test)[,-1]
sarima_vs_t <- Arima(tms, xreg = X_vs, order = c(2,1,1), seasonal = c(0,1,0))

preds <- forecast(sarima_std_t, xreg = Xt_std)$mean
rmse(y_test, preds)
var_red(y_test, preds)
preds <- forecast(sarima_pca_t, xreg = Xt_pca)$mean
rmse(y_test, preds)
var_red(y_test, preds)
preds <- forecast(sarima_vs_t, xreg = Xt_vs)$mean
rmse(y_test, preds)
var_red(y_test, preds)

# GIVEN THAT PCA SARIMA IS THE BEST ONE, LETS LOOK AT EACH COMPONENT AND HOW THEY DO
# Explanation Break down -----------

summary(sarima_pca)
ci = confint(sarima_pca)
sig = !((ci[,1] <= 0) & (ci[,2] >= 0)) %>% as.integer()
cbind(sig, ci)

minYear <- pca$Year %>% min()
minInt <- pca$TS_interval %>% min()
tms <- ts(data = y, frequency = 1, start = c(minInt, 1))

# view time relation/lag variance explanation
# using  order = c(0,1,5), seasonal = c(0,1,0))
exp_ts <- Arima(tms, order = c(0,1,5), seasonal = c(0,1,0))
preds <- exp_ts$fitted
rmse(y, preds)
var_red(y, preds)

# viewing weather related information

X_wthr <- model.matrix(Num_Crimes ~ -1 + is_FullMoon_BLOCK + suntime + temp_pca + precip_pca + wind_pca + radiation_pca, data = pca)[,-1] %>% as.data.frame()
exp_wthr <- lm(y ~ ., data = X_wthr)
preds <- exp_wthr$fitted
rmse(y, preds)
var_red(y, preds)
summary(exp_wthr)

# holiday, national incidents, 

X_hol <-  model.matrix(Num_Crimes ~ -1 + Time_Interval*Holiday+NationalIncident, data = pca)[,-1] %>% as.data.frame()
exp_hol <- lm(y ~ ., data = X_hol)
preds <- exp_hol$fitted
rmse(y, preds)
var_red(y, preds)
summary(exp_hol)



