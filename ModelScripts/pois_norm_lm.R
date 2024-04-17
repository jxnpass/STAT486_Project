library(tidyverse)
library(glmnet)

source("ModelScripts/CVFunctions.R")

std <- read_csv("Data/FE/crime_num_fe.csv")
pca <- read_csv("Data/FE/crime_num_dr.csv")[,-1]
vs <- read_csv("Data/FE/crime_num_varselect.csv")[,-1]

y <- std$Num_Crimes # same with each df

# train/test split
train_size = .8 * nrow(std)
train_idx <- sample(1:nrow(std), size = train_size)


# Model 1: poisson regression -------------------
# the response is Poisson, so when using linear models, we want to set family to poisson as an option

# inference
pois_lm_std <- glm(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year, data = std, family = "poisson")
pois_lm_pca <- glm(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year, data = pca, family = "poisson")
pois_lm_vs <- glm(Num_Crimes ~ Time_Interval + . -Date-TS_interval-Day-Month-Year, data = vs, family = "poisson")

summary(pois_lm_std)
summary(pois_lm_pca)
summary(pois_lm_vs)

# prediction (full scale)

pois_preds_std <- predict.glm(object = pois_lm_std, newdata = std) %>% exp()
rmse(y, pois_preds_std)
var_red(y, pois_preds_std)
pois_preds_pca <- predict.glm(object = pois_lm_pca, newdata = pca) %>% exp()
rmse(y, pois_preds_pca)
var_red(y, pois_preds_pca)
pois_preds_vs <- predict.glm(object = pois_lm_vs, newdata = vs) %>% exp()
rmse(y, pois_preds_vs)
var_red(y, pois_preds_vs)

# validation (train/test split)

poislm_CV(std, type = "std") %>% colMeans()
poislm_CV(pca, type = "pca") %>% colMeans()
poislm_CV(vs, type = "vs") %>% colMeans()

# graph for best fit
preds = predict.glm(object = pois_lm_std, newdata = std) %>% exp()
p_rmse <- rmse(y, preds) %>% round(digits = 2)
p_var_red <- (var_red(y, preds)*100) %>% round(digits = 2)
st = paste("RMSE: ", p_rmse, " Var Red: ", p_var_red, "%", sep = "")
cap = "No Dimension Reduction Used"

out_plt <- ggplot(data = std, mapping = aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = "Truth")) +
  geom_line(mapping = aes(y = preds, color = "Pois LM")) +
  labs(title = "Poisson Regression Fit on Crime Rates", color = "Key", y = "Crime Count",
       subtitle = st, caption = cap) +
  scale_color_manual(values = c("Truth" = "black", "Pois LM" = "purple3")) +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(file = "FitGraphs/poislm_plot.png", plot = out_plt, width = 7, height = 5, units = 'in')



# Model 2: Linear REgression -------------

# inference
norm_lm_std <- lm(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = std)
norm_lm_pca <- lm(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype, data = pca)
norm_lm_vs <- lm(Num_Crimes ~ Time_Interval + . -Date-TS_interval-Day-Month-Year, data = vs)

summary(norm_lm_std)
summary(norm_lm_pca)
summary(norm_lm_vs)

# prediction (full scale)

norm_preds_std <- predict.glm(object = norm_lm_std, newdata = std) 
rmse(y, norm_preds_std)
var_red(y, norm_preds_std)
norm_preds_pca <- predict.glm(object = norm_lm_pca, newdata = pca) 
rmse(y, norm_preds_pca)
var_red(y, norm_preds_pca)
norm_preds_vs <- predict.glm(object = norm_lm_vs, newdata = vs) 
rmse(y, norm_preds_vs)
var_red(y, norm_preds_vs)

# validation (train/test split)

normlm_CV(std, type = "std") %>% colMeans()
normlm_CV(pca, type = "pca") %>% colMeans()
normlm_CV(vs, type = "vs") %>% colMeans()


# graph for best fit
preds = predict.glm(object = norm_lm_std, newdata = std) 
p_rmse <- rmse(y, preds) %>% round(digits = 2)
p_var_red <- (var_red(y, preds)*100) %>% round(digits = 2)
st = paste("RMSE: ", p_rmse, " Var Red: ", p_var_red, "%", sep = "")
cap = "No Dimension Reduction Used"

out_plt <- ggplot(data = std, mapping = aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = "Truth")) +
  geom_line(mapping = aes(y = preds, color = "LM")) +
  labs(title = "Linear Regression Fit on Crime Rates", color = "Key", y = "Crime Count",
       subtitle = st, caption = cap) +
  scale_color_manual(values = c("Truth" = "black", "LM" = "blue")) +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(file = "FitGraphs/normlm_plot.png", plot = out_plt, width = 7, height = 5, units = 'in')






# Model 3: Lasso Linear Regression (now with tuning parameter) ------------------ 
# done in python






