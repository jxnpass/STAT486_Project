library(tidyverse)

std <- read_csv("Data/FE/crime_num_fe.csv")
y <- std$Num_Crimes

# graph for RF fit
preds = as_tibble(read.csv("ModelScripts/PyPreds/rfpreds.csv", header = F))$V1 
p_rmse <- rmse(y, preds) %>% round(digits = 2)
p_var_red <- (var_red(y, preds)*100) %>% round(digits = 2)
st = paste("RMSE: ", p_rmse, " Var Red: ", p_var_red, "%", sep = "")
cap = "No Dimension Reduction Used"

rf_plt <- ggplot(data = std, mapping = aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = "Truth")) +
  geom_line(mapping = aes(y = preds, color = "RF")) +
  labs(title = "Random Forest Fit on Crime Rates", color = "Key", y = "Crime Count",
       subtitle = st, caption = cap) +
  scale_color_manual(values = c("Truth" = "black", "RF" = "brown")) +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(file = "FitGraphs/rf_plot.png", plot = rf_plt, width = 7, height = 5, units = 'in')

# graph for KNN Fit

preds = as_tibble(read.csv("ModelScripts/PyPreds/knnpreds.csv", header = F))$V1 
p_rmse <- rmse(y, preds) %>% round(digits = 2)
p_var_red <- (var_red(y, preds)*100) %>% round(digits = 2)
st = paste("RMSE: ", p_rmse, " Var Red: ", p_var_red, "%", sep = "")
cap = "Used Lasso Variable Selection"

knn_plt <- ggplot(data = std, mapping = aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = "Truth")) +
  geom_line(mapping = aes(y = preds, color = "KNN")) +
  labs(title = "KNN Fit on Crime Rates", color = "Key", y = "Crime Count",
       subtitle = st, caption = cap) +
  scale_color_manual(values = c("Truth" = "black", "KNN" = "lightblue4")) +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(file = "FitGraphs/knn_plot.png", plot = knn_plt, width = 7, height = 5, units = 'in')

# graph for LASSO Fit

preds = as_tibble(read.csv("ModelScripts/PyPreds/lassopreds.csv", header = F))$V1 
p_rmse <- rmse(y, preds) %>% round(digits = 2)
p_var_red <- (var_red(y, preds)*100) %>% round(digits = 2)
st = paste("RMSE: ", p_rmse, " Var Red: ", p_var_red, "%", sep = "")
cap = "No Dimension Reduction Used"

lasso_plt <- ggplot(data = std, mapping = aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = "Truth")) +
  geom_line(mapping = aes(y = preds, color = "Lasso")) +
  labs(title = "Lasso Fit on Crime Rates", color = "Key", y = "Crime Count",
       subtitle = st, caption = cap) +
  scale_color_manual(values = c("Truth" = "black", "Lasso" = "gold3")) +
  theme_light() +
  theme(legend.position = 'bottom')

ggsave(file = "FitGraphs/lasso_plot.png", plot = lasso_plt, width = 7, height = 5, units = 'in')


