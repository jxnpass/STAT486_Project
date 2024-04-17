library(tidyverse)

source("ModelScripts/CVFunctions.R")

std.v <- read_csv("Data/FE/crime_violent_fe.csv")
pca.v <- read_csv("Data/FE/crime_violent_dr.csv")[,-1]
vs.v <- read_csv("Data/FE/crime_violent_varselect.csv")[,-1]
std <- read_csv("Data/FE/crime_num_fe.csv")
pca <- read_csv("Data/FE/crime_num_dr.csv")[,-1]
vs <- read_csv("Data/FE/crime_num_varselect.csv")[,-1]

holidays <- std$Holiday %>% unique()
holidays <- c(holidays[2], holidays[-2])

std.v <- std.v %>% 
  mutate(Holiday = factor(Holiday, levels = holidays))
pca.v <- pca.v %>% 
  mutate(Holiday = factor(Holiday, levels = holidays))

y <- std.v$Num_Crimes/std$Num_Crimes 
std.v <- std.v[!is.na(y),]
pca.v <- pca.v[!is.na(y),]
vs.v <- vs.v[!is.na(y),]
std <- std[!is.na(y),]
pca <- pca[!is.na(y),]
vs <- vs[!is.na(y),]
w <- std.v$Num_Crimes
y <- y[!is.na(y)]


# Log Reg on Violence Proportion -----------------------
logreg_std <- glm(y ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-Num_Crimes, 
       family = binomial(link = 'logit'), data = std.v, weights = w)
logreg_pca <- glm(y ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-preciptype-Num_Crimes, 
       family = binomial(link = 'logit'), data = pca.v, weights = w)
logreg_vs <- glm(y ~ Time_Interval + . -Date-TS_interval-Day-Month-Year-Num_Crimes, 
       family = binomial(link = 'logit'), data = vs.v, weights = w)

# self-validation on own set
preds = logreg_std$fitted
rmse(y, preds)
var_red(y, preds)
preds = logreg_pca$fitted
rmse(y, preds)
var_red(y, preds)
preds = logreg_vs$fitted
rmse(y, preds)
var_red(y, preds)

summary(logreg_std)
summary(logreg_pca)
summary(logreg_vs)
AIC(logreg_std)
AIC(logreg_pca)
AIC(logreg_vs)
logLik(logreg_std)
logLik(logreg_pca)
logLik(logreg_vs)

# on CV = 5
logreg_CV(std.v, std, type = "std") %>% colMeans()
logreg_CV(pca.v, std, type = "pca") %>% colMeans()
logreg_CV(vs.v, std, type = "vs") %>% colMeans()


