library(tidyverse)
library(forecast)
library(acp)

crime <- read_csv("Data/cleaned_data.csv") %>% 
  select(-1) %>% 
  rename(Type = `Primary Type`) %>% 
  filter(!is.na(Time_Interval)) %>% 
  arrange(Date, Time)


# Step 1: Decide on Group Var ------------------------------------------------

# Regression: Number of crimes per day, poisson response
group_vars <- c("Date", "Day", "Month", "Year", "Time_Interval")
cols_interest <- c(group_vars, "tempmax" ,    "tempmin"      ,     "temp"  ,  
  "feelslikemax"     ,    "feelslikemin"    ,     "feelslike"         ,  
  "dew"              ,    "humidity"        ,     "precip"            ,  
  "precipprob"       ,    "precipcover"     ,     "preciptype"        ,  
  "snow"             ,    "snowdepth"       ,     "windgust"          ,  
  "windspeed"        ,    "winddir"         ,     "sealevelpressure"  ,  
  "cloudcover"       ,    "visibility"      ,     "solarradiation"    ,  
  "solarenergy"      ,    "uvindex"         ,     "severerisk"        ,  
  "sunrise"          ,    "sunset"          ,     "moonphase"         ,  
  "conditions"       ,    "description"     ,     "icon"              ,  
  "is_FullMoon_BLOCK",    "suntime",              'Holiday') 
cols_interest <- c(group_vars, "temp"  ,   "feelslike"         ,  
  "dew"              ,    "humidity"        ,     "precip"            ,  
  "snow"             ,   "windgust"          ,    "winddir"         ,     
  "cloudcover"       ,   "severerisk"        ,  
  "is_FullMoon_BLOCK",    "suntime",              'Holiday') 



# crime %>% 
#   select(Date, Time, is_FullMoon, is_Night, sunrise, sunset) %>% 
#   arrange(Date, Time) %>% 
#   View()


crime_num <- crime %>% 
  group_by_at(vars(cols_interest)) %>% 
  summarise(Num_Crimes = n()) %>% 
  ungroup() %>% 
  complete(Date, nesting(Time_Interval), fill = list(Num_Crimes = 0), explicit = T) %>% 
  fill(everything(), .direction = "down") %>% 
  arrange(Date, Time_Interval)

TS_interval <- rep(1:(nrow(crime_num)))

crime_num <- crime_num %>% 
  mutate(TS_interval = TS_interval) %>% 
  select(Date, TS_interval, Time_Interval, Num_Crimes, everything())
  
# crime_num2 <- inner_join(crime %>% 
#   group_by_at(vars(cols_interest)) %>% 
#   summarise(Num_Crimes = n()) %>%
#   ungroup(),
#   crime %>%
#     group_by_at(vars(group_vars, "Type")) %>%
#     summarise(count = n()) %>%
#     pivot_wider(names_from = Type, values_from = count, values_fill = 0) %>% 
#     ungroup(),
#   by = group_vars)

# Step 2: EDA ------------------------------------------------

# crime_counts <- c(crime$Type %>% unique(), "Num_Crimes")
# 
# for (label in crime_counts) {
#   print(label)
#   print(ggplot(data = crime_num, aes(x = Day, y = .data[[label]])) +
#     geom_line(mapping = aes(color = is_Night)) + 
#     geom_point(mapping = aes(color = interaction(is_Night, is_FullMoon), size = is_FullMoon)) +
#     facet_wrap(~ Arrest + is_Night, labeller = "label_both") +
#     labs(title = paste(label)))
# }

ggplot(data = crime_num, aes(x = Date, y = Num_Crimes)) +
  geom_line(mapping = aes(color = is_FullMoon_BLOCK)) + 
  facet_wrap(~ Time_Interval, labeller = "label_both") 

### Step 3: Possible models ------------------------

crime_mod <- crime_num # %>% filter(!Arrest, Domestic)
y <- crime_mod$Num_Crimes

# poisson regression . -Date-Day-Month-Year-Holiday-TS_interval-Arrest-Domestic
pois_lm <- glm(Num_Crimes ~ . -Date-Day-Month-Year-TS_interval, data = crime_mod, family = "poisson")
pois_lm %>% summary()
preds <- predict.glm(pois_lm, newdata = crime_mod)
ggplot(data = crime_mod) +
  geom_line(mapping = aes(x = TS_interval, y = Num_Crimes, color = Time_Interval)) + 
  geom_line(mapping = aes(x = TS_interval, y = preds), color = "black") + 
  facet_wrap(~ Time_Interval, labeller = "label_both") 

(preds - y)^2 %>% mean() %>% sqrt()
sd(y)

# normal regression
norm_lm <- lm(Num_Crimes ~ . -Date-Day-Month-Year-TS_interval + Holiday:Time_Interval, data = crime_mod)
norm_lm %>% summary()
preds <- predict.lm(norm_lm, newdata = crime_mod)

ggplot(data = crime_mod) +
  geom_line(mapping = aes(x = TS_interval, y = Num_Crimes, color = Time_Interval)) + 
  geom_line(mapping = aes(x = TS_interval, y = preds), color = "black") + 
  facet_wrap(~ Time_Interval, labeller = "label_both") 

ggplot(data = crime_mod) +
  geom_line(mapping = aes(x = TS_interval, y = Num_Crimes)) +
#  geom_point(mapping = aes(x = TS_interval, y = Num_Crimes)) +
  geom_line(mapping = aes(x = TS_interval, y = preds), color = "blue") 

(preds - y)^2 %>% mean() %>% sqrt()
sd(y)

# time series
minYear <- crime_mod$Year %>% min()
minInt <- crime_mod$TS_interval %>% min()

tms_day <- ts(data = crime_mod$Num_Crimes, frequency = 1, start = c(minInt, 1))
num_features <- c("temp"  ,   "feelslike"         ,  
                  "dew"              ,    "humidity"        ,     "precip"            ,  
                  "snow"             ,   "windgust"          ,    "winddir"         ,     
                  "cloudcover"       ,   "severerisk"        ,  
                  "is_FullMoon_BLOCK",    "suntime",              'Holiday', "Time_Interval")
X <- model.matrix(~ -1 + ., data=crime_mod %>% select(all_of(num_features)))[,-11]
sarima_model <- auto.arima(tms_day, xreg = X)
sarima_model_noX <- auto.arima(tms_day)
summary(sarima_model)
summary(sarima_model_noX)
acf(sarima_model$residuals, lag.max = nrow(crime_mod))

preds <- sarima_model$fitted

ggplot(data = crime_mod) +
  geom_line(mapping = aes(x = TS_interval, y = Num_Crimes, color = Time_Interval)) + 
  geom_line(mapping = aes(x = TS_interval, y = preds), color = "black") + 
  facet_wrap(~ Time_Interval, labeller = "label_both") 

ggplot(data = crime_mod) +
  geom_line(mapping = aes(x = TS_interval, y = Num_Crimes)) +
  geom_line(mapping = aes(x = TS_interval, y = preds), color = "blue") 

(preds - y)^2 %>% mean() %>% sqrt()
sd(y)

# ACP - auto regressive conditional poisson

fts <- crime_mod %>% 
  select(temp, feelslike, suntime, is_FullMoon_BLOCK, Num_Crimes)
fts <- crime_mod %>% 
  select(Num_Crimes, temp, suntime, is_FullMoon_BLOCK)


ar_pois <- acp(Num_Crimes ~ ., p = 4, q = 1, data = fts)
ar_pois <- acp(X, y, p = 4, q = 2, startval = NULL, varopt=TRUE)
# ar_pois <- acp(Num_Crimes ~ . -Date-Day-Month-Year-Holiday-preds, p = 2, q = 1,
#                data = crime_num)
ar_pois_summary <- summary(ar_pois)
ar_pois_summary$coefficients[,c(1,4)] %>% round(digits = 4)

preds <- ar_pois$fitted.values

acf(ar_pois$residuals, lag.max = nrow(crime_mod))

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

# Cross-Validate on ACP????

# scores for the first observation
# i = 105
pred_test <- preds
y_test  <- y

# logarithmic (equivalent to deviance score up to a constant) 
dev_score = -log(dpois(y_test, lambda=pred_test))

ggplot() +
  geom_line(mapping = aes(x = TS_interval, y = y), col = "black") +
  geom_line(mapping = aes(x = TS_interval, y = pred_test), col = "blue") +
  geom_line(mapping = aes(x = TS_interval, y = dev_score), color = "red")


