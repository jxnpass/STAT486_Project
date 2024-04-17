rmse <- function(truth, preds) {
 eval_rmse = (truth - preds)^2 %>% mean() %>% sqrt()
 return(eval_rmse)
}

mae = function(truth, preds) {
  eval_mae = abs(truth - preds)
  return(eval_rmse)
}

var_red <- function(truth, preds) {
  r_squared = cor(truth, preds)^2
  return(r_squared)
}

poislm_CV <- function(df, prop = .8, ncv = 5, type = "vs"){
  
  rmse = rep(NA, times = ncv)
  var_red = rep(NA, times = ncv)
  
  for (cv in 1:ncv){
    train_size = prop * nrow(df)
    train_idx <- sample(1:nrow(df), size = train_size)
    train <- df[train_idx,]
    test <- df[-train_idx,]
    
    if (type == "vs") {
      pois_lm <- glm(Num_Crimes ~ Time_Interval + . -Date-TS_interval-Day-Month-Year, data = train, family = "poisson")
    }
    else{
      pois_lm <- glm(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year, data = train, family = "poisson")
    }
    
    # predictions
    preds = predict.glm(pois_lm, newdata = test) %>% exp()
    rmse[cv] = rmse(test$Num_Crimes, preds)
    var_red[cv] = var_red(test$Num_Crimes, preds)
  }
  
  metrics = data.frame(rmse, var_red)
  return(metrics)
}


normlm_CV <- function(df, prop = .8, ncv = 5, type = "vs"){
  
  rmse = rep(NA, times = ncv)
  var_red = rep(NA, times = ncv)
  
  for (cv in 1:ncv){
    train_size = prop * nrow(df)
    train_idx <- sample(1:nrow(df), size = train_size)
    train <- df[train_idx,]
    test <- df[-train_idx,]
    
    if (type == "vs") {
      norm_lm <- lm(Num_Crimes ~ Time_Interval + . -Date-TS_interval-Day-Month-Year, data = train)
    }
    else{
      norm_lm <- glm(Num_Crimes ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year, data = train)
    }
    
    # predictions
    preds = predict.glm(norm_lm, newdata = test)
    rmse[cv] = rmse(test$Num_Crimes, preds)
    var_red[cv] = var_red(test$Num_Crimes, preds)
  }
  
  metrics = data.frame(rmse, var_red)
  return(metrics)
}

logreg_CV <- function(df.v, df, prop = .8, ncv = 5, type = 'vs') {
  
  df.v$y <- df.v$Num_Crimes / df$Num_Crimes
  
  rmse = rep(NA, times = ncv)
  var_red = rep(NA, times = ncv)
  
  for (cv in 1:ncv){
    train_size = prop * nrow(df.v)
    train_idx <- sample(1:nrow(df.v), size = train_size)
    train <- df.v[train_idx,]
    test <- df.v[-train_idx,]
    w <- train$Num_Crimes
    
    
    if (type == "vs") {
      logreg <- glm(y ~ Time_Interval + . -Date-TS_interval-Day-Month-Year-Num_Crimes, 
                    family = binomial(link = 'logit'), data = train, weights = w)
    }
    if (type == "std") {
      logreg <- glm(y ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-Num_Crimes, 
                    family = binomial(link = 'logit'), data = train, weights = w)
    }
    if (type == "pca") {
      logreg <- glm(y ~ Holiday:Time_Interval + . -Date-TS_interval-Day-Month-Year-Num_Crimes-preciptype, 
                    family = binomial(link = 'logit'), data = train, weights = w)
    }
    
    # predictions
    preds = predict.glm(logreg, newdata = test)
    rmse[cv] = rmse(test$y, preds)
    var_red[cv] = var_red(test$y, preds)
  }
  
  metrics = data.frame(rmse, var_red)
  return(metrics)
  
  
}






