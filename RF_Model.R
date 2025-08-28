###############################################################################
# Function to train, cross validate and test a random forest for GPP prediction
# Author: Edward Godfrey
# Date: 06/06/2025
# Email: edwardgodfrey25@gmail.com | yy24665@bristol.ac.uk
###############################################################################

site_comp_tsg <- function(df, site_name){
  
  split_ts   <- initial_time_split(df, prop = 0.8)
  train_data <- training(split_ts)
  test_data  <- testing(split_ts)
  
  set.seed(77)
  cross_val <- trainControl(
    method          = "repeatedcv",
    number          = 5,
    repeats         = 3,
    savePredictions = "final"
  )
  
  set.seed(77)
  rf_fit <- train(
    GPP_NT_VUT_MEAN ~ SW_IN_F_MDS + TA_F_MDS + temp_lag +
      VPD_F_MDS + VPD_lag + cumulative_cwd_7day,
    data      = train_data,
    method    = "rf",
    tuneGrid  = expand.grid(mtry = 2:6),
    ntree     = 500,
    metric    = "RMSE",
    trControl = trainControl(method = "repeatedcv",
                             number = 5, repeats = 3,
                             savePredictions = "final")
  )
  
  preds <- predict(rf_fit, newdata = test_data)
  stats <- postResample(preds, test_data$GPP_NT_VUT_MEAN)
  
  tibble(
    site      = site_name,
    datetime  = test_data$DATE,        
    observed  = test_data$GPP_NT_VUT_MEAN,
    predicted = preds,
    RMSE     = stats["RMSE"],
    Rsquared = stats["Rsquared"],
    MAE      = stats["MAE"]
  )
  
}