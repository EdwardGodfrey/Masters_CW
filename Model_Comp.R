###############################################################################
# ML model comparison 
# Author: Edward Godfrey 
# Date: 06/06/2025
# Email: edwardgodfrey25@gmail.com | yy24665@bristol.ac.uk
# Note: 
###############################################################################

split_ts   <- initial_time_split(df_de_Hei, prop = 0.8)
train_data <- training(split_ts)
test_data  <- testing(split_ts)

cross_val <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  savePredictions = "final", 
  verboseIter = FALSE
)

predictor_formula <- GPP_NT_VUT_MEAN ~
  SW_IN_F_MDS +
  TA_F_MDS +
  temp_lag +
  VPD_F_MDS +
  VPD_lag +
  cumulative_cwd_7day

set.seed(77)

# Random Forest model 
rf_model <- train(
  predictor_formula,
  data = train_data,
  method = "rf",
  tuneGrid = expand.grid(mtry = c(2, 3, 4, 5)),
  ntree = 500,
  metric = "RMSE",
  trControl = cross_val
)

# XGBoost Model
xgboost_model <- train(
  predictor_formula,
  data     = train_data,
  method   = "xgbTree",
  tuneGrid = expand.grid(
    nrounds = c(100, 200, 500),
    max_depth = c(4, 6, 8),
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  ),
  metric    = "RMSE",
  trControl = cross_val
)

# GAM
gam_model <- train(
  predictor_formula,
  data      = train_data,
  method    = "gam",
  preProcess = c("center", "scale"),
  metric    = "RMSE",
  trControl = cross_val
)

# PLSR
plsr_model <- train(
  predictor_formula,
  data       = train_data,
  method     = "pls",
  preProcess = c("center", "scale"),
  tunelength = 5,
  trControl  = cross_val,
  metric     = "RMSE"
)

# neural network 
nn_fit <- train(
  predictor_formula,
  data       = train_data,
  method     = "nnet",
  preProcess = c("center", "scale"),
  tuneGrid   = expand.grid(
    size  = c(5, 10),    # 5 or 10 hidden units
    decay = c(0.01, 0.1) # weight decay (L2 penalty)
  ),
  linout     = TRUE,     # for regression
  trace      = FALSE,    # turn off iterative output
  maxit      = 500,      # max training iterations
  metric     = "RMSE",
  trControl  = cross_val
)

plot(nn_fit)
prediction_rf  <- predict(rf_model,  newdata = test_data)
prediction_xgb <- predict(xgboost_model, newdata = test_data)
prediction_gam <- predict(gam_model, newdata = test_data)
prediction_plsr <- predict(plsr_model, newdata = test_data)
prediction_nn  <- predict(nn_fit, newdata = test_data)
observed_test <- test_data$GPP_NT_VUT_MEAN

performance <- rbind(
  RF  = postResample(prediction_rf,  observed_test),
  XGB = postResample(prediction_xgb, observed_test),
  GAM = postResample(prediction_gam, observed_test),
  PLSR = postResample(prediction_plsr, observed_test),
  NN = postResample(prediction_nn, observed_test)
)
print(performance)
