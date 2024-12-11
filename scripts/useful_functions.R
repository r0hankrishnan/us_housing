#Global palette variables 
palette <- c("#5F0F40",
             "#9A031E",
             "#FB8B24",
             "#E36414",
             "#0F4C5C")

year_palette <- c("#5F0F40","#67153F","#6F1B3D",
                  "#76223C","#7E283A","#862E39",
                  "#8E3438","#963A36","#9D4135",
                  "#A54733","#AD4D32","#B55331",
                  "#BD592F","#C4602E","#CC662C",
                  "#D46C2B","#DC722A","#E47828",
                  "#EB7F27","#F38525","#FB8B24")

#Take df, split based on parameters, return train and test set to global env
train_test_split <- function(df, propTrain, propTest, 
                             trainName = "train", testName = "test"){
  #Set seed for reproducibility
  set.seed(100)
  
  #Check types and make sure proportion values are within 0 and 1
  stopifnot(is.double(propTrain)==T, is.double(propTest)==T, 
            propTrain<1 & propTrain>0, propTest<1 & propTest>0,
            propTrain + propTest == 1, is.character(trainName), 
            is.character(testName))
  
  #Generate T/F vector of length nrow(df) to sample df
  sample <- sample(c(TRUE,FALSE), nrow(df),  
                   replace=TRUE, prob=c(propTrain,propTest))
  
  #Sample df with .70 T values for train & assign to global env
  assign(trainName, df[sample, ], envir = .GlobalEnv)
  
  #Take rest as test & assign to global env
  assign(testName, df[!sample, ], envir = .GlobalEnv)
}


#Take modelling df, remove dates, convert year and month to factors
model_df_preprocess <- function(df, name = "df"){
  #Unselect date, convert year & month to ordingal factors
  newdf <- df %>%
    select(-date) %>%
    mutate(year = factor(year, ordered = TRUE, levels = c(unique(df$year))),
           month = factor(month, ordered = TRUE, levels = c(unique(df$month))))
  
  #Assign to global env
  assign(name, newdf, envir = .GlobalEnv)
}

#MSE
mse <- function(pred, test_df, target_var){
  mse <- mean((pred - test_df[,target_var])^2)
  return (mse)
}


#RMSE
rmse <- function(pred, test_df, target_var){
  rmse <- sqrt(mean((pred - test_df[,target_var])^2))
  return (rmse)
}

#MAE
mae <- function(pred, test_df, target_var){
  mae <- mean(abs(pred - test_df[,target_var]))
  return (mae)
}

#MAPE
mape <- function(pred, test_df, target_var){
  mape <- mean(abs((pred - test_df[,target_var])/test_df[,target_var]))
  return (mape)
}

#Create empty df for errors
initialize_errors <- function(nrows, row_names){
  #' @param nrows int. The number of rows of the empty errors df
  #' @param row_name vector. A vector of row names, corresponding to what types of errors you are tracking
  
  errors <- data.frame(matrix(data = "", nrow = nrows, ncol = 0))
  rownames(errors) <- row_names
  
  return (errors)
  
}
  
  
  
# Append model performance to errors df
append_errors <- function(errors_df, preds, model_name, test, target){
  
  mse <- mse(pred = preds, test_df = test, target_var = target)
  rmse <- rmse(pred = preds, test_df = test, target_var = target)
  mae <- mae(pred = preds, test_df = test, target_var = target)
  mape <- mape(pred = preds, test_df = test, target_var = target)
  
  errors[[model_name]] <- c(mse, rmse, mae, mape)

  return(errors)
}
