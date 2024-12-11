
load_lin_mod_select_libraries <- function(){
  library(glmnet)
  library(tidyverse)
}

create_x_matrix <- function(target, train_or_test_df){
  formula_ <- as.formula(paste(target, "~.", sep = ""))
  x_mat <- model.matrix(formula_, train_or_test_df)[,-1]
  
  return (x_mat)
}

create_y_vector <- function(target, train_or_test_df){
  y_vec <- train_or_test_df %>% select(target) %>%
    unlist() %>% as.numeric()
  
  return (y_vec)
}

#Fit Lasso or Ridge CV regression on training data and get optimal lambda

get_best_lambda <- function(x_train, y_train, alpha){
  #' @param alpha int. 1 for Lasso, 0 for Ridge.
  
  set.seed(100)
  model <- cv.glmnet(x_train, y_train, alpha = alpha)
  
  lambda_opt <- model$lambda.min
  
  return (lambda_opt)
}

