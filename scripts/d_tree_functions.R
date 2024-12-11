load_dt_libraries <- function(){
  library(rpart)
  library(rpart.plot)
  library(purrr)
  library(tidyverse)
}


#Perform grid search for optimal cp and minsplit
grid_tree <- expand.grid(
  minsplit = seq(5,20,1),
  maxdepth = seq(6,12,1)
)

#Iterate through grid and generate a tree for each combination of hyperparameter
get_dt_hyperparameters <- function(grid_tree, train_df){
  model_list_tree <- list()
  
  for (i in 1:nrow(grid_tree)){
    minsplit <- grid_tree$minsplit[i]
    maxdepth <- grid_tree$maxdepth[i]
    
    set.seed(100)
    model_list_tree[[i]]<- rpart(clcsHPI~., data = train_df, method = "anova",
                                 control = list(minsplit = minsplit, maxdepth = maxdepth))
  }
  
  return (model_list_tree)
  
}

# Helper functions
get_best_cp_ <- function(y){
  min <- which.min(y$cptable[,"xerror"])
  cp <- y$cptable[min, "CP"]
  return (cp)
}

get_min_error_ <- function(y){
  min <- which.min(y$cptable[,"xerror"])
  xerror <- y$cptable[min, "xerror"]
  return (xerror)
}

create_optimal_table_ <- function(grid_tree, model_list_tree){
  
  top_tbl <- grid_tree %>%
    mutate(
      cp = map_dbl(model_list_tree, get_best_cp_),
      error = map_dbl(model_list_tree, get_min_error_)
    ) %>%
    arrange(error) %>%
    top_n(-5, wt = error)
  
  return (top_tbl)
}


#Find optimal values
find_optimum_minsplit <- function(grid_tree, model_list_tree){
  top_tbl <- create_optimal_table_(grid_tree = grid_tree, model_list_tree = model_list_tree)
  
  return(top_tbl$minsplit[1])
}

find_optimum_cp <- function(grid_tree, model_list_tree){
  top_tbl <- create_optimal_table_(grid_tree = grid_tree, model_list_tree = model_list_tree)
  
  return(top_tbl$cp[1])
}

