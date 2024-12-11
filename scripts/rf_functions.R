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




load_rf_libraries <- function(){
  library(randomForest)
  library(caret)
  library(gridExtra)
  library(tidyverse)
}


plot_rf_var_imp <- function(model_rf, title){
  imp_vars_rf <- as.data.frame(importance(model_rf))
  imp_vars_rf$name <- row.names(imp_vars_rf)
  
  plot <- imp_vars_rf %>%
    arrange(`%IncMSE`, IncNodePurity) %>%
    mutate(Name = factor(name, levels = name)) %>%
    ggplot(aes(x = Name, y = `%IncMSE`)) + 
    geom_segment(aes(x = Name, xend = Name, y = 0, yend = `%IncMSE`), color = palette[1]) +
    geom_point(aes(size = IncNodePurity), color = palette[2], alpha = 0.8) + 
    labs(title = title) + 
    theme_bw() + 
    coord_flip() + 
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
    )
  
  return (plot)
}


tune_rf_get_best_mtry <- function(train_df, target_col, 
                             step_factor, improve, ntree){
  
  set.seed(100)
  
  train_rf_x <- train_df %>% select(-target_col)
  train_rf_y <- train_df %>% select(target_col) %>% unlist() %>% as.numeric()
  
  
  tune_RF_output <- tuneRF(x = train_rf_x, y = train_rf_y, 
                      stepFactor = step_factor, improve = improve, 
                      ntree = ntree, plot = FALSE, trace = TRUE)
  
  
  mtry_best_df <- data.frame(tune_RF_output)
  
  best_idx <- which.min(mtry_best_df$OOBError)
  best_mtry_val <- mtry_best_df$mtry[best_idx]
  
  return (best_mtry_val)
}

grid_search_get_best_mtry <- function(train_df, num, rpts, mtry_range,
                                      plot = FALSE, print = FALSE){
  control <- trainControl(method="repeatedcv", number = num, repeats = 3, search="grid")
  
  set.seed(100)
  
  tune_grid <- expand.grid(.mtry=mtry_range)
  
  forest_grid_search <- train(clcsHPI~., data=train_df, 
                              method="rf", metric="RMSE", 
                              tuneGrid=tune_grid, trControl=control)
  if (plot == TRUE){
    plot(forest_grid_search)
  }
  
  if (print == TRUE){
    print(forest_grid_search)
  }
  
  best_idx <- which.min(forest_grid_search$results$RMSE)
  
  best_mtry_val <- forest_grid_search$results$mtry[best_idx]
  
  return (best_mtry_val)

} 



