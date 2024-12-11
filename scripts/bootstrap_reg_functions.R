load_boostrap_reg_libraries <- function(){
  library(broom)
  library(modelr)
  library(purrr)
}

#initialize bootstrap
initialize_bootstrap <- function(df, n_samples, target){
  boot_formula <- as.formula(paste(target, "~.", sep = ""))
  
  boot <- bootstrap(df, n_samples)
  mods <- map(boot$strap, ~lm(boot_formula, data = .))
  
  return(mods)
}

generate_bootstrap_models <- function(models){
  models_boot <- map_df(mods, tidy, .id = "id")
  
  return(models_boot)
}

calc_95_perc_int <- function(df, exclude_cols, mods_boot){
  #' @param df data.frame. Data frame used for mgp
  #' @param exclude_cols vector. Vector of column names to exclude (should be date and clcsHPI)
  #' @param mods_boot data.frame. Data frame of output data of bootstrapped models.
  
  col_names <- colnames(df[,!names(df) %in% exclude_cols])
  
  boot_res <- data.frame(name = character(),
                         lower_bound = double(),
                         upper_bound = double())
  
  for (i in 1:length(col_names)){
    x <- mods_boot %>%
      filter(term == col_names[i]) %>%
      summarize(name = col_names[i],
                lower_bound = quantile(estimate, 0.025),
                upper_bound = quantile(estimate, 0.975))
    
    boot_res <- rbind(boot_res, x)
  }
  
  boot_res <- boot_res %>%
    mutate(lower_bound = as.numeric(lower_bound),
           upper_bound = as.numeric(upper_bound))
  
  return (boot_res)
}


