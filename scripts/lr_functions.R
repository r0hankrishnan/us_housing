load_lr_libraries <- function(){
  library(tidyverse)
  library(ggthemes)
  library(stargazer)
  library(ggpubr)
}

#Plot residuals given a modal
plot_residuals <- function(model, title){
  
  #Create residual df
  df_residuals <- data.frame("residuals" = model$residuals)
  df_residuals$index = c(1:nrow(df_residuals))
  
  #Plot
  residual_plot <- df_residuals %>%
    ggplot(aes(x = index, y = residuals)) +
    geom_point() +
    labs(title = title, subtitle = "",
         x = "Index", y = "Residuals") + 
    geom_hline(yintercept = 0, col = palette[3], linetype = "dashed") + 
    theme_bw() + 
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"))
  
  return (residual_plot)
}

#Create grid plot of two models' residuals
plot_residuals_comparison <- function(model_nf, model_f){
  plot_no_factor <- plot_residuals(model = model_nf, title = "No Factoring")
  
  plot_factor <- plot_residuals(model = model_f, title = "Ordinal Factoring")
  
  title = "Ordinal Factoring of Month and Year Features"
  subtitle = "Factoring (Right) seems to look more random than no factoring (Left)"
  
  plot_compare <- grid.arrange(plot_no_factor, 
                               plot_factor, 
                               nrow = 1, 
                               top = text_grob(title, face = "bold", size = 15),
                               bottom = text_grob(subtitle, face = "italic", size =10))
}


