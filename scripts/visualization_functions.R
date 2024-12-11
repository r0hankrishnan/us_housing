#Define global variable for palette
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


## Loading libraries used for visualization
load_viz_libraries <- function(){
  
  library(tidyverse)
  library(gridExtra)
  library(grid)
  library(ggthemes)
  library(ggcorrplot)
  library(lubridate)
  library(flextable)
  library(ggpubr)
  
}

## Load visualization data from ../data/cleaned/visualizationData.csv
load_viz_data <- function(){
  
  if (!require(tidyverse)){
    stop("tidyverse not installed.")
  } else{
    load_data <- read.csv("../data/cleaned/visualizationData.csv") 
    
    # Assert types
    load_data$DATE <- as.Date(load_data$DATE)
    load_data$year <- lubridate::year(load_data$DATE)
    load_data$month <- lubridate::month(load_data$DATE)
    
    return(load_data)
    
  }
}

## Side by side GDP
plot_GDP_side_by_side <- function(viz_data){
  if (!require(tidyverse, ggthemes, grid, gridExtra)){
    stop("tidyverse, ggthemes, grid, gridExtra, lubridate not installed.")
  } else{
    
    #Generate plot 1
    gdp_plot <- viz_data %>%
      ggplot(aes(x = DATE, y = GDP.x)) +
      geom_point(cex = 1) + 
      geom_smooth(se = FALSE, 
                  color = palette[3], alpha = 0.1, 
                  formula = y ~ x, method = "loess") +
      geom_vline(xintercept = as.Date("2020-03-15"),
                 linetype = 2, linewidth = 0.50,
                 color = palette[2]) +
      geom_vline(xintercept = as.Date("2008-09-15"),
                 linetype = 2, linewidth = 0.50,
                 color = palette[2]) +
      annotate(x = ymd("2020-03-15"), y = +Inf,
               label = "COVID-19", vjust = 2, 
               geom = "label") +
      annotate(x = ymd("2008-09-15"), y = +Inf,
               label = "Financial Crisis",
               vjust = 2, geom = "label") +
      labs(x = "", y = "GDP (Quarterly)",
           title = "",
           subtitle = "Reported Quarterly \n(Smoothed Est. in Orange)") +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold", size = 25, hjust = 0),
            plot.subtitle = element_text(face = "italic", size = 10, hjust = 0))
    
    #Generate plot 2
    impute_gdp_plot <- viz_data %>%
      ggplot(aes(x=DATE, y=imputeGDP)) +
      geom_point(cex = 1) + 
      geom_vline(xintercept = as.Date("2020-03-15"),
                 linetype = 2, linewidth = 0.50, 
                 color = palette[2]) +
      geom_vline(xintercept = as.Date("2008-09-15"),
                 linetype = 2, linewidth = 0.50, 
                 color = palette[2]) + 
      annotate(x = ymd("2020-03-15"), y = +Inf, 
               label = "COVID-19", vjust = 2, 
               geom = "label") +
      annotate(x = ymd("2008-09-15"), y = +Inf, 
               label = "Fin. Crisis", vjust = 2, 
               geom = "label") +
      labs(x = "", y = "GDP (Imputed from Quarterly Values)",
           title = " ", 
           subtitle = "Monthly (Imputed)") + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 25, hjust = 0),
            plot.subtitle = element_text(face = "italic", size = 10, hjust = 0))
    
    #Organize plots on grid
    grid_plot <- suppressWarnings(grid.arrange(gdp_plot, impute_gdp_plot, nrow = 1,
                                               top = text_grob(
                                                 "Non-Imputed (Left) vs Imputed (Right) U.S. GDP", 
                                                 size = 15,
                                                 face = "bold"))) 
    
    
    return(grid_plot)
    
  }
}

## Plot hpi values
plot_hpi_values <- function(viz_data, date_col, hpi_col, ff_rt_col){
  if (!require(tidyverse, ggthemes)){
    stop("tidyverse, ggthemes not installed")
  } else{
    hpi_plot <- viz_data %>%
      ggplot(aes(x = viz_data[,date_col], y = viz_data[,hpi_col], 
                 color = viz_data[,ff_rt_col])) + 
      geom_line(aes(linewidth = viz_data[,ff_rt_col]), lineend = "round", na.rm = TRUE) + 
      geom_vline(xintercept = as.Date("2020-03-15"),
                 linetype = 2, linewidth = 0.50, color = palette[2]) +
      geom_vline(xintercept = as.Date("2008-09-15"),
                 linetype = 2, linewidth = 0.50, color = palette[2]) + 
      annotate(x = ymd("2020-03-15"), y = + Inf, 
               label = "COVID-19 Starts", vjust = 2, 
               geom = "label") +
      annotate(x = ymd("2008-09-15"), y = + Inf, 
               label = "Peak of Financial Crisis", 
               vjust = 2, geom = "label") +
      labs(x = "", y = "National Home Price Index",
           title = "U.S. National Home Price Index over Time", 
           subtitle = "Changes in Federal Funds Rate shown using color and size") + 
      theme_bw() +
      scale_color_gradient(name = "Federal Funds Rate",high = palette[2], low = palette[3]) +
      scale_linewidth(name = "Federal Funds Rate", range = c(0.5,2.5)) +
      guides(color=guide_legend(override.aes=list(fill=NA))) + 
      theme(plot.title = element_text(face = "bold", size = 25, hjust = 0),
            plot.subtitle = element_text(face = "italic", size = 10, hjust = 0),
            legend.key = element_rect(fill = "transparent"),
            legend.title = element_text(size = 10, face = "bold"),
            legend.background = element_rect(linewidth = 0, linetype = "blank"))
    
    return(hpi_plot)
    
  }
}

##Line plot matrix -- CAN'T USE
plot_line_matrix <- function(viz_data){
  if (!require(tidyverse, ggthemes, grid, gridExtra)){
    stop("tidyverse, ggthemes, grid, gridExtra not installed.")
  } else{
    
    #Initialize list of plots and index
    plots <- list()
    ind = 1
    
    #For loop to create graphs for all columns except 1 and 18
    for(i in 1: ncol(viz_data)){
      
      #Skip column 1
      if (i == 1){
        
        #print("pass")

      } 
      
      #Special plot for column 4
      else if (i == 4){
        plots[[ind]] <- ggplot(viz_data, aes_string(x = viz_data[,1], y = viz_data[,i])) +
          geom_point(na.rm = TRUE) + 
          labs(x = "", y = colnames(viz_data)[i]) +   
          geom_vline(xintercept = as.Date("2020-03-15"),
                     linetype = 2, linewidth = 0.50, color = palette[2]) +
          geom_vline(xintercept = as.Date("2008-09-15"),
                     linetype = 2, linewidth = 0.50, color = palette[2]) + 
          # annotate(x = ymd("2020-03-15"), y = +Inf, 
          #          label = "C19", vjust = 2, geom = "label") +
          # annotate(x = ymd("2008-09-15"), y = +Inf, 
          #          label = "Fin. Crisis", vjust = 2, geom = "label") + 
          theme_bw()
        ind = ind + 1
        
      } 
      
      #Skip column 18
      else if (i == 18){
        
        #print("pass")
        
      } 
      
      #Create standard line plot
      else{
        plots[[ind]] <- ggplot(viz_data, aes_string(x = viz_data[,1], y = viz_data[,i])) +
          geom_line(na.rm = TRUE) + 
          labs(x = "", y = colnames(viz_data)[i]) +
          geom_vline(xintercept = as.Date("2020-03-15"),
                     linetype = 2, linewidth = 0.50, color = palette[2]) +
          geom_vline(xintercept = as.Date("2008-09-15"),
                     linetype = 2, linewidth = 0.50, color = palette[2]) + 
          # annotate(x = ymd("2020-03-15"), y = +Inf, 
          #          label = "C19", vjust = 2, geom = "label") +
          # annotate(x = ymd("2008-09-15"), y = +Inf, 
          #          label = "Fin. Crisis", vjust = 2, geom = "label") + 
          theme_bw()
        ind = ind + 1
      }
    }
    
    #Organize plot matrix
    line_matrix <- grid.arrange(grobs = plots, nrow = 4, 
                       top = textGrob(
                         substitute(
                           paste(
                             bold("Numeric Variables by Year")))))
    
    return(line_matrix)
    
  }
}

## HPI and CPI over time
plot_hpi_cpi <- function(viz_data){
  if (!require(tidyverse, ggthemes)){
    
    stop("tidyverse, ggthemes not installed")
    
  } else{
    
    hpi_cpi_plot <- viz_data %>%
      select(year, CSUSHPISA, CPIAUCSL) %>%
      na.omit() %>%
      group_by(year) %>%
      summarize(avgHPI = mean(CSUSHPISA),
                avgCPI = mean(CPIAUCSL)) %>%
      ggplot() +
      geom_bar(aes(x = year, y = avgHPI, 
                   fill = factor(year, order = TRUE, levels = c(unique(viz_data$year)))), 
               stat = "identity", na.rm = TRUE) +
      scale_fill_manual(values = year_palette) +
      geom_line(aes(x = year, y = avgCPI, color = year), na.rm = TRUE) + 
      scale_y_continuous(name = "Average HPI",
                         sec.axis = sec_axis(trans = ~., name = "Average CPI")) + 
      labs(x = "", y = "Average HPI", fill = "Year") + 
      scale_color_gradient(low = palette[1], high = palette[4]) + 
      guides(color = "none", fill = guide_legend(nrow = 10)) + 
      theme_bw() + 
      theme(axis.title.y = element_text(size = 7))
    
    return(hpi_cpi_plot)
    
  }
}

## Correlation plot
plot_correlations <- function(viz_data){
  
  if (!require(tidyverse, ggthemes, ggcorrplot)){
    
    stop("tidyverse, ggthemes, ggcorrplot not installed.")
    
  } else{
    
    numeric_vars <- c()
    for (i in colnames(viz_data)){
      
      if (i == "imputedYN"){
        next
      }
      
      if (is.numeric(viz_data[,i]) == TRUE){
        numeric_vars <- append(numeric_vars, i)
      }
    }
    
    correlations <- cor(na.omit(viz_data[,c(numeric_vars)]))
    
    corr_plot <- ggcorrplot(corr =  correlations, colors = c(palette[1], palette[2], palette[3]),
                        ggtheme = "theme_bw", title = "Correlations", tl.cex = 8, legend.title = "") +
      theme(plot.title = element_text(face = "bold"))
    
    return(corr_plot)
  }
}

## Create important variable df
filter_imp_vars <- function(viz_data){
  
  if (!require(tidyverse, flextable)){
    
    stop("tidyverse, flextable not installed.")
    
  } else{
    
    numeric_vars <- c()
    
    for (i in colnames(viz_data)){
      
      if(i == "imputedYN"){
        
        next
        
      } else if (is.numeric(viz_data[,i]) == TRUE){
        
        numeric_vars <- append(numeric_vars, i)
      }
    }
    
    correlations <- cor(na.omit(viz_data[,c(numeric_vars)]))
    correlations_df <- data.frame(correlations)
    
    imp_vars <- correlations_df %>% 
      select(CSUSHPISA) %>%
      filter(abs(CSUSHPISA) > 0.50)
    
    imp_vars <- imp_vars %>%
      rename(hpi = CSUSHPISA) %>%
      mutate(name = rownames(imp_vars)) %>%
      relocate(name)
      
      return(imp_vars)
  }
}

## Line matrix of imp variables
plot_imp_line_matrix <- function(viz_data, imp_vars){
  filtered_data <- viz_data[,c(unique(imp_vars$name))]
  
  plots <- list()
  ind = 1
  hpi_idx <- which(colnames(filtered_data) == "CSUSHPISA")
  
  for (i in 1:ncol(filtered_data)){
    plots[[ind]] <- ggplot(filtered_data, 
                           aes_string(x = filtered_data[,i], 
                                      y = filtered_data[,hpi_idx])) +
      geom_point(na.rm = TRUE) + 
      geom_smooth(na.rm = TRUE, se = FALSE, color = palette[3], 
                  formula = y ~ x, method = "loess", alpha = 0.50) +
      labs(x = colnames(filtered_data)[i], y = "HPI") + 
      theme_bw()
    
    ind = ind + 1
  }
  
  grid.arrange(grobs = plots, nrow = 4, 
               top = textGrob(
                 substitute(
                   paste(
                     bold("Variables with absolute correlation > 0.50 with HPI")))))
}

