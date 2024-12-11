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

#Load libraries needed for report
load_preprocessing_libraries <- function(){
  
  #Updated as code is re-factored
  library(tidyverse)
  library(ggthemes)
  library(flextable)
  library(stargazer)
  library(skimr)
  library(imputeTS)
  library(lubridate)
  
}

#Load raw data
load_raw_data <- function(){
  temp <- list.files(path = "../data/raw-data/")
  
  macro_data <- read.csv(file = paste("../data/raw-data/", temp[1], sep = ""))
  
  # Left join all other data (except growth) onto macro_data bc macro data has
  # the most recent data
  for (i in temp[c(2:6,8)]){
    
    path <- paste("../data/raw-data/", i, sep = "")
    temp_df <- read.csv(file = path)
    macro_data <- merge(macro_data, temp_df, by = "DATE", all.x = TRUE)
    
  }
  
  #Convert "DATE" column to Date type to ensure type accuracy
  macro_data$DATE <- as.Date(macro_data$DATE)
  
  return (macro_data)
}


# Load preliminary data
load_preliminary_data <- function(){
  
  if (!require(tidyverse)){
    
    stop("tidyverse not installed.")
    
  } else{
    
    temp <- list.files(path = "../data/raw-data/")
    
    macro_data <- read.csv(file = paste("../data/raw-data/", temp[1], sep = ""))
    
    # Left join all other data (except growth) onto macro_data bc macro data has
    # the most recent data
    for (i in temp[c(2:6,8)]){
      
      path <- paste("../data/raw-data/", i, sep = "")
      temp_df <- read.csv(file = path)
      macro_data <- merge(macro_data, temp_df, by = "DATE", all.x = TRUE)
      
    }
    
    #Convert "DATE" column to Date type to ensure type accuracy
    macro_data$DATE <- as.Date(macro_data$DATE)
    
    #Filter data to only be after 2003 bc most data before that was NA
    post_2003 <- macro_data %>%
      filter(DATE >= "2003-01-01")
    
    return(post_2003)
    
  }
}


# Display dynamic sentence describing input data
basic_describe_data <- function(data, data_name){
  
  ncols <- ncol(data)
  min_date <- min(data[,"DATE"])
  max_date <- max(data[,"DATE"])
  max_date_str <- paste(max_date, ",", sep = "")
  nrows <- nrow(data)
  na_values <- sum(is.na(data))

  write(paste("The", data_name, "data set has", ncols, "columns,", nrows, 
              "rows, spans", min_date, "to", max_date_str, "\nand has", 
              na_values, "NA values."), stdout())
  
}


#Extract na values per column
extract_na_per_column <- function(data){
  if (!require(tidyverse)){
    
    stop("tidyverse not installed.")
    
  } else{
    na_vals <- tibble:: rownames_to_column(
      data.frame(
        na_values = colSums(
          is.na(data))
      ), 
      "variable")
    
    return (na_vals)
  }
}


# Display preliminary data in a flextable-- CAN'T USE
display_preliminary_table <- function(preliminary_data){
  
  if (!require(tidyverse, flextable, skimr)){
    
    stop("tidyver, flextable, and skimr not installed.")
    
  } else{
    
    table <- preliminary_data %>%
      skimr:: skim() %>%
      dplyr::select(skim_type, skim_variable, n_missing, numeric.hist) %>%
      flextable::flextable()%>%
      set_caption(
        caption = as_paragraph(
          as_chunk("Table 1: Structure of Intermediate Data",
                   props = fp_text_default(font.family = "Times New Roman",
                                           font.size = 12,
                                           italic = TRUE)))) %>%
      set_header_labels(values = list(
        skim_type = "Type",
        skim_variable = "Name",
        n_missing = "NA values",
        numeric.hist = "Histogram")) %>%
      set_table_properties(layout = "autofit")%>%
      bg(i = seq(2,ncol(preliminary_data),2), bg = "light grey")%>%
      font(part = "all", fontname = "Times New Roman") %>%
      fontsize(size = 12) %>%
      border(i = c(1:ncol(preliminary_data)), 
             border.top = fp_border_default(color = "black"))
    
    return(table)
    
  }
}


# Plot GDP before imputation
plot_pre_imputed_GDP <- function(preliminary_data){
  
  if (!require(tidyverse, ggthemes)){
    
    stop("tidyverse and ggthemes not installed.")
    
  } else{
    
    min_date <- as.Date(min(preliminary_data$DATE))
    max_date <- as.Date(max(preliminary_data$DATE))
    date_limit <- c(min_date, max_date)
    
    plot <- preliminary_data %>%
      ggplot(aes(x = DATE, y = GDP.x)) +
      geom_point(na.rm = T, cex = 1) + 
      labs(x = "", y = "U.S. GDP (Reported Quarterly)",
           title = "GDP Values Before Imputation") +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_x_date(limit = date_limit)
    
    return(plot)
    
  }
}

#Impute GDP
impute_GDP <- function(preliminary_data){
  if (!require(tidyverse, imputeTS)){
    
    stop("tidyverse and imputeTS not installed.")
    
  } else{
    
    GDP <- preliminary_data$GDP.x
    impute_GDP <- suppressWarnings(imputeTS::na_seadec(GDP))
    
    
    return(impute_GDP)
  }
}

#Determine if a value was imputed or not
create_imputed_column <- function(imputed_preliminary_data, 
                                  non_imputed_col, imputed_col){
  
  if (!require(tidyverse)){
    
    stop("tidyverse not installed.")
    
  } else{
    
    imputed_preliminary_data$imputed <- as.factor(
      ifelse(
        is.na(
          ifelse(imputed_preliminary_data[,non_imputed_col] 
                 == imputed_preliminary_data[,imputed_col], 
                 0, 1)
        ) 
        == T, 1,0))
    
    return(imputed_preliminary_data)
    
  }
}

# Graph imputed GDP
plot_imputed_GDP<- function(imputed_preliminary_data,
                            date_col, imputed_GDP_col, imputed_cat_col){
  
  if (!require(tidyverse, ggthemes)){
    
    stop("tidyverse and ggthemes not installed.")
    
  } else{
    temp_data <- imputed_preliminary_data
    # Rename columns so that plotting function can be easily used
    colnames(temp_data)[
      colnames(temp_data) == date_col] <- "DATE"
    colnames(temp_data)[
      colnames(temp_data) == imputed_GDP_col] <- "impute_GDP"
    colnames(temp_data)[
      colnames(temp_data) == imputed_cat_col] <- "imputed"
    
    #Define min and max dates
    min_date <- min(temp_data$DATE)
    max_date <- max(temp_data$DATE)
    date_limit <- c(as.Date(min_date), as.Date(max_date))
    
    # Plot shows imputed GDP values in a different color and draws a line
    # through points
    plot<- temp_data %>%
      ggplot(aes(x = DATE, y = impute_GDP, color = imputed)) + 
      geom_point(na.rm = T, cex = 1, alpha = 0.50) + 
      geom_line(aes(x = DATE, y = impute_GDP), na.rm = T, alpha = 1) + 
      labs(x = "", y = "U.S. GDP (Monthly Imputed)",
           title = "Imputed Monthly U.S. GDP From Quarterly Values",
           color = "Imputed") +
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"),
        axis.text.x=element_text(angle = 60, hjust = 1)) +
      scale_x_date(limit= date_limit) +
      scale_color_manual(labels = c("No", "Yes"), values = c(palette[1], palette[3]))
    
    return(plot)
    
  }
}

# Display table of NA values after most pre-processing & renaming -- CAN'T USE
display_intermediate_nas <- function(intermediate_data){
  
  if (!require(tidyverse)){
    stop("tidyverse not installed.")
  } else{
    
    #Create NA df
    na_vals <- tibble:: rownames_to_column(
      data.frame(
        na_values = colSums(
          is.na(post_2003_model))
        ), 
      "variable")
    
    #Display as flex table
    tbl <- na_vals %>%
      flextable:: flextable() %>%
      set_caption(
        caption = as_paragraph(
          as_chunk("Table 2: Remaining NA Values After Processing",
                   props = fp_text_default(font.family = "Times New Roman",
                                           font.size = 12,
                                           italic = TRUE)))) %>%
      set_header_labels(values = list(
        variable = "Variable",
        na_values = "Number of Missing Values")) %>%
      set_table_properties(layout = "autofit") %>%
      bg(i = seq(2, nrow(na_vals), 2), bg = "light grey")%>%
      font(part = "all", fontname = "Times New Roman") %>%
      fontsize(size = 12) %>%
      border(i = c(1:nrow(na_vals)), border.top = fp_border_default(color = "black"))
    
    return(tbl)
  }
}

#Final pre-processing - filter out NA, extract month and year
final_preprocessing <- function(intermediate_data){
  if (!require(tidyverse, lubridate)){
    
    stop("tidyverse and lubridate not installed")
    
  } else{
    temp_data <- na.omit(intermediate_data)
    
    #Add date features -- don't need day since monthly data
    temp_data$year <- lubridate::year(temp_data$date)
    temp_data$month <- lubridate::month(temp_data$date)
    
    return(temp_data)
    }
}

