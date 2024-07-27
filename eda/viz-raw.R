library(tidyverse)
library(gridExtra)
library(ggthemes)

data <- read.csv("./data/cleaned/visualizationData.csv")
data$DATE <- as.Date(data$DATE)

palette <- c("#5F0F40",
             "#9A031E",
             "#FB8B24",
             "#E36414",
             "#0F4C5C")

#Visualize GDP imputation
p1 <- data %>%
  ggplot(aes(x=DATE, y=GDP.x)) +
  geom_point(cex = 1) + 
  geom_smooth(se = F, color = palette[3]) + 
  geom_vline(xintercept = as.Date("2020-03-15"),
             linetype = 2, linewidth = 0.50, color = palette[2]) +
  geom_vline(xintercept = as.Date("2008-09-15"),
             linetype = 2, linewidth = 0.50, color = palette[2]) + 
  annotate(x = ymd("2020-03-15"), y = +Inf, label = "COVID-19 Starts", vjust = 2, geom = "label") +
  annotate(x = ymd("2008-09-15"), y = +Inf, label = "Peak of Financial Crisis", vjust = 2, geom = "label") +
  labs(x = "", y = "GDP (Quarterly)",
       title = "US GDP", 
       subtitle = "Reported quarterly (smoothed estimate in orange)") + 
  ggthemes::theme_igray() +
  theme(plot.title = element_text(face = "bold", size = 25, hjust = 0),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = 0))

#Graph GDP and color in imputed data -- looks like it did pretty good!
p2 <- data %>%
  ggplot(aes(x=DATE, y=imputeGDP)) +
  geom_point(cex = 1) + 
  geom_vline(xintercept = as.Date("2020-03-15"),
             linetype = 2, linewidth = 0.50, color = palette[2]) +
  geom_vline(xintercept = as.Date("2008-09-15"),
             linetype = 2, linewidth = 0.50, color = palette[2]) + 
  annotate(x = ymd("2020-03-15"), y = +Inf, label = "COVID-19 Starts", vjust = 2, geom = "label") +
  annotate(x = ymd("2008-09-15"), y = +Inf, label = "Peak of Financial Crisis", vjust = 2, geom = "label") +
  labs(x = "", y = "GDP (Monthly Imputed from Quarterly Values)",
       title = "US GDP", 
       subtitle = "Monthly (Imputed)") + 
  ggthemes::theme_igray() +
  theme(plot.title = element_text(face = "bold", size = 25, hjust = 0),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = 0))

p3 <- grid.arrange(p1, p2, nrow = 1)

#Save to assets directory
ggsave("gdp-imputed.png",
       plot = p3,
       path = "./assets/",
       width = 14.5,
       height = 5.5,
       units = "in")

#Plot hpi values
p4 <- data %>%
  ggplot(aes(x = DATE, y = CSUSHPISA, color = FEDFUNDS)) + 
  geom_line(aes(linewidth = FEDFUNDS), lineend = "round") + 
  geom_vline(xintercept = as.Date("2020-03-15"),
           linetype = 2, linewidth = 0.50, color = palette[2]) +
  geom_vline(xintercept = as.Date("2008-09-15"),
             linetype = 2, linewidth = 0.50, color = palette[2]) + 
  annotate(x = ymd("2020-03-15"), y = +Inf, 
           label = "COVID-19 Starts", vjust = 2, geom = "label") +
  annotate(x = ymd("2008-09-15"), y = +Inf, 
           label = "Peak of Financial Crisis", vjust = 2, geom = "label") +
  labs(x = "", y = "National Home Price Index",
       title = "S&P CoreLogic Case-Shiller U.S. National Home Price Index over time", 
       subtitle = "Changes in Federal Funds Rate shown using color and size") + 
  ggthemes::theme_igray() +
  scale_color_gradient(name = "Federal Funds Rate",high = palette[2], low = palette[4]) +
  scale_linewidth(name = "Federal Funds Rate", range = c(0.5,2.5)) +
  guides(color=guide_legend(override.aes=list(fill=NA))) + 
  theme(plot.title = element_text(face = "bold", size = 25, hjust = 0),
        plot.subtitle = element_text(face = "italic", size = 10, hjust = 0),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 10, face = "bold"))

#Save to assets directory
ggsave("hpi-ffr.png",
       plot = p4,
       path = "./assets/",
       width = 14.5,
       height = 5.5,
       units = "in")

#For loop to create a line plot for each variable across time frame
plots <- list()
ind = 1
for(i in 1: ncol(data)){
  if(i == 1){
    print("pass")
  }else if(i == 4){
    plots[[ind]] <- ggplot(data, aes_string(x = data[,1], y = data[,i])) + geom_point() + 
      labs(x = "", y = colnames(data)[i]) +   
      geom_vline(xintercept = as.Date("2020-03-15"),
                 linetype = 2, linewidth = 0.50, color = palette[2]) +
      geom_vline(xintercept = as.Date("2008-09-15"),
                 linetype = 2, linewidth = 0.50, color = palette[2]) + 
      annotate(x = ymd("2020-03-15"), y = +Inf, 
               label = "C19", vjust = 2, geom = "label") +
      annotate(x = ymd("2008-09-15"), y = +Inf, 
               label = "Fin. Crisis", vjust = 2, geom = "label") + 
      theme_igray()
    ind = ind + 1
  }else if(i == 18){
    print("pass")
  }
  else{
    plots[[ind]] <- ggplot(data, aes_string(x = data[,1], y = data[,i])) + geom_line() + 
      labs(x = "", y = colnames(data)[i]) +
      geom_vline(xintercept = as.Date("2020-03-15"),
                 linetype = 2, linewidth = 0.50, color = palette[2]) +
      geom_vline(xintercept = as.Date("2008-09-15"),
                 linetype = 2, linewidth = 0.50, color = palette[2]) + 
      annotate(x = ymd("2020-03-15"), y = +Inf, 
               label = "C19", vjust = 2, geom = "label") +
      annotate(x = ymd("2008-09-15"), y = +Inf, 
               label = "Fin. Crisis", vjust = 2, geom = "label") + 
      theme_igray()
    ind = ind + 1
  }
}

grid.arrange(grobs = plots, nrow = 4)
#Save to assets directory
