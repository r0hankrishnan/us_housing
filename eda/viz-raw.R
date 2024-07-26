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
  geom_vline(xintercept = as.Date("2020-03-15"),
             linetype = 2, linewidth = 0.50, color = palette[2]) +
  geom_vline(xintercept = as.Date("2008-09-15"),
             linetype = 2, linewidth = 0.50, color = palette[2]) + 
  annotate(x = ymd("2020-03-15"), y = +Inf, label = "COVID-19 Starts", vjust = 2, geom = "label") +
  annotate(x = ymd("2008-09-15"), y = +Inf, label = "Peak of Financial Crisis", vjust = 2, geom = "label") +
  labs(x = "", y = "GDP (Quarterly)",
       title = "US GDP", 
       subtitle = "Reported quarterly") + 
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

#Save to assets to directory


