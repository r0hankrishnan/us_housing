#Load libraries
library(tidyverse)
library(shiny)
library(factoextra)
library(cluster)
source("../scripts/useful-functions.R")

#Load data, preprocess, and scale
df <- read.csv("../data/modelling/modelling.csv")
df <- model_df_preprocess(df)

dfScale <- df %>%
  select(-clcsHPI, -year, -month) %>%
  scale()

sliderInputText <- "Use the slider below to select the number of clusters to use 
for the K Means algorithm. The charts to the left will display the clusters as 
well as the median hpi between all the clusters. 
The silhouette score for this data indicated 6 clusters as optimal!
Select number of clusters: "


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("US Housing Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("clusters",
                        sliderInputText,
                        min = 1,
                        max = 10,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("kMeansPlot"),
           plotOutput("hpiPlot")
        )
    )
)

# Define server logic required to draw plots
server <- function(input, output) {
  
  clusters <- eventReactive(input$clusters, {
    return(input$clusters)
  })
  
  modelKmeans <- eventReactive(input$clusters,{
    kmeans(dfScale, centers = input$clusters)
  } 
)
  
  output$kMeansPlot <- renderPlot({
    fviz_cluster(modelKmeans(), dfScale, stand = FALSE, main = "K Means Clustering",
                 labelsize = 0) +
      labs(main = "K Means Clustering",
           subtitle = ifelse(clusters() == 1, 
                             paste(clusters(), "cluster"),
                             paste(clusters(), "clusters"))) + 
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  })
  
  output$hpiPlot <- renderPlot({
    #Build boxplot df
    dfKMeans<-df %>%
      cbind(cluster = modelKmeans()["cluster"]) %>%
      mutate(cluster = as.factor(cluster))
    
    #Plot hpi boxplots separated by cluster
    dfKMeans %>%
      ggplot(aes(x=cluster, y=clcsHPI, color = cluster))+
      geom_boxplot() + 
      labs(title="CLCS HPI By Cluster",
           subtitle = ifelse(clusters() == 1,
                             paste(clusters(), "cluster"),
                             paste(clusters(), "clusters"))) + 
      theme_minimal() + 
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
