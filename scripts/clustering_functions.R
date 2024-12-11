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


#Libraries
load_clustering_libraries <- function(){
  
  library(cluster)
  library(factoextra)
  library(factoextra)

}

#Load in modeling data, preprocess it, and scale it
load_preprocess_scale_data <- function(){
  df <- read.csv("../data/modelling/modelling.csv")

  df_scaled <- df %>% 
    select(-date, -clcsHPI) %>%
    scale()
  
  return (df_scaled)
}

#Diagnostic visualization template
plot_clustering_diagnostics <- function(df_scaled, method = c("silhouette", "gap_stat", "wss"),
                                        algorithm = c("kmeans", "pam")){
  
  if (algorithm == "kmeans"){
    
    if (method == "silhouette"){
      title = "K Means Sihouette Scores"
    } else if (method == "gap_stat"){
      title = "K Means Gap Statistics"
    } else {
      title = "K Means Elbow Plot"
    }
    
    plot <- fviz_nbclust(df_scaled, kmeans, 
                         method = method,
                         linecolor = palette[2],
                         verbose = FALSE) +
      labs(title = title) +
      theme_bw() + 
      scale_color_manual(values = palette[1]) +
      theme(plot.title = element_text(face = "bold"))
    
    
  } else{
    
    if (method == "silhouette"){
      title = "PAM Sihouette Scores"
    } else if (method == "gap_stat"){
      title = "PAM Gap Statistics"
    } else {
      title = "PAM Elbow Plot"
    }
    
    plot <- fviz_nbclust(df_scaled, pam, 
                         method = method,
                         linecolor = palette[2],
                         verbose = FALSE) +
      labs(title = title) +
      theme_bw() + 
      scale_color_manual(values = palette[1]) +
      theme(plot.title = element_text(face = "bold"))
  }
  
  return (plot)
}

plot_clusters <- function(model, df_scaled, algorithm = c("kmeans", "pam")){
  
  if (algorithm == "kmeans"){
    plot <- fviz_cluster(model, df_scaled, stand = FALSE, main = "Kmeans Clustering",
                         labelsize = 0) +
      scale_color_manual("Cluster", values = year_palette[c(1, 5, 9, 14, 17, 21)]) + 
      scale_fill_manual("Cluster", values = year_palette[c(1, 5, 9, 14, 17, 21)]) +
      scale_shape_manual('Cluster', values=c(19, 20, 21, 22, 23, 24)) + 
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"))
  } else{
    plot <- fviz_cluster(model, df_scaled, stand = FALSE, main = "PAM Clustering",
                         labelsize = 0) +
      scale_color_manual("Cluster", values = year_palette[c(1, 9, 21)]) + 
      scale_fill_manual("Cluster", values = year_palette[c(1, 9, 21)]) +
      scale_shape_manual('Cluster', values=c(22, 23, 24)) + 
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"))
  }
  
  return (plot)
  
}

create_clustered_df_ <- function(model){
  df <- read.csv("../data/modelling/modelling.csv")
  
  df_cluster <- cbind(df, cluster = model$cluster)
  df_cluster$cluster <- as.factor(df_cluster$cluster)
  
  return (df_cluster)
}

plot_median_hpi_across_clusters <- function(model, algorithm = c("kmeans", "pam")){
  
  
  if (algorithm == "kmeans"){
    
    df_cluster <- create_clustered_df_(model)
    
    plot <- df_cluster %>%
      ggplot(aes(x=cluster,y=clcsHPI, color = cluster))+
      geom_boxplot() + 
      labs(title="U.S. HPI By K Means Clusters", 
           x = "Cluster (K Means)", y = "U.S. HPI") + 
      scale_color_manual("Cluster (K Means)", values = year_palette[c(1, 5, 9, 14, 17, 21)]) + 
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"))
    
  } else{
    
    df_cluster <- create_clustered_df_(model)
    
    plot <- df_cluster %>%
      ggplot(aes(x=cluster,y=clcsHPI, color = cluster))+
      geom_boxplot() + 
      labs(title="U.S. HPI By PAM Clusters", 
           x = "Cluster (PAM)", y = "U.S. HPI") + 
      scale_color_manual("Cluster (PAM)", values = year_palette[c(1, 9, 21)]) + 
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"))
    
  }
  
  return (plot)
}

plot_bar_median_hpi_across_clusters <- function(model, algorithm = c("kmeans", "pam")){
  
  if (algorithm == "kmeans"){
    df_cluster <- create_clustered_df_(model)
    
    plot <- df_cluster %>%
      group_by(cluster) %>%
      summarize(meanHPI = mean(clcsHPI)) %>%
      ggplot(aes(x = cluster , y = meanHPI, fill = cluster)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Average U.S. HPI by K Means Cluster", 
           x = "Cluster (K Means)", y = "Average U.S. HPI",
           fill = "K Means Cluster") + 
      scale_fill_manual("Cluster (K Means)", values = year_palette[c(1, 5, 9, 14, 17, 21)]) + 
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"))
  } else{
    df_kmeans <- create_clustered_df_(model)
    
    plot <- df_kmeans %>%
      group_by(cluster) %>%
      summarize(meanHPI = mean(clcsHPI)) %>%
      ggplot(aes(x = cluster , y = meanHPI, fill = cluster)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Average U.S. HPI by PAM Cluster", 
           x = "Cluster (PAM)", y = "Average U.S. HPI") + 
      scale_fill_manual("Cluster (PAM)", values = year_palette[c(1, 9, 21)]) + 
      theme_bw() + 
      theme(plot.title = element_text(face = "bold"))
  }
  
  return (plot)
}
















