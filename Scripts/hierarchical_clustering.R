library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)


calculate_within_between <- function(hclust_result, data, year, method, max_clusters = 10) {
  withinss <- numeric(max_clusters)
  betweenss <- numeric(max_clusters)
  between_total <- numeric(max_clusters)
  
  total_mean <- colMeans(data)
  total_ss <- sum(scale(data, center = total_mean, scale = FALSE)^2)
  
  for (k in 1:max_clusters) {
    clusters <- cutree(hclust_result, k = k)
    withinss[k] <- sum(sapply(unique(clusters), function(cluster) {
      cluster_data <- data[clusters == cluster, ,drop = FALSE]
      cluster_mean <- colMeans(cluster_data)
      sum(scale(cluster_data, center = cluster_mean, scale = FALSE)^2)
    }))
    betweenss[k] <- total_ss - withinss[k]
    between_total[k] <- betweenss[k] / total_ss
  }
  
  # Plot Scree Plot for Within Sum of Squares
  plot(1:max_clusters, withinss, type = "b", pch = 19, frame = FALSE,
       xlab = "Number of clusters K", ylab = "Within Sum of Squares",
       main = paste0("Scree Plot (Within Sum of Squares)", year))
  
  # Save the plot
  dev.copy(png, file = paste0("Data/Clustering/within_ss_plot_",year,"_",method,".png"))
  dev.off()
  
  # Plot Scree Plot for Between Sum of Squares
  plot(1:max_clusters, betweenss, type = "b", pch = 19, frame = FALSE,
       xlab = "Number of clusters K", ylab = "Between Sum of Squares",
       main = paste0("Scree Plot (Between Sum of Squares)", year))
  
  # Save the plot
  dev.copy(png, file = paste0("Data/Clustering/between_ss_plot_",year,"_",method,".png"))
  dev.off()

  # Plot Scree Plot for Between/Total Ratio
  plot(1:max_clusters, between_total, type = "b", pch = 19, frame = FALSE,
       xlab = "Number of clusters K", ylab = "Between/Total Ratio",
       main = paste0("Scree Plot (Between/Total Ratio)", year))
  
  #Save the plot
  dev.copy(png, file = paste0("Data/Clustering/between_total_plot_",year,"_",method,".png"))
  dev.off()
  
  return(list(withinss = withinss, betweenss = betweenss, between_total = between_total))
}


hierarchical_clustering <- function(data, year,method="complete", max_clusters = 10) {
  # Filter for the specific year
  year_data <- data %>% filter(Time == year)
  
  # Aggregate data by Country and Indicator, calculating the mean
  aggregated_data <- year_data %>%
    group_by(Country, Indicator) %>%
    summarize(Value = mean(Value, na.rm = TRUE)) %>%
    spread(Indicator, Value)
  
  # Remove any rows with NA
  aggregated_data <- na.omit(aggregated_data)
  
  # Normalize the data
  scaled_data <- scale(aggregated_data[,-1])
  
  # Hierarchical clustering
  dist_matrix <- dist(scaled_data)
  hclust_result <- hclust(dist_matrix, method = method)
  
  #Plot the dendrogram
  plot(hclust_result, labels = aggregated_data$Country, main = paste("Hierarchical clustering dendrogram -", year, "-", method))
  
  #Save the plot
  dev.copy(png, file = paste0("Data/Clustering/dendrogram_", year, "_", method, ".png"))
  dev.off()

  # Calculate within and between sum of squares for different numbers of clusters
  ss_values <- calculate_within_between(hclust_result, scaled_data, year, method, max_clusters = max_clusters)
  
  #Save the results
  write.csv(ss_values, paste0("Data/Clustering/within_between_values_", year, "_", method, ".csv"))
  
  return(list(hclust_result = hclust_result, scaled_data = scaled_data, aggregated_data = aggregated_data))
}


create_clusters_from_hclust <- function(hclust_result, scaled_data, aggregated_data, optimal_k, year, method) {
  # Cut the dendrogram to create the clusters
  aggregated_data$Cluster <- cutree(hclust_result, k = optimal_k)
  
  # Add a column for the year
  aggregated_data$Year <- year
  
  return(aggregated_data)
}


run_clustering_and_create_clusters <- function(filtered_dataset, years_of_interest, method) {
  all_results <- list()
  
  for (year in years_of_interest) {
    # Hierarchical clustering
    clustering_results <- hierarchical_clustering(filtered_dataset, year, method = method)
    
    optimal_k <- as.numeric(readline(prompt = paste("Insert the optimal number of clusters for ", year, " ", method, " clustering: ")))
    
    final_results <- create_clusters_from_hclust(clustering_results$hclust_result, clustering_results$scaled_data, clustering_results$aggregated_data, optimal_k, year, method)
    
    all_results[[as.character(year)]] <- final_results
  }
  
  combined_results <- bind_rows(all_results)
  
  return(combined_results)
}

##################################### MAIN #####################################

# Load dataset
dataset <- read.csv("Data/final_dataset.csv")

# Define years of interest
years_of_interest <- c(2005, 2008, 2015, 2020)

#Define indicator of interest
indicators_of_interest <- c("Level of consumption (at farm gate)", "Level of production", "Producer price (at farm gate)")

# Filter dataset for relevant indicators and years
filtered_dataset <- dataset %>%
  filter(Indicator %in% indicators_of_interest &
           Time %in% years_of_interest)


combined_hierarchical_results_complete <- run_clustering_and_create_clusters(filtered_dataset, years_of_interest, method = "complete")

combined_hierarchical_results_single <- run_clustering_and_create_clusters(filtered_dataset, years_of_interest, method = "single")

#Save the results
write.csv(combined_hierarchical_results_complete, "Data/Clustering/hierarchical_results_complete.csv")
write.csv(combined_hierarchical_results_single, "Data/Clustering/hierarchical_results_single.csv")

# Plot the results complete method
ggplot(combined_hierarchical_results_complete, aes(x = Year, y = Country, color = as.factor(Cluster), group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Hierarchical cluster evolution (complete)", x = "Year", y = "Country") +
  theme_minimal()

ggsave("Data/Clustering/hierarchical_cluster_movements_complete.png", width = 10, height = 8)

# Plot the results single method
ggplot(combined_hierarchical_results_single, aes(x = Year, y = Country, color = as.factor(Cluster), group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Hierarchical cluster evolution (single)", x = "Anno", y = "Paesi") +
  theme_minimal()

ggsave("Data/Clustering/hierarchical_cluster_movements_single.png", width = 10, height = 8)




