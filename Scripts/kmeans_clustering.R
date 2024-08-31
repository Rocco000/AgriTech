library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)


calculate_elbow_point <- function(data, year, max_clusters = 10) {
  wss <- numeric(max_clusters)
  
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
  
  for (k in 1:max_clusters) {
    kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)
    wss[k] <- kmeans_result$tot.withinss
  }
  
  plot(1:max_clusters, wss, type = "b", pch = 19, frame = FALSE,
       xlab = "Number of clusters K", ylab = "Total Within Sum of Squares",
       main = paste("Elbow Plot ", year))
  
  #Save the plot
  dev.copy(png, file = paste0("Data/Clustering/elbow_plot_",year,".png"))
  dev.off()
}


kmeans_clustering <- function(data, year, k) {
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
  
  # Apply k-means clustering with the Elbow Method
  set.seed(42)
  kmeans_result <- kmeans(scaled_data, centers = k)
  
  # Calculate the between/total ratio
  between_total_ratio <- calculate_between_total_ratio(kmeans_result, scaled_data)
  
  # Add the clusters to the aggregated data
  aggregated_data$Cluster <- as.factor(kmeans_result$cluster)
  
  # Add a column for the year
  aggregated_data$Year <- year
  
  # Add a column for the between/total ratio
  aggregated_data$BetweenTotalRatio <- between_total_ratio
  
  return(aggregated_data)
}


calculate_between_total_ratio <- function(kmeans_result, data) {
  # Calcola il centroide globale
  global_mean <- colMeans(data)
  
  # Calcola la somma delle distanze quadre totali (total sum of squares)
  total_ss <- sum(rowSums((data - global_mean)^2))
  
  # Calcola la somma delle distanze quadre tra i cluster (between sum of squares)
  cluster_means <- kmeans_result$centers
  sizes <- kmeans_result$size
  between_ss <- sum(sizes * rowSums((cluster_means - global_mean)^2))
  
  # Calcola il rapporto between/total
  ratio <- between_ss / total_ss
  return(ratio)
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

#Calculate the elbow point for the years_of_interest
lapply(years_of_interest, function(year) calculate_elbow_point(filtered_dataset, year))

# Ask the user to insert the number of clusters for each year
input <- readline(prompt = "Insert the optimal number of clusters for each year (separated by spaces):")
optimal_k <- as.numeric(unlist(strsplit(input, " ")))
print(optimal_k)

# Apply k-means clustering with the Elbow Method for each year of interest
kmeans_cluster_results <- lapply(1:length(years_of_interest), function(i) kmeans_clustering(filtered_dataset, years_of_interest[i], optimal_k[i]))


# Combine the results into a single dataframe
combined_kmeans_results <- bind_rows(kmeans_cluster_results)

# Print the results
print(combined_kmeans_results)
write.csv(combined_kmeans_results, "Data/Clustering/kmeans_results.csv")

# Visualize the movements of countries between clusters over time
ggplot(combined_kmeans_results, aes(x = Year, y = Country, color = Cluster, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Clusters evolution in years", x = "Year", y = "Country") +
  theme_minimal()

# Save the plot
ggsave("Data/Clustering/kmeans_cluster_movements.png",width = 10 , height = 8)

