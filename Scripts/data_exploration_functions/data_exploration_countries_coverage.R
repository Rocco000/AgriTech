library(countrycode)
library(dplyr)
library(magrittr)

# Add the continent column to dataset
add_continent <- function(dataset){
  dataset_with_continent <- dataset %>% mutate(Continent = countrycode(Country, "country.name", "continent"))
  return(dataset_with_continent)
}

# Get the number of countries per continent
get_num_countries_per_continent <- function(dataset, field_sum_name){
  field_sum_sym <- sym(field_sum_name)
  
  num <- dataset %>%
    group_by(Continent) %>%
    summarise(!!field_sum_sym := n_distinct(Country))
  return(num)
}

# Plot the countries coverage
plot_countries_coverage <- function(dataset, save_path1, save_path2){
  
  # Create a dataset containing the number of countries per continent in the world
  total_countries_per_continent <- data.frame(
    Continent = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
    Total_countries = c(54, 35, 49, 47, 16)
  )
  
  # Combine the two datasets
  combined_data <- num_countries_per_continent_in_ds %>%
    full_join(total_countries_per_continent, by = "Continent") %>%
    replace_na(list(agri_country_count = 0, world_country_count = 0))
  
  # Pivot the data for plotting
  plot_data <- combined_data %>%
    pivot_longer(cols = c("Countries_in_dataset", "Total_countries"), 
                 names_to = "dataset", 
                 values_to = "count")
  
  # Create the bar plot
  # una sopra l'altro
  ggplot() +
    geom_bar(data = plot_data %>% filter(dataset == "Total_countries"), 
             aes(x = Continent, y = count, fill = dataset), 
             stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_bar(data = plot_data %>% filter(dataset == "Countries_in_dataset"), 
             aes(x = Continent, y = count, fill = dataset), 
             stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_text(data = plot_data %>% filter(dataset == "Total_countries"), 
              aes(x = Continent, y = count, label = count), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 3.5) +
    geom_text(data = plot_data %>% filter(dataset == "Countries_in_dataset"), 
              aes(x = Continent, y = count, label = count), 
              position = position_stack(vjust = 0.5, reverse = TRUE), 
              color = "white", size = 3.5) +
    labs(title = "Comparison of Dataset with World", 
         x = "Continent", 
         y = "Number of Countries", 
         fill = "Data Source") +
    theme_minimal() +
    scale_fill_manual(values = c("Countries_in_dataset" = "blue", "Total_countries" = "red"))
  
  ggsave(save_path1)
  
  #uno a fianco a l'altro
  ggplot(plot_data, aes(x = Continent, y = count, fill = dataset)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = count), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, size = 3.5, color = "black") +
    labs(title = "Comparison of Dataset with World", 
         x = "Continent", 
         y = "Number of Countries", 
         fill = "Data Source") +
    theme_minimal() +
    scale_fill_manual(values = c("Countries_in_dataset" = "blue", "Total_countries" = "red"))
  
  ggsave(save_path2)
}