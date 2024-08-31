library(countrycode)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(rlang)
library(RColorBrewer)
source("Scripts/data_exploration_functions/utilities.R")

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
  combined_data <- dataset %>%
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
              position = position_stack(vjust = 0.7, reverse = TRUE), 
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

main_geographical_analysis <- function(ds){
  # Get only the unique values in country field
  countries_in_ds <- data.frame(Country= unique(ds$Country))
  
  # Add continent
  countries_in_ds <- add_continent(countries_in_ds)
  
  # Check for any unmatched countries
  unmatched <- countries_in_ds[is.na(countries_in_ds$Continent), "Country"]
  print("Countries with NA value in Continent field:")
  print(unique(unmatched))
  
  # Count the number of countries per continent
  num_countries_per_continent_in_ds <- get_num_countries_per_continent(countries_in_ds, "Countries_in_dataset")
  
  print("Number of countries per continent in our dataset:")
  num_countries_per_continent_in_ds 
  
  # Bar plot for the number of countries per continent in our dataset
  ggplot(num_countries_per_continent_in_ds, aes(x = Continent, y = Countries_in_dataset, fill=Continent)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of countries per continent in dataset", x = "Continent", y = "Number of countries") +
    theme_minimal() +
    scale_fill_manual(values = c("Africa" = "red", "Americas" = "blue", "Asia" = "green", "Europe" = "purple", "Oceania" = "orange", "NA" = "grey"))
  
  ggsave("Data/DataExploration/countries_per_continent.png")
  
  # Our dataset contains other countries because the value OECD as country
  # represents 38 countries, instead European Union represents the 5 countries that are not in OECD group
  oecd <- c("Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Latvia","Lithuania","Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovakia","Slovenia","South Korea","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States")
  common_countries <- intersect(oecd, countries_in_ds$Country)
  oecd <- setdiff(oecd, common_countries)
  
  oecd_countries <- data.frame(
    Country = oecd,
    Continent = rep(NA,length(oecd))
  )
  
  # Remove the tuples with OECD or European Union as value in country field
  countries_in_ds <- countries_in_ds %>% filter(Country!="OECD - Total" & Country!="European Union")
  
  # Substitute the OECD values with the countries name which are member of OECD
  countries_in_ds <- rbind(countries_in_ds, oecd_countries)
  
  # Re-add the continent for this new values
  countries_in_ds <- add_continent(countries_in_ds)
  
  # Re-count the number of countries per continent in our dataset
  num_countries_per_continent_in_ds <- get_num_countries_per_continent(countries_in_ds, "Countries_in_dataset")
  
  # Re-plot
  ggplot(num_countries_per_continent_in_ds, aes(x = Continent, y = Countries_in_dataset, fill=Continent)) +
    geom_bar(stat = "identity") +
    labs(title = "Updated plot of countries per continent in dataset", x = "Continent", y = "Number of countries") +
    theme_minimal()+
    scale_fill_brewer(palette = "Set1")
  
  ggsave("Data/DataExploration/updated_countries_per_continent.png")
  
  #Pie plot
  ggplot(num_countries_per_continent_in_ds, aes(x = "", y = Countries_in_dataset, fill=Continent)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start=0) +
    labs(title = "Number of countries per continent in dataset", x = "", y = "") +
    theme_void() +
    scale_fill_brewer(palette = "Set1")
  
  ggsave("Data/DataExploration/countries_per_continent_pie.png")
  
  
  # Plot the countries coverage
  plot_countries_coverage(num_countries_per_continent_in_ds, "Data/DataExploration/countries_coverage.png", "Data/DataExploration/countries_coverage2.png")
}
