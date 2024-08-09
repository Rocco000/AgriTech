library(dplyr)
library(stringr)
library(countrycode)
library(scales)
library(ggplot2)
source("Scripts/data_exploration_functions/utilities.R")


country_annual_indicaror_trend <- function(subdataset){
  #Obtain the total value for the indicator for every country and every year in the dataset
  total_value <- subdataset %>%
    group_by(Country, Time) %>%
    summarise(Total_Value = sum(Value, na.rm = TRUE))
  return(total_value)
}

country_annual_indicators_trend_continent <- function(subdataset){
  #Obtain the total value for the indicator for every country and every year in the dataset grouped by continent
  total_value <- subdataset %>%
    group_by(Country, Time, Continent) %>%
    summarise(Total_Value = sum(Value, na.rm = TRUE))
  return(total_value)
}

plot_single_time_series <- function(plot_data, title){
  # Build the time series
  time_series_data <- plot_data %>%
    group_by(Country) %>%
    summarise(ts_data = list(ts(Total_Value, start = min(Time), end = max(Time), frequency = 1)))
  
  # Plot the time series
  ggplot(plot_data, aes(x = Time, y = Total_Value, color = Country, group = Country)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::number) +
    labs(title = paste("Total", title, "Over Time by Country"),
         x = "Year",
         y = paste("Total", title)) +
    theme_minimal()
  
  #Create the destination folder if not exists
  folder_path <- str_replace_all(paste("./Data/DataExploration/TimeSeries/", title, "/"), " ", "")
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
    cat("Folder created:", folder_path, "\n")
  }
  
  #Save the plot
  ggsave(str_replace_all(paste(folder_path,"country_total_",tolower(title),"_per_year.png"), " ", ""), width = 10 , height = 5)
}


plot_multiple_time_series_for_each_continent <- function(plot_data, title){
  #Obtain continents
  continents <- unique(plot_data$Continent)
  
  for (continent in continents) {
    # Filter data for continent
    data_continent <- plot_data %>%
      filter(Continent == continent)
    
    # Create the plot
    ggplot(data_continent, aes(x = Time, y = Total_Value, color = Country, group = Country)) +
      geom_line() +
      geom_point()+
      scale_y_continuous(labels = scales::number) +
      labs(title = paste("Total", title, "Over Time in", continent),
           x = "Year",
           y = paste("Total", title)) +
      theme_minimal()
      
    #Create the destination folder if not exists
    folder_path <- str_replace_all(paste("./Data/DataExploration/TimeSeries/", title, "/"), " ", "")
    if (!dir.exists(folder_path)) {
      dir.create(folder_path)
      cat("Folder created:", folder_path, "\n")
    }
    
    # Save the plot
    ggsave(str_replace_all(paste(folder_path,"country_total_", tolower(title),"_per_year_", continent, ".png")," ",""), width = 6, height = 3 )
  }
}


generate_time_series_plots <- function(subdataset, title){
  #Obtain the total value for the indicator for every country and every year in the dataset
  total_value <- country_annual_indicaror_trend(subdataset)
  
  #Plot the total indicator over time by country
  plot_single_time_series(total_value, title)
  
  #Add the Continent column to the dataset
  subdataset_continent <- add_continent(subdataset)
  
  #Replace NA values with "NA"
  subdataset_continent[is.na(subdataset_continent)] <- "NA"
  
  total_value_continent <- country_annual_indicators_trend_continent(subdataset_continent)
  
  #Plot the total indicator over time by continent
  plot_multiple_time_series_for_each_continent(total_value_continent, title)
}


consumption_price <- read.csv("./Data/SubDatasets/consumption_price.csv")
generate_time_series_plots(consumption_price, "Consumption Price")

level_of_consumption_at_farm_gate <- read.csv("./Data/SubDatasets/level_of_consumption_at_farm_gate.csv")
generate_time_series_plots(level_of_consumption_at_farm_gate, "Consumption")

level_of_production <- read.csv("./Data/SubDatasets/level_of_production.csv")
generate_time_series_plots(level_of_production, "Production")

market_price_differential <- read.csv("./Data/SubDatasets/market_price_differential.csv")
generate_time_series_plots(market_price_differential, "MDP")

producer_price_at_farm_gate <- read.csv("./Data/SubDatasets/producer_price_at_farm_gate.csv")
generate_time_series_plots(producer_price_at_farm_gate, "Producer Price")

