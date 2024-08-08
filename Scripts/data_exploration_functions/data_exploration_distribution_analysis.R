library(ggplot2)
library(dplyr)
library(stringr)

#Local import
source("Scripts/data_exploration_functions/utilities.R")

# Create box plot per indicator and commodity
box_plot_per_indicator_commodity <- function(ds, commodity, indicator){
  
  ggplot(ds, aes(x = Value, y = Country, fill = Country)) +
    geom_boxplot() +
    labs(title = paste("Box Plot of",commodity," Indicators by Country"),
         x = "Value",
         y = "Country") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(labels = scales::comma)
  
  save_path = str_replace_all(paste("Data/DataExploration/Summary/",indicator,"/boxplot_",commodity,".png"), " ", "")
  ggsave(save_path, width = 10, height = 5)
}

# Execute the summary function for each commodity and country
summary_per_country_commodity <- function(ds, indicator){
  
  # Get only the tuples with indicator parameter value as Indicator
  ds <- subset(ds, Indicator == indicator)
  
  # Get all countries and commodities
  countries <- get_countries(ds)
  commodities <- get_commodities(ds)
  
  to_csv <- data.frame(
    Country = character(0),
    Commodity = character(0),
    Min = numeric(0),
    First_quartile = numeric(0),
    Median = numeric(0),
    Mean = numeric(0),
    Third_quartile = numeric(0),
    Max = numeric(0)
  )
  
  # Execute the summary function for each commodity per country
  for(commodity in commodities){
    
    for(country in countries){
      
      filtered_ds <- subset(ds, Commodity == commodity & Country == country)
      x <- summary(filtered_ds$Value)
      df <- data.frame(
        Country = c(country),
        Commodity = c(commodity),
        Min = c(x[1]),
        First_quartile = c(x[2]),
        Median = c(x[3]),
        Mean = c(x[4]),
        Third_quartile = c(x[5]),
        Max = c(x[6])
      )
      
      if (nrow(to_csv) == 0) {
        to_csv <- df
      } else {
        to_csv <- rbind(to_csv, df)
      }
    }
    
  }
  
  # Create the folder path if doesn't exist
  indicator <- str_trim(indicator)
  indicator <- str_replace_all(indicator, " ", "_")
  folder_path <- str_replace_all(paste("Data/DataExploration/Summary/",indicator), " ", "")
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Folder created:", folder_path, "\n")
  }
  
  # Store the summary results in a CSV file
  save_path <- str_replace_all(paste(folder_path,"/summary.csv"), " ", "")
  write.csv(to_csv, file = save_path, row.names = FALSE)
  
  # Create the box plot
  for(commodity in commodities){
    filtered_ds <- subset(ds, Commodity == commodity)
    box_plot_per_indicator_commodity(filtered_ds, commodity, indicator)
  }
  
}

main_distribution_analysis <- function(ds){
  
  indicators <- get_indicators(ds)
  
  for(indicator in indicators){
    summary_per_country_commodity(ds, indicator)
  }
}