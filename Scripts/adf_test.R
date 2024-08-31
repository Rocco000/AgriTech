library(tseries)

# Local import
source("Scripts/data_exploration_functions/utilities.R")

agricultural_data <- read.csv("Data/final_dataset.csv")

countries <- get_countries(agricultural_data)

indicators <- get_indicators(agricultural_data)

commodities <- get_commodities(agricultural_data)

to_csv <- data.frame(
  Country = character(0),
  Indicator = numeric(0),
  Commodity = numeric(0),
  p_value = numeric(0)
)

# ADF test for each country
for (country in countries) {
  for (indicator in indicators) {
    for (commodity in commodities) {
      # Filter data
      data <- subset(agricultural_data, Country == country & Indicator == indicator & Commodity == commodity)
      
      # Check if data is empty
      if (nrow(data) == 0) {
        next
      }
      else{
        # ADF test
        adf_result <- adf.test(data$Value)
        
        to_csv <- base::rbind(to_csv, data.frame(Country = country, Indicator = indicator, Commodity = commodity, p_value = adf_result$p.value))
      }
    }
  }
}

folder_path <- "Data/Correlation/"

if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
  cat("Folder created:", folder_path, "\n")
}

# Store the CSV file
save_path <- paste0(folder_path, "ADF_test.csv")
write.csv(to_csv, file = save_path, row.names = FALSE)

no_stationariness <- subset(to_csv, p_value > 0.05)
ok <- subset(to_csv, p_value <= 0.05)

write.csv(ok, file = "Data/Correlation/stationary.csv", row.names = FALSE)
