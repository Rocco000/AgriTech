library(dplyr)
library(tidyr)
library(forecast)

dataset <- read.csv("Data/final_dataset.csv")

incremental_estimate_past_years <- function(dataset){
  # Order the dataset by Country, Commodity, Indicator, and Time
  dataset_sorted <- dataset %>%
    arrange(Country, Commodity, Indicator, Time)
  
  dataset_sorted <- dataset_sorted %>%
    group_by(Country, Commodity, Indicator) %>%
    mutate(Increment = Value - lag(Value)) %>%
    ungroup()
  
  
  # Creates a success indicator where the increase is positive (1) and not positive (0)
  dataset_sorted <- dataset_sorted %>%
    mutate(Success = ifelse(Increment > 0, 1, 0))
  
  # Calculates the probability of success for each country and Indicator (considering all commodities)
  probability_success <- dataset_sorted %>%
    group_by(Country, Indicator) %>%
    summarise(Probability_of_Success = mean(Success, na.rm = TRUE)) %>%
    ungroup()
  
  # Pivot the data to have the Indicator as columns
  probability_success_pivot <- probability_success %>%
    pivot_wider(names_from = Indicator, values_from = Probability_of_Success)
  
  # Save the results to a CSV file
  write.csv(probability_success_pivot, "Data/Estimations/probability_of_increment_2020.csv", row.names = FALSE)
}


incremental_estimate_next_years <- function(dataset){
  # Aggregate the data by Country, Indicator, and Time
  aggregated_data <- dataset %>%
    group_by(Country, Indicator, Time) %>%
    summarise(Aggregated_Value = sum(Value, na.rm = TRUE), .groups = 'drop')
  
  results <- list()
  
  countries <- unique(aggregated_data$Country)
  indicators <- unique(aggregated_data$Indicator)
  
  for (country in countries) {
    for (indicator in indicators) {
      # Filter the data for the specific country and indicator
      filtered_data <- aggregated_data %>%
        filter(Country == country, Indicator == indicator) %>%
        arrange(Time)
      
      # Check if there are enough data points for modeling
      if (nrow(filtered_data) > 5) {
        # Check if there are at least two unique values
        if (length(unique(filtered_data$Aggregated_Value)) > 1) {  # Controlla se ci sono almeno due valori unici
          # Convert the data to a time series
          ts_data <- ts(filtered_data$Aggregated_Value, start = min(filtered_data$Time), frequency = 1)

          # Fit an ARIMA model to the time series data
          arima_model <- auto.arima(ts_data)
          
          # Check the normality of the residuals
          residuals <- residuals(arima_model)
          
          # Shapiro-Wilk test for normality
          shapiro_test <- shapiro.test(residuals)  
          
          # Assume normality if p-value > 0.05
          if (shapiro_test$p.value > 0.05) {  
            # Forecast the next 5 years
            future_predictions <- forecast(arima_model, h = 5)  
          
            # Extract the forecast for 2025
            forecast_2025 <- future_predictions$mean[5]
            
            # Extract the 80% prediction interval for 2025
            lower_80 <- future_predictions$lower[5, 1]
            upper_80 <- future_predictions$upper[5, 1]
            
            # Compute the standard deviation of the forecast
            z <- qnorm(0.99)  
            forecast_sd_2025 <- (upper_80 - lower_80) / (2 * z)
            
            # Take the value for 2020
            value_2020 <- filtered_data$Aggregated_Value[filtered_data$Time == 2020]
           
            if (!is.na(value_2020) && !is.na(forecast_2025) && !is.na(forecast_sd_2025)) {
              # Compute Z
              Z <- (value_2020 - forecast_2025) / forecast_sd_2025
              
              # Compute the probability that the value in 2025 will be greater than the value in 2020
              probability_greater_than_2020 <- 1 - pnorm(Z)
              
              # Save the results
              results[[paste(country, indicator, sep = "_")]] <- data.frame(
                Country = country,
                Indicator = indicator,
                Value_2020 = value_2020,
                Forecast_2025 = forecast_2025,
                High_80 = upper_80,
                Low_80 = lower_80,
                Probability = probability_greater_than_2020
              )
            }
          } else {
            message(paste("I residui per", country, "e", indicator, "non sono normali"))
            results[[paste(country, indicator, sep = "_")]] <- data.frame(
              Country = country,
              Indicator = indicator,
              Value_2020 = filtered_data$Aggregated_Value[filtered_data$Time == 2020],
              Forecast_2025 = "",
              High_80 = "",
              Low_80 = "",
              Probability = ""
            )
          }
        } else {
          message(paste("I dati per", country, "e", indicator, "sono costanti, saltato il fitting del modello ARIMA."))
          results[[paste(country, indicator, sep = "_")]] <- data.frame(
            Country = country,
            Indicator = indicator,
            Value_2020 = filtered_data$Aggregated_Value[filtered_data$Time == 2020],
            Forecast_2025 = "",
            High_80 = "",
            Low_80 = "",
            Probability = ""
          )
        }
      }
    }
  }
  
  results_df <- do.call(rbind, results)
  
  # Save the results to a CSV file
  write.csv(results_df, "Data/Estimations/probability_of_increment_2025.csv", row.names = FALSE)
  
}

incremental_estimate_past_years(dataset)
incremental_estimate_next_years(dataset)





