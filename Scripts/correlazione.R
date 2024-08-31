library(tidyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(car)
library(scatterplot3d)
library(lmtest)


# Local import
source("Scripts/data_exploration_functions/utilities.R")

pivoting_data <- function(ds, indicators){
  # Get the data for the two indicators
  filtered_data <- subset(ds, Indicator %in% indicators)
  
  selected_features <- c("Country","Commodity","Indicator","Time", "Value")
  
  # Get the necessary columns
  filtered_data <- subset(filtered_data, select = selected_features)
  
  # Pivot the data
  pivot_data <- spread(filtered_data, key = Indicator, value = Value)
  
  # Remove rows with NA values
  pivot_data <- na.omit(pivot_data)
  
  return(pivot_data)
}

save_correlation <- function(csv_file, indicator1, indicator2, file_name){
  # Create the folder path if doesn't exist
  indicator1 <- str_replace_all(indicator1, " ", "_")
  indicator2 <- str_replace_all(indicator2, " ", "_")
  
  folder_path <- paste0("Data/Correlation/",indicator1, "_", indicator2, "/")
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Folder created:", folder_path, "\n")
  }
  
  # Store the CSV file
  save_path <- paste0(folder_path, file_name, ".csv")
  write.csv(csv_file, file = save_path, row.names = FALSE)
}

calculate_global_correlation_covariance <- function(ds, indicator1, indicator2){
  # Get all years
  years <- get_years(ds)
  
  pivot_data <- pivoting_data(ds, c(indicator1, indicator2))
  
  to_csv <- data.frame(
    Year = character(0),
    Correlation = numeric(0),
    Covariance = numeric(0)
  )
  
  for(year in years){
    # Get the data for the year
    x <- subset(pivot_data, Time == year)
    
    # Calculate correlation
    correlation <- cor(x[[indicator1]], x[[indicator2]], use = "complete.obs") 
    
    # Calculate covariance7
    covariance <- cov(x[[indicator1]], x[[indicator2]], use = "complete.obs")
    
    to_csv <- base::rbind(to_csv, data.frame(Year = year, Correlation = correlation, Covariance = covariance))

  }
  
  save_correlation(to_csv, indicator1, indicator2, "global_correlation_per_year") 

}

# Calculate the correlation between two indicators for each commodity divided by year
calculate_correlation_covariance_per_commodity <- function(ds, indicator1, indicator2){
  
  # Get all commodities
  commodities <- get_commodities(ds)
  # Get all years
  years <- get_years(ds)
  
  pivot_data <- pivoting_data(ds, c(indicator1, indicator2))
  
  to_csv <- data.frame(
    Commodity = character(0),
    Year = numeric(0),
    Correlation = numeric(0),
    Covariance = numeric(0)
  )
  
  for(commodity in commodities) {
    for (year in years){
      # Get the data for the commodity
      x <- subset(pivot_data, Commodity == commodity & Time == year)
      
      # Check if the data is empty or contains NA values
      if (nrow(x) == 0) {
        next
      } else if (sum(is.na(x[indicator1])) == nrow(x) || sum(is.na(x[indicator2])) == nrow(x)) {
        next
      } else{
      
        # Calculate correlation
        correlation <- cor(x[[indicator1]], x[[indicator2]], use = "complete.obs")
        
        # Calculate covariance
        covariance <- cov(x[[indicator1]], x[[indicator2]], use = "complete.obs")
        
        # Store the data
        to_csv <- base::rbind(to_csv, data.frame(Commodity = commodity, Year = year, Correlation = correlation, Covariance = covariance))
      }
    }
    
  }
  
  save_correlation(to_csv, indicator1, indicator2, "correlation_per_commodity_year")
  
}

regression_analysis <- function(ds, indicator1, indicator2){
  # Get all years
  years <- get_years(ds)
  
  pivot_data <- pivoting_data(ds, indicator1, indicator2)
  
  to_csv <- data.frame(
    year = integer(),
    r_squared = numeric(),
    residual_se = numeric(),
    f_statistic = numeric(),
    p_value_f = numeric()
  )
  
  # Create the folder if doesn't exist
  ind1 <- str_replace_all(indicator1, " ", "_")
  ind2 <- str_replace_all(indicator2, " ", "_")
  folder_path <- paste0("Data/Correlation/", ind1, "_" , ind2, "/")
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Folder created:", folder_path, "\n")
  }
  
  for(year in years){
    # Get the data for the year
    x <- subset(pivot_data, Time == year)
    
    # Perform linear regression
    model <- lm(x[[indicator2]] ~ x[[indicator1]])
    
    # Get the summary of the model
    model_summary <- summary(model)
    
    f_statistic <- model_summary$fstatistic[1]
    
    # Store evaluation data
    to_csv <- rbind(to_csv, data.frame(
      year = year,
      r_squared = model_summary$r.squared,
      residual_se = model_summary$sigma,
      f_statistic = model_summary$fstatistic[1],
      p_value_f = pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    ))
    
    # Plot and save the regression line
    png(filename = paste(folder_path, "regression_plot_", year, ".png", sep=""))
    
    plot(x[[indicator1]], x[[indicator2]], main = paste("Year:", year), 
         xlab = indicator1, 
         ylab = indicator2,
         xaxt = 'n', yaxt = 'n'
         )
    
    abline(model, col = "blue", lwd = 2)
    
    axis(1, at = pretty(x[[indicator1]]), labels = format(pretty(x[[indicator1]]), scientific = FALSE))
    axis(2, at = pretty(x[[indicator2]]), labels = format(pretty(x[[indicator2]]), scientific = FALSE))
    
    dev.off()
  }
  
  write.csv(to_csv, paste0(folder_path, "regression_results.csv"), row.names = FALSE)
}

is_there_multicollinearity <- function(ds, indicators){
  # Pivot data
  pivot_data <- pivoting_data(ds, indicators)
  
  #pivot_data <- subset(pivot_data, pivot_data[[indicators[1]]] != 0)

  # Create the formula
  regression_formula <- as.formula(paste("`", indicators[3], "` ~ `", indicators[1], "` + `", indicators[2], "`", sep = ""))
  
  # Get all years
  years <- get_years(ds)
  
  vif_vector <- c()
  
  for(year in years){
    # Get the data for the year
    filtered_data <- subset(pivot_data, Time == year)
    
    # Fit a linear regression model
    model <- lm(regression_formula, data = filtered_data)
    
    # Calculate the VIF
    vif_values <- car::vif(model)
    
    vif_vector <- append(vif_vector, vif_values)
  }
  
  to_csv <- data.frame(
    year = years,
    vif = vif_vector
  )
  
  write.csv(to_csv, "Data/Correlation/Mpd_Level_consumption_Consumption_price/vif_values.csv", row.names = FALSE)
  
  if(any(vif_vector > 10)){
    return(TRUE)
  } else{
    return(FALSE)
  }
  
}

plot_bivariate_analysis <- function(ds, indicators, year, model){
  # Create the folder path if doesn't exist
  folder_path <- "Data/Correlation/Mpd_Level_consumption_Consumption_price/Plots_regression/"
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Folder created:", folder_path, "\n")
  }
  
  png(paste0(folder_path, "3d_scatterplot_", year,".png"), width = 800, height = 600)
  
  x_data <- ds[[indicators[1]]]
  y_data <- ds[[indicators[2]]]
  z_data <- ds[[indicators[3]]]
  
  # Create 3D plot
  s3d <- scatterplot3d(x_data,
                       y_data,
                       z_data,
                       xlab=indicators[1],
                       ylab=indicators[2],
                       zlab=indicators[3],
                       color="blue",
                       pch=19)
  
  # Add regression plane
  s3d$plane3d(model, draw_polygon = TRUE, polygon_args = list(col = "gray", border = "black"))
  
  dev.off()
  
}

check_omoschedasticita <- function(ds, model, year){
  # Create the folder path if doesn't exist
  folder_path <- "Data/Correlation/Mpd_Level_consumption_Consumption_price/Plot_residuals/"
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Folder created:", folder_path, "\n")
  }
  
  png(paste0(folder_path, "residuals_",year,".png"), width = 800, height = 600)
  
  # Get predicted values
  predicted_values <- predict(model)
  
  # Get residuals (y^ - y)
  residuals <- resid(model)
  
  # Plot residuals vs predicted values
  plot(predicted_values, residuals,
       xlab = "Valori Predetti", ylab = "Residui",  xaxt = "n", yaxt = "n")
  abline(h = 0, col = "red")
  
  # Better visualization of axes values
  axis(1, at = pretty(predicted_values), labels = format(pretty(predicted_values), scientific = FALSE))
  axis(2, at = pretty(residuals), labels = format(pretty(residuals), scientific = FALSE))

  box()
  
  dev.off()

}

# Breusch-Pagan test
bp_test <- function(model){
  results <- lmtest::bptest(model)
  
  return(results$p.value)
}

q_q_plot <- function(ds, model, year){
  # Create the folder path if doesn't exist
  folder_path <- "Data/Correlation/Mpd_Level_consumption_Consumption_price/QQ_plots/"
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Folder created:", folder_path, "\n")
  }
  
  png(paste0(folder_path, "qq_plot_",year,".png"), width = 800, height = 600)
  
  # Get residuals (y^ - y)
  residuals <- resid(model)
  
  # Q-Q plot dei residui
  qqnorm(residuals,  xlab = "Quantili Teorici", ylab = "Quantili Campionari", main = year ,yaxt = "n")
  
  qqline(residuals, col = "red", lwd = 2)
  
  # Better visualization of axes values
  axis(2, at = pretty(residuals), labels = format(pretty(residuals), scientific = FALSE)) 
  
  box()
  
  dev.off()
  
}

shapiro_test_on_model_residuals <- function(ds, model){
  # Get residuals (y^ - y)
  residuals <- resid(model)
  
  # Get Shapiro-Wilk test results
  shapiro_test <- shapiro.test(residuals)
  
  return(shapiro_test)

}


bivariate_analysis <- function(ds, indicators){
  
  # Pivot data
  pivot_data <- pivoting_data(ds, indicators)
  
  #pivot_data <- subset(pivot_data, pivot_data[[indicators[1]]] != 0)
  
  # Evaluation metrics CSV
  to_csv <- data.frame(
    year = integer(),
    r_squared = numeric(),
    adjusted_r_squared = numeric(),
    residual_se = numeric(),
    f_statistic = numeric(),
    p_value_f = numeric()
  )
  
  #BP test CSV
  bp_csv <- data.frame(
    year = integer(),
    p_value = numeric()
  )
  
  # Shapiro-Wilk test CSV
  shapiro_csv <- data.frame(
    year = integer(),
    W_statistic = numeric(),
    p_value = numeric()
  )
  
  # Get all years
  years <- get_years(ds)
  
  # Create formula
  regression_formula <- as.formula(paste("`", indicators[3], "` ~ `", indicators[1], "` + `", indicators[2], "`", sep = ""))
  
  # Perform regression analysis per year
  for(year in years){
    
    # Get the data for the year
    filtered_data <- subset(pivot_data, Time == year)
    
    # Fit a linear regression model
    model <- lm(regression_formula, data = filtered_data)
    
    # Get the evaluation metrics
    model_summary <- summary(model)
    
    f_statistic <- model_summary$fstatistic[1]
    
    # Store evaluation metrics
    to_csv <- rbind(to_csv, data.frame(
      year = year,
      r_squared = model_summary$r.squared,
      adjusted_r_squared = model_summary$adj.r.squared,
      residual_se = model_summary$sigma,
      f_statistic = model_summary$fstatistic[1],
      p_value_f = pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    ))
    
    # Plot regression
    plot_bivariate_analysis(filtered_data, indicators, year, model)
    
    # Check omoschedasticità
    check_omoschedasticita(filtered_data, model, year)
    
    # Store BP test
    bp_test_results <- bp_test(model)
    bp_csv <- rbind(bp_csv, data.frame(
      year = year,
      p_value = bp_test_results
    ))
    
    # Q-Q plot
    q_q_plot(filtered_data, model, year)
    
    # Store Shapiro-Wilk test
    shapiro_test_results <- shapiro_test_on_model_residuals(filtered_data, model)
    shapiro_csv <- rbind(shapiro_csv, data.frame(
      year = year,
      W_statistic = shapiro_test_results$statistic,
      p_value = shapiro_test_results$p.value
    ))
  }
  
  # Create the folder path if doesn't exist
  folder_path <- "Data/Correlation/Mpd_Level_consumption_Consumption_price/"
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    cat("Folder created:", folder_path, "\n")
  }
  
  write.csv(to_csv, paste0(folder_path, "bivariate_regression_results.csv"), row.names = FALSE)
  write.csv(bp_csv, paste0(folder_path, "bp_test_results.csv"), row.names = FALSE)
  write.csv(shapiro_csv, paste0(folder_path, "shapiro_test_results.csv"), row.names = FALSE)
  
  
}

agricultural_data <- read.csv("Data/final_dataset.csv")

# Level of production & Level of consumption
calculate_global_correlation_covariance(agricultural_data, "Level of production", "Level of consumption (at farm gate)")

calculate_correlation_covariance_per_commodity(agricultural_data, "Level of production", "Level of consumption (at farm gate)")

regression_analysis(agricultural_data, "Level of production", "Level of consumption (at farm gate)")

# Level of production & Consumption price
calculate_global_correlation_covariance(agricultural_data, "Level of production", "Consumption price")

calculate_correlation_covariance_per_commodity(agricultural_data, "Level of production", "Consumption price")

regression_analysis(agricultural_data, "Level of production", "Consumption price")

# Level of consumption & Consumption price
calculate_global_correlation_covariance(agricultural_data, "Level of consumption (at farm gate)", "Consumption price")

calculate_correlation_covariance_per_commodity(agricultural_data, "Level of consumption (at farm gate)", "Consumption price")

regression_analysis(agricultural_data, "Level of consumption (at farm gate)", "Consumption price")

# MPD & Producer price
calculate_global_correlation_covariance(agricultural_data, "Market Price Differential", "Producer price (at farm gate)")

calculate_correlation_covariance_per_commodity(agricultural_data, "Market Price Differential", "Producer price (at farm gate)")

regression_analysis(agricultural_data, "Market Price Differential", "Producer price (at farm gate)")


# Bivariate regression

indicators <- c("Market Price Differential", "Level of consumption (at farm gate)", "Consumption price")

# Check for multicollinearity
if(is_there_multicollinearity(agricultural_data, indicators)){
  cat("There is multicollinearity between the two indicators")
} else{
  bivariate_analysis(agricultural_data, indicators)
}
