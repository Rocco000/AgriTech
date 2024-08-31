library(tidyr)

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

check_normal_distribution <- function(ds){
  years <- get_years(ds)
  
  to_csv <- data.frame(
    Group = character(),
    Time = numeric(),
    W = numeric(),
    p_value = numeric()
  )
  
  for(year in years){
    # Get the data for the year
    filtered_data <- subset(pivoted_data, Time == year)
    
    # Get the data for the countries that have applied the policy
    policy_applied <- subset(filtered_data, PolicyApplied == "Yes")
    
    no_policy_applied <- subset(filtered_data, PolicyApplied == "No")
    
    # Check if the data is normally distributed
    results <- shapiro.test(policy_applied$`Consumption price`)
    
    to_csv <- rbind(to_csv, data.frame(Group = "Applied", Time = year, W = results$statistic, p_value = results$p.value))
    
    results <- shapiro.test(no_policy_applied$`Consumption price`)
    
    to_csv <- rbind(to_csv, data.frame(Group = "NO applied", Time = year, W = results$statistic, p_value = results$p.value))
    
  }
  
  return(to_csv)
}

# Wilcoxon-Mann-Whitney U test
mann_whitney_u_test <- function(ds){
  # Get all years
  years <- get_years(ds)
  
  to_csv <- data.frame(
    Year = numeric(),
    Statistic_U = numeric(),
    p_value = numeric()
  )
  
  for(year in years){
    # Get the data for the year
    filtered_data <- subset(ds, Time == year)
    
    # Get the data for the countries that have applied the policy
    policy_applied <- subset(filtered_data, PolicyApplied == "Yes")
    
    no_policy_applied <- subset(filtered_data, PolicyApplied == "No")
    
    results <- wilcox.test(policy_applied$`Consumption price`, no_policy_applied$`Consumption price`)
    
    to_csv <- rbind(to_csv, data.frame(Year = year, Statistic_U = results$statistic, p_value = results$p.value))
    
  }
  
  return(to_csv)
}

agricultural_data <- read.csv("Data/final_dataset.csv")

pivoted_data <- pivoting_data(agricultural_data, c("Market Price Differential", "Consumption price"))

# Check which countries have applied any policy
pivoted_data$PolicyApplied <- ifelse(pivoted_data$`Market Price Differential` != 0, "Yes", "No")

to_csv <- check_normal_distribution(pivoted_data)

if (!dir.exists("Data/Ipotesi")) {
  dir.create("Data/Ipotesi", recursive = TRUE)
  cat("Folder created: Data/Ipotesi\n")
}

write.csv(to_csv, "Data/Ipotesi/shapiro_test.csv", row.names = FALSE)

# Log transformation
log_data <- pivoted_data
log_data$`Consumption price` <- log(log_data$`Consumption price`)

to_csv <- check_normal_distribution(log_data)

write.csv(to_csv, "Data/Ipotesi/shapiro_test_log.csv", row.names = FALSE)

# Wilcoxon-Mann-Whitney U test
to_csv <- mann_whitney_u_test(pivoted_data)

write.csv(to_csv, "Data/Ipotesi/mann_whitney_u_test.csv", row.names = FALSE)



