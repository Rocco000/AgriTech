library(dplyr)
library(tidyr)
library(ggplot2)

# Load dataset
data <- read.csv("Data/final_dataset.csv")

selected_indicators <- c("Level of consumption (at farm gate)", "Level of production", "Producer price (at farm gate)")

# Filter data for selected indicators
data <- data %>% filter(Indicator %in% selected_indicators)

# Apply log transformation to the Value column
data$Value <- ifelse(data$Value > 0, log(data$Value), NA)

# Shapiro-Wilk test for normality function
shapiro_test <- function(x) {
  result <- shapiro.test(x)
  return(c(statistic = result$statistic, p.value = result$p.value))
}

# Apply the Shapiro-Wilk test to each group of data (Indicator and Time)
results <- data %>%
  group_by(Indicator, Time) %>%
  summarise(
    shapiro_statistic = shapiro_test(Value)[1],
    p_value = shapiro_test(Value)[2],
    .groups = 'drop'
  )

# Add a column to indicate if the data is normally distributed
results <- results %>%
  mutate(Normal_Distribution = p_value > 0.01)

# Save the results to a CSV file
write.csv(results, "Data/Estimations/shapiro_results_log.csv", row.names = FALSE)

# Function to calculate the confidence interval
confidence_interval <- function(x, conf_level = 0.99) {
  n <- length(x)
  mean_x <- mean(x)
  stderr <- sd(x) / sqrt(n)
  error_margin <- qt(conf_level + (1 - conf_level) / 2, df = n-1) * stderr
  return(c(lower = mean_x - error_margin, upper = mean_x + error_margin))
}

# Calculate the 99% confidence intervals for each group of data (Indicator and Time)
conf_intervals <- data %>%
  group_by(Indicator, Time) %>%
  summarise(
    mean_value = mean(Value),
    lower_CI = confidence_interval(Value, conf_level = 0.99)[1],
    upper_CI = confidence_interval(Value, conf_level = 0.99)[2],
    .groups = 'drop'
  )

# Save the results to a CSV file
write.csv(conf_intervals, "Data/Estimations/confidence_intervals_log.csv", row.names = FALSE)

# select years to plot
years <- c(2005, 2008, 2015, 2020)

# Filter the data for selected years
conf_intervals <- conf_intervals %>% filter(Time %in% years)

# Plot the confidence intervals only if the data is normally distributed
conf_intervals <- conf_intervals %>% filter(Indicator %in% c("Level of consumption (at farm gate)", "Level of production"))
ggplot(conf_intervals, aes(x = Time, y = mean_value, color = Indicator)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +  
  facet_wrap(~ Indicator, scales = "free_y") +  
  labs(title = "99% confidence intervals",
       x = "Year",
       y = "Value") +
  theme_minimal()

# Save the plot
ggsave("Data/Estimations/confidence_intervals_log.png", width = 10, height = 8)
