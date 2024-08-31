library(ggplot2)
library(dplyr)
library(stringr)

compute_hist_and_kernel_density_plot <- function(data, commodity, indicator){
  #Filter data for commodity
  filtered_data <- data %>%
    filter(Commodity == commodity)
  
  # Group by country and sum the value for each
  country_indicator <- filtered_data %>%
    group_by(Country) %>%
    summarise(Value = sum(Value))
  
  #Histogram
  options(scipen = 999)
  hist(country_indicator$Value, main = paste("Histogram of", commodity, " ", indicator), xlab = indicator , ylab = "Frequency", col = "lightblue", border = "black", xaxt='n', yaxt='n')
  axis(1, at = pretty(country_indicator$Value), labels = pretty(country_indicator$Value))
  axis(2, at = pretty(hist(country_indicator$Value, plot = FALSE)$counts), 
       labels = pretty(hist(country_indicator$Value, plot = FALSE)$counts))
  
  #save the histogram
  dev.copy(png, file = str_replace_all(paste("Data/DataExploration/Hist&Kernel&ParetoCharts/histogram_", commodity, "_", indicator, ".png")," ", ""))
  dev.off()
  
  #Gaussian kernel density plot
  plot(density(country_indicator$Value), main = paste("Kernel Density Plot of", commodity, " ", indicator), xlab = indicator, ylab = "Density", col = "blue", lwd = 2)
  #save the kernel density plot
  dev.copy(png, file = str_replace_all(paste("Data/DataExploration/Hist&Kernel&ParetoCharts/kernel_density_", commodity, "_", indicator, ".png")," ", ""))
  dev.off()
}

compute_pareto_chart <- function(data, commodity, indicator){
  #Filter data for commodity
  filtered_data <- data %>%
    filter(Commodity == commodity)
  
  # Group by country and sum the value for each
  country_indicator <- filtered_data %>%
    group_by(Country) %>%
    summarise(Value = sum(Value)) %>%
    arrange(desc(Value))
  
  country_indicator <- country_indicator %>%
    mutate(Cumulative_Percentage = cumsum(Value) / sum(Value) * 100)
  
  ggplot(country_indicator, aes(x = reorder(Country, -Value), y = Value)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_line(aes(y = Cumulative_Percentage * max(Value) / 100, group = 1), color = "red") +
    geom_point(aes(y = Cumulative_Percentage * max(Value) / 100), color = "red") +
    scale_y_continuous(
      sec.axis = sec_axis(~ . / max(country_indicator$Value) * 100, name = "Cumulative Percentage")
    ) +
    labs(title = paste("Pareto Chart for", commodity, indicator),
         x = "Country",
         y = indicator) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept = 0.8 * max(country_indicator$Value), linetype = "dashed", color = "blue")
  
  #Save the pareto chart
  ggsave(filename = str_replace_all(paste("Data/DataExploration/Hist&Kernel&ParetoCharts/pareto_chart_", commodity, "_", indicator, ".png")," ", ""), width = 10 , height = 8)
}

# Load data
consumption_price <- read.csv("./Data/SubDatasets/consumption_price.csv")

compute_hist_and_kernel_density_plot(consumption_price, "Maize", "Consumption Price")
compute_pareto_chart(consumption_price, "Maize", "Consumption Price")

#load data
level_of_consumption <- read.csv("./Data/SubDatasets/level_of_consumption_at_farm_gate.csv")

compute_hist_and_kernel_density_plot(level_of_consumption, "Maize", "Level of Consumption")
compute_pareto_chart(level_of_consumption, "Maize", "Level of Consumption")

#load data
level_of_production <- read.csv("./Data/SubDatasets/level_of_production.csv")

compute_hist_and_kernel_density_plot(level_of_production, "Maize", "Level of Production")
compute_pareto_chart(level_of_production, "Maize", "Level of Production")

#load data
market_price_differential <- read.csv("./Data/SubDatasets/market_price_differential.csv")

compute_hist_and_kernel_density_plot(market_price_differential, "Maize", "Market Price Differential")
compute_pareto_chart(market_price_differential, "Maize", "Market Price Differential")

#load data
producer_price_at_farm_gate <- read.csv("./Data/SubDatasets/producer_price_at_farm_gate.csv")

compute_hist_and_kernel_density_plot(producer_price_at_farm_gate, "Maize", "Producer Price at Farm Gate")
compute_pareto_chart(producer_price_at_farm_gate, "Maize", "Producer Price at Farm Gate")




