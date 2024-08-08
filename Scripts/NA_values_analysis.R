na_counter_per_year <- function(subdataset){
  # Count NA values per year
  na_count_per_year_indicator <- aggregate(Value ~ Time + Indicator, data = subdataset, function(x) sum(is.na(x)))
}

na_values <- na_counter_per_year(cleaned_dataset)
na_values <- subset(na_values, Indicator %in% c("Level of production","Level of consumption (at farm gate)","Consumption price","Producer price (at farm gate)","Market Price Differential"))

# Load ggplot2
library(ggplot2)

# Build temporal series
ggplot(na_values, aes(x = Time, y = Value, color = Indicator, group = Indicator)) +
  geom_line() + 
  geom_point() +
  labs(title = "Conteggio dei valori NA",
       x = "Anno",
       y = "Numero di Valori NA") +
  theme_minimal()

ggsave("./Data/DataExploration/NA_values_analysis.png")