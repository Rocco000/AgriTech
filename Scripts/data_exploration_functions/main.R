library(dplyr)
library(ggplot2)
library(tidyr)
library(rlang)
library(RColorBrewer)

# Local import
source("Scripts/data_exploration_functions/data_exploration_countries_coverage.R")


# Get only the unique values in country field
agricultural_data <- data.frame(Country= unique(CleanedData$Country))

# Add continent
agricultural_data <- add_continent(agricultural_data)

# Check for any unmatched countries
unmatched <- agricultural_data[is.na(agricultural_data$Continent), "Country"]
print("Countries with NA value in Continent field:")
print(unique(unmatched))

# Count the number of countries per continent
num_countries_per_continent_in_ds <- get_num_countries_per_continent(agricultural_data, "Countries_in_dataset")

print("Number of countries per continent in our dataset:")
num_countries_per_continent_in_ds 

# Bar plot for the number of countries per continent in our dataset
ggplot(num_countries_per_continent_in_ds, aes(x = Continent, y = Countries_in_dataset, fill=Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of countries per continent in dataset", x = "Continent", y = "Number of countries") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggsave("Data/DataExploration/countries_per_continent.png")

# Our dataset contains other countries because the value OECD as country
# represents 38 countries, instead European Union represents the 5 countries that are not in OECD group
oecd_countries <- data.frame(
  Country = c("Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Latvia","Lithuania","Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovakia","Slovenia","South Korea","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States"),
  Continent = rep(NA,38)
)

# Remove the tuples with OECD or European Union as value in country field
agricultural_data <- agricultural_data %>% filter(Country!="OECD - Total" & Country!="European Union")

# Substitute the OECD values with the countries name which are member of OECD
agricultural_data <- rbind(agricultural_data, oecd_countries)

# Re-add the continent for this new values
agricultural_data <- add_continent(agricultural_data)

# Re-count the number of countries per continent in our dataset
num_countries_per_continent_in_ds <- get_num_countries_per_continent(agricultural_data, "Countries_in_dataset")

# Re-plot
ggplot(num_countries_per_continent_in_ds, aes(x = Continent, y = Countries_in_dataset, fill=Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Updated plot of countries per continent in dataset", x = "Continent", y = "Number of countries") +
  theme_minimal()+
  scale_fill_brewer(palette = "Set1")

ggsave("Data/DataExploration/updated_countries_per_continent.png")

# Plot the countries coverage
plot_countries_coverage(num_countries_per_continent_in_ds, "Data/DataExploration/countries_coverage.png", "Data/DataExploration/countries_coverage2.png")