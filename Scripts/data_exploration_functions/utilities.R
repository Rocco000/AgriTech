# Get countries
get_countries <- function(ds){
  return(unique(ds$Country))
}

# Get commodities
get_commodities <- function(ds){
  return(unique(ds$Commodity))
}

# Get indicators
get_indicators <- function(ds){
  return(unique(ds$Indicator))
}

# Get years
get_years <- function(ds){
  return(unique(ds$Time))
}

# Add the continent column to dataset
add_continent <- function(dataset){
  dataset_with_continent <- dataset %>% mutate(Continent = countrycode(Country, "country.name", "continent"))
  return(dataset_with_continent)
}