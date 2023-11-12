# Data exploration functions


#Get countries
get_countries <- function(subdataset){
  countries <- unique(subdataset$Country)
  return(countries)
}

#Get commodities
get_commodities <- function(subdataset){
  commodities <- unique(subdataset$Commodity)
  return(commodities)
}

# Data exploration per country

data_exploration_per_country <- function(subdataset, folder_name){
  library(stringr)
  
  #Get the countries
  countries <- get_countries(subdataset)
  
  countries
  
  csv_country <- data.frame(
    Country = character(0),
    Min = numeric(0),
    First_quartile = numeric(0),
    Median = numeric(0),
    Mean = numeric(0),
    Third_quartile = numeric(0),
    Max = numeric(0)
  )
  
  
  for (country in countries) {
    subset_country <- subset(subdataset, Country == country)
    x <- summary(subset_country$Value)
    df <- data.frame(
      Country = c(country),
      Min = c(x[1]),
      First_quartile = c(x[2]),
      Median = c(x[3]),
      Mean = c(x[4]),
      Third_quartile = c(x[5]),
      Max = c(x[6])
    )
    
    if (nrow(csv_country) == 0) {
      csv_country <- df
    } else {
      csv_country <- rbind(csv_country, df)
    }
  }
  
  folder_path <- paste("./Data/DataExploration/", folder_name)
  folder_path <- str_replace_all(folder_path, " ", "")
  file_path <- paste(folder_path,"/DataExplorationPerCountry.csv")
  file_path <- str_replace_all(file_path, " ", "")
  
  write.csv(csv_country, file = file_path, row.names = FALSE)
}


# Data exploration per commodity
data_exploration_per_commodity <- function(subdataset, folder_name){
  library(stringr)
  
  #Get commodities
  commodities <- get_commodities(subdataset)
  
  commodities
  
  csv_commodity <- data.frame(
    Commodity = character(0),
    Min = numeric(0),
    First_quartile = numeric(0),
    Median = numeric(0),
    Mean = numeric(0),
    Third_quartile = numeric(0),
    Max = numeric(0)
  )
  
  for (commodity in commodities) {
    subset_commodity <- subset(subdataset, Commodity == commodity)
    x <- summary(subset_commodity$Value)
    df <- data.frame(
      Commodity = c(commodity),
      Min = c(x[1]),
      First_quartile = c(x[2]),
      Median = c(x[3]),
      Mean = c(x[4]),
      Third_quartile = c(x[5]),
      Max = c(x[6])
    )
    
    if (nrow(csv_commodity) == 0) {
      csv_commodity <- df
    } else {
      csv_commodity <- rbind(csv_commodity, df)
    }
  }
  
  folder_path <- paste("./Data/DataExploration/", folder_name)
  folder_path <- str_replace_all(folder_path, " ", "")
  file_path <- paste(folder_path,"/DataExplorationPerCommodity.csv")
  file_path <- str_replace_all(file_path, " ", "")
  
  write.csv(csv_commodity, file = file_path, row.names = FALSE)
}



#Data exploration per country & commodity

exploration_per_country_commodity <- function(data, country, commodity){
  sub_dataset <- subset(data, Country == country & Commodity == commodity)
  summary(sub_dataset$Value)
}

data_exploration_per_country_and_commodity <- function(subdataset, folder_name){
  library(stringr)
  
  csv_country_commodity <- data.frame(
    Country_Commodity = character(0),
    Min = numeric(0),
    First_quartile = numeric(0),
    Median = numeric(0),
    Mean = numeric(0),
    Third_quartile = numeric(0),
    Max = numeric(0)
  )
  
  countries <- get_countries(subdataset)
  commodities <- get_commodities(subdataset)
  
  
  for (country in countries){
    for (commodity in commodities){
      
      s1 <- paste(country,commodity)
      s2 <- str_replace_all(s1, " ", "_")
      s2
      
      result_exploration <- exploration_per_country_commodity(subdataset, country, commodity)
      
      new_data <- data.frame(
        Country_Commodity = c(s2),
        Min = c(result_exploration[1]),
        First_quartile = c(result_exploration[2]),
        Median = c(result_exploration[3]),
        Mean = c(result_exploration[4]),
        Third_quartile = c(result_exploration[5]),
        Max = c(result_exploration[6])
      )
      
      if (nrow(csv_country_commodity) == 0) {
        csv_country_commodity <- new_data
      } else {
        csv_country_commodity <- rbind(csv_country_commodity, new_data)
      }
    }
  }
  
  folder_path <- paste("./Data/DataExploration/", folder_name)
  folder_path <- str_replace_all(folder_path, " ", "")
  file_path <- paste(folder_path,"/DataExplorationPerCountryCommodity.csv")
  file_path <- str_replace_all(file_path, " ", "")
  
  write.csv(csv_country_commodity, file = file_path, row.names = FALSE)
}




