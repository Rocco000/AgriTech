data_cleaning <- function(){
  origin_data <- read.csv("Data/origin_data.csv")
  
  # Delete irrelevant features
  selected_features <- c("Country","Commodity","Indicator","Measure","Time","Unit","PowerCode","Value")
  
  cleaned_data <- subset(origin_data, select = selected_features)
  
  columns <- names(cleaned_data)
  print(columns)
  
  #Extract unit of interest
  cleaned_data<- subset(cleaned_data, Measure %in% c("Tonnes","Euro","Percentage","Ratio"))
  
  #Scale the values
  cleaned_data$Value <- ifelse(cleaned_data$Measure == "Tonnes" & cleaned_data$PowerCode == "Thousands", cleaned_data$Value * 1000, cleaned_data$Value)
  
  cleaned_data$Value <- ifelse(cleaned_data$Measure == "Euro" & cleaned_data$PowerCode == "Millions", cleaned_data$Value * 1000000, cleaned_data$Value)
  
  write.csv(cleaned_data, "./Data/cleaned_dataset.csv", row.names = FALSE)
}

data_cleaning()

feature_selection <- function(){
  cleaned_dataset <- read.csv("Data/cleaned_dataset.csv")
  
  # Delete irrelevant features
  selected_features <- c("Country","Commodity","Indicator","Measure","Time","Unit","Value")
  
  final_data <- subset(cleaned_dataset, select = selected_features)
  
  # Extract years of interest
  final_data <- subset(final_data, Time %in% 2005:2020)
  
  # Extract indicators of interest
  final_data <-  subset(final_data, Indicator %in% c("Level of production","Level of consumption (at farm gate)","Consumption price","Producer price (at farm gate)","Market Price Differential"))
  
  # Extract commodities of interest
  final_data <- subset(final_data, Commodity %in% c("Wheat", "Soybeans", "Maize", "Rice", "Sugar", "Oats", "Beef and veal", "Coffee"))
  
  #Save the csv file
  write.csv(final_data, "./Data/final_dataset.csv", row.names = FALSE)
}

feature_selection()