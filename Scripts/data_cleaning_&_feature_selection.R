data_cleaning <- function(){
  # Delete irrelevant features
  selected_features <- c("Country","Commodity","Indicator","Measure","Time","Unit","PowerCode","Value")
  
  cleaned_data <- subset(origin_data, select = selected_features)
  
  columns <- names(cleaned_data)
  print(columns)
  
  #Extract unit of interest
  cleaned_data<- subset(cleaned_data, Measure %in% c("Tonnes","Euro","Percentage","Ratio"))
  
  write.csv(cleaned_data, "./Data/cleaned_dataset.csv", row.names = FALSE)
}

data_cleaning()

feature_selection <- function(){
  #Extract years of interest
  final_data <- subset(cleaned_dataset, Time %in% 2005:2020)
  
  #Extract indicators of interest
  final_data <-  subset(final_data, Indicator %in% c("Level of production","Level of consumption (at farm gate)","Consumption price","Producer price (at farm gate)","Market Price Differential"))
  
  #Save the csv file
  write.csv(final_data, "./Data/final_dataset.csv", row.names = FALSE)
}

feature_selection()