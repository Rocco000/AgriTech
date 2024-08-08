#Load the dataset
final_dataset <- read.csv("./Data/final_dataset.csv")

# Split dataset based on selected Indicators
sub_datasets <- split(final_dataset, final_dataset$Indicator)

names(sub_datasets)

name_sub_dataset <- c("consumption_price","level_of_consumption_at_farm_gate",
                      "level_of_production","market_price_differential","producer_price_at_farm_gate"
                      )

names(sub_datasets)<-name_sub_dataset

#Save the sub datasets
lapply(names(sub_datasets), function(name) {
  dataset <- sub_datasets[[name]] 
  file_name <- file.path("./Data/SubDatasets", paste0(name, ".csv"))  
  write.csv(dataset, file = file_name, row.names = FALSE)
})