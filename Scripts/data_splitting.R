# Split dataset based on selected Indicators
sub_datasets <- split(FinalDataset, FinalDataset$Indicator)

names(sub_datasets)

name_sub_dataset <- c("ConsumptionPrice","LevelOfConsumption_AtFarmGate",
                      "LevelOfProduction","MarketPriceDifferential","ProducerPrice_AtFarmGate"
                      )

names(sub_datasets)<-name_sub_dataset

#Save the sub datasets
lapply(names(sub_datasets), function(nome) {
  dataset <- sub_datasets[[nome]] 
  file_name <- file.path("./Data/SubDatasets", paste0(nome, ".csv"))  
  write.csv(dataset, file = file_name, row.names = FALSE)
})