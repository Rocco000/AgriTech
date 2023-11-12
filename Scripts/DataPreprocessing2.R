# Split dataset based on Indicator values
sub_datasets <- split(CleanedData, CleanedData$Indicator)

names(sub_datasets)

name_sub_dataset <- c("BudgetaryTransfers","ConsumerNominalProtectionCoefficient",
                      "ConsumerSingleCommodityTransfers","ConsumptionPrice",
                      "ExcessFeedCost","ExcessFeedCost_Crops",
                      "ExcessFeedCost_Livestock","LevelOfConsumption_AtFarmGate",
                      "LevelOfProduction","MarketPriceDifferential",
                      "MarketPriceSupport","OtherTransfersFromConsumers",
                      "PaymentsBasedOnOutput","PaymentsBasedOnOutputPerTonne",
                      "PercentageSingleCommodityTransfers","PriceLevies",
                      "ProducerNominalProtectionCoefficient","ProducerPrice_AtFarmGate",
                      "ProducerSingleCommodityTransfers","ReferencePrice_AtFarmGate",
                      "TransfersToConsumersFromTaxpayers_ForCommodities","TransfersToProducersFromConsumers",             
                      "TransfersToProducersFromTaxpayers","ValueOfConsumption_AtFarmGate",
                      "ValueOfProduction_AtFarmGate"
                      )

names(sub_datasets)<-name_sub_dataset

#Save the sub datasets
lapply(names(sub_datasets), function(nome) {
  dataset <- sub_datasets[[nome]] 
  file_name <- file.path("./Data/SubDatasets", paste0(nome, ".csv"))  
  write.csv(dataset, file = file_name, row.names = FALSE)
})