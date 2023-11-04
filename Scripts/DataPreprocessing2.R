# Split dataset based on Indicator values
sub_datasets <- split(CleanedData, CleanedData$Indicator)

names(sub_datasets)

name_sub_dataset <- c("BudgetaryTransfers","ConsumerNominalProtectionCoefficient",
                      "ConsumerSingleCommodityTransfers","ConsumptionPrice",
                      "ExcessFeedCost","ExcessFeedCost(Crops)",
                      "Excessfeedcost(Livestock)","LevelOfConsumption(AtFarmGate)",
                      "LevelOfProduction","MarketPriceDifferential",
                      "MarketPriceSupport","OtherTransfersFromConsumers",
                      "PaymentsBasedOnOutput","PaymentsBasedOnOutputPerTonne",
                      "PercentageSingleCommodityTransfers","PriceLevies",
                      "ProducerNominalProtectionCoefficient","ProducerPrice(AtFarmGate)",
                      "ProducerSingleCommodityTransfers","ReferencePrice(AtFarmGate)",
                      "TransfersToConsumersFromTaxpayers(ForCommodities)","TransfersToProducersFromConsumers",             
                      "TransfersToProducersFromTaxpayers","ValueOfConsumption(AtFarmGate)",
                      "ValueOfProduction(AtFarmGate)"
                      )

names(sub_datasets)<-name_sub_dataset

#Save the sub datasets
lapply(names(sub_datasets), function(nome) {
  dataset <- sub_datasets[[nome]] 
  file_name <- file.path("./Data/", paste0(nome, ".csv"))  
  write.csv(dataset, file = file_name, row.names = FALSE)
})