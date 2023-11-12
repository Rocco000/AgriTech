# Data exploration
summary_dataframe<-data.frame(Indicator=character(0),
                              Min=numeric(0),
                              First_quartile=numeric(0),
                              Median=numeric(0),
                              Mean=numeric(0),
                              Third_quartile=numeric(0),
                              Max=numeric(0),
                              Zero_values=numeric(0))
for(selected_indicator in 1:25){
  
  if(selected_indicator==1){
    
    data_euro = subset(BudgetaryTransfers, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Budgetary transfers"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)

  } else if(selected_indicator==2){
    
    result <- summary(ConsumerNominalProtectionCoefficient$Value)
    print(result)
    zeros <- sum(ConsumerNominalProtectionCoefficient$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Consumer nominal protection coefficient"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if (selected_indicator==3){
    
    data_euro = subset(ConsumerSingleCommodityTransfers, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Consumer single commodity transfers"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==4){
    
    data_euro = subset(ConsumptionPrice, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Consumption price"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==5){
    
    data_euro = subset(ExcessFeedCost_Crops, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Excess feed costs (crops)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==6){
    
    data_euro = subset(ExcessFeedCost_Livestock, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Excess feed costs (livestok)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==7){
    
    data_euro = subset(ExcessFeedCost, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Excess feed cost"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==8){
    
    result <- summary(LevelOfConsumption_AtFarmGate$Value)
    print(result)
    zeros <- sum(LevelOfConsumption_AtFarmGate$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Level of consumption (at farm gate)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==9){
    
    result <- summary(LevelOfProduction$Value)
    print(result)
    zeros <- sum(LevelOfProduction$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Level of production"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==10){
    
    data_euro = subset(MarketPriceDifferential, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Market price differential"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==11){
    
    data_euro = subset(MarketPriceSupport, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Market price support"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==12){
    
    data_euro = subset(OtherTransfersFromConsumers, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Other transfers from consumers"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==13){
    
    data_euro = subset(PaymentsBasedOnOutput, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Payments based on output"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==14){
    
    data_euro = subset(PaymentsBasedOnOutputPerTonne, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Payments based on output per tonne"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==15){
    
    result <- summary(PercentageSingleCommodityTransfers$Value)
    print(result)
    zeros <- sum(PercentageSingleCommodityTransfers$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Percentange single commodity transfers"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==16){
    
    data_euro = subset(PriceLevies, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Price levies"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==17){
    
    result <- summary(ProducerNominalProtectionCoefficient$Value)
    print(result)
    zeros <- sum(ProducerNominalProtectionCoefficient$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Producer nominal protection coefficient"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==18){
    
    data_euro = subset(ProducerPrice_AtFarmGate, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Producer price (at farm gate)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==19){
    
    data_euro = subset(ProducerSingleCommodityTransfers, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Producer single commodity transfers"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==20){
    
    data_euro = subset(ReferencePrice_AtFarmGate, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Reference price (at farm gate)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==21){
    
    data_euro = subset(TransfersToConsumersFromTaxpayers_ForCommodities, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Transfers to consumers from taxpayers (from commodities)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==22){
    
    data_euro = subset(TransfersToProducersFromConsumers, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Transfers to producers from consumers"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==23){
    
    data_euro = subset(TransfersToProducersFromTaxpayers, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Transfers to producers from taxpayers"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==24){
    
    data_euro = subset(ValueOfConsumption_AtFarmGate, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Value of consumption (at farm gate)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  } else if(selected_indicator==25){
    
    data_euro = subset(ValueOfProduction_AtFarmGate, Unit=="Euro")
    data_euro
    result <- summary(data_euro$Value)
    print(result)
    zeros <- sum(data_euro$Value == 0)
    print(zeros)
    new_row<-data.frame(Indicator=c("Value of production (at farm gate)"),
                        Min=c(result[1]),
                        First_quartile=c(result[2]),
                        Median=c(result[3]),
                        Mean=c(result[4]),
                        Third_quartile=c(result[5]),
                        Max=c(result[6]),
                        Zero_values=c(zeros))
    summary_dataframe<-rbind(summary_dataframe,new_row)
    
  }
}
write.csv(summary_dataframe, "Data/IndicatorsSummary.csv", row.names = FALSE)