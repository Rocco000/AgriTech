# Delete irrelevant features and select period 2010-2021
selected_features <- c("Country","Commodity","Indicator","Measure","Time","Unit","PowerCode","Value")

cleaned_data <- subset(OriginData, select = selected_features)

columns <- names(cleaned_data)
print(columns)

#Extract years of interest
cleaned_data <- subset(cleaned_data, Time %in% 2010:2021)

#Extract unit of interest
cleaned_data<- subset(cleaned_data, Measure %in% c("Tonnes","Euro","Percentage","Ratio"))

write.csv(cleaned_data, "./Data/CleanedData.csv", row.names = FALSE)