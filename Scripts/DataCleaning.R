# Delete irrelevant features and select period 2010-2021
selected_features <- c("Country","Commodity","Indicator","Measure","Time","Unit","PowerCode","Value")

cleaned_data <- subset(OriginData, select = selected_features)

columns <- names(cleaned_data)
print(columns)

cleaned_data <- subset(cleaned_data, Time %in% c(2010,2021))

write.csv(cleaned_data, "./Data/CleanedData.csv", row.names = FALSE)