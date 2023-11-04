# Delete irrelevant features
selected_features <- c("Country","Commodity","Indicator","Measure","Time","Unit","PowerCode","Value")

cleaned_data <- subset(OriginData, select = selected_features)

columns <- names(cleaned_data)
print(columns)

write.csv(cleaned_data, "./Data/CleanedData.csv", row.names = FALSE)