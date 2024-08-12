# Local import
source("Scripts/data_exploration_functions/data_exploration_geographical_analysis.R")
source("Scripts/data_exploration_functions/data_exploration_distribution_analysis.R")

agricultural_dataset <- read.csv("Data/final_dataset.csv")
#agricultural_dataset$Country <- factor(agricultural_dataset$Country)

main_geographical_analysis(read.csv("Data/cleaned_dataset.csv"))

main_distribution_analysis(agricultural_dataset)