library(randomForest)
library(openxlsx)

rm(list=ls())
wd=setwd("Z://transfer//projects//TRIO//R//PredictRF")



rf_model <- readRDS("Z:/transfer/projects/TRIO/R/AnalyseWheatR/results/wheat/models_M/rf/bm/meanmedianminmaxstd_spectral_height.rds")

results_path <- file.path(wd, "results")
new_data <- read.xlsx(file.path(results_path, "stats","wheat", "join", "GGE_ZE1.xlsx"))


#get variables of trained data
predictors <- names(rf_model$trainingData)
predictors <- predictors[!predictors %in% c(".outcome")]


#Subset new data accordingly
new_data_subset <- new_data[, predictors, drop = FALSE]
new_data_subset[is.na(new_data_subset)] <- 0


#predicting
predictions <- predict(rf_model, newdata = new_data_subset)

result <- cbind(FID = new_data$FID, prediction = predictions)
write.xlsx(result, file = file.path(results_path, "predict","wheat", "GGE_ZE1_predict.xlsx"), rowNames = FALSE)

