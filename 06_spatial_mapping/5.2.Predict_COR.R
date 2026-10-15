library(randomForest)
library(openxlsx)

rm(list=ls())
wd=setwd("Z://transfer//projects//TRIO//R//PredictRF")

#caraway

model <- readRDS("Z:/transfer/projects/TRIO/R/AnalyseMedicinal/results/Coriander/models_M/plsr/bm/meanmedianminmaxstd_spectral_height.rds")

results_path <- file.path(wd, "results")

field_id <- "DFH_ZE3"
new_data <- read.xlsx(file.path(results_path, "stats", "medicinal", "join", paste0(field_id, "_m.xlsx")))



#get variables of trained data
predictors <- names(model$trainingData)
predictors <- predictors[!predictors %in% c(".outcome")]


#Subset new data accordingly
new_data_subset <- new_data[, predictors, drop = FALSE]
new_data_subset[is.na(new_data_subset)] <- 0


#predicting
predictions <- predict(model, newdata = new_data_subset)
cat("Number of negative predictions set to zero:", sum(predictions < 0), "\n")
predictions[predictions < 0] <- 0


result <- cbind(FID = new_data$FID, prediction = predictions)
write.xlsx(result, file = file.path(results_path, "predict", "medicinal", "coriander", paste0(field_id, "_predict_m_co.xlsx")), rowNames = FALSE)
