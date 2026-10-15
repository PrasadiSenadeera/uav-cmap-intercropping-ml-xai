# Author: Prasadi Sendeera
# Date: 2025-04-01
# Time: 09:55
# To model wheat biomass using ffs method on several classifiers (wheat)


rm(list=ls())
setwd("Z:/transfer/projects/TRIO/R/2024/AnalyseWheatR")
conflicted::conflicts_prefer(dplyr::select)

library(dplyr); library(readxl); library(PerformanceAnalytics); library(writexl); library(terra); library(caret);
library(geodata); library(CAST); library(tidyr); library(randomForest); library(ggplot2); library(shapviz); library(tmap);
library(sf)

response_var= "DM_PerArea"

clean_data <- function(file_path, remove_crops, remove_partners, filter_na, rename_map = NULL, canopy_cover = FALSE, remove_extra_cols = FALSE, type) {
  df <- read_excel(file_path) %>%
    dplyr::filter(!(Factor_crop %in% remove_crops), !(Partner %in% remove_partners), !is.na(.data[[filter_na]]))
  
  
  
  if (remove_extra_cols) {
    col_names <- colnames(df)
    pixel_count_cols <- grep("_pixel_count$", col_names, value = TRUE)
    area_m2_cols <- grep("_area_m2$", col_names, value = TRUE)
    df <- dplyr::select(df, -c(pixel_count_cols[-1], area_m2_cols[-1]))
  }
  
  if (!is.null(rename_map)) {
    names(df) <- ifelse(names(df) %in% names(rename_map), rename_map[names(df)], names(df))
  }
  
  if (canopy_cover && all(c("crop_area", "Total_area") %in% names(df))) {
    df <- mutate(df, Canopy_cover = crop_area / Total_area * 100)
  }
  
  df <- df %>%
    mutate(Site = factor(Site, levels = c("DFH", "FOR", "GGE", "WBG")),
           Growth_stage = factor(Growth_stage, levels = c("ZE1", "ZE2", "ZE3"))) %>%
    arrange(Site, Growth_stage) %>%
    group_by(Site, Growth_stage) %>%
    mutate(CV_fold = paste0(Site, "_", Growth_stage)) %>%
    ungroup()
  
  
  df <- df %>% mutate(DM_PerArea = (`DM_Total(g)` / Total_area))
  
  return(df)
}

#### load data ####
spectral_file <- "data/wheat/spectral_and_situData_W.xlsx"
spectral_rename <- c("Red_pixel_count" = "crop_pixelCount", "Red_area_m2" = "crop_area")
spectral_df <- clean_data(spectral_file, c("COR", "CAR"), c("Caraway", "Coriander"), "DM_Total(g)", spectral_rename, TRUE, TRUE, "spectral")
write.xlsx( spectral_df,"Z:/transfer/projects/TRIO/R/2025/Process2025/results/ZEcut_stat/2024/spectral_2024_ca.xlsx",overwrite = TRUE)

height_file <- "data/wheat/height_and_situData_W.xlsx"
height_rename <- c( "H_area_m2" = "H_area", "H_pixel_count" = "pixelCount")
height_df <- clean_data(height_file, c("COR", "CAR"), c("Caraway", "Coriander"), "Mean_height", height_rename, FALSE, FALSE, "height")
write.xlsx( spectral_df,"Z:/transfer/projects/TRIO/R/2025/Process2025/results/ZEcut_stat/2024/height_2024_ca.xlsx",overwrite = TRUE)

# Plot biomass distribution
ggplot(spectral_df, aes(x = Site, y = `DM_PerArea`, fill = Growth_stage)) + geom_boxplot()


#### Functions to train and evaluate models ####
rf_model <- function(df, predictors,  response_var=response_var,spacevar = "Site") {
  indices <- CreateSpacetimeFolds(df, spacevar = spacevar, k = 4)
  
  set.seed(10)
  model <- ffs(
    df[, predictors],
    df[[response_var]],
    method = "rf",
    tuneGrid = expand.grid(mtry = round(length(predictors) / 3, 0)), 
    preProcess = c("center", "scale"), 
    ntree = 500,
    verbose = FALSE,
    trControl = trainControl(method = "cv", index = indices$index, savePredictions = "final")
  )
  
  summary_df <- df %>% group_by(!!sym(spacevar)) %>% reframe(DM_range = diff(range(!!sym(response_var))), DM_mean = mean(!!sym(response_var)))
  
  results <- model$resample %>%
    mutate(LO_Site = unique(df[[spacevar]]), DM_range = summary_df$DM_range, DM_mean = summary_df$DM_mean, 
           nRMSE_range = RMSE / DM_range, nRMSE_mean = RMSE / DM_mean) %>% 
    dplyr::select(Resample, LO_Site, everything())
  
  
  ggplot(data = model$pred, aes(y = obs, x = pred, col = Resample)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) +
    xlim(c(0, 1200)) + ylim(c(0, 1200)) +
    coord_equal(ratio = 1)
  
  return(list(model = model, summary = results))
}
svm_model <- function(df, predictors, response_var = response_var, spacevar = "Site") {
  indices <- CreateSpacetimeFolds(df, spacevar = spacevar, k = 4)
  
  set.seed(10)
  model <- ffs(
    df[, predictors],
    df[[response_var]],
    method = "svmRadial",
    tuneGrid = expand.grid(sigma = 0.1, C = 1), 
    preProcess = c("center", "scale"), 
    verbose = FALSE,
    trControl = trainControl(method = "cv", index = indices$index, savePredictions = "final")
  )
  
  summary_df <- df %>% group_by(!!sym(spacevar)) %>% reframe(DM_range = diff(range(!!sym(response_var))), DM_mean = mean(!!sym(response_var)))
  
  results <- model$resample %>%
    mutate(LO_Site = unique(df[[spacevar]]), DM_range = summary_df$DM_range, DM_mean = summary_df$DM_mean, 
           nRMSE_range = RMSE / DM_range, nRMSE_mean = RMSE / DM_mean) %>% 
    dplyr::select(Resample, LO_Site, everything())
  
  
  ggplot(data = model$pred, aes(y = obs, x = pred, col = Resample)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) +
    xlim(c(0, 1200)) + ylim(c(0, 1200)) +
    coord_equal(ratio = 1)
  
  return(list(model = model, summary = results))
}
plsr_model <- function(df, predictors, response_var = response_var, spacevar = "Site") {
  indices <- CreateSpacetimeFolds(df, spacevar = spacevar, k = 4)
  
  set.seed(10)
  model <- ffs(
    df[, predictors],
    df[[response_var]],
    method = "pls",  # Change method to "pls" for Partial Least Squares Regression
    tuneGrid = expand.grid(ncomp = 1:10),  # Number of components to test
    preProcess = c("center", "scale"),  # Standardization
    verbose = FALSE,
    trControl = trainControl(method = "cv", index = indices$index, savePredictions = "final")
  )
  
  summary_df <- df %>% group_by(!!sym(spacevar)) %>% reframe(DM_range = diff(range(!!sym(response_var))), DM_mean = mean(!!sym(response_var)))
  
  results <- model$resample %>%
    mutate(LO_Site = unique(df[[spacevar]]), DM_range = summary_df$DM_range, DM_mean = summary_df$DM_mean, 
           nRMSE_range = RMSE / DM_range, nRMSE_mean = RMSE / DM_mean) %>% 
    dplyr::select(Resample, LO_Site, everything())
  
  ggplot(data = model$pred, aes(y = obs, x = pred, col = Resample)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) +
    xlim(c(0, 1200)) + ylim(c(0, 1200)) +
    coord_equal(ratio = 1)
  
  return(list(model = model, summary = results))
}
xgb_model <- function(df, predictors, response_var = response_var, spacevar = "Site") {
  indices <- CreateSpacetimeFolds(df, spacevar = spacevar, k = 4)
  
  tuneGrid <- expand.grid(nrounds = 100, # Boosting iterations
                          max_depth = c(2), # Maximal tree depth
                          eta = seq(0.05, 0.25, by = 0.05), # Shrinkage parameter
                          gamma = c(0), # Minimum loss reduction
                          colsample_bytree = 1, # Subsample ratio of columns
                          min_child_weight = 1, # Minimum sum of instance weight
                          subsample = 1) # Subsample percentage
  set.seed(10)
  model <- ffs(
    df[, predictors],
    df[[response_var]],
    method = "xgbTree",
    tuneGrid = tuneGrid,
    preProcess = c("center", "scale"),
    verbose = FALSE,
    trControl = trainControl(method = "cv", index = indices$index, savePredictions = "final"),
    metric = "RMSE"
  )
  # Generate summary for the results
  summary_df <- df %>% group_by(!!sym(spacevar)) %>%
    reframe(DM_range = diff(range(!!sym(response_var))), DM_mean = mean(!!sym(response_var)))
  
  results <- model$resample %>%
    mutate(LO_Site = unique(df[[spacevar]]),
           DM_range = summary_df$DM_range,
           DM_mean = summary_df$DM_mean,
           nRMSE_range = RMSE / DM_range,
           nRMSE_mean = RMSE / DM_mean) %>%
    dplyr::select(Resample, LO_Site, everything())
  
  # Plot predictions
  ggplot(data = model$pred, aes(y = obs, x = pred, col = Resample)) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) +
    xlim(c(0, 1200)) + ylim(c(0, 1200)) +
    coord_equal(ratio = 1)
  
  return(list(model = model, summary = results))
}


#### Prepare data sets ####
spectral_BM <- spectral_df %>%
  dplyr::select(ends_with("_max"), "Feature_ID", "crop_area", "CV_fold", "DM_Total(g)", "FM_Total(g)", "Site", "DM_PerArea","LAI") %>% 
  dplyr::filter(!is.na(Blue_max))
height_BM <- height_df %>% 
  dplyr::select(starts_with("H_"), "Feature_ID", "H_area", "Site", "DM_Total(g)", "FM_Total(g)", "DM_PerArea","LAI")
combined_BM <- height_BM %>% 
  full_join(spectral_BM, by = "Feature_ID") %>% select(-ends_with(".x")) %>% rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))


#### Define predictors ####
predictors_S <- spectral_BM %>% dplyr::select(ends_with("_mean"),ends_with("_median"),ends_with("_max"),ends_with("_min"),ends_with("_std"),"crop_area") %>% names()
predictors_H <- height_BM %>% dplyr::select(starts_with("H_")) %>% names()
predictors_CM <- combined_BM %>% dplyr::select(starts_with("H_"),ends_with("_mean"),ends_with("_median"),ends_with("_max"),ends_with("_min"),ends_with("_std"),"crop_area") %>% names()

#### Run models- RF ####

S_RF  <- rf_model(spectral_BM, predictors_S, response_var)
H_RF <- rf_model(height_BM, predictors_H,response_var)
CM_RF <- rf_model(combined_BM, predictors_CM,response_var)


#save RF stat results
m_variable= "bm" #modelling
s_variable= "meanmedianminmaxstd"
p_variable ="rf" 
sp_variable = "SP_Max"
sph_variable = "PH_SP_Max"

dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)

PH_RF= H_RF$summary
PH_RF$rsvariables <- "PH"
dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
write_xlsx(PH_RF, file.path("results", "wheat","stats_M", p_variable, m_variable,"height.xlsx"))

SP_RF= S_RF$summary
SP_RF$rsvariables <- sp_variable
write_xlsx(SP_RF, file.path("results", "wheat","stats_M",  p_variable,m_variable, paste0(s_variable,"_spectral.xlsx")))

SPPH_RF= CM_RF$summary
SPPH_RF$rsvariables <- sph_variable
write_xlsx(SPPH_RF, file.path("results", "wheat","stats_M",  p_variable, m_variable,paste0(s_variable,"_spectral_height.xlsx")))

#### save models
dir.create(file.path("results", "wheat", "models_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
saveRDS(S_RF$model, file = file.path("results", "wheat", "models_M",  p_variable, m_variable,paste0(s_variable, "_spectral.rds")))
saveRDS(H_RF$model, file.path("results", "wheat","models_M",  p_variable, m_variable,paste0(s_variable,"_height.rds")))
saveRDS(CM_RF$model, file.path("results", "wheat","models_M", p_variable,  m_variable ,paste0(s_variable,"_spectral_height.rds")))


S_RF$model
H_RF$model
CM_RF$model

S_RF$model$selectedvars
H_RF$model$selectedvars
CM_RF$model$selectedvars

SP_RF
PH_RF
SPPH_RF

plot(S_RF$model)
plot(H_RF$model)
plot(CM_RF$model)



#### Run models- SVM ####
S_SVM  <- svm_model(spectral_BM, predictors_S,response_var)
H_SVM  <- svm_model(height_BM, predictors_H, response_var)
CM_SVM <- svm_model(combined_BM, predictors_CM, response_var)


# save SVM stat results
m_variable= "bm"
#s_variable= "meanmedianmax"
p_variable ="svm"
#sp_variable = "SP_Mean_SP_Median_SP_Max"
#sph_variable = "PH_SP_Mean_SP_Median_SP_Max"


dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)

PH_SVM= H_SVM$summary
PH_SVM$rsvariables <- "PH"
dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
write_xlsx(PH_SVM, file.path("results", "wheat","stats_M", p_variable, m_variable,"height.xlsx"))

SP_SVM= S_SVM$summary
SP_SVM$rsvariables <- sp_variable
write_xlsx(SP_SVM, file.path("results", "wheat","stats_M",  p_variable,m_variable, paste0(s_variable,"_spectral.xlsx")))

SPPH_SVM= CM_SVM$summary
SPPH_SVM$rsvariables <- sph_variable
write_xlsx(SPPH_SVM, file.path("results", "wheat","stats_M",p_variable,m_variable, paste0(s_variable,"_spectral_height.xlsx")))

#### save models

dir.create(file.path("results", "wheat", "models_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
saveRDS(S_SVM$model, file = file.path("results", "wheat", "models_M",  p_variable, m_variable,paste0(s_variable, "_spectral.rds")))
saveRDS(H_SVM$model, file.path("results", "wheat","models_M",  p_variable, m_variable,paste0(s_variable,"_height.rds")))
saveRDS(CM_SVM$model, file.path("results", "wheat","models_M", p_variable,  m_variable ,paste0(s_variable,"_spectral_height.rds")))


S_SVM$model
H_SVM$model
CM_SVM$model

S_SVM$model$selectedvars
H_SVM$model$selectedvars
CM_SVM$model$selectedvars

SP_SVM
PH_SVM
SPPH_SVM

plot(S_SVM$model)
plot(H_SVM$model)
plot(CM_SVM$model)

#### Run model - PLSR ####
S_PLSR  <- plsr_model(spectral_BM, predictors_S, response_var)
H_PLSR  <- plsr_model(height_BM, predictors_H, response_var)
CM_PLSR <- plsr_model(combined_BM, predictors_CM, response_var)


m_variable= "bm"
#s_variable= "meanmedianmax"
p_variable ="plsr"
#sp_variable = "SP_Mean_SP_Median_SP_Max"
#sph_variable = "PH_SP_Mean_SP_Median_SP_Max"


dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
PH_PLSR= H_PLSR$summary
PH_PLSR$rsvariables <- "PH"
dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
write_xlsx(PH_PLSR, file.path("results", "wheat","stats_M", p_variable, m_variable,"height.xlsx"))


SP_PLSR= S_PLSR$summary
SP_PLSR$rsvariables <- sp_variable
write_xlsx(SP_PLSR, file.path("results", "wheat","stats_M",  p_variable,m_variable, paste0(s_variable,"_spectral.xlsx")))

SPPH_PLSR= CM_PLSR$summary
SPPH_PLSR$rsvariables <- sph_variable
write_xlsx(SPPH_PLSR, file.path("results", "wheat","stats_M",p_variable,m_variable, paste0(s_variable,"_spectral_height.xlsx")))

#### save models

dir.create(file.path("results", "wheat", "models_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
saveRDS(S_PLSR$model, file = file.path("results", "wheat", "models_M",  p_variable, m_variable,paste0(s_variable, "_spectral.rds")))
saveRDS(H_PLSR$model, file.path("results", "wheat","models_M",  p_variable, m_variable,paste0(s_variable,"_height.rds")))
saveRDS(CM_PLSR$model, file.path("results", "wheat","models_M", p_variable,  m_variable ,paste0(s_variable,"_spectral_height.rds")))


S_PLSR$model
H_PLSR$model
CM_PLSR$model

S_PLSR$model$selectedvars
H_PLSR$model$selectedvars
CM_PLSR$model$selectedvars

SP_PLSR
PH_PLSR
SPPH_PLSR

plot(S_PLSR$model)
plot(H_PLSR$model)
plot(CM_PLSR$model)

#### Run model -XGB ####

# S_XGB  <- xgb_model(spectral_BM, predictors_S, response_var)
# H_XGB  <- xgb_model(height_BM, predictors_H, response_var)
# CM_XGB <- xgb_model(combined_BM, predictors_CM, response_var)
# 
# # Save XGBoost statistical results
# m_variable= "bm" #modelling 
# s_variable= "meanmedianmaxminstd" #stat 
# p_variable ="rf" #processing method
# sp_variable = "SP_Mean_SP_Median_SP_Max_SP_Min_SP_Std"
# sph_variable = "PH_SP_Mean_SP_Median_SP_Max_SP_Min_SP_Std"
# 
# dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
# 
# PH_XGB= H_XGB$summary
# PH_XGB$rsvariables <- "PH"
# dir.create(file.path("results", "wheat", "stats_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
# write_xlsx(PH_XGB, file.path("results", "wheat","stats_M", p_variable, m_variable,"height.xlsx"))
# 
# SP_XGB= S_XGB$summary
# SP_XGB$rsvariables <- sp_variable
# write_xlsx(SP_XGB, file.path("results", "wheat","stats_M",  p_variable,m_variable, paste0(s_variable,"_spectral.xlsx")))
# 
# SPPH_XGB= CM_XGB$summary
# SPPH_XGB$rsvariables <- sph_variable
# write_xlsx(SPPH_XGB, file.path("results", "wheat","stats_M",  p_variable, m_variable,paste0(s_variable,"_spectral_height.xlsx")))
# 
# #### save models
# dir.create(file.path("results", "wheat", "models_M", p_variable, m_variable), recursive = TRUE, showWarnings = FALSE)
# saveRDS(S_XGB$model, file = file.path("results", "wheat", "models_M",  p_variable, m_variable,paste0(s_variable, "_spectral.rds")))
# saveRDS(H_XGB$model, file.path("results", "wheat","models_M",  p_variable, m_variable,paste0(s_variable,"_height.RData")))
# saveRDS(CM_XGB$model, file.path("results", "wheat","models_M", p_variable,  m_variable ,paste0(s_variable,"_spectral_height.RData")))
# 
# # Model outputs
# S_XGB$model
# H_XGB$model
# CM_XGB$model
# 
# S_XGB$model$selectedvars
# H_XGB$model$selectedvars
# CM_XGB$model$selectedvars
# 
# SP_XGB
# PH_XGB
# SPPH_XGB
# 
# # Plot results
# plot(S_XGB$model)
# plot(H_XGB$model)
# plot(CM_XGB$model)




