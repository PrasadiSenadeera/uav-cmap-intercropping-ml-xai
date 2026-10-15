library(fastshap)
library(randomForest)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggbeeswarm)
library(scales)
library(purrr)
library(igraph)


rm(list = ls())
conflicted::conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::lag,
  .quiet = TRUE
)

#### Model ####
model <- readRDS("results/Coriander/models_M/plsr/bm/meanmedianminmaxstd_spectral_height.rds")
required_vars <- setdiff(names(model$trainingData), ".outcome")

clean_data <- function(file_path, remove_crops, remove_partners, filter_na, rename_map = NULL, canopy_cover = FALSE, remove_extra_cols = FALSE) {
  df <- read_excel(file_path) %>%
    filter(!(Factor_crop %in% remove_crops),
           !(Partner %in% remove_partners),
           !is.na(.data[[filter_na]]))
  
  if (remove_extra_cols) {
    col_names <- colnames(df)
    pixel_count_cols <- grep("_pixel_count$", col_names, value = TRUE)
    area_m2_cols <- grep("_area_m2$", col_names, value = TRUE)
    df <- select(df, -c(pixel_count_cols[-1], area_m2_cols[-1]))
  }
  
  if (!is.null(rename_map)) {
    names(df) <- ifelse(names(df) %in% names(rename_map), rename_map[names(df)], names(df))
  }
  
  if (canopy_cover && all(c("crop_area", "Total_area") %in% names(df))) {
    df <- mutate(df, Canopy_cover = crop_area / Total_area * 100)
  }
  
  df <- df %>%
    mutate(
      Site = factor(Site, levels = c("DFH", "FOR", "GGE", "WBG")),
      Growth_stage = factor(Growth_stage, levels = c("ZE1", "ZE2", "ZE3")),
      DM_PerArea = (`DM_Total(g)` / Total_area) / 100
    ) %>%
    arrange(Site, Growth_stage) %>%
    group_by(Site, Growth_stage) %>%
    mutate(CV_fold = paste0(Site, "_", Growth_stage)) %>%
    ungroup()
  
  return(df)
}

#### df ####
spectral_file <- "data/spectral_and_situData_MP.xlsx"
spectral_rename <- c("Red_pixel_count" = "crop_pixelCount", "Red_area_m2" = "crop_area")
spectral_df <- clean_data(
  file_path = spectral_file,
  remove_crops = c("WHE", "CAR","WCA"),
  remove_partners = c("Wheat"),
  filter_na = "DM_Total(g)",
  rename_map = spectral_rename,
  canopy_cover = TRUE,
  remove_extra_cols = TRUE
)

height_file <- "data/height_and_situData_MP.xlsx"
height_rename <- c("H_area_m2" = "H_area", "H_pixel_count" = "pixelCount")
height_df <- clean_data(
  file_path = height_file,
  remove_crops = c("WHE", "CAR","WCA"),
  remove_partners = c("Wheat"),
  filter_na = "Mean_height",
  rename_map = height_rename,
  canopy_cover = FALSE,
  remove_extra_cols = FALSE
)

spectral_BM <- spectral_df %>%
  dplyr::select(ends_with("_mean"), ends_with("_median"), ends_with("_max"), ends_with("_min"), ends_with("_std"),
                Feature_ID, crop_area, CV_fold, `DM_Total(g)`, `FM_Total(g)`, Site, DM_PerArea, LAI, Growth_stage) %>%
  dplyr::filter(!is.na(Blue_mean))

height_BM <- height_df %>%
  dplyr::select(starts_with("H_"), Feature_ID, H_area, Site, `DM_Total(g)`, `FM_Total(g)`, DM_PerArea, LAI)

combined_BM <- full_join(height_BM, spectral_BM, by = "Feature_ID") %>%
  dplyr::select(-ends_with(".x")) %>%
  rename_with(~ gsub("\\.y$", "", .), ends_with(".y"))

#### SHAP ####

X <- combined_BM %>%
  select(all_of(required_vars)) %>%
  na.omit()

y <- combined_BM$DM_PerArea[as.numeric(rownames(X))]

#### groups from correlation ####


cor_threshold <- 0.85

cor_mat <- cor(X, use = "complete.obs")

adj <- abs(cor_mat) > cor_threshold
diag(adj) <- FALSE

g <- graph_from_adjacency_matrix(
  adj,
  mode = "undirected",
  diag = FALSE
)

comp <- components(g)

group_list <- split(
  names(comp$membership),
  comp$membership
)

# Only keep real groups
corr_groups <- group_list[lengths(group_list) > 1]

grouped_vars <- unlist(corr_groups)

single_vars <- setdiff(colnames(X), grouped_vars)

# Better group names
names(corr_groups) <- sapply(
  corr_groups,
  function(x) paste(x, collapse = " + ")
)

feature_groups <- c(
  corr_groups,
  setNames(as.list(single_vars), single_vars)
)

#################################################
# PRINT GROUPS
#################################################

cat("\nCorrelation groups (>0.85)\n")

for(i in seq_along(feature_groups)){
  
  if(length(feature_groups[[i]]) > 1){
    
    cat(
      paste0(
        names(feature_groups)[i],
        " : ",
        paste(feature_groups[[i]], collapse = ", "),
        "\n"
      )
    )
    
  }
  
}

####shap values ####

shap_values <- fastshap::explain(
  object = model,
  X = X,
  pred_wrapper = function(object, newdata) as.vector(predict(object, newdata)),
  nsim = 100
)

# group shap ###
group_shap <- sapply(
  feature_groups,
  function(vars){
    
    rowSums(
      shap_values[, vars, drop = FALSE]
    )
    
  }
)

group_shap <- as.data.frame(group_shap)



#################################################
# GROUP FEATURE VALUES
#################################################

group_X <- map_dfc(
  feature_groups,
  function(vars){
    
    if(length(vars) == 1){
      
      X[, vars, drop = TRUE]
      
    } else {
      
      rowMeans(
        X[, vars, drop = FALSE],
        na.rm = TRUE
      )
      
    }
    
  }
)

group_X <- as.data.frame(group_X)

feature_labels <- c(
  `Red_std` = "Red (std)",
  `H_mean` = "Crop height (mean)",
  `H_median` = "Crop height (median)",
  `myIndex.2_median`="SRRE (median) ", 
  `Blue_std` = "Blue(std)",
  `EVI_median` = "EVI (median)"
)

group_feature_labels <- sapply(
  names(feature_groups),
  function(group_name){
    
    vars <- feature_groups[[group_name]]
    
    labels <- ifelse(
      vars %in% names(feature_labels),
      feature_labels[vars],
      vars
    )
    
    paste(labels, collapse = "+\n")
    
  }
)

group_feature_labels

#### Average Plot ####
library(ggplot2)

shap_long <- reshape2::melt(group_shap)
shap_summary <- shap_long %>%
  group_by(variable) %>%
  summarise(mean_abs_shap = mean(abs(value))) %>%
  arrange(desc(mean_abs_shap))

a=ggplot(shap_summary, aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
  geom_col(fill = "#0072B2") +
  geom_text(aes(label = round(mean_abs_shap, 3)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_x_discrete(labels = group_feature_labels) +
  theme_minimal() +
  labs(
    title = "Average Feature Importance",
    subtitle = "Mean absolute SHAP values across all samples- coriander",
    x = "Feature",
    y = "Mean Absolute SHAP Value"
  ) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5),
    axis.title.x = element_text(size = 6, face = "bold"),
    axis.title.y = element_text(size = 6, face = "bold"),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted")
  ) +
  expand_limits(y = max(shap_summary$mean_abs_shap) * 1.1) 

ggplot2::ggsave(
  filename = "results/ExAI/group_average_shap_COR.png",
  plot = a,
  width = 8, height = 2, units = "in",
  dpi = 600, bg = "white"
)

#ggsave("results/ExAI/average_shap_CAR.jpeg", height = 4 ,width = 8,units = "in")


#### beeswarm plot ####
shap_long <- group_shap%>%
  as.data.frame() %>%
  mutate(ID = row_number()) %>%
  pivot_longer(-ID, names_to = "Feature", values_to = "SHAP")

X_long <- group_X %>%
  mutate(ID = row_number()) %>%
  pivot_longer(-ID, names_to = "Feature", values_to = "Feature_value")
plot_data <- left_join(shap_long, X_long, by = c("ID", "Feature"))

plot_data <- plot_data %>%
  group_by(Feature) %>%
  mutate(Value_category = cut(
    Feature_value,
    breaks = quantile(Feature_value, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE,
    ordered_result = TRUE
  )) %>%
  ungroup()

#value_colors <- c("Low" = "#440154", "Medium" = "#21908C", "High" = "#FDE725") 
value_colors <- c("Low" = "#1F75FE", "Medium" = "#21908C", "High" = "#ff1493")


# Plot
b=ggplot(plot_data, aes(x = SHAP, y = reorder(Feature, abs(SHAP), FUN = median), color = Value_category)) +
  geom_quasirandom(groupOnX = TRUE, alpha = 0.7, size = 1.5) +
  scale_color_manual(values = value_colors) +
  theme_minimal() +
  labs(
    title = "SHAP Beeswarm Plot",
    subtitle = "Individual feature importance- coriander",
    x = "SHAP Value (Impact on Prediction)",
    y = "Feature",
    color = "Feature Value"
  )+ 
  scale_y_discrete(labels = group_feature_labels)+
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 6, face = "bold"),
    axis.title.y = element_text(size = 6, face = "bold"),
    axis.text.y = element_text(size = 7),
  )

ggplot2::ggsave(
  filename = "results/ExAI/group_beeswarm_shap_COR.png",
  plot = b,
  width = 7, height = 2, units = "in",
  dpi = 600, bg = "white"
)
#ggsave("results/ExAI/beeswarm_CAR.jpeg", height = 4, width = 7,units = "in")

#### per site ####

shap_long <- as.data.frame(shap_values) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    -row_id,
    names_to = "Feature",
    values_to = "SHAP"
  )

# Add Site and Growth_stage info by joining with combined_BM metadata using row_id
meta_info <- combined_BM %>%
  mutate(row_id = row_number()) %>%
  select(row_id, Site, Growth_stage)

shap_long <- shap_long %>%
  left_join(meta_info, by = "row_id")

# Add feature values for each observation and feature
feature_values <- combined_BM %>%
  mutate(row_id = row_number()) %>%
  select(row_id, all_of(colnames(shap_values))) %>%
  pivot_longer(
    -row_id,
    names_to = "Feature",
    values_to = "Feature_value"
  )

plot_data <- shap_long %>%
  left_join(feature_values, by = c("row_id", "Feature")) %>%
  mutate(
    Site = factor(Site, levels = c("DFH", "FOR", "GGE", "WBG")),
    Growth_stage = factor(Growth_stage, levels = c("ZE1", "ZE2", "ZE3")),
    Feature = factor(Feature)
  ) %>%
  select(row_id, Site, Growth_stage, SHAP, Feature, Feature_value)

plot_data <- plot_data %>%
  group_by(Feature) %>%
  mutate(Value_category = cut(
    Feature_value,
    breaks = quantile(Feature_value, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE,
    ordered_result = TRUE
  )) %>%
  ungroup()


value_colors <- c("Low" = "#1F75FE", "Medium" = "#21908C", "High" = "#ff1493")

ggplot(plot_data, aes(x = SHAP, y = reorder(Feature, abs(SHAP), FUN = median), color = Value_category)) +
  geom_quasirandom(groupOnX = FALSE, alpha = 0.7, size = 1.2) +
  scale_color_manual(values = value_colors, name = "Feature Value") +
  facet_wrap(~ Site) +
  theme_minimal(base_size = 14) +
  labs(
    title = "SHAP Beeswarm Plot by Site",
    x = "SHAP Value (Impact on Prediction)",
    y = "Feature"
  ) +
  scale_y_discrete(labels = group_feature_labels)+
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )
ggsave("results/ExAI/group_beeswarm_Site_COR.jpeg", height = 4, units = "in")




#check for multicollinearity added on 10.06.2026

library(corrplot)

cor_mat <- cor(X, use = "complete.obs")

round(cor_mat, 2)

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

library(caret)

high_cor <- findCorrelation(
  cor_mat,
  cutoff = 0.85,
  names = TRUE
)

high_cor