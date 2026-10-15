library(sf)
library(dplyr)
library(writexl)
library(stringr)

field_id <- "DFH"
base_path <- "Z:/transfer/projects/TRIO/R/2024/PredictRF/results/predict/medicinal/caraway/shp"
field_ids <- paste0(field_id, "_ZE", 1:3)

shps <- lapply(field_ids, function(id) {
  shp_path <- file.path(base_path, paste0(id, "_Caraway.shp"))
  st_read(shp_path)
})

shp1 <- shps[[1]]
shp2 <- shps[[2]]
shp3 <- shps[[3]]

# Compute centroids from shp1
centroids <- st_centroid(shp1) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(X_centroid = X, Y_centroid = Y) %>%
  mutate(split_id = shp1$split_id)

# Select relevant columns and rename pred columns
df1 <- shp1 %>% st_drop_geometry() %>% dplyr::select(split_id, prediction, Code) %>% rename(pred_ZE1 = prediction)
df2 <- shp2 %>% st_drop_geometry() %>% dplyr::select(split_id, prediction) %>% rename(pred_ZE2 = prediction)
df3 <- shp3 %>% st_drop_geometry() %>% dplyr::select(split_id, prediction) %>% rename(pred_ZE3 = prediction)

# Join dataframes by split_id
joined_df <- df1 %>%
  left_join(df2, by = "split_id") %>%
  left_join(df3, by = "split_id") %>%
  left_join(centroids, by = "split_id") %>%
  mutate(
    split = as.integer(str_extract(split_id, "(?<=_)[0-9]+$")),
    purity = case_when(
      str_detect(Code, "_WCO") | str_detect(Code, "WCA") ~ "mixed",
      TRUE ~ "pure"
    )
  ) %>%
  filter(split <= 10) %>%
  dplyr::select(Code, split_id, split, purity,
                X_centroid, Y_centroid,
                pred_ZE1, pred_ZE2, pred_ZE3)

# Write to Excel
file_path <- file.path("Z:/transfer/projects/TRIO/R/2024/PredictRF/results/predict/timeSeries",
                       paste0("predictions_by_ZE_caraway_", field_id, ".xlsx"))
write_xlsx(joined_df, file_path)
