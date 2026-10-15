library(sf)
library(dplyr)
library(writexl)
library(stringr)
library(tidyr)

# Read the shapefiles
shp1 <- st_read("Z:\\transfer\\projects\\TRIO\\R\\2024\\PredictRF\\results\\predict\\wheat\\shp\\DFH_ZE1.shp")
shp2 <- st_read("Z:\\transfer\\projects\\TRIO\\R\\2024\\PredictRF\\results\\predict\\wheat\\shp\\DFH_ZE2.shp")
shp3 <- st_read("Z:\\transfer\\projects\\TRIO\\R\\2024\\PredictRF\\results\\predict\\wheat\\shp\\DFH_ZE3.shp")

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
    ),
    mixedwith = case_when(
      purity == "mixed" & str_detect(Code, "_WCO") ~ "coriander",
      purity == "mixed" & str_detect(Code, "WCA") ~ "caraway",
      TRUE ~ "wheat"
    )
  ) %>%
  filter(split <= 10) %>%
  dplyr::select(Code, split_id, split, purity, mixedwith, 
                X_centroid, Y_centroid,
                pred_ZE1, pred_ZE2, pred_ZE3)

# Write to Excel
dir.create(dirname("Z:/transfer/projects/TRIO/R/2024/PredictRF/results/predict/timeSeries/predictions_by_ZE_wheat_DFH.xlsx"), recursive = TRUE, showWarnings = FALSE)
write_xlsx(joined_df, "Z:\\transfer\\projects\\TRIO\\R\\2024\\PredictRF\\results\\predict\\timeSeries\\predictions_by_ZE_wheat_DFH.xlsx")
