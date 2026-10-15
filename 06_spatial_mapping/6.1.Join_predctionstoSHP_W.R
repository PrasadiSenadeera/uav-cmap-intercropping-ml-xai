library(sf)
library(openxlsx)
library(dplyr)

# Paths
field_id= "DFH_ZE3"
# shp_path <- "results/parcelSpit/GGE_split.shp"
# excel_path <- "results/predict/wheat/GGE_ZE3_predict.xlsx"
# output_path <- "results/predict/wheat/shp/GGE_ZE3.shp"

shp_path <- file.path("results", "parcelSpit", paste0(substr(field_id, 1, nchar(field_id) - 3), "split.shp"))
excel_path <- file.path("results", "predict", "wheat", paste0(field_id, "_predict.xlsx"))
output_path <- file.path("results", "predict", "wheat", "shp", paste0(field_id, ".shp"))

# Read shapefile
shp <- st_read(shp_path)
shp_wheat <- shp %>%
  filter(Type == "Wheat", split_num < 10)

# Read Excel
excel_data <- read.xlsx(excel_path)
excel_data$prediction <- as.numeric(excel_data$prediction)

# Check column names
# print(names(shp))
# print(names(excel_data))


excel_data <- rename(excel_data, split_id = FID)
shp_joined <- left_join(shp_wheat, excel_data, by = "split_id")  

st_write(shp_joined, output_path, delete_layer = TRUE)
