library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(openxlsx)

fix_excel_colnames <- function(file) {
  wb <- loadWorkbook(file)
  sheet <- sheets(wb)[1]
  df <- read.xlsx(wb, sheet = sheet)
  names(df) <- gsub("pct", "%", names(df))
  writeData(wb, sheet = sheet, x = df, withFilter = FALSE)
  saveWorkbook(wb, file, overwrite = TRUE)
}

getStats <- function(field_id) {
  raster_file <- file.path("data", "CHM", "Medicinal", paste0(field_id, "_CHM_masked_MP.tif"))
  #shapefile_file <- file.path("results", "parcelSpit",paste0(field_id, "split.shp"))
  shapefile_file <- file.path("results", "parcelSpit", paste0(substr(field_id, 1, nchar(field_id) - 3), "split.shp"))
  
  
  id_column_name <- "split_id"
  
  raster_data <- stack(raster_file)
  shapefile_data <- st_read(shapefile_file, quiet = TRUE)
  
  if (!(id_column_name %in% names(shapefile_data))) {
    stop(paste("ID column '", id_column_name, "' not found in shapefile.", sep = ""))
  }
  
  shapefile_data <- st_transform(shapefile_data, crs = crs(raster_data))
  
  all_stats <- lapply(1:nrow(shapefile_data), function(i) {
    polygon <- shapefile_data[i, ]
    polygon_id <- polygon[[id_column_name]]
    
    stats_list <- lapply(1:nlayers(raster_data), function(j) {
      raster_band <- raster_data[[j]]
      band_name <- names(raster_data)[j]
      
      buffered_polygon <- st_buffer(polygon, dist = res(raster_band)[1] / 2)
      cropped_raster <- crop(raster_band, extent(buffered_polygon))
      masked_raster <- mask(cropped_raster, buffered_polygon)
      values <- na.omit(values(masked_raster))
      
      pixel_count <- length(values)
      pixel_area <- prod(res(raster_band))
      area <- pixel_count * pixel_area
      
      if (length(values) == 0) {
        stat_values <- rep(NA, 13)
      } else {
        top_5 <- quantile(values, 0.95)
        top_3 <- quantile(values, 0.97)
        top_2 <- quantile(values, 0.98)
        top_1 <- quantile(values, 0.99)
        
        stat_values <- c(
          mean(values),
          median(values),
          min(values),
          max(values),
          quantile(values, 0.97, names = FALSE),
          quantile(values, 0.98, names = FALSE),
          quantile(values, 0.99, names = FALSE),
          mean(values[values >= top_5]),
          mean(values[values >= top_3]),
          mean(values[values >= top_2]),
          mean(values[values >= top_1]),
          area
        )
      }
      
      names(stat_values) <- c("H_mean", "H_median", "H_min", "H_max", "H_p97", "H_p98", "H_p99",
                              "H_m_5pct", "H_m_3pct", "H_m_2pct", "H_m_1pct", "H_area")
      
      as.data.frame(as.list(stat_values))
    })
    
    stats_combined <- bind_cols(stats_list)
    stats_combined <- cbind(FID = polygon_id, stats_combined)
    stats_combined
  })
  
  stats_df <- bind_rows(all_stats)
  output_file <- file.path("results", "stats", "medicinal","chm", paste0(field_id, "_CHM_statistics_m.xlsx"))
  write.xlsx(stats_df, output_file, rowNames = FALSE)
  
  fix_excel_colnames(output_file)
}

field_ids <- c("DFH_ZE1", "DFH_ZE2", "DFH_ZE3")
#field_ids <- c("GGE_ZE1", "GGE_ZE2", "GGE_ZE3")
#field_ids <- c("FOR_ZE1", "FOR_ZE2", "FOR_ZE3")
lapply(field_ids, getStats)
