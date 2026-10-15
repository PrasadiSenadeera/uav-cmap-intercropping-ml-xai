library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(openxlsx)

getStats <- function(field_id) {
  raster_file <- file.path("data", "indices", "Medicinal", paste0(field_id, "_indices_Soil.tif"))
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
      
      if (length(values) == 0) {
        stat_values <- c(NA, NA, NA)
      } else {
        stat_values <- c(mean(values), median(values), max(values),sd(values),min(values))
      }
      
      names(stat_values) <- paste0(band_name, c("_mean", "_median", "_max","_std", "_min"))
      return(as.data.frame(as.list(stat_values)))
    })
    
    stats_combined <- bind_cols(stats_list)
    stats_combined <- cbind(FID = polygon_id, stats_combined)
    return(stats_combined)
  })
  
  stats_df <- bind_rows(all_stats)
  
  output_file <- file.path("results", "stats","medicinal","spectral", paste0(field_id, "_statistics_m.xlsx"))
  write.xlsx(stats_df, output_file, rowNames = FALSE)
}

field_ids <- c("GGE_ZE1", "GGE_ZE2", "GGE_ZE3")
#field_ids <- c("DFH_ZE1", "DFH_ZE2", "DFH_ZE3")
lapply(field_ids, getStats)
