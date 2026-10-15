library(raster)
library(sf)
library(dplyr)
library(openxlsx)

#setwd("Z:/projects/TRIO/R/ProcessR/results")

getStats <- function(field_id) {
  parent_wd <- getwd()
  
  raster_dir <- file.path(getwd(), "results", "indices")
  raster_file <- file.path(raster_dir, paste0(field_id, "_indices.tif"))
  
  shapefile_dir <- file.path(getwd(), "Data", "ZEPlots")
  shapefile_file <- file.path(shapefile_dir, paste0(field_id, "_Cut_Poly.shp"))
  
  id_column_name <- "ID"
  
  raster_data <- stack(raster_file)
  shapefile_data <- st_read(shapefile_file)
  
  if (!(id_column_name %in% names(shapefile_data))) {
    stop(paste("ID column '", id_column_name, "' not found in shapefile.", sep = ""))
  }
  
  shapefile_data <- st_transform(shapefile_data, crs = crs(raster_data))
  
  calculate_stats <- function(raster_band, polygon) {
    buffered_polygon <- st_buffer(polygon, dist = res(raster_data)[1] / 2)
    cropped_raster <- crop(raster_band, extent(buffered_polygon))
    masked_raster <- mask(cropped_raster, buffered_polygon)
    values <- na.omit(values(masked_raster))
    pixel_count <- length(values)
    
    resolution <- res(raster_band)[1]  # Assuming square pixels
    area <- pixel_count * (resolution^2)
    
    if (pixel_count == 0) {
      return(c(NA, NA, NA, NA, NA, NA, NA))
    }
    
    stats <- setNames(
      c(
        mean(values),
        median(values),
        min(values),
        max(values),
        sd(values),
        pixel_count,
        area
      ),
      c("mean", "median", "min", "max", "std", "pixel_count", "area_m2")
    )
    
    return(stats)
  }
  
  results_list <- list()
  
  for (i in 1:nrow(shapefile_data)) {
    polygon <- shapefile_data[i, ]
    feature_id <- polygon[[id_column_name]]
    feature_results <- data.frame(Feature_ID = feature_id, pixel_count = NA)
    
    total_pixel_count <- 0  # Initialize pixel count for this feature
    
    for (j in 1:nlayers(raster_data)) {
      band <- raster_data[[j]]
      stats <- calculate_stats(band, polygon)
      
      total_pixel_count <- max(total_pixel_count, stats["pixel_count"], na.rm = TRUE)
      
      col_names <- paste0(names(raster_data)[j], "_", names(stats))
      feature_results <- cbind(feature_results, setNames(data.frame(t(stats)), col_names))
    }
    
    feature_results$pixel_count <- total_pixel_count  # Store final pixel count for the feature
    results_list[[i]] <- feature_results
  }
  
  final_results <- do.call(rbind, results_list)
  
  output_dir <- file.path(parent_wd, "results", "stats_medicinalPlants")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(output_dir, paste0(field_id, "_spectral_MP.xlsx"))
  
  wb <- createWorkbook()
  addWorksheet(wb, "RasterStats")
  writeData(wb, "RasterStats", final_results)
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  print(paste("Processing complete. Results saved to", output_file))
}

field_ids <- c("DFH_ZE1", "DFH_ZE2", "DFH_ZE3" , "FOR_ZE1", "FOR_ZE2", "FOR_ZE3", "GGE_ZE1", "GGE_ZE2", "GGE_ZE3","WBG_ZE1", "WBG_ZE2", "WBG_ZE3")
#field_ids <- c( "DFH_ZE3")


lapply(field_ids, getStats)
