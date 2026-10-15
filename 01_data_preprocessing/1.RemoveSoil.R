library(terra)
library(sf)
library(FIELDimageR)

########################################
############ Normalize Bands ###########
########################################

normalize_bands <- function(raster_data, field_id, output_dir) {
  normalized_raster <- raster_data / 32768
  
  # Save normalized raster
  normalized_dir <- file.path(output_dir, "normalized")
  if (!dir.exists(normalized_dir)) dir.create(normalized_dir, recursive = TRUE)
  output_file_normalized <- file.path(normalized_dir, paste0(field_id, "_normalized.tif"))
  writeRaster(normalized_raster, output_file_normalized, datatype = "FLT4S", overwrite = TRUE)
  message("Normalized data saved for field ID: ", field_id)
  
  return(normalized_raster)
}

########################################
################  Remove soil ##########
########################################

Removesoil <- function(field_id, output_dir) {
  raster_dir <- "./Data/5B"
  raster_file <- file.path(raster_dir, paste0(field_id, "_30m5B.tif"))
  if (!file.exists(raster_file)) stop("B5 raster is not exist: ", raster_file)
  raster_data <- rast(raster_file)
  
  # call Normalize
  raster_data <- normalize_bands(raster_data, field_id,output_dir )
  
  # Remove soil
  raster_data.RemSoil <- tryCatch({
    fieldMask(
      mosaic = raster_data, 
      Red = 3, Green = 2, Blue = 1, RedEdge = 4, NIR = 5, 
      index = "HUE"
    )
  }, error = function(e) {
    stop(paste("Error applying fieldMask for", field_id, ":", e$message))
  })
  
  if (is.list(raster_data.RemSoil)) {
    raster_data.RemSoil <- raster_data.RemSoil[[1]]
  }
  
  soil_removed_dir <- file.path(output_dir, "soilRemoved")
  if (!dir.exists(soil_removed_dir)) dir.create(soil_removed_dir, recursive = TRUE)
  output_file_RS <- file.path(soil_removed_dir, paste0(field_id, "_SR.tif"))
  writeRaster(raster_data.RemSoil, output_file_RS, datatype = "FLT4S", overwrite = TRUE)
  message("Soil removed and saved for field ID: ", field_id)
}


########################################
############# Soil removed Mask ########
########################################
# Generate a mask using the soil-removed raster for crop extraction.
# Apply the majority filter to the mask for smoothing, rather than applying it 
# directly to the soil-removed raster  to reduce processing time.


create_mask <- function(field_id, output_dir, band = 1) {
  raster_dir <- file.path(output_dir, "soilRemoved")
  raster_file <- file.path(raster_dir, paste0(field_id, "_SR.tif"))
  if (!file.exists(raster_file)) stop("Raster file does not exist: ", raster_file)
  
  raster <- rast(raster_file)[[band]]
  resolution <- res(raster)
  mask <- ifel(is.na(raster), 0, 1) # 0 for soil,1 for crops
  mask_dir <- file.path(output_dir, "soilRemoved", "soilBinary")
  dir.create(mask_dir, recursive = TRUE, showWarnings = FALSE)
  maskPath <- file.path(mask_dir, paste0(field_id, "_binary.tif"))
  writeRaster(mask, maskPath, datatype = "INT1U", overwrite = TRUE)
}

########################################
############# Majority filter  #########
########################################

majorityFilter <- function(field_id, output_dir) {
  mask_dir <- file.path(output_dir, "soilRemoved", "soilBinary")
  mask_file <- file.path(mask_dir, paste0(field_id, "_binary.tif"))
  if (!file.exists(mask_file)) stop("Raster file does not exist: ", mask_file)
  
  mask <- rast(mask_file)
  filtered_mask <- focal(mask, w = 3, fun = "modal", na.policy = "omit", na.rm = TRUE)
  
  crs(filtered_mask) <- crs(mask)
  
  filtered_dir <- file.path(output_dir, "soilRemoved", "soilBinary", "majorityFiltered")
  dir.create(filtered_dir, recursive = TRUE, showWarnings = FALSE)
  filteredPath <- file.path(filtered_dir, paste0(field_id, "_binary_filtered.tif"))
  writeRaster(filtered_mask, filteredPath, datatype = "INT1U", overwrite = TRUE)
}

###############################################
##########  Apply filterd Mask  ###############
###############################################
#use the 0,1 image and extract only the vegetation areas from B5 raster

Masking <- function(field_id, output_dir) {
  
  #open B5 normalized raster
  
  B5_norm_dir <- file.path(output_dir, "normalized")
  B5_norm_file <- file.path(B5_norm_dir, paste0(field_id, "_normalized.tif"))
  if (!file.exists(B5_norm_file)) stop("B5 normalized raster is not exist: ", B5_norm_file)
  B5 <- rast(B5_norm_file)
  
  
  #open mask
  mask_dir <- file.path(output_dir, "soilRemoved", "soilBinary", "majorityFiltered")
  maskPath <- file.path(mask_dir, paste0(field_id, "_binary_filtered.tif"))
  mask <- rast(maskPath)
  
  
  maskedSoilRemoved <- mask(B5, mask, maskvalues = 0, updatevalue = NA)
  maskedSoilRemoved_dir <- file.path(output_dir, "soilRemoved","vegetation")
  dir.create(maskedSoilRemoved_dir, recursive = TRUE, showWarnings = FALSE)
  maskedSoilRemoved_filepath <- file.path(maskedSoilRemoved_dir, paste0(field_id, "_vegetation.tif"))
  writeRaster(maskedSoilRemoved, maskedSoilRemoved_filepath, overwrite = TRUE)
  cat("Masked raster saved to:", maskedSoilRemoved_filepath, "\n")
}


field_ids <- c("DFH_ZE1", "DFH_ZE2", "DFH_ZE3","FOR_ZE1", "FOR_ZE2", "FOR_ZE3", "GGE_ZE1", "GGE_ZE2","GGE_ZE3", "WBG_ZE1", "WBG_ZE2", "WBG_ZE3")


# Ensure output directory exists
output_dir <- "./results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Process each field ID
lapply(field_ids, function(id) {
  Removesoil(id, output_dir)
  create_mask(id, output_dir)
  majorityFilter(id, output_dir)
  Masking (id, output_dir)
})
