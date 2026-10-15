#change the masked directory and output folders to use the same script for wheat and medicinal plants

library(raster)
library(sf)
library(dplyr)
library(openxlsx)

setwd("/transfer/projects/TRIO/R/ProcessR/results")

maskCHM <- function(field_id) {
  parent_wd <- dirname(getwd())
  
  raster_dir <- file.path(parent_wd, "results", "CHM")
  raster_file <- file.path(raster_dir, paste0(field_id, "chm_onlysoil.tif"))
  
  mask_dir <- file.path(parent_wd, "results", "soilRemoved", "soilBinary","majorityFiltered" )
  mask_file <- file.path(mask_dir, paste0(field_id, "_binary_filtered.tif"))
  
  print(paste("Processing field:", field_id)) 
  
  if (!file.exists(raster_file)) {
    stop(paste("Raster file not found:", raster_file))
  }
  if (!file.exists(mask_file)) {
    stop(paste("Mask file not found:", mask_file))
  }
  
  chm_raster <- raster(raster_file)
  mask_raster <- raster(mask_file)
  
  print("Raster and mask files loaded.") 
  
  mask_raster <- resample(mask_raster, chm_raster, method = "ngb") 
  print("Mask raster resampled.")  
  
  chm_masked <- mask(chm_raster, mask_raster, maskvalue = 0)
  print("Raster masked.") 
  
  output_dir <- file.path(raster_dir, "CHM_masked_MP")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_file <- file.path(output_dir, paste0(field_id, "_CHM_masked_MP.tif"))
  writeRaster(chm_masked, output_file, format = "GTiff", overwrite = TRUE)
  
  print(paste("Masked CHM saved at:", output_file))
  
  return(paste("Masked CHM saved at:", output_file))
}


#field_ids <- c("DFH_ZE1", "DFH_ZE2", "FOR_ZE1", "FOR_ZE2", "FOR_ZE3", "GGE_ZE1", "GGE_ZE2",  "WBG_ZE1", "WBG_ZE2")

field_ids <- c( "DFH_ZE3", "GGE_ZE3", "WBG_ZE3")


lapply(field_ids, maskCHM)
