library(raster)

heightDifference <- function(field, output_dir) {
  # Define file paths
  dtm_dir <- file.path(output_dir, "DTM", "DTM_onlySoil")
  dtm_file <- file.path(dtm_dir, paste0(field, "_ZE1_DTM.tif"))
  
  dsm_dir <- "./Data/DSM"
  dsm_files <- c(
    ZE1 = file.path(dsm_dir, paste0(field, "_ZE1_30mB6_Dem.tif")),
    ZE2 = file.path(dsm_dir, paste0(field, "_ZE2_30mB6_Dem.tif")),
    ZE3 = file.path(dsm_dir, paste0(field, "_ZE3_30mB6_Dem.tif"))
  )
  
  # Load DTM
  if (!file.exists(dtm_file)) stop(paste("DTM file not found:", dtm_file))
  dtm <- raster(dtm_file)
  
  # Initialize output directory
  chm_dir <- file.path(output_dir, "CHM")
  dir.create(chm_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Process each DSM
  for (zone in names(dsm_files)) {
    dsm_file <- dsm_files[[zone]]
    if (!file.exists(dsm_file)) {
      warning(paste("DSM file not found:", dsm_file))
      next
    }
    
    dsm <- raster(dsm_file)
    resampled_DTM <- resample(dtm, dsm, method = "bilinear")
    chm <- dsm - resampled_DTM
    
    # Save CHM raster
    chm_path <- file.path(chm_dir, paste0(field, "_", zone, "chm_onlysoil.tif"))
    writeRaster(chm, chm_path, overwrite = TRUE)
  }
}

# Main execution
output_dir <- "./results"
fields <- c("GGE", "WBG", "DFH", "FOR")

for (id in fields) {
  heightDifference(id, output_dir)
}

