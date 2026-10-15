library(terra)
library(sf)
library(FIELDimageR)

########################################
############# Calculate Indices ########
########################################

calIndex <- function(field_id, output_dir) {
  SRvegetation_dir <- file.path(output_dir, "soilRemoved", "vegetation")
  SR_file <- file.path(SRvegetation_dir, paste0(field_id, "_vegetation.tif"))
  
  # Determine which file to process
  raster_file <- SR_file
  
  # Load raster
  raster_data <- tryCatch({
    rast(raster_file)
  }, error = function(e) {
    message(paste("Error loading raster for", field_id, ":", e$message))
    return(NULL)
  })
  
  if (is.null(raster_data)) return(NULL)
  
  # Calculate indices
  raster_data.Indices <- tryCatch({
    fieldIndex(
      mosaic = raster_data,
      Red = 1, Green = 2, Blue = 3, NIR = 5, RedEdge =4,
      index = c("GLI","NGRDI", "BGI", "NDVI", "GNDVI","RVI", "NDRE","EVI"),
      myIndex = c(
        MCARI = "(RedEdge-Red)-0.2*(RedEdge-Green)*(RedEdge/Red)",  
        SRRE = "NIR/RedEdge"  
      )
    )
  }, error = function(e) {
    message(paste("Error calculating indices for", field_id, ":", e$message))
    return(NULL)
  })
  
  if (is.null(raster_data.Indices)) return(NULL)
  
  if (is.list(raster_data.Indices)) {
    raster_data.Indices <- raster_data.Indices[[1]]
  }
  
  # Create a directories for output
  indices_dir <- file.path(output_dir, "indices")
  if (!dir.exists(indices_dir)) dir.create(indices_dir, recursive = TRUE)
  
  # Define output file path
  output_file_indices <- file.path(indices_dir, paste0(field_id, "_indices.tif"))
  
  # Save raster
  tryCatch({
    writeRaster(raster_data.Indices, output_file_indices, datatype = "FLT4S", overwrite = TRUE)
    message("Indices calculated and saved for field ID: ", field_id, " in directory.")
  }, error = function(e) {
    message(paste("Error saving indices for", field_id, ":", e$message))
  })
}

# Main script
output_dir <- "./results"
field_ids <- c("DFH_ZE1", "DFH_ZE2", "DFH_ZE3", "FOR_ZE1", "FOR_ZE2", "FOR_ZE3", 
               "GGE_ZE1", "GGE_ZE2", "GGE_ZE3", "WBG_ZE1", "WBG_ZE2", "WBG_ZE3")

# Apply the function for each field ID and factor
lapply(field_ids, function(id) {
  calIndex(id, output_dir)
})
