library(terra)
library(sp)
library(raster)
library(gstat)


create_dtm_from_random_points <- function(field_id, output_dir, num_points = 10000) {
  mask_dir <- file.path(output_dir, "soilRemoved", "soilBinary", "majorityFiltered")
  dsm_dir <- file.path("./Data/DSM")
  points_dir <- file.path(output_dir, "DTM", "points")

  mask_file <- file.path(mask_dir, paste0(field_id, "_binary_filtered.tif"))
  dsm_file <- file.path(dsm_dir, paste0(field_id, "_30mB6_Dem.tif"))

  mask <- rast(mask_file)
  dsm <- rast(dsm_file)

  #get ground
  mask_vals <- values(mask)
  ground_cells <- which(mask_vals == 0) # mask has 0 for soil

  #random cells
  set.seed(123)
  random_cells <- sample(ground_cells, num_points)
  random_coords <- xyFromCell(mask, random_cells)
  dsm_values <- extract(dsm, random_coords)

  #get DSM
  if (is.list(dsm_values)) {
    dsm_values <- unlist(dsm_values)
  }

  valid_indices <- which(!is.na(dsm_values)) 
  if (length(valid_indices) == 0) {
    cat("Error: No valid DSM values found.\n")
    return(NULL)
  }

  # Extract valid coordinates and DSM values
  valid_coords <- random_coords[valid_indices, ]
  valid_dsm_values <- dsm_values[valid_indices]

  points_df <- data.frame(x = valid_coords[, 1], y = valid_coords[, 2], z = valid_dsm_values)

  if (!dir.exists(points_dir)) dir.create(points_dir, recursive = TRUE)
  points_file <- file.path(points_dir, paste0(field_id, "_random_points.csv"))

  # Save CSV
  write.csv(points_df, points_file, row.names = FALSE)
  cat("Points saved to:", points_file, "\n")

  # Convert mask to RasterLayer and then to SpatialGridDataFrame
  mask_raster <- raster(mask)  
  mask_sp <- as(mask_raster, "SpatialGridDataFrame")  

  # Create spatial points data frame
  sp_points <- SpatialPointsDataFrame(coords = points_df[, c("x", "y")], data = points_df, proj4string = CRS(projection(mask_raster)))

  # Perform kriging interpolation to generate the DTM
  dtm_interpolated <- krige(z ~ 1, sp_points, mask_sp, nmax = 10)

  # Save the interpolated DTM
  dtm_dir <- file.path(output_dir, "DTM", "DTM_onlySoil")
  if (!dir.exists(dtm_dir)) dir.create(dtm_dir, recursive = TRUE)
  dtm_file <- file.path(dtm_dir, paste0(field_id, "_DTM.tif"))

  writeRaster(raster(dtm_interpolated, layer = 1), dtm_file, overwrite = TRUE)
  cat("DTM created and saved to:", dtm_file, "\n")
}

output_dir <- "./results"
field_ids <- c("DFH_ZE1", "FOR_ZE1", "GGE_ZE1", "WBG_ZE1")


# Apply the functions for each field ID
lapply(field_ids, function(id) {
  invertVegetation(id, output_dir)
  create_dtm_from_random_points(id, output_dir)
})
