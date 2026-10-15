library(sf)
library(ggplot2)
library(dplyr)
library(magick)
library(ggspatial)

crops <- c("wheat", "coriander", "caraway")
stages <- c("ZE1", "ZE2", "ZE3")
stage_labels <- c("shooting", "flowering", "milk ripening")

output_dir <- "results/predict"

color_palette <- c("yellow", "darkgreen")

all_shps <- list()

for (stage in stages) {
  shp_paths <- c(
    file.path(output_dir, "wheat", "shp", paste0("DFH_", stage, ".shp")),
    file.path(output_dir, "medicinal", "coriander", "shp", paste0("DFH_", stage, "_Coriander.shp")),
    file.path(output_dir, "medicinal", "caraway", "shp", paste0("DFH_", stage, "_Caraway.shp"))
  )
  
  for (path in shp_paths) {
    if (file.exists(path)) {
      shp <- st_read(path, quiet = TRUE)
      crop_name <- ifelse(grepl("wheat", path), "Wheat",
                          ifelse(grepl("coriander", path, ignore.case = TRUE), "Coriander",
                                 ifelse(grepl("caraway", path, ignore.case = TRUE), "Caraway", NA)))
      shp$crop <- crop_name
      shp <- st_transform(shp, 32632)
      all_shps <- c(all_shps, list(shp))
    }
  }
}

combined_all <- do.call(rbind, all_shps)
common_min <- min(combined_all$prediction, na.rm = TRUE)
common_max <- max(combined_all$prediction, na.rm = TRUE)

jpeg_files <- c()

for (i in seq_along(stages)) {
  stage <- stages[i]
  stage_label <- stage_labels[i]
  
  shp_paths <- c(
    file.path(output_dir, "wheat", "shp", paste0("DFH_", stage, ".shp")),
    file.path(output_dir, "medicinal", "coriander", "shp", paste0("DFH_", stage, "_Coriander.shp")),
    file.path(output_dir, "medicinal", "caraway", "shp", paste0("DFH_", stage, "_Caraway.shp"))
  )
  
  shps <- lapply(shp_paths, function(path) {
    if (file.exists(path)) {
      shp <- st_read(path, quiet = TRUE)
      crop_name <- ifelse(grepl("wheat", path), "Wheat",
                          ifelse(grepl("coriander", path, ignore.case = TRUE), "Coriander",
                                 ifelse(grepl("caraway", path, ignore.case = TRUE), "Caraway", NA)))
      shp$crop <- crop_name
      shp <- st_transform(shp, 32632)
      return(shp)
    } else {
      NULL
    }
  })
  
  shps <- Filter(Negate(is.null), shps)
  
  if (length(shps) == 0) {
    message("Skipping stage ", stage, " because no shapefiles found.")
    next
  }
  
  combined_shp <- do.call(rbind, shps)
  
  label_df <- combined_shp %>%
    group_by(ID, crop) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    mutate(
      centroid = st_centroid(geometry),
      coords = st_coordinates(centroid),
      lon = coords[,1],
      lat = coords[,2]
    ) %>%
    select(-coords)
  
  get_angle <- function(poly) {
    coords <- st_coordinates(poly)[,1:2]
    if (nrow(coords) < 2) return(0)
    dx <- coords[2,1] - coords[1,1]
    dy <- coords[2,2] - coords[1,2]
    angle_rad <- atan2(dy, dx)
    angle_deg <- angle_rad * 180/pi
    return(angle_deg)
  }
  
  label_df$angle <- sapply(label_df$geometry, get_angle)
  
  label_df <- label_df %>%
    mutate(
      angle = ifelse(angle < -90, angle + 180,
                     ifelse(angle > 90, angle - 180, angle)),
      angle = ifelse(angle > -90 & angle < 90, angle + 180, angle)
    )
  
  p <- ggplot(combined_shp) +
    geom_sf(aes(fill = prediction), color = NA) +
    geom_text(
      data = label_df,
      aes(x = lon, y = lat, label = crop, angle = angle),
      size = 2, fontface = "plain", color = "black",
      inherit.aes = FALSE
    ) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    scale_fill_gradientn(
      colours = c("white", "yellow", "darkgreen"),
      values  = c(0, 1e-6, 1),
      limits  = c(0, common_max),
      breaks  = c(0, seq(100, floor(common_max/100)*100, by = 100)),
      labels  = c("0", seq(100, floor(common_max/100)*100, by = 100)),
      na.value = "grey80",
      name = expression("Predicted Biomass (g " * m^{-2} * ")")
    ) +
    ggtitle(paste("Predicted biomass at Frankenhausen during", tolower(stage_label))) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_line(color = "black"),
      panel.background = element_rect(fill = "#f0f0f0"),
      panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
      panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  out_dir_stage <- file.path(output_dir, "QGIS_maps_with_white")
  if (!dir.exists(out_dir_stage)) dir.create(out_dir_stage, recursive = TRUE)
  
  filename <- file.path(out_dir_stage, paste0("combined_DFH_", tolower(stage), ".png"))
  jpeg_files <- c(jpeg_files, filename)
  
  ggsave(filename, plot = p, width = 8, height = 7, dpi = 300, device = "png")
}

imgs <- image_read(jpeg_files)
gif <- image_animate(imgs, fps = 0.5)
image_write(gif, file.path(out_dir_stage, "biomass_growth_DFH_combined.gif"))

cat("GIF saved as", file.path(out_dir_stage, "biomass_growth_DFH_combined.gif"), "\n")