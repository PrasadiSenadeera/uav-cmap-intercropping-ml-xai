library(sf)
library(ggplot2)
library(viridis)
library(magick)



# site_id <- "GGE"
# station= "Großgerau"
# stages <- c("05.06.2024", "20.06.2024", "11.07.2024") # for gge


site_id <- "DFH"
station= "Frankenhausen"
stages <- c("10.06.2024", "08.07.2024", "24.07.2024") # for dfh




shp_paths <- paste0(
  "results/predict/medicinal/caraway/shp/",
  site_id, "_ZE", 1:3, "_Caraway.shp"
)


shps <- lapply(shp_paths, st_read)

all_preds <- unlist(lapply(shps, function(x) x$prediction))
min_pred <- min(all_preds, na.rm = TRUE)
max_pred <- max(all_preds, na.rm = TRUE)

color_palette <- c("yellow", "orange", "lightgreen", "forestgreen", "darkgreen")

plot_save <- function(shp, stage, filename) {
  shp$predi <- as.numeric(shp$predi)
  title_text <- paste("Predicted Coriander Biomass –", stage, ", ",station)
  p <- ggplot(shp) +
    geom_sf(aes(fill = prediction), color = NA) +
    scale_fill_gradientn(
      colours = color_palette,
      limits = c(min_pred, max_pred),
      na.value = "grey80",
      name = expression("Predicted \nBiomass (g " * m^{-2} * ")")
    ) +
    ggtitle(title_text) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_line(color = "black"),
      panel.background = element_rect(fill = "#f0f0f0"),
      panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
      panel.grid.minor = element_line(color = "grey90", linetype = "dotted")
    )
  
  ggsave(filename, plot = p, width = 8, height = 7, dpi = 300, device = "jpeg")
}


output_dir <- "results/predict/medicinal/caraway/maps"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

jpeg_files <- file.path(output_dir, paste0("stage_", tolower(stages), "_", site_id, ".jpeg"))

for (i in seq_along(shps)) {
  plot_save(shps[[i]], stages[i], jpeg_files[i])
}

imgs <- image_read(jpeg_files)

gif <- image_animate(imgs, fps = 0.5)  # slower fps = longer to view each stage

image_write(gif, file.path(output_dir, paste0("biomass_growth_ca_", site_id, ".gif")))

cat("GIF saved as", file.path(output_dir, paste0("biomass_growth_ca_", site_id, ".gif")), "\n")
