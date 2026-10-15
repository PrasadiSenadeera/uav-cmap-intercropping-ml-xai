library(sf)
library(ggplot2)
library(viridis)
library(magick)

shp_paths <- c(
  "results/predict/wheat/shp/DFH_ZE1.shp",
  "results/predict/wheat/shp/DFH_ZE2.shp",
  "results/predict/wheat/shp/DFH_ZE3.shp"
)

stages <- c("Shooting", "Flowering", "Milk Ripening")

shps <- lapply(shp_paths, st_read)

all_preds <- unlist(lapply(shps, function(x) x$prediction))
min_pred <- min(all_preds, na.rm = TRUE)
max_pred <- max(all_preds, na.rm = TRUE)

color_palette <- c("yellow", "orange", "lightgreen", "forestgreen", "darkgreen")

plot_save <- function(shp, stage, filename) {
  title_text <- paste("Predicted Biomass of Wheat at", stage, "at Großgerau")
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
      panel.background = element_rect(fill = "#f0f0f0"),   # light gray bg for softer look
      panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
      panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  ggsave(filename, plot = p, width = 8, height = 7, dpi = 300, device = "jpeg")
}

output_dir <- "results/predict/wheat/maps"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

jpeg_files <- file.path(output_dir, paste0("stage_", tolower(stages), "_DFH.jpeg"))

for (i in seq_along(shps)) {
  plot_save(shps[[i]], stages[i], jpeg_files[i])
}

imgs <- image_read(jpeg_files)

gif <- image_animate(imgs, fps = 0.5)  # slower fps = longer to view each stage

image_write(gif, file.path(output_dir, "biomass_growth_DFH.gif"))

cat("GIF saved as", file.path(output_dir, "biomass_growth_DFH.gif"), "\n")
