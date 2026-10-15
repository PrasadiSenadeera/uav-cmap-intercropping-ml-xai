library(sf)
library(dplyr)
library(units)

# Estimate the orientation angle of a polygon
estimate_orientation <- function(poly) {
  coords <- st_coordinates(poly)[, 1:2]
  pca <- prcomp(coords, center = TRUE, scale. = FALSE)
  angle <- atan2(pca$rotation[2, 1], pca$rotation[1, 1]) + pi/2
  return(angle)
}

# Rotate a geometry by a given angle (in radians)
rotate_geometry <- function(geom, angle, center) {
  rotate_coords <- function(coords) {
    coords_shifted <- coords - matrix(center, nrow = nrow(coords), ncol = 2, byrow = TRUE)
    rot_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), ncol = 2)
    coords_rotated <- coords_shifted %*% rot_matrix
    coords_rotated + matrix(center, nrow = nrow(coords), ncol = 2, byrow = TRUE)
  }
  
  geom_type <- st_geometry_type(geom, by_geometry = FALSE)
  
  coords_list <- st_geometry(geom) %>% st_cast("POLYGON") %>% lapply(function(p) {
    rings <- st_coordinates(p)
    ring_ids <- unique(rings[, "L2"])
    lapply(ring_ids, function(rid) {
      rotate_coords(rings[rings[, "L2"] == rid, 1:2])
    })
  })
  
  polygons <- lapply(coords_list, function(rings) {
    st_polygon(rings)
  })
  
  if (geom_type == "POLYGON") {
    return(polygons[[1]])  # return single sfg polygon
  } else {
    return(st_multipolygon(lapply(polygons, unclass)))  # return sfg multipolygon
  }
}



# Split rotated polygon horizontally and rotate back
split_parallel_polygon <- function(poly, step = 1.5, id_col = "ID") {
  angle <- estimate_orientation(poly)
  center <- st_centroid(st_geometry(poly)) %>% st_coordinates()
  
  rotated <- st_sfc(rotate_geometry(poly, -angle, center), crs = st_crs(poly))
  bbox <- st_bbox(rotated)
  height <- bbox["ymax"] - bbox["ymin"]
  n <- ceiling(height / step)
  
  strips <- lapply(0:(n-1), function(i) {
    ymin <- bbox["ymin"] + i * step
    ymax <- min(ymin + step, bbox["ymax"])
    
    st_polygon(list(matrix(c(
      bbox["xmin"], ymin,
      bbox["xmax"], ymin,
      bbox["xmax"], ymax,
      bbox["xmin"], ymax,
      bbox["xmin"], ymin
    ), ncol = 2, byrow = TRUE)))
  })
  
  strips_sf <- st_sf(geometry = st_sfc(strips, crs = st_crs(poly))) %>%
    mutate(split_num = 1:n)
  
  rotated_cut <- strips_sf %>%
    mutate(geometry = st_intersection(geometry, rotated)) %>%
    filter(!st_is_empty(geometry))
  
  rotated_back <- rotated_cut %>%
    mutate(geometry = st_geometry(.) %>% lapply(rotate_geometry, angle = angle, center = center) %>% st_sfc(crs = st_crs(poly)))
  
  poly_attribs <- st_drop_geometry(poly)
  replicated_attribs <- poly_attribs[rep(1, nrow(rotated_back)), , drop = FALSE]
  replicated_attribs$split_num <- rotated_back$split_num
  replicated_attribs$split_id <- paste0(replicated_attribs[[id_col]], "_", rotated_back$split_num)
  
  result <- st_sf(replicated_attribs, geometry = rotated_back$geometry, crs = st_crs(poly))
  return(result)
}


# Read and apply
polys <- st_read("data/parcels/FOR.shp")

splits_all <- do.call(rbind, lapply(1:nrow(polys), function(i) {
  split_parallel_polygon(polys[i, ], step = 1.5, id_col = "ID")
}))


st_write(splits_all, "results/parcelSpit/FOR_split.shp")
