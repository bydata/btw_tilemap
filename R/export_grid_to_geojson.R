pacman::p_load("tidyverse", "sf", "geojsonsf", "here")

grid <- read_csv(here::here("data", "de_constituencies_grid.csv"))
str(grid)

grid_sf_points <- grid %>% 
  # Zeilennummern (row) drehen
  mutate(row = max(row) - row + 1) %>% 
  st_as_sf(coords = c("col", "row"))

ggplot(grid_sf_points) +
  geom_sf() +
  coord_sf()

#' Convert sf objects to GeoJSON
#' https://cran.r-project.org/web/packages/geojsonsf/vignettes/geojson-sf-conversions.html#what-it-does
grid_geojson_pts <- sf_geojson(grid_sf_points)
write_file(grid_geojson_pts, here("data", "de_constituencies_grid.geojson.json"))
