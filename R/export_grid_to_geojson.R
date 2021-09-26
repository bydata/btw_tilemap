pacman::p_load("tidyverse", "sf", "geojsonsf", "here")

# Grid einlesen
grid <- read_csv(here::here("data", "de_constituencies_grid.csv"))
str(grid)

# Koordinaten auf ungefähre Koordinaten Deutschlands normalisieren
bb_germany <- osmdata::getbb("Germany", featuretype = "country")

grid_normalized <- 
  grid %>% 
  select(wk, row, col) %>% 
  mutate(row = max(row) - row + 1,
         lat = row/max(row) * (bb_germany["y", "max"] - bb_germany["y", "min"]) + 
           bb_germany["y", "min"],
         lon = col/max(col) * (bb_germany["x", "max"] - bb_germany["x", "min"]) + 
           bb_germany["x", "min"])

bb_germany
summary(grid_normalized$lat)
summary(grid_normalized$lon)


# # Grid-Koordinaten in sf-Geometry übersetzen
# grid_sf_points <- grid %>% 
#   # Zeilennummern (row) drehen
#   mutate(row = max(row) - row + 1) %>% 
#   st_as_sf(coords = c("col", "row"))
# str(grid_sf_points)


# Grid-Koordinaten in sf-Geometry übersetzen
grid_sf_points <- grid_normalized %>% 
  st_as_sf(coords = c("lon", "lat"))
str(grid_sf_points)

ggplot(grid_sf_points) +
  geom_sf() +
  coord_sf()


#' Convert sf objects to GeoJSON format
#' https://cran.r-project.org/web/packages/geojsonsf/vignettes/geojson-sf-conversions.html#what-it-does
grid_geojson_pts <- sf_geojson(grid_sf_points)
write_file(grid_geojson_pts, here("data", "de_constituencies_points.geojson.json"))



## Test it at https://geojson.io/
