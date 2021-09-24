pacman::p_load("tidyverse", "sf", "here", "geojsonio", "colorspace", "gsheet")

sf::sf_use_s2(FALSE)

## READ GEOMETRY ==============================================
#' https://pitchinteractiveinc.github.io/tilegrams/
#' Download geometry "Germany - Constituencies" as TopoJSON
#' and place it in the data directory

filepath_topo <- here("data", "tiles.topo.json")
wk_topo <- topojson_read(filepath_topo)

# Simple plot of hexagons
ggplot(wk_topo, aes(geometry = geometry)) +
  geom_sf(col = "grey20", size = 0.2) +
  geom_sf_text(aes(label = id),
               size = 1.75) +
  coord_sf() +
  theme_void()


## Mapping Wahlkreise in Bundesländern ===================================
#' Source: Bundeswahlleiter
#' https://www.bundeswahlleiter.de/service/glossar/w/wahlkreise.html
wk2bl <- tribble(
  ~bundesland, ~wknr_min, ~wknr_max,
  "Schleswig-Holstein", 1, 11,
  "Mecklenburg-Vorpommern",	12, 17,
  "Hamburg",	18, 23,
  "Niedersachsen",	24, 53,
  "Bremen",	54, 55,
  "Brandenburg", 56, 65,
  "Sachsen-Anhalt", 	66, 74,
  "Berlin",	75,  86,
  "Nordrhein-Westfalen",	87, 150,
  "Sachsen",	151, 166,
  "Hessen",	167, 188,
  "Thüringen", 189, 196,
  "Rheinland-Pfalz", 197, 211,
  "Bayern", 212, 257,
  "Baden-Württemberg",	258, 295,
  "Saarland",	296, 299
)

# Expand the dataframe to all constituencies
wk2bl <- wk2bl %>% 
  pivot_longer(cols = c(wknr_min, wknr_max), 
               names_to = "X1", values_to = "wknr") %>% 
  select(-X1) %>% 
  right_join(tibble(wknr = 1:299), by = "wknr") %>% 
  arrange(wknr) %>% 
  fill(bundesland, .direction = "down") %>% 
  mutate(wknr = as.character(wknr))



# Plot with constituencies colored by federal state
wk_topo %>% 
  inner_join(wk2bl, by = c("id" = "wknr")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = bundesland),
          col = "grey20", size = 0.2, show.legend = FALSE) +
  geom_sf_text(aes(label = id),
               size = 1.75) +
  coord_sf() +
  theme_void()

wk_topo %>% 
  inner_join(wk2bl, by = c("id" = "wknr")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(col = bundesland),
          size = 0.2, show.legend = FALSE) +
  geom_sf_text(aes(label = id),
               size = 1.75) +
  coord_sf() +
  theme_void()


# Merge shapes of constituencies into state-level shapes 
bland_shape <- wk_topo %>% 
  inner_join(wk2bl, by = c("id" = "wknr")) %>% 
  group_by(bundesland) %>% 
  summarize(geometry_bland = st_union(geometry))

bland_shape %>% 
  ggplot(aes(geometry = geometry_bland)) +
  geom_sf(aes(group = bundesland),
          size = 0.2, show.legend = FALSE) +
  coord_sf() +
  theme_void()


# Plot constituencies with state boundaries 
wk_topo %>% 
  inner_join(wk2bl, by = c("id" = "wknr")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(size = 0.1, col = "grey90", fill = "#334a2b", 
          alpha = 0.33, show.legend = FALSE) +
  geom_sf_text(aes(label = id),
               size = 1.75) +
  geom_sf(data = bland_shape,
          aes(geometry = geometry_bland, 
              group = bundesland),
          fill = NA, col = "grey20",
          size = 0.5, show.legend = FALSE) +
  coord_sf() +
  theme_void()


## LOAD SURVEY DATA ====================================
#' Source: Arndt Leiniger
#' https://aleininger.eu/citizens_forecast2021/

sheet_url <- "https://docs.google.com/spreadsheets/d/1xOg9kNRMfmUXoJAYNp7R93UHfPlVus-2VV-i-Rdx1Uc/edit#gid=0"
bvorh <- gsheet2tbl(sheet_url)

## Simple calculation of the winner's party in each constituency
wk_winner <- bvorh %>% 
  group_by(id = as.character(wkr)) %>% 
  filter(Stimmenanteil_Median == max(Stimmenanteil_Median)) %>% 
  mutate(group_rows = n()) %>%
  summarize(winner = ifelse(max(group_rows) == 1, max(party), "undecided"))

# Party colors
party_colors <- c("CDU" = "grey9",
                  "CSU" = "grey9",
                  "SPD" = "#E3000F", 
                  "AfD" = rgb(0, 158, 224, maxColorValue = 255),
                  "FDP" = darken("#ffed00", 0.1),
                  "Linke" = "purple",
                  "Grüne" = rgb(100, 161, 45, maxColorValue = 255))


wk_topo %>% 
  inner_join(wk2bl, by = c("id" = "wknr")) %>% 
  inner_join(wk_winner, by = "id") %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = winner), size = 0.1, col = "grey80", 
          alpha = 0.45, show.legend = FALSE) +
  geom_sf_text(aes(label = id),
               size = 1.75) +
  geom_sf(data = bland_shape,
          aes(geometry = geometry_bland, 
              group = bundesland),
          fill = NA, col = "grey20",
          size = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c(party_colors, c("undecided" = "grey85"))) +
  coord_sf() +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = "white"))
ggsave(here("plots", "tilegrid_wk.png"), width = 1200, height = 1200, units = "px")

