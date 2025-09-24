# Load hex-shaped US states
hex_url <- "https://raw.githubusercontent.com/holtzy/D3-graph-gallery/master/DATA/us_states_hexgrid.geojson.json"
# Merge your data
hex <- hex %>%
  left_join(state_data, by = c("iso3166_2" = "state"))  # Make sure your keys match

# Plot
ggplot(hex) +
  geom_sf() +
  theme_void() +
  coord_sf(crs = 3395) +
  geom_sf_text(aes(geometry = geometry, label = iso3166_2),
               fun.geometry = function(x) st_centroid(x)) +
  theme_void()

hex_url <- "https://raw.githubusercontent.com/Z3tt/30DayMapChallenge/master/data/us_states_hexgrid.geojson.json"
hex <- st_read(hex_url, quiet = TRUE)



library(tilemaps)
library(dplyr)
library(ggplot2)
library(sf)

z = tigris::states()

z$fips.numeric = as.numeric(z$GEOID)
z = z[z$fips.numeric <= 56, ]


z = z %>% filter(!STUSPS %in% c('HI', 'AK'))


state_map = choroplethr::state.map
state_map = merge(state_map, choroplethr::state.regions)
state_map = state_map %>% filter(!state.abb %in% c('HI', 'AK'))

z <- z %>%
  mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE,
                                 interpolate = 1, smoothness = 0))

ggplot(z) +
  geom_sf()
  
all_states <- governors %>%
  add_row(abbreviation = "AK", party = "Republican",
          tile_map = create_island(governors$tile_map, "lower left")) %>%
  add_row(abbreviation = "HI", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-12050000, 3008338)))

ggplot(all_states) +
  geom_sf(aes(geometry = tile_map)) +
  geom_sf_text(aes(geometry = tile_map, label = abbreviation),
               fun.geometry = function(x) st_centroid(x)) +
  theme_void()


  add_row(abbreviation = "HI", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-12050000, 3008338)))

ggplot(governors) +
  geom_sf(aes(geometry = tile_map))

state_map_old = choroplethr::state.regions
state.map.hex = merge(state_map_old, governors[, c('abbreviation', 'tile_map')],
                      by.x = 'state.abb', by.y = 'abbreviation')



ggplot(governors) +
  geom_sf(aes(geometry = tile_map)) +
  geom_sf_text(aes(geometry = tile_map, label = abbreviation),
               fun.geometry = function(x) st_centroid(x)) 
  
governors <- governors %>%
  mutate(tile_map = generate_map(geometry, square = TRUE, flat_topped = TRUE,
                                 prop = 0,
                                 interpolate = 2.1,
                                 smoothness = 0,
                                 shift = c(0,0)))

all_states <- governors %>%
  add_row(abbreviation = "AK", party = "Republican",
          tile_map = create_island(governors$tile_map, "lower left")) %>%
  add_row(abbreviation = "HI", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-12050000, 3008338))) %>%
  add_row(abbreviation = "DC", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-8540000, 4635000)))


ggplot(all_states) +
  geom_sf(aes(geometry = tile_map))

governors <- governors %>%
  mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE))

all_states <- governors %>%
  add_row(abbreviation = "AK", party = "Republican",
          tile_map = create_island(governors$tile_map, "lower left")) %>%
  add_row(abbreviation = "HI", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-12050000, 3008338))) %>%
  add_row(abbreviation = "DC", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-8540000, 4635000)))


ggplot(all_states) +
  geom_sf(aes(geometry = tile_map)) +
geom_sf_text(aes(geometry = tile_map, label = abbreviation),
             fun.geometry = function(x) st_centroid(x)) +
  theme_void()
