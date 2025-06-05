# Load hex-shaped US states
hex_url <- "https://raw.githubusercontent.com/holtzy/D3-graph-gallery/master/DATA/us_states_hexgrid.geojson.json"
# Merge your data
hex <- hex %>%
  left_join(state_data, by = c("iso3166_2" = "state"))  # Make sure your keys match

# Plot
ggplot(hex) +
  geom_sf() +
  theme_void() +
  coord_sf(crs = 3395)

hex_url <- "https://raw.githubusercontent.com/Z3tt/30DayMapChallenge/master/data/us_states_hexgrid.geojson.json"
hex <- st_read(hex_url, quiet = TRUE)


library(tilemaps)
governors <- governors %>%
  mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE))

all_states <- governors %>%
  add_row(abbreviation = "AK", party = "Republican",
          tile_map = create_island(governors$tile_map, "lower left")) %>%
  add_row(abbreviation = "HI", party = "Democrat",
          tile_map = create_island(governors$tile_map, c(-12050000, 3008338)))

ggplot(all_states) +
  geom_sf(aes(geometry = tile_map))
  
  
  geom_sf(aes(geometry = tile_map)) +
  geom_sf_text(aes(geometry = tile_map, label = abbreviation),
               fun.geometry = function(x) st_centroid(x)) +
  theme_void()
