library(sf)
library(ggplot2)
library(dplyr)
library(tigris)      # for US state boundaries
library(viridis)     # for color scales

options(tigris_class = "sf")
states <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))

# Reposition AK and HI (without using case_when)
ak_index <- states$STUSPS == "AK"
hi_index <- states$STUSPS == "HI"

# Scale + shift Alaska (in WGS84 space — still degrees)
states$geometry[ak_index] <- (states$geometry[ak_index] - st_centroid(states$geometry[ak_index])) * 0.35 +
  st_point(c(-120, 25))

# Shift Hawaii
states$geometry[hi_index] <- states$geometry[hi_index] + c(35, 4)



options(tigris_class = "sf")
states_sf <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))  # remove territories
set.seed(42)
state_data <- data.frame(
  STUSPS = state.abb,
  value = runif(50, 0, 100)
)


# Merge with spatial data
states_sf <- left_join(states_sf, state_data, by = "STUSPS")


# Reposition AK and HI (without using case_when)
ak_index <- states_sf$STUSPS == "AK"
hi_index <- states_sf$STUSPS == "HI"

# Scale + shift Alaska (in WGS84 space — still degrees)
states_sf$geometry[ak_index] <- (states_sf$geometry[ak_index] - st_centroid(states_sf$geometry[ak_index])) * 0.35 +
  st_point(c(-120, 25))

# Shift Hawaii
states_sf$geometry[hi_index] <- states_sf$geometry[hi_index] + c(35, 4)



states_sf_proj <- st_transform(states_sf, 5070)

st_geometry(states_sf_proj[states_sf_proj$STUSPS == "AK", ]) <-
  st_geometry(states_sf_proj[states_sf_proj$STUSPS == "AK", ]) * 0.35 + c(4e6, -2.5e6)

st_geometry(states_sf_proj[states_sf_proj$STUSPS == "HI", ]) <-
  st_geometry(states_sf_proj[states_sf_proj$STUSPS == "HI", ]) + c(5e6, -1e6)


ggplot(states_sf_proj) +
  geom_sf(aes(fill = value)) +
  theme_void()


st_geometry(states_sf[states_sf$STUSPS == "AK", ]) <- 
  st_geometry(states_sf[states_sf$STUSPS == "AK", ]) * 0.35 + c(55, -130)

st_geometry(states_sf[states_sf$STUSPS == "HI", ]) <- 
  st_geometry(states_sf[states_sf$STUSPS == "HI", ]) + c(52, -5)

ggplot(states_sf) +
  geom_sf(aes(fill = value), color = "white", size = 0.2) +
  scale_fill_viridis(option = "plasma", name = "Example Value") +
  theme_void() +
  theme(legend.position = "bottom")
