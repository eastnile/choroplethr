library(tigris)
library(rnaturalearth)
library(sf)
library(dplyr)
library(ggplot2)
library(rmapshaper)

## 1. US States ----
state.map = states(cb = TRUE, year = 2024)
state.map = shift_geometry(state.map, geoid_column = 'GEOID', position = 'below')
state.map = st_transform(state.map, 4326)
alaska = state.map[state.map$NAME == 'Alaska', ]
alaska_cropped = st_crop(alaska, xmin = -122, ymin = 19, xmax = -108.5, ymax = 30)
state.map = state.map[state.map$NAME != 'Alaska',]
state.map = rbind(state.map, alaska_cropped)

if (F) {
  ggplot(alaska_cropped) + geom_sf() 
  ggplot(state.map) + geom_sf() 
}

state.map = state.map[, c('NAME', 'STUSPS', 'STATEFP')]
names(state.map) = c('name.proper', 'state.abb', 'fips.character', 'geometry')
state.map$name.lower = tolower(state.map$name.proper)
state.map$fips.numeric = as.numeric(state.map$fips.character)
state.map = state.map[state.map$fips.numeric <= 56, ]
state.map = state.map[order(state.map$fips.numeric), ]
sum(!st_is_valid(state.map))

state.regions = st_drop_geometry(state.map)
saveRDS(state.regions, 'dev/state.regions.rds')
save(state.regions, file = 'data/state.regions.rda')

# Enlarge DC --
dc = state.map %>% filter(name.proper == "District of Columbia")
dc_geom = st_geometry(dc)                                     # extract geometry
scaled_dc = (dc_geom - st_centroid(dc_geom)) * 3 + st_centroid(dc_geom)
shift_vector = st_sfc(st_point(c(.05, .05)), crs = 4269)
shifted_dc = scaled_dc + shift_vector                         # scale/shift
st_geometry(dc) = shifted_dc                                  # apply new geometry
other_states = state.map %>% filter(name.proper != "District of Columbia")
st_crs(dc) = st_crs(other_states)
state.map.bigdc = rbind(other_states, dc)                        # recombine
sum(!st_is_valid(state.map.bigdc))
if (F) {
  ggplot(state.map.bigdc) +
    geom_sf() +  xlim(-100, -50) + ylim(30,50)
}

# Simplify geometries

state.map.hires <- ms_simplify(state.map, keep = 0.05, keep_shapes = TRUE)
state.map.lores <- ms_simplify(state.map, keep = 0.01, keep_shapes = TRUE)
state.map.bigdc <- ms_simplify(state.map.bigdc, keep = 0.01, keep_shapes = TRUE)

if (F) {
  state.map.test1 <- ms_simplify(state.map, keep = 0.01, keep_shapes = TRUE)
  state.map.test2 <- st_simplify(state.map, dTolerance = 1000)
  object.size(state.map)
  object.size(state.map.test1)
  object.size(state.map.test2)
  ggplot(state.map.test1) +
    geom_sf()
  ggplot(state.map.test2) +
    geom_sf()
}


state.map.hires = state.map.hires[, c('fips.numeric')]
save(state.map.hires, file = 'data/state.map.hires.rda')

state.map.lores = state.map.lores[, c('fips.numeric')]
save(state.map.lores, file = 'data/state.map.lores.rda')

state.map.bigdc = state.map.bigdc[, c('fips.numeric')]
save(state.map.bigdc, file = 'data/state.map.bigdc.rda')


# Hexmap

# Plot
hex_url = "https://raw.githubusercontent.com/Z3tt/30DayMapChallenge/master/data/us_states_hexgrid.geojson.json"
state.map.hex = st_read(hex_url, quiet = TRUE)
state.map.hex = select(state.map.hex, iso3166_2, geometry)
state.map.hex = merge(state.map.hex, state.regions, by.x = 'iso3166_2', by.y = 'state.abb')

if (F) {
  ggplot(state.map.hex) +
    geom_sf(aes(geometry = geometry)) +
    theme_void() +
    coord_sf(crs = 3395) +
    theme_void()
}

state.map.hex = state.map.hex[, c('fips.numeric', "geometry")]
save(state.map.hex, file = 'data/state.map.hex.rda')

# US Counties (2024) ----

prep_county_map = function(tigris_output) {
  county.map = tigris_output
  county.map = shift_geometry(county.map, geoid_column = 'GEOID', position = 'below')
  county.map = filter(county.map, !STATEFP %in% c("60", "66", "69", "72", "78"))
  stopifnot(length(table(county.map$STATEFP))==51)
  county.map = st_transform(county.map, 4326)
  
  alaska = county.map[county.map$STATEFP == '02', ]
  alaska_cropped = st_crop(alaska, xmin = -122, ymin = 19, xmax = -108.5, ymax = 30)
  county.map = county.map[county.map$STATEFP != '02',]
  county.map = rbind(county.map, alaska_cropped)
  
  if (F) {
    ggplot(county.map) + geom_sf() 
    ggplot(alaska_cropped)+ geom_sf() 
  }
  
  county.map = county.map[, c('STATEFP', 'GEOID', 'NAME', 'geometry')]
  names(county.map) = c('state.fips.character', 'fips.character', 'name.proper', 'geometry')
  
  states = choroplethr::state.regions[, c('fips.numeric', 'fips.character', 'state.abb', 'name.proper')]
  names(states) = c('state.fips.numeric', 'state.fips.character', 'state.abb', 'state.name.proper')
  
  county.map = left_join(county.map, states, by = 'state.fips.character')
  county.map$fips.numeric = as.numeric(county.map$fips.character)
  county.map = county.map[, c('fips.numeric', 'fips.character', 'name.proper', 'state.fips.numeric', 'state.fips.character', 'state.abb', 'state.name.proper', 'geometry')]
  county.map = county.map[order(county.map$state.fips.numeric, county.map$fips.numeric), ]
  county.map = ms_simplify(county.map, keep = 0.05, keep_shapes = TRUE)
  sum(!st_is_valid(county.map))
  return(county.map)
}


county.map.2024 = prep_county_map(tigris::counties(cb = TRUE, year = 2024))
county.map.2015 = prep_county_map(tigris::counties(cb = TRUE, year = 2015))

if (F) {
  ggplot(county.map.2024) + geom_sf()
  ggplot(county.map.2015) + geom_sf()
  county.map.2024 <- ms_simplify(county.map.2024, keep = 0.05, keep_shapes = TRUE)
}

county.regions.2024 = st_drop_geometry(county.map.2024)
save(county.regions.2024, file = 'data/county.regions.2024.rda')
county.map.2024 = county.map.2024[, c('fips.numeric', 'geometry')]
save(county.map.2024, file = 'data/county.map.2024.rda')

county.regions.2015 = st_drop_geometry(county.map.2015)
save(county.regions.2015, file = 'data/county.regions.2015.rda')
county.map.2015 = county.map.2015[, c('fips.numeric', 'geometry')]
save(county.map.2015, file = 'data/county.map.2015.rda')


## 3. World  ----
world <- ne_countries(scale = 50, returnclass = "sf")
worldsmall <- ne_countries(scale = 110, returnclass = "sf")
world <- ms_simplify(world, keep = 0.33, keep_shapes = FALSE) # simply geometries, drops some regions

world = world[world$adm0_a3 %in% worldsmall$adm0_a3, ]

if (F) {
  ggplot(world) + geom_sf()
  ggplot(world) + geom_sf() + coord_sf(xlim = c(-10, 40), ylim = c(35, 65))
  # cross reference simplified geometries with smaller world map
  worldbig <- ne_countries(scale = 50, returnclass = "sf")
  worldsmall <- ne_countries(scale = 110, returnclass = "sf")
  diff = setdiff(union(worldsmall$adm0_a3,  worldbig$adm0_a3), 
                 intersect(worldsmall$adm0_a3,  worldbig$adm0_a3))
  z = worldbig[worldbig$adm0_a3 %in% diff, ]
  z = worldsmall[worldsmall$adm0_a3 %in% diff, ]
  world = world[, c('admin', 'geometry')]
  sum(!st_is_valid(worldbig))
}


# Establish unique geoid ----

# 1. Establish candidate geoids: no duplicates, no wierd symbols
world_meta = st_drop_geometry(world)
duplicated = unlist(lapply(world_meta,  function(x){sum(duplicated(x))}))
nonalpha = unlist(lapply(world_meta, function(x) {sum(grepl("[^A-Za-z0-9 -]", x))}))
summary = data.frame(var = names(world_meta), ndup = duplicated, nonalpha = nonalpha)
summary$id_maybe = ifelse(summary$ndup == 0 & summary$nonalpha == 0, 1, 0)
id_maybe = summary$var[summary$id_maybe == 1]

# 2. Establish difference getween admin & geounit variables
world_meta$chk1 = ifelse(world_meta$admin != world_meta$geounit, 1, 0)
#View(world_meta[, c('admin', 'geounit', 'chk1')]) # admin and geounit differ only for tanzania
world_meta$chk2 = ifelse(world_meta$name_long != world_meta$name_en, 1, 0)
# View(world_meta[, c('name_long', 'name_en', 'chk2')]) # small difference between name_long and name_en

# 3. Four geoids identified, but some need work

# A. adm0_a3 is an internal code used by natural earth; it differs from the official iso_a3 variable as follows:
# France has a code (FRA)
# Norway has a code (NOR)
# Northern Cyprus (CYN), Somaliland (SOL), and Kosovo (KOS) have codes.

# B. iso_a2 is included because it is commonly used, but the following changes need to be made:

# Taiwan is coded TW instead of CN-TW

world$iso_a2[world$adm0_a3 == "FRA"] = 'FR' # France needs a code
world$iso_a2[world$adm0_a3 == "NOR"] = 'NO' # Ditto Norway
world$iso_a2[world$adm0_a3 == "CYN"] = 'XC' # Codes for some disputed territories
world$iso_a2[world$adm0_a3 == "SOL"] = 'XS' # Codes for some disputed territories
world$iso_a2[world$adm0_a3 == "KOS"] = 'XK' # Codes for some disputed territories
world$iso_a2[world$adm0_a3 == "TWN"] = 'TW' # Codes for some disputed territories


world = st_transform(world, 4326)
world$admin_lower = tolower(world$admin)

world = select(world, c('admin', 'admin_lower', 'adm0_a3', 'iso_a2', 'continent'),
               contains('name'), fips_10, un_a3, wb_a2, wb_a3, contains('woe'),
               geometry, -name, -name_long, -name_sort, -name_alt, -name_len)
names(world)

world = world %>%
  rename(
    name.proper = admin,
    name.lower = admin_lower,
    iso_a3 = adm0_a3)

# Assert unique ID and correct lengths
sum(duplicated(world$name.proper))
sum(duplicated(world$name.lower))
sum(duplicated(world$iso_a3))
sum(duplicated(world$iso_a2))
sum(nchar(world$iso_a3) != 3)
sum(nchar(world$iso_a2) != 2)


if (F) { # old code, obsolete
  sf_use_s2(FALSE)
  world <- st_simplify(world, dTolerance = 0) # fixes some invalid regions
  world_test = ms_simplify(world, keep = 0.99, keep_shapes = FALSE)
  ggplot(filter(world_test, iso_a3=='USA')) + geom_sf()
  ggplot(world) + geom_sf()
}

sum(!st_is_valid(world))

country.map = world
country.regions = st_drop_geometry(country.map)
save(country.regions, file = 'data/country.regions.rda', compress = TRUE)

country.map = country.map[, c('iso_a3', 'geometry')]
save(country.map, file = 'data/country.map.rda', compress = TRUE)

# Admin1

sf_admin1 = rnaturalearth::ne_states(returnclass = "sf")
sf_admin1 = st_transform(sf_admin1, 4326)
vars_needed = names(sf_admin1)[-grep('FCLASS', names(sf_admin1))]
sf_admin1 = sf_admin1[, vars_needed]

sf_admin1_lores <- ms_simplify(sf_admin1, keep = 0.05, keep_shapes = TRUE)

format(object.size(sf_admin1), 'MB')

format(object.size(sf_admin1_lores), 'MB')

ggplot(sf_admin1_lores %>% filter(geonunit == 'Japan')) + geom_sf()


