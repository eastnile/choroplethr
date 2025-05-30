library(tigris)
library(choroplethrMaps)
library(rnaturalearth)
library(sf)
# preloaded maps in coroplethrMaps
data(package = 'choroplethrMaps')
data(country.map)
data(state.map)
data(county.map)
data(state.regions)
data(country.regions)

## US States ----
state_sf = states(cb = TRUE, year = 2024)
state_sf = shift_geometry(state_sf, geoid_column = 'GEOID', position = 'below')
state_sf <- st_transform(state_sf, 4326)
st_crs(state_sf)

alaska = state_sf[state_sf$NAME == 'Alaska', ]
# ggplot(alaska) + geom_sf() 

alaska_cropped = st_crop(alaska, xmin = -122, ymin = 19, xmax = -108.5, ymax = 30)
# ggplot(alaska_cropped) + geom_sf() 
state_sf = state_sf[state_sf$NAME != 'Alaska',]
state_sf = rbind(state_sf, alaska_cropped)

state_sf = state_sf[, c('NAME', 'STUSPS', 'STATEFP')]
names(state_sf) = c('name.proper', 'state.abb', 'fips.character', 'geometry')
state_sf$name.lower = tolower(state_sf$name.proper)
state_sf$fips.numeric = as.integer(state_sf$fips.character)
state_sf = state_sf[state_sf$fips.numeric <= 56, ]
state_sf = state_sf[order(state_sf$fips.numeric), ]
sum(!st_is_valid(state_sf))
# ggplot(state_sf) + geom_sf() 

state_sf = state_sf[, c('name.proper', 'name.lower', 'state.abb', 'fips.character', 'fips.numeric')]
saveRDS(state_sf, 'dev/sf_state.rds')

state_sf_meta = st_drop_geometry(state_sf)
saveRDS(state_sf_meta, 'dev/regions_state.rds')

# Enlarge DC ----
dc = state_sf %>% filter(name.proper == "District of Columbia")
dc_geom = st_geometry(dc)                                     # extract geometry
scaled_dc = (dc_geom - st_centroid(dc_geom)) * 3 + st_centroid(dc_geom)
shift_vector = st_sfc(st_point(c(.05, .05)), crs = 4269)
shifted_dc = scaled_dc + shift_vector                         # scale/shift
st_geometry(dc) = shifted_dc                                  # apply new geometry
other_states = state_sf %>% filter(name.proper != "District of Columbia")
st_crs(dc) = st_crs(other_states)
state_sf_dcbig = rbind(other_states, dc)                        # recombine

if (F) {
  # preview
  ggplot(state_sf_dcbig) +
    geom_sf() + ylim(30,50) + xlim(-100, -50)
}

sum(!st_is_valid(state_sf_dcbig))
saveRDS(state_sf_dcbig, 'dev/sf_state_bigdc.rds')

# Prep County SF Map
county.regions.legacy = readRDS('dev/regions_county_legacy.rds')
county_sf = tigris::counties(cb = TRUE, year = 2024)
county_sf = filter(county_sf, !STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))
stopifnot(length(table(county_sf$STUSPS))==51)
county.regions = county_sf[, c('STATEFP', 'GEOID', 'NAMELSAD', 'STUSPS', 'STATE_NAME')]
county.regions = st_drop_geometry(county.regions)
names(county.regions) = c('state.fips.character', 'fips.character', 'name.proper', 'state.abb', 'state.name.proper')
county.regions$state.fips.numeric = as.integer(county.regions$state.fips.character)
county.regions$fips.numeric = as.integer(county.regions$fips.character)
county.regions = county.regions[, c('fips.numeric', 'fips.character', 'name.proper', 'state.fips.numeric', 'state.fips.character', 'state.abb', 'state.name.proper')]
saveRDS(county.regions, 'dev/regions_county.rds')

## Country SFs ----
world <- ne_countries(scale = "small", returnclass = "sf")

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

# 3. Four geoids identified, but some need work
world_good = world[, c('admin', 'adm0_a3', 'iso_a2', 'continent')]
names(world_good) = c('name.proper', 'iso_a3', 'iso_a2', 'continent', 'geometry')

# A. adm0_a3 is an internal code used by natural earth; it differs from the official iso_a3 variable as follows:
# France has a code (FRA)
# Norway has a code (NOR)
# Northern Cyprus (CYN), Somaliland (SOL), and Kosovo (KOS) have codes.

# B. iso_a2 is included because it is commonly used, but the following changes need to be made:

# Taiwan is coded TW instead of CN-TW

world_good$iso_a2[world_good$iso_a3 == "FRA"] = 'FR' # France needs a code
world_good$iso_a2[world_good$iso_a3 == "NOR"] = 'NO' # Ditto Norway
world_good$iso_a2[world_good$iso_a3 == "CYN"] = 'XC' # Codes for some disputed territories
world_good$iso_a2[world_good$iso_a3 == "SOL"] = 'XS' # Codes for some disputed territories
world_good$iso_a2[world_good$iso_a3 == "KOS"] = 'XK' # Codes for some disputed territories
world_good$iso_a2[world_good$iso_a3 == "TWN"] = 'TW' # Codes for some disputed territories
world_good$name.lower = tolower(world_good$name.proper)
world_good = world_good[, c('name.proper', 'name.lower', 'iso_a3', 'iso_a2', 'continent', 'geometry')]

# Assert unique ID and correct lengths
sum(duplicated(world_good$name.proper))
sum(duplicated(world_good$name.lower))
sum(duplicated(world_good$iso_a3))
sum(duplicated(world_good$iso_a2))
sum(nchar(world_good$iso_a3) != 3)
sum(nchar(world_good$iso_a2) != 2)

world_good = st_transform(world_good, 4326)

# Fix broken geoms (?)
st_is_valid(world_good, reason = TRUE)
world_good = st_make_valid(world_good)
bad_geom = world_good[!st_is_valid(world_good), ]
saveRDS(world_good, 'dev/sf_country.rds')

world_meta_good = st_drop_geometry(world_good)
saveRDS(world_meta_good, 'dev/regions_country.rds')

