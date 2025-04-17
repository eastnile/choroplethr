library(choroplethrMaps)
library(rnaturalearth)
# preloaded maps in coroplethrMaps
data(package = 'choroplethrMaps')
data(country.map)
data(state.map)
data(county.map)
data(state.regions)
data(country.regions)

# choroplethrMaps::country.map

# Prep State SF Map

state_sf = tigris::states(cb = TRUE, year = 2024)
state.regions = readRDS('dev/st_regions.rds')
state_sf_small = state_sf[, c('NAME')]
names(state_sf_small) = c('region', 'geometry')
state_sf_small$region = tolower(state_sf_small$region)
state_sf_small = state_sf_small[state_sf_small$region %in% state.regions$name.lower, ]
stopifnot(nrow(state_sf_small) != 51)
rownames(state_sf_small) = NULL
saveRDS(state_sf_small, 'dev/sf_state.rds')

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

# Country SFs
world <- ne_countries(scale = "small", returnclass = "sf")

# This one is more complicated

world = st_drop_geometry(world)

?duplicated

duplicated = unlist(lapply(world,  function(x){sum(duplicated(x))}))
nonalpha = unlist(lapply(world, function(x) {sum(grepl("[^A-Za-z0-9 -]", x))}))

meta = data.frame(var = names(world), ndup = unlist(lapply(world,  function(x){sum(duplicated(x))})),
                  nonalpha =  unlist(lapply(world, function(x) {sum(grepl("[^A-Za-z0-9 -]", x))})))

meta$id_maybe = ifelse(meta$ndup == 0 & meta$nonalpha == 0, 1, 0)
