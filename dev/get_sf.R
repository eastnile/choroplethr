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

world_meta = st_drop_geometry(world)

duplicated = unlist(lapply(world_meta,  function(x){sum(duplicated(x))}))
nonalpha = unlist(lapply(world_meta, function(x) {sum(grepl("[^A-Za-z0-9 -]", x))}))

summary = data.frame(var = names(world_meta), ndup = duplicated, nonalpha = nonalpha)
summary$id_maybe = ifelse(summary$ndup == 0 & summary$nonalpha == 0, 1, 0)
id_maybe = summary$var[summary$id_maybe == 1]

#View(world_meta[, id_maybe])
world_meta$chk1 = ifelse(world_meta$admin != world_meta$geounit, 1, 0)
#View(world_meta[, c('admin', 'geounit', 'chk1')]) # admin and geounit differ only for tanzania

world_meta_good = world_meta[, c('admin', 'adm0_a3', 'iso_a2', 'continent')]
names(world_meta_good) = c('name.proper', 'iso_a3', 'iso_a2', 'continent')

# adm0_a3 is an internal code used by natural earth; it differs from the iso_a3 variable in natrual earth as follows:
# France has a code (FRA)
# Norway has a code (NOR)
# Northern Cyprus (CYN), Somaliland (SOL), and Kosovo (KOS) have codes.

# iso_a2 is included because it is common, with the following changes compared to the iso_a2 variable in natural earth:
# France has a code (FR)
# Norway has a code (NO)
# Northern Cyprus, Somaliland, and Kosovo are missing.
# Taiwan is coded TW instead of CN-TW

world_meta_good$iso_a2[world_meta_good$iso_a3 == "FRA"] = 'FR'
world_meta_good$iso_a2[world_meta_good$iso_a3 == "NOR"] = 'NO'
world_meta_good$iso_a2[world_meta_good$iso_a3 == "CYN"] = NA
world_meta_good$iso_a2[world_meta_good$iso_a3 == "SOL"] = NA
world_meta_good$iso_a2[world_meta_good$iso_a3 == "KOS"] = NA

world_meta_good$name.lower = tolower(world_meta_good$name.proper)


saveRDS(world_meta_good, 'dev/regions_country.rds')

world_good = world[, c('adm0_a3')]
names(world_good) = c('iso_a3', 'geometry')

saveRDS(world_good, 'dev/sf_country.rds')

