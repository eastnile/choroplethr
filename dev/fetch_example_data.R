library(rnaturalearth)
library(sf)

# Country

df_country_demographics = ne_countries(scale = "small", returnclass = "sf")
df_country_demographics = df_country_demographics[, c('adm0_a3', 'pop_est', 'gdp_md', 'type')]
stopifnot(sum(duplicated(df_country_demographics$adm0_a3)) == 0)
df_country_demographics = st_drop_geometry(df_country_demographics)
names(df_country_demographics) = c('region', 'population', 'gdp', 'region_type')
save(df_country_demographics, file = 'data/df_country_demographics.rda')
data(df_country_demographics)
choroplethr::df_country_demographics
#saveRDS(df_world, 'dev/df_world.rds')

# State
devtools::load_all()
df_state_demographics = get_state_demographics()
save(df_state_demographics, file = 'data/df_state_demographics.rda')
data(df_state_demographics)
choroplethr::df_state_demographics

# county
df_county_demographics = get_county_demographics()
save(df_county_demographics, file = 'data/df_county_demographics.rda')
data(df_county_demographics)
choroplethr::df_county_demographics

# tract
df_ny_tract_demographics = get_tract_demographics(state_name = 'NY', endyear = 2022)
save(df_ny_tract_demographics, file = 'data/df_ny_tract_demographics.rda')
data(df_ny_tract_demographics)
choroplethr::df_ny_tract_demographics

# zip
# zip/state/county lookup
load('C:/Users/zhaochenhe/git/choroplethrZip/data/zip.regions.rdata')
zip_lookup = zip.regions

zip_lookup = left_join(zip_lookup, choroplethr::county.regions.2015, join_by('county.fips.numeric' == 'fips.numeric'))
zip_lookup = zip_lookup[, c('region', 'county.fips.numeric', 'state.fips.numeric', 'county.name', 'state.abb', 'cbsa', 'cbsa.title')]

names(zip_lookup) = c('zip_code', 'fips.numeric', 'state.fips.numeric', 'county.name', 'state.abb', 'cbsa', 'cbsa.title')

for (var in c('county.name', 'state.abb', 'cbsa', 'cbsa', 'cbsa.title')) {
  zip_lookup[[var]] = as.factor(zip_lookup[[var]])
}
lapply(zip_lookup, object.size)

save(zip_lookup, file = 'data/zip_lookup.rda', compress = 'xz')

# zip demo data
ri_zips = zip_lookup %>% filter(state.name == 'rhode island') %>% pull(region)
df_ri_zip_demographics = tidycensus::get_acs(geography = 'zcta', 
                             variable = c('B01003_001', 'B19013_001'), 
                             year = 2022, dataset = 'acs5', output = 'wide')

df_ri_zip_demographics = df_ri_zip_demographics[, c(1, 3, 5)]
df_ri_zip_demographics = df_ri_zip_demographics[df_ri_zip_demographics$region %in% ri_zips, ]
names(df_ri_zip_demographics) = c('region', 'population',  'median_hh_income')
save(df_ri_zip_demographics, file = 'data/df_ri_zip_demographics.rda')
