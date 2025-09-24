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
