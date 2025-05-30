library(rnaturalearth)
library(sf)

# Country

df_country_demographics = ne_countries(scale = "small", returnclass = "sf")
df_country_demographics = df_country_demographics[, c('adm0_a3', 'pop_est', 'gdp_md', 'type')]
stopifnot(sum(duplicated(df_country_demographics$adm0_a3)) == 0)
df_country_demographics = st_drop_geometry(df_country_demographics)
names(df_country_demographics) = c('region', 'population', 'median_gdp', 'region_type')
usethis::use_data(df_country_demographics, overwrite = TRUE)

#saveRDS(df_world, 'dev/df_world.rds')

# State
df_state_demographics = get_state_demographics()
usethis::use_data(df_state_demographics, overwrite = TRUE)
#data(df_state_demographics)
