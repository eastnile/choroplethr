# library(choroplethrMaps)
# library(tidycensus)
devtools::load_all()

# data("df_state_demographics")
# names(df_state_demographics)[2] = 'value'
# 
# st_zoom = df_state_demographics$region[1:10]
# st_zoom = 'alaska'

# 
data("df_pop_country")

# data('country.regions')
# saveRDS(country.regions, 'dev/country_regions.rds')

df_pop_country$value = ifelse(df_pop_country$value>1000000000, df_pop_country$value, -1*df_pop_country$value)

country_choropleth(df = df_pop_country, num_colors = 1)

# state_choropleth(df = df_state_demographics, value.name = 'median_rent')
# state_choropleth(df = df_state_demographics, zoom = st_zoom)
# z = StateChoropleth$new(df_state_demographics) 
# sfdf = z$map.df
# z$set_zoom(st_zoom)
# z2 = z$render()
