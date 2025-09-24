# library(choroplethrMaps)
# library(tidycensus)
devtools::load_all() 

# data("df_state_demographics")
# 
# state_choropleth(df_state_demographics, value.name = 'total_population')
# names(df_state_demographics)[2] = 'value'
# 
# st_zoom = df_state_demographics$region[1:10]
# st_zoom = 'alaska'

# 
data("df_pop_country")
country_choropleth(df = df_pop_country, num_colors = 5, projection = 'robinson',
                   # continent = c('Oceania'),
                   zoom = NULL,
                   # zoom=c("united states of america", "canada", "mexico", "australia"),
                   return = 'plot')
# tester$choropleth.df
# tester$set_num_colors(5)
# tester$render()
# 
# tester$prepare_map()
# tester$get_aea_proj(tester$choropleth.df)

class(z)
sf::st_bbox(z)

tester$get_bounding_box()

library(rnaturalearth)
world <- ne_countries(scale = "small", returnclass = "sf")

lon_min <- -100
lon_max <- 100
lat_min <- -50
lat_max <- 50
z = c(-50, 50)

ggplot(world) + geom_sf(aes(fill = gdp_md), color = "dark grey", size = 0.2) + 
  coord_sf(    xlim = NULL,
               ylim = NULL,
               default_crs = 4326,
    crs = '+proj=robin')
ggplot(world) + geom_sf(aes(fill = gdp_md), color = "dark grey", size = 0.2) + 
  coord_sf(crs = 3857, ylim=c(-80,85), default_crs = 4326)
  
  coord_sf(    xlim = NULL,
               ylim = NULL,
               default_crs = 4326,
               crs = '+proj=robin')

coord_sf(crs = 3857, ylim=c(-15538711,19926188), default_crs = 4326)

lat_to_meters <- function(lat) {
  R <- 6378137  # radius of Earth in meters for Web Mercator
  meters <- R * log(tan(pi/4 + (lat * pi/180) / 2))
  return(meters)
}

lat_to_meters(80)

projection_list = list(cartesian = coord_sf(crs = 4326),
                       mercator = coord_sf(crs = 3857, ylim=c(-15538711,19926188)),
                       robinson = coord_sf(crs = '+proj=robin'))


c$projection_sf = switch(projection,
                         'cartesian' = 4326,
                         'mercator'= 3857,
                         'robinson' = '+proj=robin')
# state_choropleth(df = df_state_demographics, value.name = 'median_rent')
# state_choropleth(df = df_state_demographics, zoom = st_zoom)
# z = StateChoropleth$new(df_state_demographics) 
# sfdf = z$map.df
# z$set_zoom(st_zoom)
# z2 = z$render()
