devtools::load_all() 
if (F) {
  devtools::load_all() 
  devtools::document()
  devtools::check()
}

df_world = choroplethr::df_world
df_pop_state = choroplethr::df_pop_state
df_county_demographics = choroplethr::df_county_demographics

# Baseline
state_choropleth(df = df_pop_state, value.name = 'value', label = 'state.abb')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population')
county_choropleth(df = df_county_demographics, geoid.name = 'region', value.name = 'median_hh_income')
county_choropleth(df = df_county_demographics, map_year = 2015, geoid.name = 'region', value.name = 'median_hh_income')

# Categorical
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'region_type')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'region_type', custom.colors = c('grey', 'green', 'blue', 'violet', 'brown', 'orange'))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'region_type', num_colors = 2) # should give warning that num_colors is ignored
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'region_type', custom.colors = c('red', 'green')) # should give error about too few colors

# Continuous
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 4)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 1)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 0)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 0, nbreaks = 2)

# Test discretization
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_colors = -1) 
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_colors = 0.5)
df_world_onecolor = df_world
df_world_onecolor$population = 1
country_choropleth(df = df_world_onecolor, geoid.name = 'iso_a3', value.name = 'population', num_colors = 7) # should give warning about too few colors

# Test colors
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 1,
                   color.min = 'red', color.max = 'jomama')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 0,
                   color.min = 'red', color.max = 'green')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 1,
                   color.min = 'red', color.max = 'green')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', num_color = 7,
                   color.min = 'red', color.max = 'green')


# Test labels
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', label = 'jomama') # should give error
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', label = 'iso_a3')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', label = 'iso_a3', ggrepel_options = list(label.padding = 1))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', label = 'iso_a3', label_text_size = .5)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', label = 'iso_a3', label_text_color = 'red')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', label = 'iso_a3', label_text_color = 'red', label_box_color = 'lightblue')

# Test other render features
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', border_color = 'red')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', border_color = 'red', border_thickness = 2)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', border_color = 'red', border_thickness = 2, background_color = 'blue')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', gridlines = TRUE)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', gridlines = TRUE, latlon_ticks = TRUE)

country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', title = 'jomama')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', title = 'jomama', legend = 'jomama')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', title = 'jomama', legend = 'jomama', legend_position = 'bottom')


# Test zoom
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', zoom = c('jomama'))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', zoom = c('USA', 'CAN', 'MEX'))

# Test projection
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'mercator')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'cartesian')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'robinson')
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'albers')

country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'mercator', limits_lat = c(20,60), limits_lon = c(-160, -50))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'cartesian', limits_lat = c(20,60), limits_lon = c(-160, -50))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'robinson', limits_lat = c(20,60), limits_lon = c(-160, -50), reproject = TRUE)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'albers', limits_lat = c(20,60), limits_lon = c(-160, -50))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'albers', limits_lat = NULL, limits_lon = c(-160, -50))

country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'albers', limits_lat = c(20,60), limits_lon = c(-160, -50), reproject = FALSE)
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'albers', limits_lat = c(20,60), limits_lon = c(-160, -50), gridlines = TRUE, latlon_ticks = TRUE)

country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'mercator', zoom = c('USA', 'CAN', 'MEX'))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'cartesian', zoom = c('USA', 'CAN', 'MEX'))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'robinson', zoom = c('USA', 'CAN', 'MEX'))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'albers', zoom = c('USA', 'CAN', 'MEX'))

country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'mercator', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'cartesian', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'robinson', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))
country_choropleth(df = df_world, geoid.name = 'iso_a3', value.name = 'population', projection = 'albers', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))

