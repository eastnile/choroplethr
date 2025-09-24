devtools::load_all() 
if (F) {
  #install.packages(c('Hmisc','stringr', 'ggplot2', 'dplyr', 'R6', 'ggrepel', 'tigris', 'sf', 'tidycensus', 'rnatrualearth'))
  tools::resaveRdaFiles("data/", compress = "xz")
  devtools::load_all() 
  devtools::document()
  devtools::check(cran = T, manual = T)
  devtools::check(manual = T)
  # to check in console, first build, then run:
  #"C:\Program Files\R\R-4.3.3\bin\R.exe" CMD check choroplethr_5.0.1.tar.gz
}

df_world = choroplethr::df_country_demographics
df_pop_state = choroplethr::df_pop_state
df_county_demographics = choroplethr::df_county_demographics

# Baseline
state_choropleth(df = df_pop_state)
state_choropleth(df = df_pop_state, value.name = 'value', label = 'state.abb', style = 'hexgrid', projection = 'mercator')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population')
county_choropleth(df = df_county_demographics, map_year = 2015, geoid.name = 'region', value.name = 'median_hh_income')
county_choropleth(df = df_county_demographics, map_year = 2015, geoid.name = 'region', value.name = 'median_hh_income', add_state_outline = FALSE)
county_choropleth(df = df_county_demographics, geoid.name = 'region', value.name = 'median_hh_income', state_zoom = c('CA','OR','WA'))


# return = sf 
z = country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'continent', return = 'sf')
class(z)

# Categorical
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'region_type')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'region_type', custom.colors = c('grey', 'green', 'blue', 'violet', 'brown', 'orange'))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'region_type', num_colors = 2) # should give warning that num_colors is ignored
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'region_type', custom.colors = c('red', 'green')) # should give error about too few colors

# Continuous
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 4)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 6, color.min = 'red', color.max = 'green')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 0)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 1)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 0, nbreaks = 2)

# Test discretization
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_colors = -1) 
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_colors = 0.5)
df_world_onecolor = df_world
df_world_onecolor$population = 1
country_choropleth(df = df_world_onecolor, geoid.name = 'region', value.name = 'population', num_colors = 7) # should give warning about too few colors

# Test colors
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 1,
                   color.min = 'red', color.max = 'jomama') # should error saying not a color
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 0,
                   color.min = 'red', color.max = 'green')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 1,
                   color.min = 'red', color.max = 'green')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_color = 7,
                   color.min = 'red', color.max = 'green')


# Test labels
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'jomama') # should give error
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', legend_position = 'bottom')

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', legend_position = 'bottom', projection = 'mercator', limits_lat = c(-80, 80))

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', legend_position = 'bottom', projection = 'robinson')

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', legend_position = 'bottom', projection = 'albers')

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', ggrepel_options = list(label.padding = 1))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', label_text_size = .5)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', label_text_color = 'red')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', label = 'iso_a3', label_text_color = 'red', label_box_color = 'lightblue')

# Test other render features
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', border_color = 'red')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', border_color = 'red', border_thickness = 2)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', border_color = 'red', border_thickness = 2, background_color = 'blue')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', gridlines = TRUE)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', gridlines = TRUE, 
                   latlon_ticks = TRUE, whitespace = FALSE)

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', title = 'jomama')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', title = 'jomama', legend = 'jomama')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', title = 'jomama', legend = 'jomama', legend_position = 'bottom')


# Test zoom
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', zoom = c('jomama'))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', zoom = c('USA', 'CAN', 'MEX'))

# Test continent
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', continent_zoom = 'Europe')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', continent_zoom = 'Europe',
                   projection = 'albers')

# Test projection
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'mercator')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'cartesian')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'robinson')
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers')

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'mercator', limits_lat = c(20,60), limits_lon = c(-160, -50))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'cartesian', limits_lat = c(20,60), limits_lon = c(-160, -50))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'robinson', limits_lat = c(20,60), limits_lon = c(-160, -50), reproject = TRUE)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers', limits_lat = c(20,60), limits_lon = c(-160, -50))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers', limits_lat = c(20,60), limits_lon = c(-160, -50), reproject = FALSE)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers', limits_lat = NULL, limits_lon = c(-160, -50))

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers', limits_lat = c(20,60), limits_lon = c(-160, -50), reproject = FALSE)
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers', limits_lat = c(20,60), limits_lon = c(-160, -50), gridlines = TRUE, latlon_ticks = TRUE)

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'mercator', zoom = c('USA', 'CAN', 'MEX'))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'cartesian', zoom = c('USA', 'CAN', 'MEX'))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'robinson', zoom = c('USA', 'CAN', 'MEX'))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers', zoom = c('USA', 'CAN', 'MEX'))

country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'mercator', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'cartesian', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'robinson', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))
country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers', zoom = c('USA', 'CAN', 'MEX'), limits_lat = c(0,60), limits_lon = c(-110, -50))

# test admin1

    library(dplyr)
    data("df_japan_census") # Our Japanese data is at the prefecture level, with names in english lower case.
    admin1_lookup = get_admin1_map() # We match our data to one of the geoids ("adm1_code", "diss_me", or "ne_id" ) in output of get_admin1_map().
    admin1_lookup = admin1_lookup[admin1_lookup$admin == 'Japan', c('adm1_code', 'name_en')] # The "name_en" variable is very close to how the prefectures are named in our data.
    admin1_lookup$name_lower = tolower(admin1_lookup$name_en)
    admin1_lookup$name_lower = iconv(admin1_lookup$name_lower, from = "UTF-8", to = "ASCII//TRANSLIT") # Remove accent marks
    admin1_lookup$name_lower = gsub(pattern = ' prefecture', replacement = '',  x = admin1_lookup$name_lower)
    data_prepped = left_join(df_japan_census, admin1_lookup[, c('adm1_code', 'name_lower')], by = join_by(region == name_lower)) # We merge in admin1_code after making name_en resemble our data.
    admin1_choropleth(data_prepped, geoid.name = 'adm1_code', value.name = 'pop_2010',
                      country_zoom = 'JPN', num_colors = 4) # Create the map
    
# test tract
    df_ny_tract_demographics = choroplethr::df_ny_tract_demographics
    tract_choropleth(df = df_ny_tract_demographics, state_name = 'NY', 
                     geoid.name = 'region', value.name = 'population')
    tract_choropleth(df = df_ny_tract_demographics, state_name = 'NY', 
                     geoid.name = 'region', value.name = 'population',
                     county_zoom = c(36005, 36047, 36061, 36081, 36085))



