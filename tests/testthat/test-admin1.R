test_that("default parameters returns ggplot", {
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
})


