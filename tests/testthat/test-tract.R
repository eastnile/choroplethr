test_that("default parameters returns ggplot", {
  df_ny_tract_demographics = choroplethr::df_ny_tract_demographics
  tract_choropleth(df = df_ny_tract_demographics, state_name = 'NY', 
                             geoid.name = 'region', value.name = 'population')
  tract_choropleth(df = df_ny_tract_demographics, state_name = 'NY', 
                             geoid.name = 'region', value.name = 'population',
                             county_zoom = c(36005, 36047, 36061, 36081, 36085))
})

