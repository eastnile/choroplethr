devtools::load_all() 
if (F) {
  tools::resaveRdaFiles("data/", compress = "xz")
  devtools::load_all() 
  devtools::document()
  devtools::check(cran = T)
}
tract_choropleth(df = df_ny_tract_demographics, state_name = 'NY', 
                 geoid.name = 'region', value.name = 'population')
