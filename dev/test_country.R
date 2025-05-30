# test country
devtools::load_all() 


# Test country
data('df_world')

# Baseline

country_choropleth(df = df_world, geoid.name = 'iso_a3', 
                   value.name = 'population', projection = 'albers', 
                   reproject = T)
