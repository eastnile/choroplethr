# library(choroplethrMaps)
# library(tidycensus)
devtools::load_all()

data("df_state_demographics")
names(df_state_demographics)[2] = 'value'

st_zoom = df_state_demographics$region[1:10]
st_zoom = 'alaska'

state_choropleth(df = df_state_demographics, zoom = st_zoom)
z = StateChoropleth$new(df_state_demographics) 
z$set_zoom(st_zoom)
z2 = z$render()
