# test state
devtools::load_all() 
load('data/df_state_demographics.rda')

# Test projections
state_choropleth(df_state_demographics, value.name = 'population', projection = 'cartesian')
state_choropleth(df_state_demographics, value.name = 'population', projection = 'mercator')
state_choropleth(df_state_demographics, value.name = 'population', projection = 'robinson')
state_choropleth(df_state_demographics, value.name = 'population', projection = 'albers')

# Test zoom for AK/HI

state_choropleth(df_state_demographics[df_state_demographics$region %in% c('california', 'washington', 'oregon', 'alaska'), ], 
                 value.name = 'population')

allstates = df_state_demographics$region
contus = setdiff(allstates, c('alaska', 'hawaii'))

state_choropleth(df_state_demographics, 
                 zoom =  c('alaska', 'hawaii'),
                 value.name = 'population', projection = 'albers')

state_choropleth(df_state_demographics, 
                 zoom = contus,
                 value.name = 'population', projection = 'albers')

state_choropleth(df_state_demographics, 
                 zoom = c('alaska', 'hawaii', 'california', 'oregon'),
                 value.name = 'population', projection = 'albers')

# Test labels
state_choropleth(df_state_demographics, value.name = 'population', label = 'state.abb', 
                 label_text_size = 2.25,
                 ggrepel_options = list(min.segment.length = .5, force = .01, 
                                        box.padding = 0.15,
                                        label.padding = 0.15, 
                                        max.overlaps = Inf))
# County

# test country
if(F) {
  devtools::document()
}
devtools::load_all() 
data(df_county_demographics)

df_county_demographics

cnty_ref = readRDS('dev/regions_county.rds')
my_counties = cnty_ref$fips.numeric[1:200]
ak_hi_counties = cnty_ref[cnty_ref$state.abb %in% c('AK', 'HI'), "fips.numeric"]

# Test projections
county_choropleth(df_county_demographics, value.name = 'total_population', projection = 'albers', zoom = my_counties)
county_choropleth(df_county_demographics, value.name = 'total_population', projection = 'albers', zoom = my_counties, add_state_outline = F)
county_choropleth(df_county_demographics, value.name = 'total_population', projection = 'albers', zoom = ak_hi_counties)

# Tract
# test country
if(F) {
  devtools::document()
}
devtools::load_all() 
data("df_ny_tract_demographics")
my_counties = c(36015, 36055)
my_tracts = c(36015010800, 36015000100)
tract_choropleth(df_ny_tract_demographics, state_name = 'new york', value.name = 'total_population', projection = 'albers', county_zoom = my_counties)
tract_choropleth(df_ny_tract_demographics, state_name = 'new york', value.name = 'total_population', projection = 'albers', tract_zoom = my_tracts, county_zoom = my_counties)
tract_choropleth(df_ny_tract_demographics, state_name = 'new york', value.name = 'total_population', projection = 'albers', tract_zoom = my_tracts, county_zoom = 36055)
tract_choropleth(df_ny_tract_demographics, state_name = 'new york', value.name = 'total_population', county_zoom = 36055)

