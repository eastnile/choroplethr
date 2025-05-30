# test country
devtools::load_all() 
load('data/df_state_demographics.rda')

state_choropleth(df_state_demographics, value.name = 'population', projection = 'cartesian')
state_choropleth(df_state_demographics, value.name = 'population', projection = 'mercator')
state_choropleth(df_state_demographics, value.name = 'population', projection = 'robinson')
state_choropleth(df_state_demographics, value.name = 'population', projection = 'albers')

state_choropleth(df_state_demographics[df_state_demographics$region %in% c('california', 'washington', 'oregon', 'alaska'), ], 
                 value.name = 'population')

allstates = df_state_demographics$region
contus = setdiff(allstates, c('alaska', 'hawaii'))

state_choropleth(df_state_demographics, 
                 zoom = contus,
                 value.name = 'population', projection = 'albers')

state_choropleth(df_state_demographics, 
                 zoom = c('alaska', 'hawaii', 'california', 'oregon'),
                 value.name = 'population', projection = 'albers')

state_choropleth(df_state_demographics, 
                 value.name = 'population', projection = 'albers')

state_choropleth(df_state_demographics, 
                 zoom = 'alaska',
                 value.name = 'population')

state_choropleth(df_state_demographics, 
                 zoom = c('alaska', 'hawaii'),
                 value.name = 'population')

state_choropleth(df_state_demographics, 
                 zoom = c('alaska', 'hawaii', 'california', 'washington', 'oregon', 'texas'),
                 value.name = 'population')

st_zoom = df_state_demographics$region[1:10]
st_zoom = 'alaska'