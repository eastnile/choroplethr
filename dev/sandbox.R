library(choroplethrMaps)
library(tidycensus)
devtools::load_all()

z = get_state_demographics(2019,5)
z = get_county_demographics(2019,5)
z = get_tract_demographics(state_name = 'AL', endyear = 2019, span = 5)

get_acs_data(variable = 'B19013_001', column_idx = NULL, 
                    map = 'county', endyear = 2012, span = 5) 

state_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, zoom=c("new york", "new jersey", "connecticut"))
county_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, state_zoom=c("new york", "new jersey", "connecticut"))
