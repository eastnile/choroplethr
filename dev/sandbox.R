library(choroplethrMaps)
library(tidycensus)
devtools::load_all()

get_acs_data(variable = 'B19013_001', column_idx = NULL, 
                    map = 'county', endyear = 2012, span = 5) 

state_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, zoom=c("new york", "new jersey", "connecticut"))
county_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, state_zoom=c("new york", "new jersey", "connecticut"))
