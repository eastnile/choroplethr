library(choroplethrMaps)
library(acs)
library(tidycensus)
devtools::load_all()


data(df_pop_ny_tract)


tract_choropleth(df_pop_ny_tract, state_name = 'new york')

county_choropleth_acs("B19301")

api.key.install('f8b2a6df01479981aef39577b3c4466f5a4c8274')

df = get_acs_data("B01003", "state")[[1]]

B19013_001

data_acs = acs.fetch(geography=make_geo('state'), table.number = 'B19013', col.names = "pretty", endyear = 2012, span = 5)

tester2 = convert_acs_obj_to_df(map = 'state', acs.data =  data_acs, column_idx = 1, include_moe = T)


tester <- get_acs(geography = "state", table = "C02003", year = 2012, cache_table = T, output = 'wide')

convert_acs_obj_to_df_2 = function(map, acs.data, column_idx, include_moe) {
  if (map == 'state') {
    
  }
}

class(tester)
