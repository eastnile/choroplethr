get_state_sf() = function() {
  if (F) {
    year = 2015
  }
  # The tigris state map files have changed over time, including the format
  # and names of key identifiers. The most recent regime starts in 2014. 
  # For the purposes of this function, use the most recent data by not
  # specifying any year.
  require(tigris)
  require(dplyr)
  require(sf)
  sf = states(cb = TRUE)
  stopifnot('sf' %in% class(sf))
  stopifnot(c('STATEFP', 'GEOID', 'STUSPS', 'NAME', 'geometry') %in% names(sf))
  sf = filter(sf, !STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))
}

check_tigris_state_avail = function(years) {
  avail = rep(1L,length(years))
  for (i in seq_along(years)) {
    cat(years[i], ' ')
    res = try(states(cb = TRUE, year = years[i]))
    if ('try-error' %in% class(res)) {
      avail[i] = 0
    }
  }
  avail_df = data.frame(year = years, avail = avail)
  return(avail_df)
}

tigris_state_avail = check_tigris_state_avail(years = 1700:2025)


