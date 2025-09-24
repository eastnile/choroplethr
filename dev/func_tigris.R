library(tigris)

tigris_state_sf = function() {
  if (F) {
    year = 2015
  }
  require(tigris)
  require(dplyr)
  require(sf)
  sf = states(cb = TRUE)
  stopifnot('sf' %in% class(sf))
  stopifnot(c('STATEFP', 'GEOID', 'STUSPS', 'NAME', 'geometry') %in% names(sf))
  sf = filter(sf, !STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))
}

check_tigris_state_avail = function(years) {
  info = list()
  for (i in seq_along(years)) {
    cat(years[i], ' ')
    res = try(states(cb = TRUE, year = years[i]))
    if ('try-error' %in% class(res)) {
      info[[i]] = data.frame(year = years[i], avail = 0, df_class = NA, ncol = NA, nrow = NA,
                  colnames = NA)
    } else {
      info[[i]] = data.frame(year = years[i], avail = 1, df_class = paste0(class(res), collapse = ';'), 
                             ncol = ncol(res), nrow = nrow(res),
                             colnames = paste0(names(res), collapse = ';'))
    }
  }
  avail_df = do.call(rbind, info)
  return(avail_df)
}

tigris_county_sf = function() {
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
  sf = counties(cb = TRUE)
  stopifnot('sf' %in% class(sf))
  stopifnot(c('STATEFP', 'GEOID', 'STUSPS', 'NAME', 'geometry') %in% names(sf))
  sf = filter(sf, !STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))
}


check_tigris_county_avail = function(years) {
  info = list()
  for (i in seq_along(years)) {
    cat(years[i], ' ')
    res = try(counties(cb = TRUE, year = years[i]))
    if ('try-error' %in% class(res)) {
      info[[i]] = data.frame(year = years[i], avail = 0, df_class = NA, ncol = NA, nrow = NA,
                             colnames = NA)
    } else {
      info[[i]] = data.frame(year = years[i], avail = 1, df_class = paste0(class(res), collapse = ';'), 
                             ncol = ncol(res), nrow = nrow(res),
                             colnames = paste0(names(res), collapse = ';'))
    }
  }
  avail_df = do.call(rbind, info)
  return(avail_df)
}

check_tigris_tract_avail = function(years) {
  info = list()
  for (i in seq_along(years)) {
    cat(years[i], ' ')
    res = try(tracts(cb = TRUE, state = 'NY', year = years[i]))
    if ('try-error' %in% class(res)) {
      info[[i]] = data.frame(year = years[i], avail = 0, df_class = NA, ncol = NA, nrow = NA,
                             colnames = NA, uniqueids = NA)
    } else {
      unique_cols = character()
      for (col in names(res)) {
        if (sum(duplicated(res[[col]])) == 0) {
          unique_cols = c(col, unique_cols)
        }
      }
      info[[i]] = data.frame(year = years[i], avail = 1, df_class = paste0(class(res), collapse = ';'), 
                             ncol = ncol(res), nrow = nrow(res),
                             colnames = paste0(names(res), collapse = ';'), uniqueids = paste0(unique_cols, collapse = ';'))
    }
  }
  avail_df = do.call(rbind, info)
  return(avail_df)
}



if (F) {
  chk_state = check_tigris_state_avail(1700:2025)
  saveRDS(chk_state, 'dev/tigris_state_avail.rds')
  # The tigris state map files have changed over time, including the format
  # and names of key identifiers. The most recent regime starts in 2014. 
  # For the purposes of this function, use the most recent data by not
  # specifying any year.
  
  chk_county = check_tigris_county_avail(1700:2025)
  saveRDS(chk_county, 'dev/tigris_county_avail.rds')
  # Only avail for 2022, 23, and 24. 
  
  chk_tract = check_tigris_tract_avail(2000:2025)
  saveRDS(chk_tract, 'dev/tigris_tract_avail.rds')
  # Only avail for 2019 to 2025, 3 naming regimes

}




