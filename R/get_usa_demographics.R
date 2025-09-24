#' Get a handful of demographic variables on US States from the US Census Bureau
#' as a data.frame.
#'
#' The data comes from the American Community Survey (ACS). The variables are
#' total population and median household income.
#' @param endyear The end year for the survey
#' @param span The span of the survey
#' @importFrom utils data
#' @export
get_state_demographics = function(endyear=2013, span=5)
{  
  span_lookup = c('1' = 'acs1', '3' = 'acs3', '5' = 'acs5')
  dataset = span_lookup[as.character(span)]
  acs_df = tidycensus::get_acs(geography = 'state', 
                               variable = c('B01003_001', 'B19013_001'), 
                               year = endyear, dataset = dataset, output = 'wide')
  acs_df = acs_df[, c(2, 3, 5)]
  names(acs_df) = c('region', 'population', 'median_hh_income')
  acs_df$region = tolower(acs_df$region)
  acs_df = acs_df[acs_df$region %in% choroplethr::state.regions$region, ]
  return(acs_df)
}


#' Get a handful of demographic variables on US Counties from the US Census
#' Bureau as a data.frame.
#'
#' The data comes from the American Community Survey (ACS). The variables are
#' total population and median household income.
#' @param endyear The end year for the survey
#' @param span The span of the survey
#' @export
get_county_demographics = function(endyear=2013, span=5)
{  
  span_lookup = c('1' = 'acs1', '3' = 'acs3', '5' = 'acs5')
  dataset = span_lookup[as.character(span)]
  acs_df = tidycensus::get_acs(geography = 'county', 
                               variable = c('B01003_001', 'B19013_001'), 
                               year = endyear, dataset = dataset, output = 'wide')
  acs_df = acs_df[, c(1, 3, 5)]
  names(acs_df) = c('region', 'population', 'median_hh_income')
  acs_df$region = as.numeric(acs_df$region)
  return(acs_df)
}

#' Get a handful of demographic variables on Census Tracts in a State from the
#' US Census Bureau as a data.frame.
#'
#' The data comes from the American Community Survey (ACS). The variables are
#' total population and median household income.
#' @param state_name The name of the state. See ?state.regions for proper
#'   spelling and capitalization.
#' @param county_fips An optional vector of county fips codes within the state.
#'   Useful to set because getting data on all tracts can be slow.
#' @param endyear The end year for the survey
#' @param span The span of the survey
#' @importFrom stringr str_sub
#' @export
get_tract_demographics = function(state_name, county_fips = NULL, endyear=2013, span=5)
{ 
  # tidycensus::get_acs requires just the *county* portion of the FIPS code
  # (i.e. the last 3 characters) 
  if (!is.null(county_fips)) {
    county_fips = str_sub(county_fips, -3)
  }
  span_lookup = c('1' = 'acs1', '3' = 'acs3', '5' = 'acs5')
  dataset = span_lookup[as.character(span)]
  acs_df = tidycensus::get_acs(geography = 'tract', state = state_name, county=county_fips,
                               variable = c('B01003_001', 'B19013_001'), 
                               year = endyear, dataset = dataset, output = 'wide')
  acs_df = acs_df[, c(1, 3, 5)]
  names(acs_df) = c('region', 'population', 'median_hh_income')
  acs_df = acs_df[order(acs_df$region), ]
  acs_df$region = as.numeric(acs_df$region)
  return(acs_df)
}
