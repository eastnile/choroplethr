if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("zip.regions"))
}

#' Create a US State choropleth from ACS data
#' 
#' Creates a choropleth of US counties using the US Census' American Community Survey (ACS) data.  
#' Requires the tidycensus package to be installed.
#' @param variable The variable you wish to plot. A list of available census 
#' variables can be obtained using tidycensus::load_variables()
#' @param tableId Alternatively, you may specify the ACS table you wish to plot. 
#' If the table has more than one variable inside it, you must also specify the 
#' variable you wish to plot.
#' @param endyear The end year of the survey to use.
#' @param span Either 1, 3, or 5, the ACS vintage you wish to use.
#' @param title A title for the plot; if not specified, a title will assigned based on the variable.
#' @param census_api_key Optional. Census API keys can be obtained at: https://api.census.gov/data/key_signup.html
#' @param ... Other arguments passed to state_choropleth; see ?state_choropleth()
#' @return A choropleth.

#' @export
#' @examples
#' \dontrun{
#' # Create a state choropleth for median household income*zooming in on New York, New Jersey and Connecticut
#'state_choropleth_acs(variable = "B19013_001", endyear = 2011, num_colors=1, 
#'zoom=c("new york", "new jersey", "connecticut"))
#'}
#'
#' @importFrom tidycensus load_variables get_acs
state_choropleth_acs = function(variable, tableId = NULL, endyear=NULL, span=5, title = NULL, 
                                census_api_key = NULL,
                                ...)
{
  acs_out = process_acs_request(variable = variable, tableId = tableId, 
                                geography = 'state', year = endyear,
                                span = span, census_api_key = census_api_key)
  
  if (is.null(title)) {
    title = acs_out$map_title
  } 
  
  # Subset for our map
  df = acs_out$acs_data[, c('NAME', 'estimate')]
  names(df) = c('region', 'value')
  df$region = tolower(df$region)
  df$value = as.numeric(df$value)
  df = df[df$region != 'puerto rico', ]
  state_choropleth(df = df, title = title, ...)
}

#' Create a US County choropleth from ACS data
#' 
#' Creates a choropleth of US States using the US Census' American Community Survey (ACS) data.  
#' Requires the tidycensus package to be installed.
#' @param variable The variable you wish to plot. A list of available census 
#' variables can be obtained using tidycensus::load_variables()
#' @param tableId Alternatively, you may specify the ACS table you wish to plot. 
#' If the table has more than one variable inside it, you must also specify the 
#' variable you wish to plot.
#' @param endyear The end year of the survey to use.
#' @param span Either 1, 3, or 5, the ACS vintage you wish to use.
#' @param title A title for the plot; if not specified, a title will assigned based on the variable.
#' @param census_api_key Optional. Census API keys can be obtained at: https://api.census.gov/data/key_signup.html
#' @param ... Other arguments passed to county_choropleth; see ?county_choropleth()
#' @return A choropleth.

#' @export
#' @examples
#' \dontrun{
#' #  Median household income, zooming in on all counties in New York, New Jersey and Connecticut
#' county_choropleth_acs(variable = "B19013_001", num_colors=1, 
#' state_zoom=c("new york", "new jersey", "connecticut"))
#' }
#' 

#' @importFrom tidycensus load_variables get_acs
county_choropleth_acs = function(variable = NULL, tableId = NULL, endyear=NULL, span=5, 
                                 title = NULL, census_api_key = NULL, ...)
{
  acs_out = process_acs_request(variable = variable, tableId = tableId,
                                geography = 'county', year = endyear,
                                span = span, census_api_key = census_api_key)
  
  if (is.null(title)) {
    title = acs_out$map_title
  } 
  
  # Subset for our map
  df = acs_out$acs_data
  df$GEOID = as.numeric(df$GEOID)
  df = df[, c('GEOID', 'estimate')]
  names(df) = c('region', 'value')
  df$value = as.numeric(df$value)
  county_choropleth(df = df, title = title, ...)
}


process_acs_request = function(variable, tableId , geography, year, span, census_api_key) {
  if (is.null(year)) {
    stop('Must specify endyear')
  }
  if (is.null(tableId) & is.null(variable)) {
    stop('Must specify either tableId or variable')
  }
  if (length(tableId) > 1| length(variable) > 1) {
    stop('Only a single tableId or variable can be requested at one time.')
  }
  
  span_lookup = c('1' = 'acs1', '3' = 'acs3', '5' = 'acs5')
  dataset = span_lookup[as.character(span)]
  allvars = tidycensus::load_variables(year = year, dataset = dataset, cache = T)
  
  # Case 1: User specifies variable but not tableId
  if (is.null(tableId) & !is.null(variable)) { 
    if(!variable %in% allvars$name) {
      stop(paste0('The requested variable was not found in the ', dataset, ' dataset.'))
    } 
    acs_df = as.data.frame(tidycensus::get_acs(geography = geography, variable = variable, year = year, cache_table = F))
  }
  
  # Case 2: User specifies tableId
  if (!is.null(tableId)) { 
    acs_df = as.data.frame(tidycensus::get_acs(geography = geography, table = tableId, year = year, cache_table = F))
    table_varnames = unique(acs_df$variable)
    # If user also specifies a variable, check that the variable is actually inside the table.
    if (!is.null(variable)) { 
      if (!variable %in% table_varnames) {
        stop(paste0('The requested variable name was not found in the requested acs_table. The requested table contains the following variables: ', paste0(table_varnames, collapse = ', ')))
      }
    } else { # If no variable is specified, insist that that table only has a single variable.
      if (length(table_varnames) > 1) {
        stop(paste0('The requested table contains more than one variable; please specify the desired variable. The variables available in the table are: ', paste0(table_varnames, collapse = ', ')))
      }
      variable = table_varnames
    }
  }
  stopifnot(c('GEOID', 'NAME', 'variable', 'estimate') %in% names(acs_df))
  acs_df = acs_df[acs_df$variable == variable,]
  var_label = allvars[allvars$name == variable, 'label'][[1]]
  var_label = gsub('!!', ' ', var_label)
  var_concept = allvars[allvars$name == variable, 'concept'][[1]]
  map_title = paste(var_concept, var_label)
  return(list(acs_data = acs_df, map_title = map_title))
}



#' Returns a list representing American Community Survey (ACS) estimates
#' THIS FUNCTION IS DEPRECIATED; USE TIDYCENSUS TO QUERY ACS DATA INSTEAD
#'
#' Given a map, ACS tableId, endyear and span. Prompts user for the column id if there 
#' are multiple tables. The first element of the list is a data.frame with estimates. 
#' The second element is the ACS title of the column.
#' Requires the acs package to be installed, and a Census API Key to be set with the 
#' acs's api.key.install function.  Census API keys can be obtained at http://api.census.gov/data/key_signup.html.
#'
#' @param tableId The id of an ACS table
#' @param map The map you want to use. Must be one of "state", "county" or "zip".
#' @param endyear The end year of the survey to use.  See acs.fetch (?acs.fetch) and http://1.usa.gov/1geFSSj for details.
#' @param span The span of time to use.  See acs.fetch and http://1.usa.gov/1geFSSj for details.
#' on the same longitude and latitude map to scale. This variable is only checked when the "states" variable is equal to all 50 states.
#' @param column_idx The optional column id of the table to use. If not specified and the table has multiple columns,
#' you will be prompted for a column id.
#' @param include_moe Whether to include the 90 percent margin of error. 
#' @export
#' @seealso http://factfinder2.census.gov/faces/help/jsf/pages/metadata.xhtml?lang=en&type=survey&id=survey.en.ACS_ACS, which lists all ACS Surveys.
#' @examples
#' \dontrun{
#' library(Hmisc) # for cut2
#' # States with greater than 1M residents
#' df       = get_acs_data("B01003", "state")[[1]] # population
#' df$value = cut2(df$value, cuts=c(0,1000000,Inf))
#' state_choropleth(df, title="States with a population over 1M", legend="Population")
#'
#' # Counties with greater than or greater than 1M residents
#' df       = get_acs_data("B01003", "county")[[1]] # population
#' df$value = cut2(df$value, cuts=c(0,1000000,Inf))
#' county_choropleth(df, title="Counties with a population over 1M", legend="Population")
#' }
get_acs_data = function(tableId, map, endyear=2012, span=5, column_idx=-1, include_moe=FALSE)
{
  warning('This function is depreciated; please use tidycensus to query ACS data instead.')
  acs.data   = acs.fetch(geography=make_geo(map), table.number = tableId, col.names = "pretty", endyear = endyear, span = span)
  if (column_idx == -1) {
    column_idx = get_column_idx(acs.data, tableId) # some tables have multiple columns 
  }
  title      = acs.data@acs.colnames[column_idx] 
  df         = convert_acs_obj_to_df(map, acs.data, column_idx, include_moe) # choroplethr requires a df
  list(df=df, title=title) # need to return 2 values here
}

# DEPRECIATED 
get_tract_acs_data = function(tracts, tableId, endyear=2012, span=5, column_idx=-1, include_moe=FALSE)
{
  warning('This function is depreciated; please use tidycensus to query ACS data instead.')
  acs.data = acs.fetch(geography    = tracts, 
                       table.number = tableId,
                       col.names    = "pretty", 
                       endyear      = endyear, 
                       span         = span)
  
  if (column_idx == -1) {
    column_idx = get_column_idx(acs.data, tableId) # some tables have multiple columns 
  }
  title      = acs.data@acs.colnames[column_idx] 
  df         = convert_acs_obj_to_df("tract", acs.data, column_idx, include_moe) # choroplethr requires a df
  list(df=df, title=title) # need to return 2 values here
}

# DEPRECIATED 
# support multiple column tables
#' @importFrom utils menu
get_column_idx = function(acs.data, tableId)
{
  column_idx = 1
  if (length(acs.data@acs.colnames) > 1)
  {
    num_cols   = length(acs.data@acs.colnames)
    title      = paste0("Table ", tableId, " has ", num_cols, " columns.  Please choose which column to render:")
    column_idx = menu(acs.data@acs.colnames, title=title)
  }
  column_idx
}

# DEPRECIATED
make_geo = function(map)
{
  stopifnot(map %in% c("state", "county", "zip"))
  if (map == "state") {
    geo.make(state = "*")
  } else if (map == "county") {
    geo.make(state = "*", county = "*")
  } else {
    geo.make(zip.code = "*")
  }
}

# DEPRECIATED
# the acs package returns data as a custom S4 object. But we need the data as a data.frame.
# this is tricky for a few reasons. one of which is that acs.data is an S4 object.
# another is that each map (state, county and zip) has a different naming convention for regions
# another is that the census data needs to be clipped to the map (e.g. remove puerto rico)
convert_acs_obj_to_df = function(map, acs.data, column_idx, include_moe) 
{
  warning('This function is depreciated; please use tidycensus to query ACS data instead.')
  stopifnot(map %in% c("state", "county", "zip", "tract"))
  
  if (map == "state") {
    # create a data.frame of (region, value) pairs
    region = tolower(geography(acs.data)$NAME) 
    region = as.character(region)
    value  = as.numeric(estimate(acs.data[, column_idx]))
    df     = data.frame(region = region, value = value)
    
    if (include_moe)
    {
      df$margin.of.error = 1.645 * as.numeric(standard.error(acs.data[, column_idx])) 
    }
    
    # subset for our map
    df[df$region != "puerto rico", ]

  } else if (map == "county") {
    # create a data.frame of (region, value) pairs
    # create fips code
    region = paste(as.character(acs.data@geography$state), 
                   acs.data@geography$county, 
                   sep = "")
    region = as.numeric(region)
    value  = as.numeric(estimate(acs.data[, column_idx]))
    df     = data.frame(region = region, value = value)
    
    if (include_moe)
    {
      df$margin.of.error = 1.645 * as.numeric(standard.error(acs.data[, column_idx])) 
    }
    
    # remove state fips code 72, which is Puerto Rico, which we don't map
    df[df$region < 72000 | df$region > 72999, ]  
    
  } else if (map == "zip") {
    # create a data.frame of (region, value) pairs
    region = geography(acs.data)$zipcodetabulationarea
    region = as.character(region)
    value  = as.numeric(estimate(acs.data[, column_idx]))
    df     = data.frame(region = region, value = value)
    
    if (include_moe)
    {
      df$margin.of.error = 1.645 * as.numeric(standard.error(acs.data[, column_idx])) 
    }

    # clipping is done in the choroplethrZip package, because that's where the region definitions are
    df
  } else if (map == "tract") {
    df = data.frame(state  = geography(acs.data)$state,   # integer
                    county = geography(acs.data)$county,  # integer
                    tract  = geography(acs.data)$tract,   # character
                    value  = as.numeric(estimate(acs.data[,column_idx])))
    
    if (include_moe)
    {
      df$margin.of.error = 1.645 * as.numeric(standard.error(acs.data[, column_idx])) 
    }    
    
    # county fips code must be 5 chars
    # 2 chars for the state (i.e. leading "0")
    df$state = as.character(df$state)
    df$state = paste0("0", df$state)
    # 3 chars for the county - i.e. leading "0" or leading "00"
    df$county = as.character(df$county)
    for (i in 1:nrow(df))
    {
      if (nchar(df[i, "county"]) == 1) {
        df[i, "county"] = paste0("00", df[i, "county"])
      } else if (nchar(df[i, "county"]) == 2) {
        df[i, "county"] = paste0("0", df[i, "county"])
      }
    } 
    
    # now concat with the tract id
    df$region = paste0(df$state, df$county, df$tract)
    
    # only include relevant columns
    if (include_moe)
    {
      df[, c("region", "value", "margin.of.error")] # only return (region, value) pairs
    } else {
      df[, c("region", "value")]
    }
  }
  
}

