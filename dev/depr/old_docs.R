

#' Create a choropleth of US States
#' 
#' The map used is state.map in the package choroplethrMaps.  See state.regions in 
#' the choroplethrMaps package for a data.frame that can help you coerce your regions 
#' into the required format.
#' 
#' @param df A data.frame with a column named "region" and a column named "value".  Elements in 
#' the "region" column must exactly match how regions are named in the "region" column in state.map.
#' @param title An optional title for the map.  
#' @param legend An optional name for the legend.
#' @param num_colors The number of colors to use on the map.  A value of 0 uses 
#' a divergent scale (useful for visualizing negative and positive numbers), A 
#' value of 1 uses a continuous scale (useful for visualizing outliers), and a 
#' value in [2, 9] will use that many quantiles. 
#' @param zoom An optional vector of states to zoom in on. Elements of this vector must exactly 
#' match the names of states as they appear in the "region" column of ?state.regions.
#' @param reference_map If true, render the choropleth over a reference map from Google Maps.
#' @examples
#' \dontrun{
#' # default parameters
#' data(df_pop_state)
#' state_choropleth(df_pop_state, 
#'                  title  = "US 2012 State Population Estimates", 
#'                  legend = "Population")
#'
#' # choropleth over reference map of continental usa
#' data(continental_us_states)
#' state_choropleth(df_pop_state, 
#'                  title         = "US 2012 State Population Estimates",
#'                  legend        = "Population",
#'                  zoom          = continental_us_states, 
#'                  reference_map = TRUE)
#'
#' # continuous scale and zoom
#' data(df_pop_state)
#' state_choropleth(df_pop_state, 
#'                  title      = "US 2012 State Population Estimates", 
#'                  legend     = "Population", 
#'                  num_colors = 1,
#'                  zoom       = c("california", "oregon", "washington"))
#' 
#' # demonstrate user creating their own discretization of the input
#' # demonstrate how choroplethr handles character and factor values
#' data(df_pop_state)
#' df_pop_state$str = ""
#' for (i in 1:nrow(df_pop_state))
#' {
#'   if (df_pop_state[i,"value"] < 1000000)
#'   {
#'     df_pop_state[i,"str"] = "< 1M"
#'   } else {
#'     df_pop_state[i,"str"] = "> 1M"
#'   }
#' }
#' df_pop_state$value = df_pop_state$str
#' state_choropleth(df_pop_state, title = "Which states have less than 1M people?")
#'
#' }