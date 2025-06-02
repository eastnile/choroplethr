#' Create a state-level choropleth
#' @export
#' @importFrom dplyr left_join
#' @include usa.R
StateChoropleth = R6Class("StateChoropleth",
  inherit = Choropleth,
  public = list(
    map.df = readRDS('dev/sf_state_bigdc.rds'),
    map.df.name = 'state.map',
    ref.regions = readRDS('dev/regions_state.rds'),
    ref.regions.name = 'state.regions',
    geoid.all = c('name.proper', 'name.lower', 'state.abb', 'fips.character', 'fips.numeric')
  )
)


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
#' @export
#' @importFrom Hmisc cut2
#' @importFrom stringr str_extract_all
#' @importFrom ggplot2 ggplot aes geom_polygon scale_fill_brewer ggtitle theme theme_grey element_blank geom_text
#' @importFrom ggplot2 scale_fill_continuous scale_colour_brewer


state_choropleth = function(df, geoid.name = 'region', geoid.type = 'auto', value.name = 'value', 
         num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', custom.colors = NULL,
         zoom = NULL, projection = 'albers', 
         border_color = 'grey15', border_thickness = 0.2,
         background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE,
         label = NULL, label_text_size = 2.25, label_text_color = 'black', label_box_color = 'white', 
         ggrepel_options = list(force = .01, 
                                box.padding = 0.15,
                                label.padding = 0.15, 
                                max.overlaps = Inf),
         legend = NULL, legend_position = 'bottom', title = NULL, return = 'plot') {
  
  c = StateChoropleth$new(user.df = df, geoid.name = geoid.name, 
                          geoid.type = geoid.type, value.name = value.name, num_colors = num_colors)
  c$set_zoom(zoom)
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = num_colors)
  if (return == 'sf') {
    return(c$choropleth.df)
  }
  plot = c$render(ggscale = ggscale, projection = projection, reproject = FALSE, ignore_latlon = TRUE,
                   border_color = border_color, border_thickness = border_thickness,
                   background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                   label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                   ggrepel_options = ggrepel_options,
                   legend = legend, legend_position = legend_position, title = title)
  return(plot)
}
