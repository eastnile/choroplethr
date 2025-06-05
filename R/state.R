#' Create a choropleth map using U.S. state level data
#' 
#' To see the list of allowed state names, see choroplethr::state.regions.
#' 
#' @inheritParams common_args
#' @param df A dataframe containing U.S. state level data
#' @param geoid.name The variable that identifies each state
#' @param geoid.type How the variable given by geoid.name specifies each state (full name, abbreviation, etc). The allowed geoid.type are given in choroplethr::state.regions. If "auto", the function will try to automatically determine geoid.type.
#' @param zoom An optional vector of states to zoom in on, written in the same manner as geoid.name.

#' @examples
#' \donttest{
#' # Plot continuous state level data:
#' data(df_state_demographics)
#' state_choropleth(df = df_state_demographics,
#'                  geoid.name = 'region',
#'                  geoid.type = 'name.lower',
#'                  value.name = 'population',
#'                  title  = "U.S. State Population",
#'                  legend = "Population")
#'
#' # Plot categorical data with custom colors:
#' data("df_president")
#' state_choropleth(df = df_president,
#'                  geoid.name = 'region',
#'                  geoid.type = 'name.lower',
#'                  value.name = 'value',
#'                  title  = "2012 US Presidential Election Results",
#'                  legend = "Candidate",
#'                  custom.colors = c('blue4', 'red3'),
#'                  border_color = 'lightgrey')
#'
#' # Label states and pass additional arguments to ggrepel
#' state_choropleth(df = df_president,
#'                  geoid.name = 'region',
#'                  geoid.type = 'name.lower',
#'                  value.name = 'value',
#'                  title  = "2012 US Presidential Election Results",
#'                  legend = "Candidate",
#'                  custom.colors = c('blue4', 'red3'),
#'                  border_color = 'lightgrey',
#'                  label = 'state.abb',
#'                  label_text_size = 4,
#'                  ggrepel_options = list(label.r = 0, force = 0.02))
#' }


#' @export
state_choropleth = function(df, style = 'geographic_bigdc', geoid.name = 'region', geoid.type = 'auto', value.name = 'value', 
         num_colors = 7, color.max = '#084594', color.min = '#eff3ff', na.color = 'grey', custom.colors = NULL, nbreaks = 5,
         zoom = NULL, projection = 'albers', 
         border_color = 'grey15', border_thickness = 0.2,
         background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE, whitespace = TRUE,
         label = NULL, label_text_size = 2.25, label_text_color = 'black', label_box_color = 'white', 
         ggrepel_options = list(force = .01, 
                                box.padding = 0.15,
                                label.padding = 0.15, 
                                max.overlaps = Inf),
         legend = NULL, legend_position = 'bottom', title = NULL, return = 'plot') {

  if (style == 'geographic_bigdc') {
    map.df = choroplethr::state.map.bigdc
  } else if (style == 'geographic') {
    map.df = choroplethr::state.map
  }

  c = Choropleth$new(ref.regions = choroplethr::state.regions, 
                     ref.regions.name = 'choroplethr::state.regions',
                     map.df = map.df, 
                     geoid.all = c('name.proper', 'name.lower', 'state.abb', 'fips.character', 'fips.numeric'),
                     user.df = df, geoid.name = geoid.name, geoid.type = geoid.type, 
                     value.name = value.name, num_colors = num_colors, label_col = label)
  
  c$set_zoom(zoom)
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = nbreaks)
  ggproj = c$get_projection(projection = projection, limits_lat = NULL, limits_lon = NULL,
                            reproject = FALSE, ignore_latlon = TRUE, whitespace = whitespace)
  if (return == 'sf') {
    return(c$choropleth.df)
  } 
  plot = c$render(ggscale = ggscale, projection = ggproj, 
                  occlude_latlon_limits = FALSE,
                  border_color = border_color, border_thickness = border_thickness,
                  background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                  label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                  ggrepel_options = ggrepel_options,
                  legend = legend, legend_position = legend_position, title = title, addl_gglayer = NULL)
  return(plot)
}
