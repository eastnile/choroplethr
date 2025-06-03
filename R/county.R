#' Create a county-level choropleth
#' @export
#' @importFrom dplyr left_join
#' @include usa.R
CountyChoropleth = R6Class("CountyChoropleth",
                           inherit = Choropleth,
                           public = list(
                             map.df = readRDS('dev/sf_county.rds'),
                             map.df.name = 'county.map',
                             ref.regions = readRDS('dev/regions_county.rds'),
                             ref.regions.name = 'county.regions',
                             geoid.all =  c('fips.numeric', 'fips.character', 'name.proper')
                           )
)


#' Create a choropleth of US Counties
#' 
#' The map used is county.map in the choroplethrMaps package.  See country.regions
#' in the choroplethrMaps package for an object which can help you coerce your regions
#' into the required format.
#' 
#' @param df A data.frame with a column named "region" and a column named "value".  Elements in 
#' the "region" column must exactly match how regions are named in the "region" column in county.map.
#' @param title An optional title for the map.  
#' @param legend An optional name for the legend.  
#' @param num_colors The number of colors to use on the map.  A value of 0 uses 
#' a divergent scale (useful for visualizing negative and positive numbers), A 
#' value of 1 uses a continuous scale (useful for visualizing outliers), and a 
#' value in [2, 9] will use that many quantiles. 
#' @param state_zoom An optional vector of states to zoom in on. Elements of this vector must exactly 
#' match the names of states as they appear in the "region" column of ?state.regions.
#' @param county_zoom An optional vector of counties to zoom in on. Elements of this vector must exactly 
#' match the names of counties as they appear in the "region" column of ?county.regions.
#' @param reference_map If true, render the choropleth over a reference map from Google Maps.
#' 
#' @examples
#' \dontrun{
#' # default parameters
#' data(df_pop_county)
#' county_choropleth(df_pop_county, 
#'                   title  = "US 2012 County Population Estimates", 
#'                   legend = "Population")
#'                   
#' # zoom in on california and add a reference map
#' county_choropleth(df_pop_county, 
#'                   title         = "California County Population Estimates", 
#'                   legend        = "Population",
#'                   state_zoom    = "california",
#'                   reference_map = TRUE)
#'
#' # continuous scale 
#' data(df_pop_county)
#' county_choropleth(df_pop_county, 
#'                  title      = "US 2012 County Population Estimates", 
#'                  legend     = "Population", 
#'                  num_colors = 1, 
#'                  state_zoom = c("california", "oregon", "washington"))
#'
#' library(dplyr)
#' library(choroplethrMaps)
#' data(county.regions)
#'
#' # show the population of the 5 counties (boroughs) that make up New York City
#' nyc_county_names = c("kings", "bronx", "new york", "queens", "richmond")
#' nyc_county_fips = county.regions %>%
#'   filter(state.name == "new york" & county.name %in% nyc_county_names) %>%
#'   select(region)
#' county_choropleth(df_pop_county, 
#'                   title        = "Population of Counties in New York City",
#'                   legend       = "Population",
#'                   num_colors   = 1,
#'                   county_zoom = nyc_county_fips$region)
#' }
#' @export
#' @importFrom Hmisc cut2
#' @importFrom stringr str_extract_all
#' @importFrom ggplot2 ggplot aes geom_polygon scale_fill_brewer ggtitle theme theme_grey element_blank geom_text
#' @importFrom ggplot2 scale_fill_continuous scale_colour_brewer
#' @importFrom grid unit
county_choropleth = function(df, geoid.name = 'region', geoid.type = 'auto', value.name = 'value', 
                             num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', custom.colors = NULL,
                             zoom = NULL, projection = 'albers', 
                             border_color = 'grey15', border_thickness = 0.2,
                             background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE,
                             label = NULL, label_text_size = 2.25, label_text_color = 'black', label_box_color = 'white', 
                             legend = NULL, legend_position = 'bottom', title = NULL, return = 'plot',
                             add_state_outline = TRUE)
{

  c = CountyChoropleth$new(user.df = df, geoid.name = geoid.name, 
                          geoid.type = geoid.type, value.name = value.name, num_colors = num_colors)
  c$set_zoom(zoom)
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = num_colors)
  
  ggproj = c$get_projection(projection = projection, reproject = TRUE, ignore_latlon = TRUE)
  
  if (return == 'sf') {
    return(c$choropleth.df)
  }
  plot = c$render(ggscale = ggscale, projection = ggproj, 
                  border_color = border_color, border_thickness = border_thickness,
                  background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                  label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                  ggrepel_options = ggrepel_options,
                  legend = legend, legend_position = legend_position, title = title)

  if (add_state_outline) {
    states_used = unique(c$choropleth.df$state.fips.numeric[c$choropleth.df$render])
    state_map = readRDS('dev/sf_state.rds')
    state_map = state_map[state_map$fips.numeric %in% states_used, ]
    state_outline = geom_sf(data = state_map, color = 'black', fill = NA, linewidth = .6)
    plot = plot + state_outline + ggproj
  }
  
  return(plot)
}
