#' An R6 object for creating country-level choropleths.
#' @export
#' @importFrom dplyr left_join
#' @importFrom R6 R6Class
#' @include choropleth.R
CountryChoropleth = R6Class("CountryChoropleth",
  inherit = Choropleth,
  public = list(
    map.df = readRDS('dev/sf_country.rds'),
    map.df.name = 'country.map',
    ref.regions = readRDS('dev/regions_country.rds'),
    ref.regions.name = 'country.regions',
    geoid.all = c('name.proper', 'name.lower', 'iso_a3', 'iso_a2'))
  )

#' Create a country-level choropleth
#' 
#' The map used is country.map in the choroplethrMaps package. See country.regions for
#' an object which can help you coerce your regions into the required format.
#' 
#' @param df A data.frame with a column named "region" and a column named "value".  Elements in 
#' the "region" column must exactly match how regions are named in the "region" column in ?country.map.
#' @param title An optional title for the map.  
#' @param legend An optional name for the legend.  
#' @param colors A vector specifying the colors to be used if the plotted value is a character 
#' or factor. These colors can be specified using R's color terms as given in ?colors, or as a
#' hex value like that returned from rgb(). The number of colors must match the number of unique
#' values in the variable to be plotted.
#' 
#'  
#' @param num_colors When plotting numeric data, the number of colors to use on the map.  
#' A value of 0 uses a divergent scale (useful for visualizing negative and positive numbers), a 
#' value of 1 uses a continuous scale (useful for visualizing outliers), and a value greater than
#' one will divide the data into that many quantiles and shade each a different color.
#' 
#' @param zoom An optional vector of countries to zoom in on. Elements of this vector must exactly 
#' match the names of countries as they appear in the "region" column of ?country.regions
#' @examples
#' # demonstrate default options
#' data(df_pop_country)
#' country_choropleth(df_pop_country, "2012 World Bank Populate Estimates")
#'
#' # demonstrate continuous scale
#' country_choropleth(df_pop_country, "2012 World Bank Populate Estimates", num_colors=1)
#'
#' # demonstrate zooming
#' country_choropleth(df_pop_country, 
#'                    "2012 World Bank Population Estimates", 
#'                    num_colors=1,
#'                    zoom=c("united states of america", "canada", "mexico"))

#' @export
#' @importFrom Hmisc cut2
#' @importFrom stringr str_extract_all
#' @importFrom ggplot2 ggplot aes geom_polygon scale_fill_brewer ggtitle theme theme_grey element_blank geom_text
#' @importFrom ggplot2 scale_fill_continuous scale_colour_brewer ggplotGrob annotation_custom 
#' @importFrom grid unit grobTree

country_choropleth = function(df, geoid.name = 'region', geoid.type = 'auto', value.name = 'value',
                              num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', nbreaks = 5, custom.colors = NULL,
                              zoom = NULL, continent_zoom = NULL, 
                              projection = 'cartesian', limits_lat = NULL, limits_lon = NULL, reproject = TRUE,
                              border_color = 'grey15', border_thickness = 0.2,
                              background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE,
                              label = NULL, label_text_size = 3, label_text_color = 'black', label_box_color = 'white', ggrepel_options = NULL,
                              legend = NULL, legend_position = 'right', title = NULL, return = 'plot')
                              
  
{
  #browser()
  c = CountryChoropleth$new(user.df = df, geoid.name = geoid.name, geoid.type = geoid.type, value.name = value.name, 
                            num_colors = num_colors)
  
  browser()
  zoom_cont = NULL
  if (!is.null(continent_zoom)) {
    stopifnot(length(continent_zoom) == 1)
    stopifnot(all(continent_zoom %in% unique(c$ref.regions$continent)))
    zoom_cont = c$ref.regions[c$ref.regions$continent %in% continent_zoom, c$geoid.type]
    if (is.null(limits_lat)) {
      limits_lat = continent_latlon[[continent_zoom]]$limits_lat
    }
    if (is.null(limits_lon)) {
      limits_lon = continent_latlon[[continent_zoom]]$limits_lon
    }
    c$set_zoom(intersect(zoom, zoom_cont))
  } else {
    c$set_zoom(zoom)
  }
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = nbreaks)
  
  ggproj = c$get_projection(projection_name = projection, limits_lat = limits_lat, limits_lon = limits_lon, reproject = reproject)
  
  if (projection %in% c('albers', 'robinson')) {
    occlude = TRUE 
  } else {
    occlude = FALSE
  }
  
  if (return == 'plot') {
    browser()
    c$render(ggscale = ggscale, projection = ggproj, occlude_latlon_limits = occlude,
             border_color = border_color, border_thickness = border_thickness,
             background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks,
             label = label, label_text_size = label_text_size, label_text_color = label_text_color, 
             label_box_color = label_box_color, ggrepel_options = ggrepel_options,
             legend = legend, legend_position = legend_position, title = title)
  } else if (return == 'sf') {
    return(c$choropleth.df)
  } else {
    stop("return must be 'plot' or 'sf'.")
  }
}

continent_latlon = list()
continent_latlon[['Europe']] = list(limits_lat= c(34,72), limits_lon = c(-17,45))
continent_latlon[['Asia']] = list(limits_lat= c(-10,58), limits_lon = c(32,155))
continent_latlon[['Africa']] = list(limits_lat= c(-37,39), limits_lon = c(-19,53))
continent_latlon[['Oceania']] = list(limits_lat= c(-48, -1), limits_lon = c(112,182))
continent_latlon[['North America']] = list(limits_lat= c(6, 79), limits_lon = c(-170,-48))
continent_latlon[['South America']] = list(limits_lat= c(-58, 14), limits_lon = c(-83,-32))

# continents = c('Africa', 'Asia', 'Europe', 'North America', 'Oceania', 'South America')
# ymin = c()


