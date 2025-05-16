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
    geoid.all = c('name.proper', 'name.lower', 'iso_a3', 'iso_a2'),
    geoid.main = 'iso_a3',
    mercator.default.limits = c(-80,85)

    # initialize with a world map
    # initialize = function(user.df, geoid.name = geoid.name, 
    #                       geoid.type = geoid.type, value.name = value.name)
    # {
    #   if (!requireNamespace("choroplethrMaps", quietly = TRUE)) {
    #     stop("Package choroplethrMaps is needed for this function to work. Please install it.", call. = FALSE)
    #   }
    #   data(country.map, package="choroplethrMaps", envir=environment())
    #   browser()
    #   super$initialize(map.df = self$map.df, 
    #                    ref.regions = self$ref.regions,
    #                    user.df = user.df,
    #                    geoid.name = geoid.name, geoid.type = geoid.type, value.name = value.name)
    #   
    #   
    #   if (private$has_invalid_regions)
    #   {
    #     warning("Please see ?country.regions for a list of mappable regions")
    #   }
    # }
    
  )
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

country_choropleth = function(df, geoid.name = 'region', geoid.type = NULL, value.name = 'value',
                              title="", legend="", 
                              colors = NULL, num_colors=7, 
                              color_max = '#084594', color_min = '#eff3ff', na.color = '',
                              continent_zoom = NULL,
                              zoom = NULL,
                              exclude = NULL,
                              limits_lat = NULL,
                              limits_long = NULL,
                              projection = 'cartesian',
                              return = 'ggplot')
{
  browser()
  c = CountryChoropleth$new(user.df = df, geoid.name = geoid.name, 
                            geoid.type = geoid.type, value.name = value.name)
  
  browser()
  c$title  = title
  c$legend = legend
  #c$set_num_colors(num_colors)
  
  
  
  if (!is.null(continent_zoom)) {
    #stopifnot(length(continent_zoom) == 1)
    stopifnot(all(continent_zoom %in% unique(c$ref.regions$continent)))
    zoom = c$ref.regions[c$ref.regions$continent %in% continent_zoom, c$geoid.type]
  }
  # 
  # if(!is.null(zoom)) {
  #   c$set_zoom(zoom = zoom, exclude = exclude)
  # }
  # 
  browser()
  c$prepare_map(include = zoom, exclude = exclude, nlvls = 5)
  # projection_list = list(cartesian = coord_sf(crs = 4326, ylim = limits_lat, xlim = limits_long),
  #                        mercator = coord_sf(crs = 3857, 
  #                                            ylim=ifelse(is.null(limits_lat), list(c(-80,85)), limits_lat)[[1]], 
  #                                            xlim = limits_long,
  #                                            default_crs = 4326),
  #                        robinson = coord_sf(crs = '+proj=robin', default_crs = 4326, ylim = limits_lat, xlim = limits_long),
  #                        test = c$get_albers_proj(c$choropleth.df, default_crs = 4326, ylim = limits_lat, xlim = limits_long))
  # c$projection_sf = projection_list[[projection]]

  browser()
  if (return == 'plot') {
    return(c$render(projection = coord_sf(crs = 4326, ylim = limits_lat, xlim = limits_long), 
                    user.colors = NULL,
                    color_max = color_max,
                    color_min = color_min))
  } else if (return == 'sf') {
    c$prepare_map()
    return(c$choropleth.df)
  } else if (return == 'R6obj') {
    return(c) 
  } else {
    stop("return must be 'plot' or 'sf'.")
  }
}