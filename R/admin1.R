#' Download a map of first level administrative regions from
#' naturalearthdata.com
#'
#' Uses the rnaturalearth package.
#' @param cache Cache the map and use cached map if available.
#' @param drop_geometry Drop geometry data?
#' @returns An sf dataframe uniquely identified by the variables "adm1_code",
#'   "diss_me", and "ne_id".
#' @importFrom sf st_transform st_drop_geometry
#' @importFrom rnaturalearth ne_states
#' @export
#' 
get_admin1_map = function(cache = TRUE, drop_geometry = TRUE) {
  
  if (!requireNamespace("rnaturalearthhires")) {
    stop("Package 'rnaturalearthhires' is required for this function. This package is not on CRAN owing to its large file size (40MB+). Please install it from GitHub using remotes::install_github('ropensci/rnaturalearthhires')")
  }
  if (!exists("sf_admin1", envir = .cache_env.choroplethr) | cache == FALSE) {
    message("Downloading spatial data using rnaturalearth.")
    sf_admin1 = rnaturalearth::ne_states(returnclass = "sf")
    sf_admin1 = st_transform(sf_admin1, 4326)
    vars_needed = names(sf_admin1)[-grep('FCLASS', names(sf_admin1))]
    sf_admin1 = sf_admin1[, vars_needed]
    assign("sf_admin1", sf_admin1, envir = .cache_env.choroplethr)
  } else {
    message("Using cached data.")
    sf_admin1 = get("sf_admin1", envir = .cache_env.choroplethr)
  }
  if (drop_geometry) {
    sf_admin1 = as.data.frame(sf::st_drop_geometry(sf_admin1))
  }
  return(sf_admin1)
}

#' Create a choropleth map using regional data at the sub-country level
#'
#' This function can be used to plot regional data at the first sub-level of
#' administration (ie., state, province, prefecture, etc.) for one or more
#' countries. Use get_admin1_map() for an object which can help you coerce your
#' region names into the required format; see below for an example with Japanese
#' data.
#'
#' Note: This function requires the package rnatrualearthhires, which is not
#' available on CRAN due to the filesize of the map being large. You can install
#' it using: 
#' remotes::install_github("ropensci/rnaturalearthhires")
#'
#' @inheritParams common_args
#' @param df A dataframe containing regional data at the sub-country level for
#'   one or more countries.
#' @param geoid.name The variable that identifies each administrative region
#' @param geoid.type How the variable given by geoid.name specifies each
#'   country. The allowed geoid.type are given by the columns "adm1_code",
#'   "diss_me", "ne_id" in the output of get_admin1_map(); use this output to
#'   match the names of your regions to the correct geoid. If "auto", the
#'   function will try to automatically determine geoid.type.
#' @param zoom An optional vector of regions to zoom in on, written in the same
#'   manner as geoid.name.
#' @param country_zoom An optional vector of countries to zoom in on, written as
#'   they appear in the "adm0_a3" column of the object returned from
#'   get_tract_map().
#' @param reproject If TRUE, the map will be cropped and centered prior to
#'   applying the projection. This will generally result in a better figure when
#'   using the Robinson and Albers, but may lead to countries near the edge of
#'   the map being occluded.
#' @examples
#' \donttest{
#' library(dplyr)
#' # Our Japanese data is at the prefecture level, with names in English lower
#' # case. We match our data to one of the geoids ("adm1_code", "diss_me", or
#' # "ne_id" ) # in the output of get_admin1_map().
#' 
#' if (requireNamespace("rnaturalearthhires")) {
#'   admin1_lookup = get_admin1_map()
#'   # The "name_en" variable is very close to how the prefectures are named in
#'   # our data.
#'   admin1_lookup = admin1_lookup[admin1_lookup$admin == 'Japan', 
#'                                 c('adm1_code', 'name_en')]
#'   admin1_lookup$name_lower = tolower(admin1_lookup$name_en)
#'   # Remove accent marks
#'   admin1_lookup$name_lower = iconv(admin1_lookup$name_lower,
#'                                   from = "UTF-8", to = "ASCII//TRANSLIT") 
#'   admin1_lookup$name_lower = gsub(pattern = ' prefecture', replacement = '',
#'                                  x = admin1_lookup$name_lower)
#'   # We merge in admin1_code after making name_en resemble our data.
#'   data_prepped = left_join(df_japan_census, 
#'                            admin1_lookup[, c('adm1_code', 'name_lower')],
#'                            by = join_by(region == name_lower))
#'   admin1_choropleth(data_prepped, geoid.name = 'adm1_code', 
#'                     value.name = 'pop_2010',
#'                     country_zoom = 'JPN', num_colors = 4) # Create the map
#' }

#' }
#' @export
admin1_choropleth = function(df, geoid.name = 'region', geoid.type = 'auto', value.name = 'value',
                               num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', custom.colors = NULL, nbreaks = 5,
                               zoom = NULL, country_zoom = NULL, 
                               projection = 'cartesian', limits_lat = NULL, limits_lon = NULL, reproject = TRUE, whitespace = TRUE,
                               border_color = 'grey15', border_thickness = 0.2,
                               background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE,
                               label = NULL, label_text_size = 3, label_text_color = 'black', label_box_color = 'white', ggrepel_options = NULL,
                               legend = NULL, legend_position = 'right', title = NULL, return = 'plot') {
  
  admin1_map = get_admin1_map(drop_geometry = FALSE) 
  c = Choropleth$new(ref.regions = sf::st_drop_geometry(admin1_map), 
                     ref.regions.name =  'output of get_admin1_regions()',
                     map.df = admin1_map[, c('adm1_code', 'geometry')], 
                     geoid.all = c('adm1_code', 'diss_me', 'ne_id'),
                     user.df = df, geoid.name = geoid.name, geoid.type = geoid.type, 
                     value.name = value.name, num_colors = num_colors, label_col = label)
  
  if (!is.null(country_zoom)) {
    if (!all(country_zoom %in% admin1_map$adm0_a3)) {
      stop('Country_zoom must match the names of countries as they appear in the "adm0_a3" column of the object returned from "get_tract_map".')
    }
    regions_in_country = admin1_map[admin1_map$adm0_a3 %in% country_zoom, ][[c$geoid.type]]
    if(!is.null(zoom)) {
      c$set_zoom(intersect(zoom, regions_in_country))
    } else {
      c$set_zoom(regions_in_country)
    }
  } else {
    c$set_zoom(zoom)
  }
  
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = nbreaks)
  
  ggproj = c$get_projection(projection_name = projection, limits_lat = limits_lat, limits_lon = limits_lon, 
                            ignore_latlon = FALSE, reproject = reproject, whitespace = whitespace)
  
  if (projection %in% c('albers', 'robinson')) {
    occlude = TRUE 
  } else {
    occlude = FALSE
  }
  
  if (return == 'sf') {
    return(c$choropleth.df)
  }
  
  plot = c$render(ggscale = ggscale, projection = ggproj, occlude_latlon_limits = occlude,
                  border_color = border_color, border_thickness = border_thickness,
                  background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                  label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                  ggrepel_options = ggrepel_options,
                  legend = legend, legend_position = legend_position, title = title, addl_gglayer = NULL)
  return(plot)
}
