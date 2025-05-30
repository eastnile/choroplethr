if (F) {
  # Make the output of cut2 a bit easier to read
  # 
  # Adds commas to numbers, removes unnecessary whitespace and allows an arbitrary separator.
  # 
  # @param x A factor with levels created via Hmisc::cut2.
  # @param nsep A separator which you wish to use.  Defaults to " to ".
  # 
  # @export
  # @examples
  # data(df_pop_state)
  #
  # x = Hmisc::cut2(df_pop_state$value, g=3)
  # levels(x)
  # # [1] "[ 562803, 2851183)" "[2851183, 6353226)" "[6353226,37325068]"
  # levels(x) = sapply(levels(x), format_levels)
  # levels(x)
  # # [1] "[562,803 to 2,851,183)"    "[2,851,183 to 6,353,226)"  "[6,353,226 to 37,325,068]"
  #
  # @seealso \url{http://stackoverflow.com/questions/22416612/how-can-i-get-cut2-to-use-commas/}, which this implementation is based on.
  
  format_levels = function(x, nsep=" to ") 
  {
    n = str_extract_all(x, "[-+]?[0-9]*\\.?[0-9]+")[[1]]  # extract numbers
    v = format(as.numeric(n), big.mark=",", trim=TRUE) # change format
    x = as.character(x)
    
    # preserve starting [ or ( if appropriate
    prefix = ""
    if (substring(x, 1, 1) %in% c("[", "("))
    {
      prefix = substring(x, 1, 1)
    }
    
    # preserve ending ] or ) if appropriate
    suffix = ""
    if (substring(x, nchar(x), nchar(x)) %in% c("]", ")"))
    {
      suffix = substring(x, nchar(x), nchar(x))
    }
    
    # recombine
    paste0(prefix, paste(v,collapse=nsep), suffix)
  }
  
  set_zoom = function(zoom, exclude)
  {
    if (is.null(zoom))
    {
      # initialize the map to the max zoom - i.e. all regions
      private$zoom = unique(self$map.df$region)
    } else {
      # convert zoomed regions to main geoid
      browser()
      lookup = self$ref.regions
      stopifnot(all(zoom %in% unique(lookup[[self$geoid.type]])))
      zoom.geoid.main = lookup[lookup[[self$geoid.type]] %in% zoom, self$geoid.main]
      exclude.geoid.main = lookup[lookup[[self$geoid.type]] %in% exclude, self$geoid.main]
      private$zoom = setdiff(zoom.geoid.main, exclude)
    }
  }
  
  get_zoom = function() { private$zoom }
  
  # support e.g. users just viewing states on the west coast
  clip = function() {
    stopifnot(!is.null(private$zoom))
    
    self$user.df = self$user.df[self$user.df$region %in% private$zoom, ]
    self$map.df  = self$map.df[self$map.df$region %in% private$zoom, ]
  },
  
  
  
  
  bind = function() {
    browser()
    self$choropleth.df = left_join(self$map.df, self$user.df, by="region")
    missing_regions = unique(self$choropleth.df[is.na(self$choropleth.df$value), ]$region)
    if (self$warn && length(missing_regions) > 0)
    {
      missing_regions = paste(missing_regions, collapse = ", ");
      warning_string = paste("The following regions were missing and are being set to NA:", missing_regions);
      warning(warning_string);
    }
    
    # does this work?
    if ("SpatialPolygonsDataFrame" %in% class(self$choropleth.df)) {
      self$choropleth.df = self$choropleth.df[order(self$choropleth.df$order), ];
    }
  },
}

# left
get_min_long = function() 
{
  min(self$choropleth.df$long)
},

# right 
get_max_long = function() 
{
  max(self$choropleth.df$long) 
},

# bottom 
get_min_lat = function() 
{
  min(self$choropleth.df$lat) 
},

# top
get_max_lat = function() 
{
  max(self$choropleth.df$lat) 
},

get_bounding_box = function(long_margin_percent, lat_margin_percent)
{
  stopifnot(!is.null(self$choropleth.df))
  if ('sf' %in% class(self$choropleth.df)) {
    st_bbox(self$choropleth.df)
  } else {
    c(self$get_min_long(), # left
      self$get_min_lat(),  # bottom
      self$get_max_long(), # right
      self$get_max_lat())  # top
  }
},

get_x_scale = function()
{
  scale_x_continuous(limits = c(self$get_min_long(), self$get_max_long()))
},

get_y_scale = function()
{
  scale_y_continuous(limits = c(self$get_min_lat(), self$get_max_lat()))
},

get_reference_map = function()
{
  # note: center is (long, lat) but MaxZoom is (lat, long)
  
  center = c(mean(self$choropleth.df$long), 
             mean(self$choropleth.df$lat))
  
  max_zoom = MaxZoom(range(self$choropleth.df$lat), 
                     range(self$choropleth.df$long))
  
  get_map(location = center,
          zoom     = max_zoom,
          color    = "bw")  
},

get_choropleth_as_polygon = function(alpha)
{
  geom_polygon(data = self$choropleth.df,
               aes(x = long, y = lat, fill = value, group = group), alpha = alpha) 
},

render_with_reference_map = function(alpha = 0.5)
{
  self$prepare_map()
  
  reference_map = self$get_reference_map()
  
  ggmap(reference_map) +  
    self$get_choropleth_as_polygon(alpha) + 
    self$get_scale() +
    self$get_x_scale() +
    self$get_y_scale() +
    self$theme_clean() + 
    ggtitle(self$title) + 
    coord_map()
},

lookup_main_geoid = function(geoid, geoid.type) {
  return(NULL)
},

get_projection = function(choropleth.df = self$choropleth.df, 
                          projection_name = 'cartesian', limits_lat = NULL, limits_lon = NULL) {
  if (projection_name == 'cartesian') {
    return(coord_sf(crs = 4326, ylim = limits_lat, xlim = limits_lon))
  } else if (projection_name == 'mercator') {
    return(coord_sf(crs = 3857, 
                    default_crs = 4326, 
                    ylim = ifelse(is.null(limits_lat), list(c(-80,85)), limits_lat)[[1]], 
                    xlim = limits_lon))
  } else if (projection_name == 'robinson') {
    return(coord_sf(crs = '+proj=robin', default_crs = 4326, ylim = limits_lat, xlim = limits_lon))
  } else if (projection_name == 'albers') {
    stopifnot('sf' %in% class(choropleth.df))
    bbox = sf::st_bbox(sf_obj)
    lat_1 = bbox["ymin"] + 0.25 * (bbox["ymax"] - bbox["ymin"])
    lat_2 = bbox["ymin"] + 0.75 * (bbox["ymax"] - bbox["ymin"])
    lat_0 = (bbox["ymin"] + bbox["ymax"]) / 2
    lon_0 = (bbox["xmin"] + bbox["xmax"]) / 2
    # Construct PROJ string
    proj_str = sprintf("+proj=aea +lat_1=%.6f +lat_2=%.6f +lat_0=%.6f +lon_0=%.6f",
                       lat_1, lat_2, lat_0, lon_0)
    return(coord_sf(crs = proj_str))
  }
},

prepare_map = function(include = NULL, exclude = NULL, nlvls = NULL) 
{
  browser()
  if (is.null(nlvls) & self$user_value_factor_or_string) {
    warning('num_colors will be ignored since the plotted value is a factor.')
  }
  # Check include/exclude
  
  if (!all(c(include, exclude) %in% self$ref.regions[, self$geoid.type])) {
    stop('The zoom and exclude regions must be in the list of allowed regions (', self$ref.regions.name, ') and must match the geoid.type of the data (', self$geoid.type, ').' )
  }
  
  #include = map_to_geoid_main(x = include, geoid.type = self$geoid.type, ref.regions = self$ref.regions, geoid.main = self$geoid.main)
  #exclude = map_to_geoid_main(x = exclude, geoid.type = self$geoid.type, ref.regions = self$ref.regions, geoid.main = self$geoid.main)
  
  # --- bind
  #geoid_common = intersect(names(self$map.df), names(self$user.df.prepped))
  names_by = intersect(names(self$map.df), names(self$user.df.prepped))
  choropleth.df = left_join(self$map.df, self$user.df.prepped, by = names_by)
  
  missing_regions = unique(choropleth.df[[self$geoid.type]][is.na(choropleth.df[[self$value.name]])])
  if (self$warn && length(missing_regions) > 0)
  {
    missing_regions = paste(missing_regions, collapse = ", ");
    warning_string = paste("The following regions were missing and are being set to NA:", missing_regions);
    warning(warning_string);
  }
  
  # --- clip
  if (!is.null(include)) {
    choropleth.df = choropleth.df[choropleth.df[[self$geoid.type]] %in% include, ]
  }
  choropleth.df =  choropleth.df[!choropleth.df[[self$geoid.type]] %in% exclude, ]
  rownames(choropleth.df) = NULL
  
  # --- discretize if needed
  #browser()
  # if (!is.null(nlvls)) {
  #   # print('jomama')
  #   choropleth.df[[self$value.name]] = discretize(x = choropleth.df[[self$value.name]], nlvls = nlvls)
  #   self$value_was_discretized = T
  # }
  self$choropleth.df = choropleth.df
}


map_to_geoid_main = function(x, geoid.type, ref.regions, geoid.main) {
  if (F) {
    ref.regions = self$ref.regions
    geoid.main = self$geoid.main
    x = c(user.df.prepped$region, 'jomama')
  }
  idx = match(x, ref.regions[, geoid.type])
  stopifnot(all(!is.na(idx)))
  ref.regions[idx, geoid.main]
}
bbox_user = c(limits_lon_user[1], limits_lat_user[1], limits_lon_user[2], limits_lat_user[2])
bbox_combined = ifelse(is.na(bbox_user), bbox, bbox_user)
bbox_final = st_bbox(c(xmin = bbox_combined[1], ymin = bbox_combined[2], 
                       xmax = bbox_combined[3], ymax = bbox_combined[4]), crs = st_crs(choropleth.df))
lat_1 = bbox_final["ymin"] + 0.25 * (bbox_final["ymax"] - bbox_final["ymin"])
lat_2 = bbox_final["ymin"] + 0.75 * (bbox_final["ymax"] - bbox_final["ymin"])
lat_0 = (bbox_final["ymin"] + bbox_final["ymax"]) / 2
lon_0 = (bbox_final["xmin"] + bbox_final["xmax"]) / 2
# -- Apply projections
if (projection == 'cartesian') {
  projection = coord_sf(crs = 4326, ylim = limits_lat, xlim = limits_lon)
} else if (projection == 'mercator') {
  if (bbox['ymin'] <= -90 & is.null(limits_lon)) {
    limits_lat = c(-75, bbox['ymax'])
  }
  projection = coord_sf(crs = 3857, 
                        default_crs = 4326, 
                        ylim = limits_lat, 
                        xlim = limits_lon)
} else if (projection == 'robinson') {
  proj_str = sprintf("+proj=robin +lon_0=%.6f", lon_0)
  projection = coord_sf(crs = proj_str)
  # projection = coord_sf(crs = proj_str, default_crs = 4326, ylim = limits_lat, xlim = limits_lon)
} else if (projection == 'albers') {
  # Construct PROJ string
  proj_str = sprintf("+proj=aea +lat_1=%.6f +lat_2=%.6f +lat_0=%.6f +lon_0=%.6f",
                     lat_1, lat_2, lat_0, lon_0)
  projection = coord_sf(crs = proj_str)
}

country_choropleth = function(df, geoid.name = 'region', geoid.type = 'auto', value.name = 'value',
                              num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', nbreaks = 5, custom.colors = NULL,
                              zoom = NULL, continent_zoom = NULL, 
                              projection = 'cartesian', limits_lat = NULL, limits_lon = NULL, reproject = TRUE,
                              border_color = 'grey15', border_thickness = 0.2,
                              background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE,
                              label = NULL, label_text_size = 3, label_text_color = 'black', ggrepel_options = NULL,
                              label_box_color = 'white', label_box_size = 0.25, 
                              label_box_padding = 0.25, label_point_padding = 0.25,
                              label_repel_force = 1, label_max_overlaps = 10, label_min_segment = 0.5, label_nudge_x = 0, label_nudge_y = 0,
                              legend = NULL, legend_position = 'right', title = NULL, return = 'plot')
  
  # gglabel = ggrepel::geom_label_repel(data = label_points,
  #                                     aes(label = .data[[label]], geometry = geometry), 
  #                                     stat = "sf_coordinates",
  #                                     size = label_text_size,
  #                                     color = label_text_color,
  #                                     fill = label_box_color,
  #                                     label.padding = label_box_size,
  #                                     box.padding = label_padding,
  #                                     point.padding = point_padding,
  #                                     force = label_repel_force,
  #                                     max.overlaps = label_max_overlaps,
  #                                     min.segment.length = label_min_segment,
  #                                     nudge_x = label_nudge_x,
  #                                     nudge_y = label_nudge_y)
  
  
  if (setequal(remainder, 'HI')) { # HI only
    final = c$render(choropleth.df =  c$choropleth.df[c$choropleth.df$state.abb == 'HI', ], 
                     ggscale = ggscale, projection = projection, reproject = TRUE,
                     limits_lon = c( -160.5, -154.5), limits_lat = c(18.5, 22.5),
                     border_color = border_color, border_thickness = border_thickness,
                     background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                     label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                     ggrepel_options = ggrepel_options,
                     legend = legend, legend_position = legend_position, title = title)
  } else if (setequal(c$choropleth.df$state.abb, 'AK')) { # AK only
    final = c$render(choropleth.df =  c$choropleth.df[c$choropleth.df$state.abb == 'AK', ], 
                     ggscale = ggscale, projection = projection, reproject = TRUE,
                     limits_lon = c(-181, -127), limits_lat = c(51, 72), 
                     border_color = border_color, border_thickness = border_thickness,
                     background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                     label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                     ggrepel_options = ggrepel_options,
                     legend = legend, legend_position = legend_position, title = title)
  } else if (setequal(c$choropleth.df$state.abb, c('AK', 'HI'))) { # HI and AK only
    ak_map = c$render(choropleth.df =  c$choropleth.df[c$choropleth.df$state.abb %in% c('HI', 'AK'), ], 
                      ggscale = ggscale, projection = projection, reproject = TRUE, occlude_latlon_limits = FALSE,
                      limits_lon = c(-181, -127), limits_lat = c(51, 72), 
                      border_color = border_color, border_thickness = border_thickness,
                      background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                      label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                      ggrepel_options = ggrepel_options,
                      legend = NULL, title = NULL) 
    hi_map = c$render(choropleth.df =  c$choropleth.df[c$choropleth.df$state.abb %in% c('HI', 'AK'), ], 
                      ggscale = ggscale, projection = projection, reproject = TRUE, occlude_latlon_limits = FALSE,
                      limits_lon = c( -160.5, -154.5), limits_lat = c(18.5, 22.5),
                      border_color = border_color, border_thickness = border_thickness,
                      background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                      label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                      ggrepel_options = ggrepel_options,
                      legend = NULL, title = NULL) 
    final =  ak_map + hi_map + plot_layout(guides = 'collect') 
  } else {
    if ('HI' %in% remainder) {
      hi_map = c$render(choropleth.df =  c$choropleth.df[c$choropleth.df$state.abb == 'HI', ], 
                        ggscale = ggscale, projection = projection, reproject = TRUE,
                        limits_lon = c( -160.5, -154.5), limits_lat = c(18.5, 22.5),
                        border_color = border_color, border_thickness = border_thickness,
                        background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                        label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                        ggrepel_options = ggrepel_options,
                        legend = NULL, title = NULL) + theme(legend.position = "none", plot.margin = ggplot2::margin(0, 0, 0, 0)) 
      hi_grob = ggplotGrob(hi_map)
    } else {
      hi_grob = grid::nullGrob()
    }
    if ('AK' %in% remainder) {
      ak_map = c$render(choropleth.df =  c$choropleth.df[c$choropleth.df$state.abb == 'AK', ], 
                        ggscale = ggscale, projection = projection, reproject = TRUE,
                        limits_lon = c(-181, -127), limits_lat = c(51, 72), 
                        border_color = border_color, border_thickness = border_thickness,
                        background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                        label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                        ggrepel_options = ggrepel_options,
                        legend = NULL, title = NULL) + theme(legend.position = "none", plot.margin = ggplot2::margin(0, 0, 0, 0)) 
      ak_grob = ggplotGrob(ak_map)
    } else {
      ak_grob = grid::nullGrob()
    }
    
    browser()
    bbox = st_bbox(c$choropleth.df[c$choropleth.df$state.abb %in% contus, ])
    xrange = bbox['xmax'] - bbox['xmin']
    limits_lon_pretty = c(bbox['xmin']-xrange*0.02, bbox['xmax']+xrange*0.02)
    
    if (projection == 'albers') {
      limits_lat = c(24, 53)
    } else {
      limits_lat = c(24, 51)
    }
    
    contus_map = c$render(choropleth.df = c$choropleth.df,
                          ggscale = ggscale, projection = projection, reproject = TRUE, occlude_latlon_limits = FALSE,
                          limits_lat = limits_lat,
                          limits_lon = limits_lon_pretty,
                          border_color = border_color, border_thickness = border_thickness,
                          background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                          label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                          ggrepel_options = ggrepel_options,
                          legend = legend, legend_position = legend_position, title = title) + theme(legend.position = "none", plot.margin = ggplot2::margin(0, 0, 0, 0)) 
    
    if (projection == 'albers') {
      final = contus_map + annotation_custom(grob = ak_grob, xmin = -123, xmax = -108, ymin = 21.5, ymax = 31) +
        annotation_custom(grob = hi_grob, xmin = -110.5, xmax = -105.5, ymin = 22.75, ymax = 30)
    } else {
      final = contus_map + annotation_custom(grob = ak_grob, xmin = -124, xmax = -114, ymin = 24, ymax = 32) +
        annotation_custom(grob = hi_grob, xmin = -113.5, xmax = -105.4, ymin = 24, ymax = 30)
    }
  }

ggdraw() +
  draw_plot(contus_map, x= 0, y = 0, width = 1, height = 1) +
  draw_plot(ak_map, x = 0, y = 0, width = .5, height = .5)