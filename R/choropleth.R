#' The base Choropleth object.
#' @importFrom R6 R6Class
#' @importFrom ggplot2 element_text element_rect element_blank labs scale_fill_gradient scale_fill_gradient2 guides guide_colorbar guide_legend scale_fill_manual scale_fill_brewer
#' @import stringr
#' @import sf
#' @export
Choropleth = R6Class("Choropleth", 
  public = list(
    user.df = NULL,  # input from user
    ref.regions = NULL,
    ref.regions.name = NULL, 
    map.df = NULL, # geometry of the map
    map.df.name = NULL,
    geoid.all = NULL,
    geoid.name = NULL,
    geoid.type = NULL,
    value.name = NULL,
    choropleth.df  = NULL, # result of binding user data with our map data
    num_colors = NULL,
    user_value_factor_or_string = NULL,
    value_was_discretized = FALSE,
    # warn           = TRUE,  # warn user on clipped or missing values                

    initialize = function(user.df, geoid.name, geoid.type, value.name, num_colors)
    {
      stopifnot(length(geoid.name) == 1)
      stopifnot(class(geoid.name) == 'character')
      stopifnot(length(value.name) == 1)
      stopifnot(class(value.name) == 'character')
      stopifnot(is_whole_number(num_colors) & num_colors >= 0)
      if(!geoid.type %in% c(self$geoid.all, 'auto') | is.null(geoid.type)) {
        stop(paste0('The allowed geoid.type are: ', paste0(self$geoid.all, collapse = ', '), '; or auto to guess the type.'))
      }
      
      if (is.factor(user.df[[value.name]])) {
        self$user_value_factor_or_string = TRUE
      } else if (is.character(user.df[[value.name]])) {
        self$user_value_factor_or_string = TRUE
        user.df[[value.name]] = as.factor(user.df[[value.name]])
        warning(paste0('The variable to be plotted is a character and will be converted to factor with ', 
                      length(levels(user.df[[value.name]])), ' levels before plotting.'))
      } else if (is.numeric(user.df[[value.name]])) {
        self$user_value_factor_or_string = FALSE
      } else {
        stop('The variable to be plotted must be a numeric, factor, or character.')
      }
      
      if (anyDuplicated(user.df[, geoid.name]) != 0) {
        stop(paste0("The variable '", geoid.name, "' must uniquely identify observations in the data to be plotted."))
      }

      # Prep user input data ----
      user.df.prepped = user.df[, c(geoid.name, value.name)]
      if (identical(geoid.type, 'auto')) { # Establish geoid if it's not specified
        geoid.type = self$guess_geoid_type(user.regions = user.df.prepped[[geoid.name]])
        message(paste0("geoid_type = 'auto'; the geoid ", geoid.name, " was determined to be of type: ", 
                     geoid.type, ". To see the list of allowed geoids, see ", self$ref.regions.name, "."))
      } 
      unmatched = user.df.prepped[[geoid.name]][!user.df.prepped[[geoid.name]] %in% self$ref.regions[[geoid.type]]]
      if (length(unmatched) > 0) {
        warning(paste0('The following regions were not found in ', self$ref.regions.name, ' and cannot be plotted: ', 
                       paste0(unmatched, collapse = ', ')))
        user.df.prepped = user.df.prepped[!user.df.prepped[[geoid.name]] %in% unmatched, ]
      }
      names(user.df.prepped) = c(geoid.type, value.name)
      user.df.prepped = dplyr::left_join(self$ref.regions, user.df.prepped, by = geoid.type)
      
      # Discretize value if need be ----
      if (num_colors > 1) {
        if (self$user_value_factor_or_string) {
          warning('num_colors will be ignored since the plotted value is a factor.')
        } else {
          user.df.prepped[[value.name]] = discretize(x = user.df.prepped[[value.name]], nlvls = num_colors)
          if (length(levels(user.df.prepped[[value.name]] )) != num_colors) {
            warning('After discretization, the number of categories did not match num_colors. This may be due to the data having fewer unique values than num_colors.')
          }
          self$value_was_discretized = TRUE
        }
      }
      # Bind geometries
      choropleth.df = left_join(self$map.df, user.df.prepped, by = intersect(names(self$map.df), names(user.df.prepped)))
      rownames(choropleth.df) = NULL
      choropleth.df$render = TRUE
      stopifnot('sf' %in% class(choropleth.df))
      
      # Set attributes
      self$user.df = user.df
      self$geoid.name = geoid.name
      self$geoid.type = geoid.type
      self$value.name = value.name
      self$choropleth.df = choropleth.df
      self$num_colors = num_colors
    },
    
    guess_geoid_type = function(user.regions) {
      stopifnot(is.vector(user.regions))
      unmatched = list()
      n_unmatched = numeric()
      for (name in self$geoid.all) {
        unmatched[[name]] = user.regions[!user.regions %in% self$ref.regions[[name]]]
        n_unmatched[name] = length(unmatched[[name]])
      }
      geoid.type.guess = names(n_unmatched)[which.min(n_unmatched)] # if there's a tie it pick the first one
      return(geoid.type.guess)
    },
    
    set_zoom = function(zoom) {
      if (is.null(zoom)) {
        return(invisible(NULL)) 
      }
      if (!all(zoom %in% self$ref.regions[, self$geoid.type])) {
        stop('The regions in zoom must be in the list of available regions (', self$ref.regions.name, ') and must match the geoid.type of the data (', self$geoid.type, ').' )
      }
      self$choropleth.df$render[!self$choropleth.df[[self$geoid.type]] %in% zoom] = FALSE
    },

    #' @importFrom ggplot2 scale_fill_gradient2
    get_ggscale = function(choropleth.df = self$choropleth.df, respect_zoom = TRUE,
                           custom.colors, color.min, color.max, na.color, nbreaks) {
      stopifnot(is_valid_color(c(color.min, color.max, na.color)))
      if (!is.null(custom.colors)) {
        stopifnot(is_valid_color(custom.colors))
      }
      
      if (respect_zoom) {
        choropleth.df = choropleth.df[choropleth.df$render == TRUE,]
      }
  
      # browser()
      # I. data is numeric
      if (class(choropleth.df[[self$value.name]]) != 'factor') { 
        if (!is.null(custom.colors)) {
          warning('user.colors ignored when the plotted variable is continuous') 
        }
        var_range = range(choropleth.df[[self$value.name]], na.rm = TRUE)
        mid = (var_range[2]-var_range[1])/2
        breaks = pretty(choropleth.df[[self$value.name]], n = nbreaks)
        if (self$num_colors == 1) {
          if (is.null(color.max)) {
            color.max = '#084594'
          }
          if (is.null(color.min)) {
            color.min = '#eff3ff'
          }
          ggscale = scale_fill_gradient(na.value = na.color, low = color.min, high = color.max, breaks = breaks, labels = scales::label_comma(),
                                                 guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"))
        } else {
          if (is.null(color.max)) {
            color.max = 'gold'
          }
          if (is.null(color.min)) {
            color.min = 'purple4'
          }
          ggscale = scale_fill_gradient2(na.value = na.color, low = color.min, high = color.max, mid = 'white', 
                                                  midpoint = mid, breaks = breaks, labels = scales::label_comma(),
                                                  guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"))
        }
      # II. data is a factor
      } else { 
        nlevels = length(levels(choropleth.df[[self$value.name]]))
        if (!is.null(custom.colors)) { # IIa. user colors override other options
          n.user.colors = length(custom.colors)
          if (n.user.colors != nlevels) {
            stop(paste0(n.user.colors, ' color(s) were specified in custom.colors but the variable to be plotted has ', nlevels, ' levels.')) 
            # also add: check if user's color syntax was valid
          }
          names(custom.colors) = levels(choropleth.df[[self$value.name]])
          ggscale = scale_fill_manual(values = custom.colors)
        } else {
          if (self$value_was_discretized) { # IIb. value is a discretized continuous variable
            if (is.null(color.max)) {
              color.max = '#084594'
            }
            if (is.null(color.min)) {
              color.min = '#eff3ff'
            }
            mycolors = colorRampPalette(c(color.min, color.max))(nlevels)
            ggscale = scale_fill_manual(values = mycolors, na.value = na.color)
          } else {
            if (!is.null(color.min) | !is.null(color.max)) { # IIc. value is categorical
              warning('color_min and color_max ignored when plotting a categorical variable.')
            }
            ggscale = scale_fill_brewer(self$legend, drop=FALSE, na.value = na.color, type = 'qual')   
          }
        }
      }
      return(ggscale)
    },
    
    get_projection = function(choropleth.df = self$choropleth.df, respect_zoom = TRUE,
                              projection_name = 'cartesian',  ignore_latlon = FALSE,
                              limits_lat = NULL, limits_lon = NULL, reproject = TRUE) {
      if (respect_zoom) {
        choropleth.df = choropleth.df[choropleth.df$render == TRUE,]
      }
      if (is.null(limits_lat) & is.null(limits_lon)) { # skip reprojection of no lat/lon limits are given
        reproject = FALSE
        ignore_latlon = TRUE
      }

      bbox = sf::st_bbox(choropleth.df)
      # If reproject == F, projection will be set with the whole map's bounding box, THEN the user's
      # lat/lon limit will be applied. This will simply crop the entire map to the user's desired limits,
      # but the resulting figure may look distorted if the region inside the user's latlon limits is
      # far away from the centroid of the whole map.
      
      # If reproject == T, the user's lat/lon limits will be imposed, and the map cropped, PRIOR to 
      # applying the projection. This will center the projection around the user's desired region and
      # will generally produce a better figure.
      
      browser()
      if (reproject) { 
        if (!is.null(limits_lat)) {
          bbox['ymin'] = limits_lat[1]
          bbox['ymax'] = limits_lat[2]
        } 
        if (!is.null(limits_lon)) {
          bbox['xmin'] = limits_lon[1]
          bbox['xmax'] = limits_lon[2]
        } 
      }
      
      lat_1 = bbox["ymin"] + 0.25 * (bbox["ymax"] - bbox["ymin"])
      lat_2 = bbox["ymin"] + 0.75 * (bbox["ymax"] - bbox["ymin"])
      lat_0 = (bbox["ymin"] + bbox["ymax"]) / 2
      lon_0 = (bbox["xmin"] + bbox["xmax"]) / 2
      
      # -- fill in lat/lon limits with bbox if they are missing; otherwise 
      # albers/robinson will throw error if the entire world is being mapped for
      # some reason. If both at & lon limits are NULL, switch to default 
      # rendering based on bbox.
      
      if (is.null(limits_lon)) {
        limits_lon = c(bbox['xmin'], bbox['xmax'])
      }
      
      if (is.null(limits_lat)) {
        limits_lat = c(bbox['ymin'], bbox['ymax'])
      }
      
      # -- Apply projections
      if (projection_name == 'cartesian') {
        if (ignore_latlon) {
          projection = coord_sf(crs = 4326, lims_method = 'geometry_bbox')
        } else {
          projection = coord_sf(crs = 4326, ylim = limits_lat, xlim = limits_lon)
        }
      } else if (projection_name == 'mercator') {
        if (ignore_latlon) {
          projection = coord_sf(crs = 3857, lims_method = 'geometry_bbox')
        } else {
          limits_lat[1] = ifelse(limits_lat[1] < -75, -75, limits_lat[1])
          projection = coord_sf(crs = 3857, default_crs = 4326,  ylim = limits_lat, xlim = limits_lon)
        }
      } else if (projection_name == 'robinson') {
        proj_str = sprintf("+proj=robin +lon_0=%.6f", lon_0)
        if (ignore_latlon) {
          projection = coord_sf(crs = proj_str, lims_method = 'geometry_bbox')
        } else {
          projection = coord_sf(crs = proj_str, default_crs = 4326, ylim = limits_lat, xlim = limits_lon)
        }
      } else if (projection_name == 'albers') {
        proj_str = sprintf("+proj=aea +lat_1=%.6f +lat_2=%.6f +lat_0=%.6f +lon_0=%.6f",
                           lat_1, lat_2, lat_0, lon_0)
        if (ignore_latlon) {
          projection = coord_sf(crs = proj_str, lims_method = 'geometry_bbox')
        } else {
          projection = coord_sf(crs = proj_str, default_crs = 4326, ylim = limits_lat, xlim = limits_lon)
        }
      } else {
        stop("projection_name must be 'cartesian', 'mercator', 'robinson', or 'albers'.")
      }
      
      return(projection)
    },
    
    
    render = function(choropleth.df = self$choropleth.df, ggscale, projection, 
                      respect_zoom = TRUE, occlude_latlon_limits = TRUE, 
                      border_color = 'black', border_thickness = 0.2,
                      background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE, 
                      label = NULL, label_text_size, label_text_color, label_box_color,
                      ggrepel_options = NULL,
                      legend = NULL, legend_position = 'right', title = NULL)
    {
      if (respect_zoom) {
        choropleth.df = choropleth.df[choropleth.df$render == TRUE,]
      }

      if (occlude_latlon_limits & !is.null(projection$limits$x) & !is.null(projection$limits$y)) {
        limits = c(projection$limits$x[1], projection$limits$x[2], 
                   projection$limits$y[1], projection$limits$y[2])
        names(limits) = c('xmin', 'xmax', 'ymin', 'ymax')
        bbox = sf::st_bbox(limits, crs = st_crs(4326))
        
        sf_use_s2(FALSE)
        bbox_poly = st_as_sfc(expand_bbox(bbox, factor = 1.1)) # Slightly expand bbox to prevent clipping of mapped regions
        choropleth.df = st_intersection(choropleth.df, bbox_poly) # Clip regions outside user's desired lat/lon region; prevents distant regions from getting hugely distorted and covering up nearby regions.
        sf_use_s2(TRUE)
      }

      
      # ---- Set other formatting options ----      

      if (class(choropleth.df[[self$value.name]]) == 'factor') {
        gg_guide = guides(fill = guide_legend(
          override.aes = list(colour = "black", size = 1, linewidth = .2)  # sets black borders on legend key regardless of country border color
        ))
      } else {
        gg_guide = NULL
      }
      
      if (!is.null(label)) {
        if (!label %in% self$geoid.all) {
          stop(paste0('The requested label must be one of the allowed geoid for this map (', paste0(self$geoid.all, collapse = ', '), ')'))
        }
        arglist_main = list(data = choropleth.df,
                            mapping = aes_string(label = label, geometry = 'geometry'),
                            stat = "sf_coordinates",
                            size = label_text_size,
                            color = label_text_color,
                            fill = label_box_color)
        arglist_all = c(arglist_main, ggrepel_options)
        gg_label = do.call(ggrepel::geom_label_repel, arglist_all)
      } else {
        gg_label = NULL
      }

      if (gridlines) {
        gg_grid = element_line(color = "dark grey", size = .1)
      } else {
        gg_grid = element_blank()
      }
      if (latlon_ticks) {
        gg_axis_text = element_text()
        gg_axis_tick = element_line()
      } else {
        gg_axis_text = element_blank()
        gg_axis_tick = element_blank()
      }
      # ---- Render map ----
      # browser()
      if (F) {
        ggplot(choropleth.df) +
          geom_sf(aes(fill = .data[[self$value.name]]), color = border_color, linewidth = border_thickness) + projection 
      }
      
      ggplot(choropleth.df) +
        geom_sf(aes(fill = .data[[self$value.name]]), color = border_color, linewidth = border_thickness) +
        ggscale +
        projection + 
        gg_label + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = background_color, color = NA), 
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          panel.grid.major = gg_grid,
          axis.text = gg_axis_text,
          axis.ticks = gg_axis_tick,
          axis.title = element_blank(),
          legend.position = legend_position,
          legend.title.align = 0.5,
          legend.text.align = 0,
          legend.background = element_rect(
            fill = "white",    # background color inside the box
            color = "black",   # border color of the box
            linewidth = 0.25    # thickness of the border
          )
        ) +
        labs(fill = ifelse(is.null(legend), self$value.name, legend))+
        gg_guide +
        ggtitle(title)
    }
  )
)

is_valid_color = function(color) {
  res = try(grDevices::col2rgb(color))
  if ('try-error' %in% class(res)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

is_whole_number = function(x) {
  is.numeric(x) && floor(x) == x
}

#' @importFrom Hmisc cut2 
discretize = function(x, nlvls) {
  #browser()
  scipen_orig = getOption("scipen") # Remove scientific notation
  options(scipen=999)
  x_cut = Hmisc::cut2(x, g = nlvls, m = 1, minmax = T)
  labelgood = character()
  for (i in seq_along(levels(x_cut))) {
    str = levels(x_cut)[i]
    strsplit = unlist(stringr::str_split(str, pattern = ','))
    if (length(strsplit) == 1) {
      labelgood[i] = str
    } else {
      left = trimws(stringr::str_replace_all(strsplit[1], "[\\[\\]\\(\\)]", ""))
      right = trimws(stringr::str_replace_all(strsplit[2], "[\\[\\]\\(\\)]", ""))
      left = format(as.numeric(left), big.mark = ',')
      right = format(as.numeric(right), big.mark = ',')
      labelgood[i] = paste0(left, ' to <', right)
    }
  }
  stopifnot(length(labelgood) == length(levels(x_cut)))
  levels(x_cut) = labelgood
  options(scipen=scipen_orig)
  stopifnot(length(x) == length(x_cut))
  return(x_cut)
}

expand_bbox = function(bbox, factor = 0.2) {
  x_range = bbox["xmax"] - bbox["xmin"]
  y_range = bbox["ymax"] - bbox["ymin"]
  bbox["xmin"] = bbox["xmin"] - x_range * factor / 2
  bbox["xmax"] = bbox["xmax"] + x_range * factor / 2
  bbox["ymin"] = bbox["ymin"] - y_range * factor / 2
  bbox["ymax"] = bbox["ymax"] + y_range * factor / 2
  return(bbox)
}

