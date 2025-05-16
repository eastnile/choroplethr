#' The base Choropleth object.
#' @importFrom R6 R6Class
#' @importFrom ggplot2 scale_color_continuous coord_quickmap coord_map scale_x_continuous scale_y_continuous geom_sf coord_sf .data
#' @importFrom ggmap get_map ggmap
#' @importFrom RgoogleMaps MaxZoom
#' @importFrom stringr str_extract_all
#' @export
Choropleth = R6Class("Choropleth", 
                     
  public = list(
    # the key objects for this class
    user.df = NULL,
    user.df.prepped        = NULL, # input from user
    ref.regions = NULL,
    ref.regions.name = NULL,
    map.df         = NULL, # geometry of the map
    map.df.name = NULL,
    geoid.all = NULL,
    geoid.main = NULL,
    choropleth.df  = NULL, # result of binding user data with our map data
    geoid.name = NULL,
    geoid.type = NULL,
    value.name = NULL,
    user.num.colors = NULL,
    #zoom = NULL,
    #omit = NULL, 
    #lim.lat = NULL,
    #lim.long = NULL,
    scale = NULL,
    user_value_factor_or_string = NULL,
    value_was_discretized = NULL,
    nlevels = NULL,
    # user.colors = NULL,
    title          = "",    # title for map
    legend         = "",    # title for legend
    warn           = TRUE,  # warn user on clipped or missing values                      
    ggplot_scale   = NULL,  # override default scale.
                            # warning, you need to set "drop=FALSE" for insets to render correctly
    
    # as of ggplot v2.1.0, R6 class variables cannot be assigned to ggplot2 objects
    # in the class declaration. Doing so will break binary builds, so assign them
    # in the constructor instead
    projection     = NULL, 
    ggplot_polygon = NULL, 
    
    # variables for working with simple features
    projection_sf  = NULL,
    ggplot_sf      = NULL,
      
    # a choropleth map is defined by these two variables
    # a data.frame of a map
    # a data.frame that expresses values for regions of each map
    initialize = function(user.df, geoid.name, geoid.type, value.name)
    {

      stopifnot('data.frame'%in%class(user.df))
      stopifnot(length(geoid.name) == 1)
      stopifnot(class(geoid.name) == 'character')
      stopifnot(length(value.name) == 1)
      stopifnot(class(value.name) == 'character')
      if (is.factor(user.df$value)) {
        self$user_value_factor_or_string = TRUE
        #self$nlevels = length(levels(self$user.df$value))
      } else if (is.character(user.df$value)) {
        self$user_value_factor_or_string = TRUE
        self$user.df$value = as.factor(self$user.df$value)
        self$nlevels = length(levels(self$user.df$value))
        print(paste0('The variable to be plotted is a character and will be converted to factor with ', self$nlevels, ' levels before plotting.'))
      } else if (is.numeric(user.df$value)) {
        self$user_value_factor_or_string = FALSE
      } else {
        stop('The variable to be plotted must be a numeric, a factor, or a character.')
      }
      
      if (anyDuplicated(user.df[, geoid.name]) != 0) {
        stop(paste0("Duplicates detected. The variable '", geoid.name, "' must uniquely identify observations in the data to be plotted"))
      }

      # ---- Prep user input data
      user.df.prepped = user.df[, c(geoid.name, value.name)]
      if (is.null(geoid.type)) { # Establish which geoid is being used in user dataset
        #print('geoid.type not specified; attempting to guess ')
        geoid.type = guess_geoid_type(user.regions = user.df[, geoid.name], ref.regions = self$ref.regions,
                                      geoid.all = self$geoid.all)
      } 
      unmatched = user.df[, geoid.name][!user.df[, geoid.name] %in% self$ref.regions[, geoid.type]]
      if (length(unmatched) > 0) {
        warning(paste0('The following regions are not found in list of available regions and will not be plotted: ', 
                       paste0(unmatched, collapse = ', '),
                       '. To see the list available regions, see regions.country, regions.state, or regions.county.'))
        user.df.prepped = user.df.prepped[!user.df.prepped[, geoid.name] %in% unmatched, ]
        rownames(user.df.prepped)=NULL
      }
      # user.df.prepped$region = map_to_geoid_main(x = user.df.prepped$region,
      #                                            geoid.type = geoid.type,
      #                                            ref.regions = self$ref.regions,
      #                                            geoid.main = self$geoid.main)
      names(user.df.prepped) = c(geoid.type, value.name)
      user.df.prepped = dplyr::left_join(self$ref.regions, user.df.prepped, by = geoid.type)
      # user.df.prepped = user.df.prepped[!user.df.prepped[, geoid.name] %in% unmatched, ]
      # user.df.prepped = dplyr::left_join(self$ref.regions, user.df.prepped, by = self$geoid.type)
      # user.df.prepped = user.df.prepped[, c(self$geoid.main, value.name)]
      # names(user.df.prepped) = c('region', 'value')
      #self$user.df = user.df.prepped
      # Set attributes
      self$user.df = user.df
      self$geoid.name = geoid.name
      self$geoid.type = geoid.type
      self$value.name = value.name
      self$user.df.prepped = user.df.prepped
      #self$set_zoom(NULL)
      

      # as of ggplot v2.1.0, R6 class variables cannot be assigned to ggplot2 objects
      # in the class declaration. Doing so will break binary builds, so assign them
      # in the constructor instead
      browser()
      self$projection     = coord_quickmap()
      self$ggplot_polygon = geom_polygon(aes(fill = .data[[self$value.name]]), color = "dark grey", size = 0.2)
      
      # experimental features for porting to simple features
      self$projection_sf = coord_sf()
      self$ggplot_sf     = geom_sf(aes(fill = .data[[self$value.name]]), color = "dark grey", size = 0.2)
    },
    
    prepare_map = function(include, exclude, nlvls = NULL) 
    {
      browser()
      if (is.null(nlvls)) {
        stopifnot(self$user_value_factor_or_string == FALSE)
      }
      include = map_to_geoid_main(x = include, geoid.type = self$geoid.type, ref.regions = self$ref.regions, geoid.main = self$geoid.main)
      exclude = map_to_geoid_main(x = exclude, geoid.type = self$geoid.type, ref.regions = self$ref.regions, geoid.main = self$geoid.main)
      
      # --- bind
      geoid_common = intersect(names(self$map.df), names(self$user.df.prepped))
      self$choropleth.df = left_join(self$map.df, self$user.df.prepped, by = geoid_common)
      missing_regions = unique(self$choropleth.df[is.na(self$choropleth.df[, self$value.name]), ][, geoid.name])
      if (self$warn && length(missing_regions) > 0)
      {
        missing_regions = paste(missing_regions, collapse = ", ");
        warning_string = paste("The following regions were missing and are being set to NA:", missing_regions);
        warning(warning_string);
      }
      
      # --- clip
      self$choropleth.df = self$choropleth.df[self$choropleth.df$region %in% include, ]
      self$choropleth.df =  self$choropleth.df[!self$choropleth.df$region %in% exclude, ]
      
      # --- discretize if needed
      browser()
      if (!is.null(nlvls)) {
        # print('jomama')
        self$choropleth.df$value = discretize(x = self$choropleth.df$value, nlvls = nlvls)
        self$value_was_discretized = T
      }
    },

    #' @importFrom ggplot2 scale_fill_gradient2
    get_scale = function(cont_scale = 'div', user.colors, color_min, color_max, na.value) {
      browser()
      if (class(self$choropleth.df$value) != 'factor') { # I. data is numeric
        if (!is.null(user.colors)) {
          warning('user.colors ignored when the plotted variable is continuous') 
        }
        if (cont_scale == 'div') {
          ggscale = ggplot2::scale_fill_gradient(self$legend, na.value = na.value, low = color_min, high = color_max)
        } else {
          ggscale = ggplot2::scale_fill_gradient2(self$legend, na.value = na.value, low = color_min, high = color_max,                                        midpoint = median(choropleth.df$value))
        }
      } else { # II. data is a factor
        nlevels = length(levels(self$choropleth.df$value))
        if (!is.null(user.colors)) { # IIa. user colors override other options
          n.user.colors = length(user.colors)
          if (n.user.colors == nlevels) {
            stop(paste0(n.user.colors, ' were specified in user.colors but the variable to be plotted only has ', nlevels, ' levels.')) 
            # also add: check if user's color syntax was valid
          }
          ggscale = scale_color_manual(values = mycolors)
        } else {
          if (self$value_was_discretized) { # IIb. value is a discretized continuous variable
            mycolors = colorRampPalette(c(color_min, color_max))(nlevels)
            ggscale = scale_fill_manual(values = mycolors, na.value = na.value, drop = F)
          } else {
            if (!is.null(color_min) | !is.null(color_max)) { # IIc. value is categorical
              warning('color_min and color_max ignored when plotting a categorical variable.')
            }
            ggscale = scale_fill_brewer(self$legend, drop=FALSE, na.value = na.value, type = 'qual')   
          }
        }
      }
      return(ggscale)
    },
    
    render = function(projection, cont_scale = 'div', 
                      user.colors = NULL, color_min = '#eff3ff', color_max = '#084594', na.value = 'grey') 
    {
      browser()
      if (is.null(self$choropleth.df)) {
        stop('self$choropleth.df not found; run prepare_map() before rendering')
      }
      # Choose scale based on user request
      ggscale = self$get_scale(cont_scale = cont_scale, 
                               user.colors = user.colors, 
                               color_min = color_min, 
                               color_max = color_max,
                               na.value = na.value)
            

      # --- get projection
      if ("sf" %in% class(self$choropleth.df)) {
        browser()
        ggplot(self$choropleth.df) +
          self$ggplot_sf +
          ggscale +
          self$theme_clean() + 
          ggtitle(self$title) +
          projection
      } else {
        browser()
        ggplot(self$choropleth.df, aes(long, lat, group = group)) +
          self$ggplot_polygon + 
          ggscale +
          self$theme_clean() + 
          ggtitle(self$title) + 
          projection
      }
    },

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


    get_projections = function() {
      
    },
    
    # theme_clean and theme_inset are used to draw the map over a clean background.
    # The difference is that theme_inset also removes the legend of the map.
    # These functions used to be based on the code from section 13.19 of  
    # "Making a Map with a Clean Background" of "R Graphics Cookbook" by Winston Chang.  
    # 
    # However, it appears that as of version 2.2.1.9000 of ggplot2 that code simply does not work
    # anymore. (In particular, calling ggplotGrob on maps created with those themes (which choroplethr
    # does for maps that appear as insets, such as Alaska) was causing a crash).
    # So these functions now use theme_void
    #' @importFrom ggplot2 theme_void
    theme_clean = function()
    {
      ggplot2::theme_void()
    },
    
    # This is a copy of the actual code in theme_void, but it also remove the legend
    #' @importFrom ggplot2 theme_void theme "%+replace%"
    theme_inset = function()
    {
      ggplot2::theme_void() %+replace%
        ggplot2::theme(legend.position = "none")
    },
  
    set_num_colors = function(user.num.colors)
    {
      # if R's ?is.integer actually tested if a value was an integer, we could replace the 
      # first 2 tests with is.integer(num_colors)
      stopifnot(is.numeric(num_colors) 
                && num_colors%%1 == 0 
                && num_colors >= 0 
                && num_colors < 10)
      
      private$num_colors = num_colors      
    }
  ),
  
  private = list(
    #zoom = NULL, # a vector of regions to zoom in on. if NULL, show all
    num_colors = 7,     # number of colors to use on the map. if 1 then use a continuous scale
    has_invalid_regions = FALSE
  )
)

get_albers_proj = function(sf_obj) {
  stopifnot('sf' %in% class(sf_obj))
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

guess_geoid_type = function(user.regions, ref.regions, geoid.all, verbose = F) {
  unmatched = list()
  n_unmatched = numeric()
  for (name in geoid.all) {
    unmatched[[name]] = user.regions[!user.regions %in% ref.regions[, name]]
    n_unmatched[name] = length(unmatched[[name]])
  }
  geoid.type.guess = names(n_unmatched)[which.min(n_unmatched)] # if there's a tie it pick the first one
  # print(paste0('geoid_type = "auto"; the geoid ', self$geoid.name, ' was determined to be of type: ', self$geoid.type, '. 
  #                To see the list of allowed geoid types, see regions.country, regions.state, or regions.county.'))
  if (verbose) {
    print('Number of unmatched regions for each potential geoid.type:')
    print(n_unmatched)
  }
  return(geoid.type.guess)
}

#' @importFrom Hmisc cut2 
discretize = function(x, nlvls) {
  browser()
  # if (!self$user_value_factor_or_string && private$num_colors > 1) {
    # if non-cont. scale is requested (e.i, num colors > 1), discretize the value to be plotted and convert it to a factor 
    scipen_orig = getOption("scipen") # Remove scientific notation
    options(scipen=999)
    x_cut = Hmisc::cut2(x, g = nlvls, m = 1, minmax = T)
    labelgood = character()
    for (i in seq_along(levels(x_cut))) {
      # i = 1
      str = levels(x_cut)[i]
      strsplit = unlist(stringr::str_split(str, pattern = ','))
      if (length(strsplit) == 1) {
        labelgood[i] = str
      } else {
        left = stringr::str_replace_all(strsplit[1], "[\\[\\]\\(\\)]", "")
        right = stringr::str_replace_all(strsplit[2], "[\\[\\]\\(\\)]", "")
        labelgood[i] = paste0(left, ' to <', right)
      }
    }
    stopifnot(length(labelgood) == length(levels(x_cut)))
    levels(x_cut) = labelgood
    options(scipen=scipen_orig)
    stopifnot(length(x) == length(x_cut))
    return(x_cut)
    #self$user.df$value = x_cut
    #self$value_was_discretized = TRUE
    #self$nlevels = length(levels(self$user.df$value))
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

is_valid_color = function(color) {
  res = try(grDevices::col2rgb(color))
  if ('try-error' %in% class(res)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
