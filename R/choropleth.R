#' The base Choropleth object.
#' @importFrom R6 R6Class
#' @importFrom ggplot2 scale_color_continuous coord_quickmap coord_map scale_x_continuous scale_y_continuous geom_sf coord_sf
#' @importFrom ggmap get_map ggmap
#' @importFrom RgoogleMaps MaxZoom
#' @importFrom stringr str_extract_all
#' @export
Choropleth = R6Class("Choropleth", 
                     
  public = list(
    # the key objects for this class
    user.df        = NULL, # input from user
    map.df         = NULL, # geometry of the map
    map.df.name = NULL,
    choropleth.df  = NULL, # result of binding user data with our map data
    geoid.name = NULL,
    geoid.type = NULL,
    value.name = NULL,
    scale = NULL,
    user_value_factor_or_string = NULL,
    value_was_discretized = NULL,
    nlevels = NULL,
    user.colors = NULL,
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
    initialize = function(map.df, map.df.name = NULL, user.df, 
                          ref.regions, geoid.name, geoid.type, value.name)
    {
      if (F) {
        geoid.type = 'auto'
        geoid.name = 'region'
        value.name = 'total_population'
        library(choroplethrMaps)
        data(state.regions)
        data(state.map)
        data(df_state_demographics)
        map.df = state.map
        user.df = df_state_demographics
        self = list()
      }
      stopifnot('data.frame'%in%class(user.df))
      stopifnot(length(geoid.name) == 1)
      stopifnot(length(value.name) == 1)
      stopifnot(class(geoid.name) == 'character')
      stopifnot(class(value.name) == 'character')
      browser()
      self$prep_user_df(user.df = user.df, ref.regions = ref.regions, 
                        geoid.name = geoid.name,
                        geoid.type = geoid.type, 
                        value.name = value.name)
      # self$user.df = self$prep_user_df(user.df = user.df, ref.regions = ref.regions, geoid.name = geoid.name,
      #                        geoid.type = geoid.type, value.name = value.name)$user.df.prepped
      # self$geoid.type = self$prep_user_df(user.df = user.df, ref.regions = ref.regions, geoid.name = geoid.name,
      #                                     geoid.type = geoid.type, value.name = value.name)$geoid.type

      
      # things like insets won't color properly if they are characters, and not factors
      browser()
      if (is.factor(user.df$value)) {
        self$user_value_factor_or_string = TRUE
        self$nlevels = length(levels(self$user.df$value))
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
      
      # initialize the map to the max zoom - i.e. all regions
      self$set_zoom(NULL)
      self$map.df = map.df
      self$geoid.name = geoid.name
      self$value.name = value.name
      
      # if the user's data contains values which are not on the map, 
      # then emit a warning if appropriate

      # self$scale = self$get_scale()
      
      # as of ggplot v2.1.0, R6 class variables cannot be assigned to ggplot2 objects
      # in the class declaration. Doing so will break binary builds, so assign them
      # in the constructor instead
      self$projection     = coord_quickmap()
      self$ggplot_polygon = geom_polygon(aes(fill = value), color = "dark grey", size = 0.2)
      
      # experimental features for porting to simple features
      self$projection_sf = coord_sf()
      self$ggplot_sf     = geom_sf(aes(fill = value), color = "dark grey", size = 0.2)
    },

    render = function() 
    {
      self$prepare_map()

      if ("sf" %in% class(self$choropleth.df)) {
        #browser()
        ggplot(self$choropleth.df) +
          self$ggplot_sf +
          self$get_scale() +
          self$theme_clean() + 
          ggtitle(self$title) +
          self$projection_sf
      } else {
        browser()
        ggplot(self$choropleth.df, aes(long, lat, group = group)) +
          self$ggplot_polygon + 
          self$get_scale() +
          self$theme_clean() + 
          ggtitle(self$title) + 
          self$projection
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
      c(self$get_min_long(), # left
        self$get_min_lat(),  # bottom
        self$get_max_long(), # right
        self$get_max_lat())  # top
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
    
    prep_user_df = function(user.df, ref.regions, geoid.name, geoid.type = 'auto', value.name) {
      if(F) {
        data(df_state_demographics)
        data(state.regions)
        user.df = df_state_demographics
        user.df$region[user.df$region=='alabama'] = 'galabama'
        ref.regions = readRDS('dev/st_regions.rds')
        geoid.name = 'region'
        geoid.type = 'auto'
        geoid.type = 'name.lower'
        value.name = 'total_population'
      }
      user.df = user.df[, c(geoid.name, value.name)]
      if (anyDuplicated(user.df[, geoid.name]) != 0) {
        stop(paste0("Duplicates detected. The variable '", geoid.name, "' must uniquely identify observations in the data to be plotted"))
      }
      if (geoid.type == 'auto') { # Establish which geoid is being used in user dataset
        unmatched = list()
        n_unmatched = numeric()
        for (name in names(ref.regions)) {
          unmatched[[name]] = user.df[, geoid.name][!user.df[, geoid.name] %in% ref.regions[, name]]
          n_unmatched[name] = length(unmatched[[name]])
        }
        geoid.type = names(n_unmatched)[which.min(n_unmatched)] # if there's a tie it pick the first one
        geoid.unmatched = unmatched[[geoid.type]]
      } else {
        geoid.unmatched = user.df[, geoid.name][!user.df[, geoid.name]%in%ref.regions[, geoid.type]]
      }
      if (length(geoid.unmatched) > 0) {
        warning(paste0('The following regions are not found in the reference map and will not be plotted: ', geoid.unmatched))
      }
      user.df.clean = user.df[!user.df[, geoid.name]%in%geoid.unmatched, c(geoid.name, value.name)]
      names(user.df.clean) = c('region', 'value')
      self$user.df = user.df.clean
      self$geoid.type = geoid.type
      # return(list(user.df.prepped = user.df.clean, geoid.type = geoid.type))
    },
    
    # support e.g. users just viewing states on the west coast
    clip = function() {
      stopifnot(!is.null(private$zoom))
      
      self$user.df = self$user.df[self$user.df$region %in% private$zoom, ]
      self$map.df  = self$map.df[self$map.df$region %in% private$zoom, ]
    },
    
    # for us, discretizing values means 
    # 1. assigning each value to one of num_colors colors
    # 2. formatting the intervals e.g. with commas
    #' @importFrom Hmisc cut2    
    discretize = function() 
    {
      if (!self$user_value_factor_or_string && private$num_colors > 1) {
        # if non-cont. scale is requested (e.i, num colors > 1), discretize the value to be plotted and convert it to a factor 
        browser()
        scipen_orig = getOption("scipen") # Remove scientific notation
        options(scipen=999)
        x = self$user.df$value
        x_cut = Hmisc::cut2(x, g = private$num_colors, m = 1, minmax = T)
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
        browser()
        stopifnot(length(labelgood) == length(levels(x_cut)))
        levels(x_cut) = labelgood
        options(scipen=scipen_orig)
        stopifnot(length(self$user.df$value) == length(x_cut))
        self$user.df$value = x_cut
        self$value_was_discretized = TRUE
        self$nlevels = length(levels(self$user.df$value))
      }
    },
    
    bind = function() {
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
    
    prepare_map = function()
    {
      self$clip() # clip the input - e.g. remove value for Washington DC on a 50 state map
      self$discretize() # discretize the input. normally people don't want a continuous scale
      self$bind() # bind the input values to the map values
    },

    #' @importFrom ggplot2 scale_fill_gradient2
    get_scale = function(color_min = '#eff3ff', color_max = '#084594', color_mid = '', 
                         midpoint = median(self$choropleth.df$value, na.rm = T), na.value = 'black')
    {
      browser()
      if (!is.null(self$ggplot_scale)) # case 1: ggplot_scale passed to function (may depreciate later)
      {
        self$ggplot_scale
      } else if (!is.null(self$user.colors)) { # case 2: user specifies set of colors
        if (!self$user_value_factor_or_string) {
          stop('User specified colors can only be used with a factor variable.')
        }
        if (length(self$user.colors) != self$nlevels) {
          stop('The number of colors specified in user.colors must match the number of categories in the value to be plotted.')
        }
        return(scale_color_manual(values = mycolors))
      } else if (private$num_colors == 0) { # case 3: continuous diverging scale
        browser()
        # min_value = min(self$choropleth.df$value, na.rm = TRUE)
        # max_value = max(self$choropleth.df$value, na.rm = TRUE)
        # stopifnot(!is.na(min_value) && !is.na(max_value))

        scale_fill_gradient(self$legend, na.value = "black", low = color_min, high = color_max)

      } else if (private$num_colors == 1) { # case 4: continuous converging scale
        color_min = ifelse(is.null(color_min), '#eff3ff', color_min)
        color_max = ifelse(is.null(color_min), '#084594', color_max)
        


        # by default, scale_fill_continuous uses a light value for high values and a dark value for low values
        # however, this is the opposite of how choropleths are normally colored (see wikipedia)
        # these low and high values are from the 7 color brewer blue scale (see colorbrewer.org)
        #return(scale_fill_continuous(self$legend, low="#eff3ff", high="#084594", na.value="black", limits=c(min_value, max_value)))
        return(ggplot2::scale_fill_gradient(self$legend, na.value = "black", low = color_min, high = color_max))
        
      } else if (!self$user_value_factor_or_string) { # Case 5: Continuous user input but with discretized values
        browser()
        stopifnot(class(self$user.df$value) == 'factor') 
        stopifnot(!is.null(self$nlevels))
        if (self$nlevels != private$num_colors) {
          warning('After making the plotted value discrete, the number of categories differed from the num_colors requested.')
        }
        mycolors = colorRampPalette(c(min, max))(self$nlevels)
        return(scale_fill_manual(values = mycolors, na.value = na.value, drop = F))       
      } else { # user variable was a factor or character
        stopifnot(class(self$user.df$value) == 'factor') 
        stopifnot(!is.null(self$nlevels))
        return(scale_fill_brewer(self$legend, drop=FALSE, na.value="black", type = 'qual'))        
      }
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
    },
    
    set_zoom = function(zoom)
    {
      if (is.null(zoom))
      {
        # initialize the map to the max zoom - i.e. all regions
        private$zoom = unique(self$map.df$region)      
      } else {
        stopifnot(all(zoom %in% unique(self$map.df$region)))
        private$zoom = zoom
      }
    },

    get_zoom = function() { private$zoom },
    
    set_num_colors = function(num_colors)
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
    zoom = NULL, # a vector of regions to zoom in on. if NULL, show all
    num_colors = 7,     # number of colors to use on the map. if 1 then use a continuous scale
    has_invalid_regions = FALSE
  )
)
