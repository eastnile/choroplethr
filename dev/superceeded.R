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