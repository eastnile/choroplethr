library(sf)
library(tigris)

counties_sf <- counties(cb = TRUE, year = 2020) 
states_sf <- states(cb = TRUE, year = 2020) 

z = shift_geometry(states_sf, geoid_column = 'GEOID', position = 'below')
ggplot(z) + geom_sf() + coord_sf(crs = 2939)

transform_latlon = function(limits_lat, limits_lon, crs_from, crs_to) {
  if (is.null(limits_lat)) {
    limits_lat_clean = c(0, 90)
  }
  if (is.null(limits_lon)) {
    limits_lon_clean = c(0, 180)
  }
  if (limits_lat == c(-90, 90)) {
    limits_lat_clean = c(-89.99, 89.99)
  }
  if (limits_lon == c(-180, 180)) {
    limits_lon_clean = c(-179.99, 179.99)
  }
  bbox_from = st_bbox(c(xmin = limits_lon_clean[1], xmax = limits_lon_clean[2],
                        ymin = limits_lat_clean[1], ymax = limits_lat_clean[2]), crs = crs_from)
  poly = st_as_sfc(bbox_from)
  bbox_to = st_bbox(st_transform(poly, crs_too))
  if (is.null(limits_lat)) {
    limits_lat_out = NULL
  } else {
    limits_lat_out = c(bbox_to['ymin'], bbox_to['ymax'])
  }
  if (is.null(limits_lon)) {
    limits_lat_out = NULL
  } else {
    limits_lon_out = c(bbox_to['xmin'], bbox_to['xmax'])
  }
  return(limits_lat = limits_lat_out, limits_lon = limits_lon_out)
}
  