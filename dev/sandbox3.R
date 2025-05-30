world = readRDS('dev/sf_country.rds')
sf_use_s2(TRUE)
world <- ne_countries(scale = "small", returnclass = "sf")
ggplot(world) +
  geom_sf() +
  coord_sf(expand = FALSE) +  # enable labels!
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black")
  )


robin_crs <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
world_robin <- st_transform(world, crs = proj_str)
ggplot(world_robin) + geom_sf() +coord_sf(default_crs = 4326, xlim = c(-120, -60), ylim = c(30,70))

small = world[world$continent == 'North America', ]
small_robin = st_transform(small, crs = "+proj=robin +lon_0=0 +datum=WGS84")
ggplot(small_robin) + geom_sf() +coord_sf()


bbox = sf::st_bbox(small)
lon_0 = (bbox["xmin"] + bbox["xmax"]) / 2
proj_str = sprintf("+proj=robin +lon_0=%.6f", lon_0)
#small_robin = st_transform(small, crs = proj_str)
ggplot(small) + geom_sf() + coord_sf()


lat_1 = bbox["ymin"] + 0.25 * (bbox["ymax"] - bbox["ymin"])
lat_2 = bbox["ymin"] + 0.75 * (bbox["ymax"] - bbox["ymin"])
lat_0 = (bbox["ymin"] + bbox["ymax"]) / 2
lon_0 = (bbox["xmin"] + bbox["xmax"]) / 2

proj_str = sprintf("+proj=aea +lat_1=%.6f +lat_2=%.6f +lat_0=%.6f +lon_0=%.6f",
                   lat_1, lat_2, lat_0, lon_0)

small_albers = st_transform(small, crs = proj_str)
ggplot(world) + geom_sf() 
ggplot(world) + geom_sf() + coord_sf(crs = proj_str, default_crs = 4326, xlim = c(-180, 180), ylim = c(-90,90))
