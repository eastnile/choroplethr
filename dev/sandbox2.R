map = choroplethr::state.map.lores
library(ggplot2)
library(ggrepel)
library(sf)
ggplot(map) + geom_sf(aes(fill = fips.numeric)) + 
  geom_label_repel(aes(label = fips.numeric, geometry = 'geometry'), 
                   stat = "sf_coordinates")

map$lon <- sf::st_coordinates(sf::st_centroid(map$geometry))[,1]
map$lat <- sf::st_coordinates(sf::st_centroid(map$geometry))[,2]


ggplot(map) + geom_sf(aes(fill = fips.numeric)) + 
  geom_label_repel(aes(label = fips.numeric, x = lon, y = lat))

myvar = 'fips.numeric'

ggplot(map) + geom_sf(aes(fill = fips.numeric)) + 
  geom_label_repel(aes(label = .data[['fips.numeric']], x = lon, y = lat))

mylabs = map$fips.numeric
myx = map$lon
myy = map$lat

ggplot(map) + geom_sf(aes(fill = fips.numeric)) + 
  geom_label_repel(aes(label = mylabs, x = myx, y = myy))

arglist = list(label = mylabs, x = myx, y = myy)
gg_lab = geom_label_repel(do.call(ggplot2::aes, arglist))

ggplot(map) + geom_sf(aes(fill = fips.numeric)) + gg_lab

arglist = list(mapping = aes(label = mylabs, x = myx, y = myy),
               size = 1,
               color = 'red',
               fill = 'blue')

gglab = do.call(ggrepel::geom_label_repel, arglist)

arglist_main = list(data = choropleth.df,
                    mapping = aes_string(label = label, geometry = 'geometry'),
                    # mapping = aes(label = .data[[label]], geometry = 'geometry'),
                    stat = "sf_coordinates",
                    size = label_text_size,
                    color = label_text_color,
                    fill = label_box_color)
arglist_all = c(arglist_main, ggrepel_options)
gg_label = do.call(ggrepel::geom_label_repel, arglist_all)