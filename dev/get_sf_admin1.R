library(tigris)
library(rnaturalearth)
library(sf)
library(dplyr)
library(ggplot2)
library(rmapshaper)
library(nngeo)
library(stringr)
library(stringi)
library(patchwork)

sf_admin1_orig = rnaturalearth::ne_states(returnclass = "sf")
sf_admin1_orig$is_valid = st_is_valid(sf_admin1_orig)
table(sf_admin1_orig$is_valid)
sf_admin1_orig[!sf_admin1_orig$is_valid,] # A few invalid regions that are hard to fix. Marking them.
sf_admin1 = sf_admin1_orig[sf_admin1_orig$is_valid, ]


# sf_admin1 = sf_admin1_orig
# sf_admin1 = st_transform(sf_admin1_orig, 3857)
# sf_admin1 = st_make_valid(sf_admin1)
# sf_admin1 = st_transform(sf_admin1, 4326)
# sf_admin1 = st_make_valid(sf_admin1)
# ggplot(sf_admin1) + geom_sf()
# ggplot(sf_admin1_orig) + geom_sf()

if (F) { # Understand relative size of various columns in sf_admin1
  size_breakdown = function(df) {
    duplicated = unlist(lapply(df,  function(x){sum(duplicated(x))}))
    isna = unlist(lapply(df,  function(x){sum(is.na(x))}))
    size = lapply(df, object.size)
    sizenum = as.numeric(size)
    names(sizenum) = names(size)
    sizenum = sort(sizenum)
    sizerel = round(sizenum/sum(sizenum), 3)
    return(data.frame(dup = duplicated,
                      isna = isna,
                      name = names(sizenum), 
                      size = sizenum, 
                      sizerel = sizerel))
  }
  size_breakdown(sf_admin1)
}

if (F) { # Understand the scalerank variable
  sf_use_s2(TRUE) 
  st_crs(sf_admin1)
  sf_admin1$size = st_area(sf_admin1)
  sf_admin1$size = units::set_units(sf_admin1$size, km^2)
  sf_admin1$log_size = log10(sf_admin1$size)
  sf_admin1_nogeo = st_drop_geometry(sf_admin1)
  sf_admin1_nogeo %>% group_by(scalerank) %>% summarize(meansize = mean(size, na.rm = T),
                                                        minsize = min(size, na.rm = T),
                                                        maxsize = max(size, na.rm = T),
                                                        nobs = n(),
                                                        log_size = mean(log_size, na.rm = T))
  # size generally monotonically falling in scalerank, except for antarctica
}

if (F) {
  # Extract set of sample countries to examine
  load('data/country.regions.rda')
  country.map$size = st_area(country.map)/1000000
  country.map$logsize = log10(country.map$size)
  hist(country.map$logsize)
  country.map$sizecat = ntile(country.map$logsize, 8)
  country.map = country.map[order(country.map$size), ]
  smallest_countries = country.map %>% group_by(sizecat) %>% slice_min(order_by = size)
  biggest_countries = country.map %>% group_by(sizecat) %>% slice_max(order_by = size)
  
  
  plot_countries = function(map, countries) {
    # map = sf_admin1
    # countries = smallest_countries$iso_a3
    plotdata = map %>% filter(adm0_a3 %in% countries)
    plotdata_list = split(plotdata, plotdata$adm0_a3)
    plots = list()
    for (country in seq_along(names(plotdata_list))) {
      plots[[country]] = ggplot(plotdata_list[[country]]) + geom_sf()
    }
    return(wrap_plots(plots))
  }
  
  sf_admin1_geo = sf_admin1[, c('geometry', 'adm1_code', 'adm0_a3')]
  object.size(sf_admin1_geo)
  sf_admin1_lores1 = ms_simplify(sf_admin1_geo, keep = 0.1, keep_shapes = TRUE)
  object.size(sf_admin1_lores1)
  
  plot_countries(sf_admin1, smallest_countries$iso_a3)
  plot_countries(sf_admin1_lores1, smallest_countries$iso_a3)
  
  plot_countries(sf_admin1, biggest_countries$iso_a3)
  plot_countries(sf_admin1_lores1, biggest_countries$iso_a3)
  
  testcountries = c(smallest_countries$iso_a3, biggest_countries$iso_a3)
  keepvals = c(1, 0.5, 0.4, 0.3, 0.2, 0.1)
  testplots = list()
  sizes = list()
  plot_final = list()
  size_final = list()
  
  for (iso in testcountries) {
    for (keepval in keepvals) {
      cat(iso, keepval)
      if (F) {
        iso = 'LUX'
        keepval = 0.5
      }
      plotdata = sf_admin1 %>% filter(adm0_a3 == iso)
      plotdata_lores = ms_simplify(plotdata, keep = keepval, keep_shapes = TRUE)
      testplots[[iso]][[as.character(keepval)]] = ggplot(plotdata_lores) + geom_sf()
      sizes[[iso]][[as.character(keepval)]] = object.size(plotdata_lores)
    }
    plot_final[[iso]] = wrap_plots(testplots[[iso]])
    size_final[[iso]] = sizes[[iso]]
  }
  
  plot_final$VAT # sizecat 1, best to keep 1
  size_final$VAT
  
  plot_final$MHL # sizecat 2, OK to do 0.5
  size_final$MHL
  
  plot_final$PYF  # sizecat 3, OK to do 0.5
  size_final$PYF                
  
  plot_final$MKD  # sizecat 4, OK to do 0.4
  size_final$MKD                   
  
  plot_final$SLE  # sizecat 5, OK to do 0.4
  size_final$SLE                    
  
  plot_final$KHM               # sizecat 6, OK to do 0.4
  size_final$KHM                 
  
  plot_final$IRQ               # sizecat 7, OK to do 0.3
  size_final$IRQ              
  
  plot_final$MRT               # sizecat 8, OK to do 0.3
  size_final$MRT         
  
  plot_final$KNA               # sizecat 1 
  plot_final$LUX               # sizecat 1
  
}

#####

country.map = rnaturalearth::ne_countries(returnclass = "sf", scale = 50)

country.map$size = st_area(country.map)/1000000
country.map$logsize = log10(country.map$size)
hist(country.map$logsize)
country.map$sizecat = ntile(country.map$logsize, 8)
country.map = country.map[order(country.map$size), ]
country_size_ref = st_drop_geometry(country.map)
country_size_ref = country_size_ref[, c('adm0_a3', 'size', 'logsize', 'sizecat')]

sf_admin1_orig = rnaturalearth::ne_states(returnclass = "sf")

sf_admin1_orig = left_join(sf_admin1_orig, country_size_ref, join_by(adm0_a3 == adm0_a3))
table(sf_admin1_orig$adm0_a3, useNA = 'always')

# simplify regioins based on size of country
chunks = split(sf_admin1_orig, f = sf_admin1_orig$sizecat)
mapping = c(1, 0.5, 0.4, 0.4, 0.4, 0.3, 0.2, 0.2)
chunks_processed = list()
for (i in seq_along(chunks)) {
  chunks_processed[[i]] = ms_simplify(chunks[[i]], mapping[i], keep_shapes = TRUE)
}

sf_admin1_lores = do.call('rbind', chunks_processed)
sf_admin1_lores2 = ms_simplify(sf_admin1_orig, keep = 0.5, keep_shapes = TRUE)
object.size(sf_admin1_lores)
object.size(sf_admin1_lores2)
object.size(sf_admin1_orig)

# fix bad regions
sf_admin1_lores$is_valid = st_is_valid(sf_admin1_lores)
badreg = sf_admin1_lores[!sf_admin1_lores$is_valid, ]
badreg_fixed = st_make_valid(badreg)
ggplot(badreg[1, ]) + geom_sf()
st_is_valid(badreg_fixed)
st_area(badreg_fixed)
sf_fixed = rbind(sf_admin1_lores[sf_admin1_lores$is_valid, ], badreg_fixed)
sum(!st_is_valid(sf_fixed))

if (F) {
  ggplot(sf_fixed) + geom_sf()
  ggplot(sf_fixed) + geom_sf() + coord_sf(xlim = c(0, 23), ylim = c(44, 56))
}
langnames = sort(grep('name_', names(sf_fixed), value = T))
otherkeep = c('adm1_code', 'diss_me', 'ne_id', 'code_hasc', 'iso_3166_2', 'iso_a2',
              'type', 'type_en', 'name', 'name_alt', 'provnum_ne', 'fips', 'woe_id',
              'woe_label', 'woe_name', 'sov_a3', 'adm0_a3', 'admin', 'geonunit',
              'gu_a3', 'gn_id', 'gn_name', 'gns_id', 'gns_name', 'geometry')
keepfinal = union(langnames, otherkeep)
sf_final = sf_fixed[, keepfinal]

sf_final = st_transform(sf_final, 4326)

admin1.map = sf_final
admin1.regions = st_drop_geometry(admin1.map)
duplicated = unlist(lapply(admin1.regions,  function(x){sum(duplicated(x))}))
types = unlist(lapply(admin1.regions, class))
sizes = sort(unlist(lapply(admin1.regions, object.size)))
admin1.regions = relocate(admin1.regions, adm1_code, diss_me, ne_id)
admin1.regions$name_len = NULL

drop_languages = c('name_ur', 'name_bn', 'name_fa', 'name_el', 'name_uk', 
                   'name_ja', 'name_ru', 'name_he', 'name_id', 'name_sv',
                   'name_it', 'name_hu', 'name_nl', 'name_pt', 'name_zht',
                   'name_pl', 'name_tr', 'name_vi', 'name_ko', 'name_de')

names_needed = setdiff(names(admin1.regions), drop_languages)
admin1.regions = admin1.regions[, names_needed]

# reduce size of logup file
# object.size(admin1.regions)
target_cols = duplicated[duplicated > 1000 & types != 'numeric' & types != 'integer']
for (col in names(target_cols)) {
  admin1.regions[[col]] = as.factor(admin1.regions[[col]])
}

save(admin1.regions, file = 'data/admin1.regions.rda', compress = 'xz')


admin1.map = admin1.map[, c('adm1_code', 'geometry')]
save(admin1.map, file = 'data/admin1.map.rda', compress = 'xz')

if (F) {
  test = left_join(admin1.map, admin1.regions)
  ggplot(test %>% filter(adm0_a3 == 'LUX')) + geom_sf()
}


#saveRDS(sf_final, 'dev/sd_admin1_lowres1.rds', compress = 'xz')