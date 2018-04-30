
# Load data
require(raster)
require(rgdal)
require(tidyverse)
require(sf)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())

# Load data
path_presences <- 'W:/_africaCoffee/_bdBM/'
wbnk <- shapefile(paste0(path_presences, '_shp/_abril/world_bankRF.shp'))
wbnk <- coordinates(wbnk) %>% cbind(wbnk@data) %>% tbl_df() %>% rename(lon = coords.x1, lat = coords.x2)
occ_rb <- read.csv(paste0(path_presences, '../_tables/_presences/1_robusta_rmDupCell.csv')) %>% tbl_df() %>% dplyr::select(Species, Lon, Lat)
occ_ar <- read.csv(paste0(path_presences, '../_tables/_presences/1_arabica_rmDupCell.csv')) %>% tbl_df() %>% dplyr::select(Species, Lon, Lat)
lbls <- data.frame(clase = c('robusta', 'arabica'), value = 1:2)

# Tidy worldbank presences
wbnk.rbs <- wbnk %>% filter(coffee == 'robusta') %>% dplyr::select(lon, lat, clase) %>% rename(Lon = lon, Lat = lat)
wbnk.ara <- wbnk %>% filter(coffee == 'arabica') %>% dplyr::select(lon, lat, clase) %>% rename(Lon = lon, Lat = lat)
occ.rbs <- occ_rb %>% mutate(clase = 1) %>% dplyr::select(-Species)
occ.ara <- occ_ar %>% mutate(clase = 2) %>% dplyr::select(-Species)

occ <- rbind(wbnk.rbs, wbnk.ara, occ.rbs, occ.ara) %>% tbl_df()
dir.create('../_data/_tbl')
write.csv(occ, '../_data/_tbl/occ_all.csv', row.names = FALSE)

# Clip with only Uganda
occ$clase <- factor(occ$clase, levels = c(1,2))
uga <- getData('GADM', country = 'UGA', level = 0)
coordinates(occ) <- ~ Lon + Lat
crs(occ) <- crs(uga)
tst <- over(occ, uga) %>% tbl_df() 
tst[complete.cases(tst),]

# Map for the presences
ggplot(data = occ, aes(x = Lon, y = Lat, colour = clase)) +
  geom_point() +
  geom_polygon(data = uga, aes(x = long, y = lat, group = group), color = 'grey', fill = 'NA') +
  coord_equal() +
  theme_bw() +
  labs(fill = 'Coffee')




