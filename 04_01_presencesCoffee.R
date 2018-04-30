
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
occ <- raster::extract(uga, occ) %>% cbind(coordinates(occ), occ@data) %>% tbl_df(.)
occ <- occ[complete.cases(occ),]
coordinates(occ) <- ~ Lon + Lat
occ <- as.data.frame(occ) %>% dplyr::select(Lon, Lat, clase) %>% tbl_df()
occ <- occ %>% rename(Coffee = clase) 
occ <- occ %>% mutate(Coffee = ifelse(Coffee == 1, 'Robusta', 'Arabica'))

# Map for the presences
gg <- ggplot(data = occ, aes(x = Lon, y = Lat, colour = Coffee)) +
  geom_point() +
  scale_colour_manual(na.value = 'white', values = c('#FFBF00', '#8A4B08'), labels = c('Arabica', 'Robusta')) +
  geom_polygon(data = uga, aes(x = long, y = lat, group = group), color = 'grey', fill = 'NA') +
  coord_equal() +
  theme_bw() +
  labs(fill = 'Coffee') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'top') 

ggsave(gg, filename = '../_png/_maps/_base/coffeePresences.png', units = 'in', width = 9, height = 9, dpi = 150)

# Write the final table
write.csv(occ, '../_data/_tbl/occ_uga.csv', row.names = FALSE)

 

