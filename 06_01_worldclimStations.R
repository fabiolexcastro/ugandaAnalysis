
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(foreign)
require(sf)

# Functions to use
tblToShp <- function(tbl, nm){
  tbl <- as.tibble(tbl)
  cnt <- unique(tbl$COUNTRY) %>% as.vector() %>% sort()
  uga <- tbl %>% filter(COUNTRY == 'UGANDA')
  crd <- uga %>% dplyr::select(LONG, LAT) %>% as.matrix()
  sft <- st_sfc(st_multipoint(crd))
  st_write(obj = sft, dsn = '../_data/_shp/_wc', layer = nm, driver = 'ESRI Shapefile', update = T)
  print('Done!')
}

# Load data
path_wc <- 'D:/Workspace/_worldclimStations/'
tbls <- list.files(path_wc, full.names = T, pattern = '.dbf') %>% 
  lapply(FUN = read.dbf)
nms <- list.files(path_wc, full.names = F) %>% 
  gsub('.dbf', '', .)

# Table to shapefile
lapply(1:length(tbls), function(x) tblToShp(tbl = tbls[[x]], nm = nms[[x]]))



