
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(rgeos)
require(sf)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = alpha)

# Function to use
zonalSt <- function(crn, ftr){
  
  vrs <- c('bio_1.asc', 'bio_12.asc')
  crn.vrs <- grep(paste0(vrs, collapse = '|'), crn, value = T)
  ftr.vrs <- grep(paste0(vrs, collapse = '|'), ftr, value = T)
  bio1crn <- grep('bio_1.asc', crn.vrs, value = T) %>% raster()
  bio12crn <- grep('bio_12.asc', crn.vrs, value = T) %>% raster()
  bio12crn <- bio12crn * 1
  bio1ftr <- grep('bio_1.asc', ftr.vrs, value = T) %>% stack() %>% mean()
  bio12ftr <- grep('bio_12.asc', ftr.vrs, value = T) %>% stack() %>% mean()
  
  # Difference
  diff.bio1 <- bio1ftr - bio1crn
  diff.bio12 <- bio12ftr - bio12crn
  diff.prc_bio12 <- (diff.bio12 / bio12crn) * 100
  
  # Rasterization
  reg@data$OBJECTID <- as.numeric(reg@data$OBJECTID)
  adm.lyr <- rasterize(reg, diff.bio1, field = 'OBJECTID')
  
  # Zonal
  znl.bio1 <- raster::zonal(diff.bio1, adm.lyr, fun = 'mean', na.rm = T) %>% as.tibble()
  znl.bio12 <- raster::zonal(diff.bio12, adm.lyr, fun = 'mean', na.rm = T) %>% as.tibble()
  znl.bio12porc <- raster::zonal(diff.prc_bio12, adm.lyr, fun = 'mean', na.rm = T) %>% as.tibble()
  
  # Inner Join
  adm.sf <- st_as_sf(as(reg, 'SpatialPolygonsDataFrame'))
  adm.sf.bio1 <- inner_join(adm.sf, znl.bio1, by = c('OBJECTID' = 'zone')) %>% dplyr::select(OBJECTID, reg, mean) %>% mutate(mean = round(mean/10, 2))
  adm.sf.bio12 <- inner_join(adm.sf, znl.bio12, by = c('OBJECTID' = 'zone')) %>% dplyr::select(OBJECTID, reg, mean) %>% mutate(mean = round(mean, 2))
  adm.sf.bio12.prc <- inner_join(adm.sf, znl.bio12porc, by = c('OBJECTID' = 'zone')) %>% dplyr::select(OBJECTID, reg, mean) %>% mutate(mean = round(mean, 1))

  # Write files
  writeRaster(diff.bio1, '../_data/_tif/_znl/diff_bio1.asc', overwrite = TRUE)
  writeRaster(diff.bio12, '../_data/_tif/_znl/diff_bio12.asc', overwrite = TRUE)
  writeRaster(diff.prc_bio12, '../_data/_tif/_znl/diff_bio12_prc.asc', overwrite = TRUE)
  write_sf(adm.sf.bio1, dsn = '../_data/_shp/_znl', layer = 'znl_bio1', driver = 'ESRI Shapefile', update = TRUE)
  write_sf(adm.sf.bio12, dsn = '../_data/_shp/_znl', layer = 'znl_bio12', driver = 'ESRI Shapefile', update = TRUE)
  write_sf(adm.sf.bio12.prc, dsn = '../_data/_shp/_znl', layer = 'znl_bio12prc', driver = 'ESRI Shapefile', update = TRUE)
  
}

# Load Data
uga <- getData('GADM', country = 'UGA', level = 1)
reg <- shapefile('../_data/_shp/UGA_reg_dissolve.shp')
crn <- list.files('../_data/_asc/_wc/_current/', full.names = T, pattern = '.asc$') %>% mixedsort()
gcm <- list.dirs('../_data/_asc/_wc/_2050/', recursive = F, full.names = F)
ftr <- paste0('../_data/_asc/_wc/_2050/', gcm) %>% list.files(full.names = T, pattern = '.asc') %>% mixedsort()

# Apply function
zonalSt(crn = crn, ftr = ftr)

# Extract by mask
lyr <- list.files('../_data/_tif/_znl', full.names = T, pattern = '.asc$') %>% stack()
lyrcut <- raster::crop(lyr, reg) %>% raster::mask(reg)
lyrcut <- unstack(lyrcut)
nms <- c('diff_bio1_cut', 'diff_bio12_cut', 'diff_bio12_prc_cut')
writeRaster(lyrcut[[2]], '../_data/_tif/_znl/diff_bio12_cut.tif', overwrite = T)
writeRaster(lyrcut[[3]], '../_data/_tif/_znl/diff_bio12_prc_cut.tif', overwrite = T)


