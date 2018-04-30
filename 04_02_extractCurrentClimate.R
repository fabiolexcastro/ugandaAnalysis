
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)

# Initial setup 
g <- gc(reset = T)
rm(list = ls())
myproj <- '+proj=longlat +datum=WGS84 +no_defs'

# Load data
occ <- read_csv('../_data/_tbl/occ_uga.csv')
vrs <- c('prec', 'tmean', 'tmax', 'tmin')

# Extract values current
crn <- list.files('../_data/_asc/_wc/_current', full.names = TRUE, pattern = '.asc$') %>%
  mixedsort() %>% 
  grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>% 
  stack()
vls <- raster::extract(crn, occ[,1:2]) %>% cbind(occ, .) %>% tbl_df() 
vls <- dplyr::select(vls, -Lon, -Lat, -Coffee)
vls <- colMeans(vls, na.rm = T) %>% data.frame() %>% t()
vls <- vls %>% tbl_df() %>% mutate(zone = 1)

write.table(vls, '../_data/_tbl/_climate/_current/occ_crn.csv', col.names = TRUE, row.names = FALSE)

