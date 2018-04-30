
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(foreach)
require(doSNOW)
require(parallel)
require(doMC)

# Initial setup 
g <- gc(reset = T)
rm(list = ls())
myproj <- '+proj=longlat +datum=WGS84 +no_defs'
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_ugandaCoffee/_codes')

# Load data
occ <- read_csv('../_data/_tbl/occ_uga.csv')
vrs <- c('prec', 'tmean', 'tmax', 'tmin')
gcms <- list.dirs('../_data/_asc/_wc/_2030', full.names = F, recursive = F)
yrs <- c('_2030', '_2050')

# Exract values future
cl <- makeCluster(19)
registerDoSNOW(cl)
registerDoMC(19)
foreach(i = 1:length(gcms), .packages = c('raster', 'rgdal', 'dplyr', 'rgeos', 'gtools', 'foreach'), .verbose = FALSE) %dopar% {
  print(gcms[i])
  foreach(y = 1:length(yrs)) %do% { 
    print(yrs[y])
    print(paste0('To make the stack ', i))
    fls <- list.files(paste0('../_data/_asc/_wc/', yrs[y], '/', gcms[i]), full.names = T, pattern = '.asc$') %>%
      .[-grep('bio', ., value = F)] %>%
      .[-grep('cons_mths', ., value = F)] %>%
      mixedsort() %>%
      stack()
    print(paste0('To make the extraction of the values for ', i))
    df <- raster::extract(fls, occ[,1:2]) %>% 
      cbind(., occ) %>% 
      tbl_df() %>% 
      dplyr::select(-c(Lon, Lat, Coffee))
    mns <- colMeans(df, na.rm = T) %>% t() %>% data.frame()
    mns <- mutate(mns, zone = 1:nrow(mns)) %>% dplyr::select(zone, prec_1:tmin_12)
    write.csv(mns, paste0('../_data/_tbl/_climate/', yrs[y], '/mean_', gcms[i], '_', yrs[y], '.csv'), row.names = F)
    print('Done!')
  }
} 
stopCluster(cl)

# Another way using lapply (it's slower)
lapply(1:length(yrs), function(y){
  print(yrs[y])
  lapply(1:length(gcms), function(i){ 
    print(gcms[i])
    fls <- list.files(paste0('../_data/_asc/_wc/', yrs[y], '/', gcms[i]), full.names = T, pattern = '.asc$') %>%
      grep(paste0(vrs, collapse = '|'), ., value = T) %>%
      mixedsort() %>%
      stack()
    df <- raster::extract(fls, occ[,1:2]) %>% 
      cbind(., occ) %>% 
      tbl_df() %>% 
      dplyr::select(-c(Lon, Lat, Coffee))
    mns <- colMeans(df, na.rm = T) %>% t() %>% data.frame()
    mns <- mutate(mns, zone = 1:nrow(mns)) %>% dplyr::select(zone, prec_1:tmin_12)
    write.csv(mns, paste0('../_data/_tbl/_climate/', yrs[y], '/mean_', gcms[i], '_', yrs[y], '.csv'), row.names = F)
    print('Done')
  }) 
})

