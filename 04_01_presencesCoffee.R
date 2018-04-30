
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
wbnk <- coordinates(wbnk) %>% cbind(wbnk@data) %>% tbl_df()
occ_rb <- read.csv(paste0(path_presences, '../_tables/_presences/1_robusta_rmDupCell.csv')) %>% tbl_df()
occ_ar <- read.csv(paste0(path_presences, '../_tables/_presences/1_arabica_rmDupCell.csv')) %>% tbl_df()

# Tidy worldbank presences