
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(rgeos)
require(velox)
require(stringr)

# Initial setup
g <- gc(reset = T)
rm(list = ls())
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_ugandaCoffee/_codes')
path_climate <- '//mnt/workspace_cluster_9/Coffee_Cocoa2/_uganda_terraClimate'

# Functions to use
stToTbl <- function(vlx, spn){
  vls <- vlx$extract_points(sp = spn)
  rsl <- vls %>%
    cbind(spn@data) %>%
    tbl_df() %>% 
    setNames(c(nms, colnames(spn@data))) %>%
    mutate(ID = 1:nrow(.)) %>%
    gather(var, value, -ID, -Lon, -Lat, -Coffee, -Coffee2) %>%
    mutate(year = str_sub(var, 2, 5),
           bio = str_sub(var, 7, nchar(var))) 
  print('Done')
  return(rsl)
}

# Load data
occ <- shapefile('../_data/_shp/_occ/occ_uganda.shp')
lyrs <- paste0(path_climate, '/_data/_raster/_tif/_ext/_bios') %>%
  list.files(., full.names = T, pattern = '.asc$') %>% 
  grep('bio', ., value = T) %>% 
  stack()
yrs <- 1997:2017


# Conver to velox object
vlx <- velox(lyrs)
nms <- names(lyrs)

# Apply the function to convert the velox object to table
system.time(expr = vls <- stToTbl(vlx, occ))

# Filtering the table in the years of interest
vls.yrs <- vls %>% filter(year %in% yrs)
vls.yrs$Coffee2 <- gsub('Robusta 2', 'robusta', vls.yrs$Coffee2)
vls.yrs.sum <- vls.yrs %>%
  group_by(Coffee2, year, bio) %>%
  summarize(avg = mean(value)) %>%
  ungroup()

# Robusta
vls.yrs.sum.rob <- vls.yrs.sum %>%
  filter(Coffee2 == 'robusta')
rslt <- vls.yrs.sum.rob %>%
  spread(year, avg)
rslt <- rslt[mixedorder(rslt$bio),]
write.csv(rslt, '../_data/_tbl/_occ/robusta_values.csv', row.names = F)
rslt <- dplyr::select(rslt, -Coffee2)
rslt <- setNames(data.frame(t(rslt[,-1])), t(rslt[,1]))

write.csv(rslt, '../_data/_tbl/_occ/robusta_values_st2.csv', row.names = T)






