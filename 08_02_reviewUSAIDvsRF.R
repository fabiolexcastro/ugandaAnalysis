
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(sf)

# Functions to use
myZnl <- function(lyr, msk, prd){
  lyr <- crn
  nm <- names(lyr)
  crd <- rasterToPoints(lyr) %>% as.tibble() 
  vls <- raster::extract(prd, crd[,1:2]) %>% 
    cbind(crd) %>%
    as.tibble() %>% 
    dplyr::select(-point.ID, -poly.ID) %>%
    rename_(class = nm) %>% 
    dplyr::select(NAME_1, reg, Coffee, ID, x, y, class)
  summ <- vls %>%
    group_by(NAME_1, class) %>% 
    summarize(count = n()) %>%
    ungroup()
  prd <- prd@data %>% 
    tbl_df() %>% 
    dplyr::select(NAME_1, reg, Coffee, ID)
  summ <- inner_join(summ, prd, by = c('NAME_1' = 'NAME_1'))
  summ <- summ[complete.cases(summ),]
  summ <- inner_join(summ, lbls, by = c('class' = 'number'))
  summ.sub <- filter(summ, type %in% c('Robusta', 'Arabica'))
  summ.spr <- summ.sub %>% 
    spread(type, count) %>% 
    dplyr::select(-class, -reg, -ID)
  summ.spr <- summ.spr %>% 
    group_by(NAME_1, Coffee) %>%
    summarise_all(funs(mean), na.rm = T) %>%
    ungroup()
  summ.spr$Arabica <- gsub(NaN, 0, summ.spr$Arabica)
  summ.spr$Robusta <- gsub(NaN, 0, summ.spr$Robusta)
  summ.spr <- summ.spr %>% mutate(comparison = ifelse(Arabica > Robusta, 'Arabica', 'Robusta'))
  lbls <- data_frame(clases = unique(summ.spr$Coffee), cafe = c('Robusta', 'Arabica', 'Arabica', 'Robusta', 'Both'))
  summ.spr <- inner_join(summ.spr, lbls, by = c('Coffee' = 'clases')) %>% dplyr::select(-Coffee)
  summ.spr <- summ.spr %>% mutate(diff = ifelse(comparison == cafe, 'Equal', 'Not Equal'))
  summ.spr <- summ.spr %>% rename(USAID = cafe, randomForest = comparison)
  
  summ.spr %>%
    group_by(diff) %>%
    summarize(count = n())
  
}

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
crn <- raster('../_RF/_run2/_results/_process/_mixed/RF_2Classes_unccurrent_uga.tif')
c30 <- raster('../_RF/_run2/_results/_process/_mixed/RF_2Classes_unc_2030_uga.tif')
c50 <- raster('../_RF/_run2/_results/_process/_mixed/RF_2Classes_unc_2050_uga.tif')

prd <- shapefile('../_data/_shp/_prd/uga1_productionDistricts.shp')
prd.sf <- st_as_sf(prd)
lbls <- data_frame(number = 1:6, type = c('Unsuitable', 'Unsuitable', 'Robusta', 'Arabica', 'Limitations', 'Mixed'))

# Visualization
prd.sf %>% dplyr::select(Coffee) %>% plot()
unique(prd@data$Coffee)
plot(crn)

# Zonal Statistics
prd@data$ID <- 1:nrow(prd@data)
msk <- rasterize(prd, crn, field = 'ID')





# End
summ.spr %>% 
  mutate(cafe = case_when(Robusta == NA_integer_  ~ Arabica, 
                          Robusta !=NA_integer_ ~ Robusta))
