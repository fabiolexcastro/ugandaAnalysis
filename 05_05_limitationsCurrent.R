
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(rgeos)
require(gtools)
require(stringr)

# Initial setup
rm(list = ls())
cat('\f')

# Load data
lyr.prb <- raster('../_RF/_run1/_results/_raw/_current/RF_5Prob_current.asc')
load('../_rds/clustereddata.rData')
occ <- occ

# Extract the probabilistic threshold
value <- raster::extract(lyr.prb, occ[,1:2])
min(value)
value <- value[!is.na(value)]
quantiles_05 <- quantile(value, seq(0,1,0.005))
png(filename = '../_png/_figures/th_prob_run1.png',  units = 'in', width = 9, height = 6, res = 100)
plot(quantiles_05, pch = 16)
dev.off()
quantiles_1 <- quantile(value, seq(0,1,0.01))
values_df <- as.data.frame(quantiles_05)
threshold <- as.numeric(subset(values_df, rownames(values_df) == '1.0%'))
save(threshold, file = '../_rData/_run1/treshold_prob.rData')

gcm <- 'current'
no.absenceclasses <- 2

limitations <- function(path_lyr_prob, path_lyr_clust, path_output, nameOutput, no.clusters, no.absenceclasses){
  require(raster)
  require(rgdal)
  require(tidyverse)
  
  lyr_prob  <- raster(path_lyr_prob)
  lyr_clust <- raster(path_lyr_clust)
  mtx_prob  <- matrix(c(0, threshold, 0, threshold, 1, 2), ncol = 3, byrow = T)
  mtx_clust <- matrix(c(0.5, no.absenceclasses + 0.5, 0, no.absenceclasses + 0.5, no.absenceclasses + no.clusters + 0.5, 1), nrow = 2, byrow = T)
  lyr_prob_rcl  <- raster::reclassify(lyr_prob, mtx_prob)
  lyr_clust_rcl <- raster::reclassify(lyr_clust, mtx_clust)
  diff <- lyr_prob_rcl - lyr_clust_rcl
  result <- lyr_clust
  result[which(diff[] == -1)] <- no.absenceclasses + no.clusters + 1
  result[which(diff[] == 2)]  <- no.absenceclasses + no.clusters + 1
  print('To Write')
  writeRaster(result, paste(path_output, nameOutput, sep = '/'))
}

limitations(path_lyr_prob = paste0('../_RF/_run1/_results/_raw/_current/RF_5Prob_current.asc'),
            path_lyr_clust = paste0('../_RF/_run1/_results/_raw/_current/RF_5Clust_current.asc'),
            path_output = paste0('../_RF/_run1/_results/_process'),
            nameOutput = paste0('RF_', no.clusters, 'Clust_lim_', gcm, '.asc'),
            no.absenceclasses = 2, 
            no.clusters = no.clusters)

limitations(path_lyr_prob = paste0('_RF/', run, '/_results/_raw/_current/RF_5Prob_current.asc'),
            path_lyr_clust = paste0('_RF/', run, '/_results/_raw/_current/RF_5Clust_current.asc'),
            path_output = paste0('_RF/', run, '/_results/_process/_percentil0_5'),
            nameOutput = paste0('RF_', no.clusters, 'Clust_lim_', gcm, '.asc'),
            no.absenceclasses = 2, 
            no.clusters = no.clusters)

