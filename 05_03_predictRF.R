
# Load libraries
library(tidyverse)
library(raster)
library(rgdal)
library(cclust)
library(outliers)
library(dismo)
library(gtools)
library(multcomp)
library(sp)
library(rgeos)
library(outliers)  
library(FactoMineR)
library(pROC)
library(randomForest)
library(stringr)

# Load files
options(scipen = 999)
cat('\f')
rm(list = ls())
run <- '_run1'

# Load data
load('../_rds/clustereddata.rData')
load(paste0('../_RF/_run1/_models/rflist_2.rdata'))
rff <- do.call(randomForest::combine, rflist)

gcmlist <- 'current'
ar5biofolder <- '../_data/_asc/_wc/_current'
resultsfolder <- paste0('../_RF/_run1/_results/_raw') 
modelfolder <- paste0('../_RF/_run1/_models')
gcm <- gcmlist
toMatch <- 'bio'
gcmfiles <- list.files(ar5biofolder, full.names = TRUE, pattern = ".asc$") %>% 
  mixedsort() %>%
  grep('bio', ., value = T)
climatelayers <- stack(gcmfiles)
climatevalues <- data.frame(getValues(climatelayers))
NumberOfClusters <- 2

# Other way
rasterProbs <- predict(rff, climatevalues, type = 'prob') 
rasterProbs_na <- na.omit(rasterProbs)
sum_rasterProbs_na <- apply(rasterProbs_na, 1, sum)
head(rasterProbs_na)
head(sum_rasterProbs_na)

rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
uncertainty <- apply(rasterProbs, 1, max)  

rasterRFprob <- climatelayers[[1]]
values(rasterRFprob) <- rasterRF 

rasterRFuncertainty <- climatelayers[[1]]
values(rasterRFuncertainty) <- uncertainty 

rasterRF <- max.col(rasterProbs, 'first')
rasterRFclass <- climatelayers[[1]]
values(rasterRFclass) <- rasterRF

writeRaster(rasterRFclass, paste0('../_RF/_run1/_results/_raw/_current/RF_5Clust_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFprob, paste0('../_RF/_run1/_results/_raw/_current/RF_5Prob_current.asc'), format = 'ascii', overwrite = T)
writeRaster(rasterRFuncertainty, paste0('../_RF/_run1/_results/_raw/_current/RF_5Unc_current.asc'), format = 'ascii', overwrite = T)


