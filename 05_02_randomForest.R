

# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(cclust)
require(outliers)
require(multcomp)
require(magrittr)
require(randomForest)
require(dismo)
require(pROC)

# Initial setup
rm(list = ls())
options(scipen = 999)
run <- '_run1'
source('../_codes/05_00_FunctionsRFclustering.R')
myproj <- CRS('+proj=longlat +datum=WGS84')

# Functions to use
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  # occ = back_swd; nforest = 50; ntrees = 500; nVars = 8; nclasses = 2
  datRF_presences <- occ[,3:ncol(occ)] %>% as.data.frame()
  print(nrow(datRF_presences))
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  return(list(labelRF, clusterdata))
}

# Load data
load(paste0('../_rds/clustereddata.rData'))
modelfolder <- paste0('../_RF/', run, '/_models')
climatefolder <- '../_data/_asc/_wc/_current'

#Load climate data
toMatch <- 'bio'
listado <- list.files(climatefolder, full.names = T, pattern = '.asc$') %>% 
  mixedsort() %>%
  grep(paste0(toMatch, collapse = '|'), ., value = TRUE) %>%
  unique()
climatelayers <- stack(listado)
crs(climatelayers) <- myproj
occ_cluster <- occ
SPspecies <- SpatialPoints(occ_cluster[,1:2]) # SPspecies <- SpatialPoints(clusteredpresdata[,1:2]) #Data frame to Shp
crs(SPspecies) <- myproj

# Generation of background (pseudoabsences) - We used 1:1 (1 pseudoabsence for each presence)
msk <- climatelayers[[1]] * 0
speciescell <- raster::extract(msk, SPspecies, cellnumber = TRUE)
msk[speciescell[,1]]  <- NA 
numberofpresences <- nrow(occ)

back_swd <- randomPoints(msk, 1*numberofpresences) %>%as_data_frame() 
back_swd <- raster::extract(climatelayers, back_swd[,1:2]) %>% cbind(back_swd) %>% as.tibble() %>% dplyr::select(x, y, bio_1:bio_20)
write.csv(back_swd, '../_data/_tbl/bck_swd_run1.csv', row.names = FALSE)

# Cluster analysis to pseudoabsences
bckclust <- rf.clust(occ = back_swd, nforest = 50, ntrees = 500, nVars = 8, nclasses = 2)
datRF <- as.data.frame(back_swd[,3:ncol(back_swd)])
attach(datRF)
no.forests <- 50#raw = 25
no.trees <- 500
distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
no.absenceclasses <- 2
labelRF <- pamNew(distRF$cl1, no.absenceclasses)
detach(datRF)
classdata <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])

# Join presences and background
presvalue_swd  <- occ_cluster[,3:ncol(occ_cluster)] %>%
  cbind(pb = (occ_cluster$cluster + no.absenceclasses), .) %>%
  na.omit() %>%
  as.data.frame() %>%
  mutate(cluster = cluster + no.absenceclasses)
presvalue_swd <- dplyr::select(presvalue_swd, pb, bio_1:bio_20)
presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))

classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background
dim(classdata_2); dim(presvalue_swd)
allclasses_swd <- rbind(classdata_2, presvalue_swd)
unique(allclasses_swd$pb)

write.csv(allclasses_swd, '../_data/_tbl/all_clases_swd_run1.csv', row.names = FALSE)

# Model RF
samplesize  <- round(min(summary(as.factor(occ_cluster$cluster))) / 2, 0)
model1 <- as.formula(paste('factor(pb) ~', paste(paste('bio',c(1:20), sep='_'), collapse = '+', sep =' ')))
rflist <- vector('list', 50) 
auc <- vector('list', 50)
NumberOfClusters <- 2

for(repe in 1:50){ # 50 bosques
  
  print(repe)
  pressample <- list()
  
  for (i in 1:(NumberOfClusters+no.absenceclasses)){
    
    if(any(i==c(1:no.absenceclasses))) { 
      
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                     size = samplesize*NumberOfClusters/2/no.absenceclasses)
    } else {
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
    }
    pressample[[i]] <- allclasses_swd[rows,] 
  }
  
  species <- na.omit(do.call(rbind, pressample)) 
  head(species)
  Samplesplit <- sample(rownames(species)) 
  
  envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
  envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
  
  rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
  
  save(rfmodel, file = paste(modelfolder, '/', 'RF_' ,NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
  rflist[[repe]] <- rfmodel
  
  # AUC 
  predicted <- as.numeric(predict(rfmodel, envtest))
  observed <- as.vector(envtest[,'pb'])
  auc[[repe]] <- auc(observed, predicted) 
  rm(rfmodel)
  
  cat(auc[[repe]] ,'\n')
  
}

auc <- unlist(auc)
rff <- do.call(randomForest::combine, rflist)
importance <- as.data.frame(rff$importance)

save(rflist, file = paste(modelfolder, '/rflist_', NumberOfClusters, '.rdata', sep=''))
save(importance, file = paste0('../_rData/', run, '/importanceRF.rData'))
save(auc, file = paste0('../_rData/', run, '/aucRF_dist.rData'))
save(rff, file = paste0('../_rData/', run, '/rff_dist.rData'))


