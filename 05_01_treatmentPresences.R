
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(cclust)
require(outliers)
require(multcomp)
require(magrittr)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
source('05_00_FunctionsRFclustering.R')
lbls <- data.frame(clase = c('robusta', 'arabica'), value = 1:2)
run <- '_run1'

# Functions to use
dup_cell <- function(path_mask, path_df){
  mask <- raster(path_mask)
  df <- read.csv(path_df)
  cellNum <- raster::extract(mask, df[,c('Lon', 'Lat')], cellnumbers = T) 
  cells <- xyFromCell(mask, cellNum[,'cells'])
  dupvec <- duplicated(cells[,c('x', 'y')])
  occ_rmDupCell <- tbl_df(df[!dupvec,])
  occ_DupCell <- tbl_df(df[dupvec,])
  return(list(occ_rmDupCell, occ_DupCell))
}
contrastPub <- function(descriptors, grouped, biomains, biolabels, pathGraph, nameGraph, Nrow, Ncol, width, height){   
  png((filename = paste(pathGraph, nameGraph, sep = '/')), width = width, height = height, res = 120)
  par(mfrow = c(Nrow, Ncol))
  for(i in 1:length(descriptors)){
    formula <- as.formula(paste(descriptors[i], "~ cluster"))
    Anov    <- aov(formula = formula, data = grouped)
    cont    <- glht(Anov, mcp(cluster = 'GrandMean'))
    plot(cont, xlab = NA, sub = NULL, main = NA)
    title(main = biomains[i], line = 1.5)
    title(ylab = 'Group', line = 2.5)
    # title(xlab = paste(biolabels[i], '\n', '?', round(mean(grouped[,descriptors[i]]), digits = 1)), line = 3) 
    # title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1), line = 3)) 
    # print(round(mean(extract2((grouped[,descriptors[i]]), 1)), 1))
    title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1))) 
  }  
  dev.off()
}

# Load data
lyrs <- list.files('../_data/_asc/_wc/_current', full.names = T, pattern = '.asc$') %>% 
  grep('bio', ., value = T) %>%
  mixedsort() %>% 
  stack()
msk <- lyrs[[1]] * 0
uga <- getData('GADM', country = 'UGA', level = 0)
uga_adm1 <- getData('GADM', country = 'UGA', level = 1)
occ <- read.csv('../_data/_tbl/occ_uga.csv') 
writeRaster(msk, '../_data/_tif/_base/mask_30sec.tif', overwrite = TRUE)

# Removing duplicate
dup <- dup_cell(path_mask = '../_data/_tif/_base/mask_30sec.tif', path = '../_data/_tbl/occ_uga.csv')
occ <- dup[[1]]
write.csv(occ, '../_data/_tbl/occ_rmDup.csv', row.names = FALSE)

# Visualization
plot(uga)
points(occ$Lon, occ$Lat, cex = 0.7, col = 'red', pch = 16)

# Extract values
occ.swd <- raster::extract(lyrs, occ[,1:2]) %>% cbind(occ, .)
dim(occ.swd)

# Outliers
norm <- scores(occ.swd[,4:ncol(occ.swd)], 'z')
norm_na <- norm
norm_na[abs(norm_na)>3.5]  <- NA 
normpoints <- cbind(occ[,1:3], norm_na) %>%
  na.omit() %>% 
  tbl_df() 
occ <- raster::extract(lyrs, normpoints[,1:2]) %>% cbind(normpoints[,1:3], .) %>% tbl_df() 
write.csv(occ, '../_data/_tbl/occ_swd_rmOtl.csv', row.names = F)

# SRTM
srtm <- raster('../_data/_tif/_base/srtm.tif')
occsrtm <- raster::extract(srtm, occ[,1:2]) %>% cbind(occ[,1:3], srtm = .) %>% tbl_df()

# Count points by polygon
# prodCocoa <- read_csv('_tables/_FAO/summ_2000_2010.csv')
# bias <- inner_join(prodCocoa, cntOcc, by = c('Area' = 'NAME_0')) %>%
#   mutate(prob = count/Produccion)
# bias.shape <- inner_join(adm0, bias, by = c('NAME_ENGLI' = 'Area'))
# bias.shape <- as(bias.shape, 'Spatial')
# bias.raster <- rasterize(bias.shape, lyrs[[1]], field = 'prob', fun = 'mean')#bias.raster <- poly_to_raster(bias.shape, lyrs[[1]], copy.data = TRUE)

# Manual Cluster
occ <- occ %>% mutate(cluster = ifelse(Coffee == 'robusta', 1, 2))
grouped <- occ %>% dplyr::select(-c(Lon, Lat, Coffee)) %>% mutate(cluster = as.factor(cluster))
labelRF <- occ %>% pull(24)
no.clusters <- 2
saveRDS(grouped, '../_rds/grouped.rds')
save(occ, no.clusters, grouped, labelRF, file = '../_rds/clustereddata.rData')

# Contrast plot
descriptors <- colnames(occ[4:(ncol(occ)-1)])

# Temperature (centigrados)
centigrados <- c(1, 2, 5, 6, 7, 8, 9, 10, 11)
desc_temp_grados <- names(grouped[,centigrados])
grouped_temp_grados <- grouped[centigrados] %>% 
  cbind(., grouped['cluster']) %>%
  mutate(bio_1 = bio_1/10,
         bio_2 = bio_2/10,
         bio_5 = bio_5/10,
         bio_6 = bio_6/10,
         bio_7 = bio_7/10,
         bio_8 = bio_8/10,
         bio_9 = bio_9/10,
         bio_10 = bio_10/10,
         bio_11 = bio_11/10) 
biolabels_temp_grados <- rep("°C", length(centigrados))
biomains_temp_grados <- c("Annual mean temp", "Mean diurnal range", "Max Temp of Warmest Month", "Min temp of coldest month", 
                          "Temp annual range", "Mean temp of wettest quarter", "Mean temp of driest quarter", "Mean temp of warmest quarter", 
                          "Mean temp of coldest quarter")
contrastPub(descriptors = desc_temp_grados, grouped = grouped_temp_grados, biomains = biomains_temp_grados,
            biolabels = biolabels_temp_grados, pathGraph = paste0('../_png/_figures/_contrastPub/', run), 
            nameGraph = 'Contrast Plot Temp Grados.png', Nrow = 3, Ncol = 3, width = 1200, height = 1000)

# Temperature (no centigrados)
noCentigrados <- c(3, 4)
desc_temp_nogrados <- names(grouped[noCentigrados])  
grouped_temp_nogrados <- grouped[noCentigrados] %>% cbind(., grouped['cluster'])
biolabels_temp_nogrados <- rep("-", length(noCentigrados)) 
biomains_temp_nogrados <- c('Isothermality (bio 2/bio 7) * 100', 'Temperature seasonality (sd * 100)')
contrastPub(descriptors = desc_temp_nogrados, grouped = grouped_temp_nogrados, biomains = biomains_temp_nogrados,
            biolabels = biolabels_temp_nogrados, pathGraph = paste0('../_png/_figures/_contrastPub/', run), 
            nameGraph = 'Contrast Plot Temp No Grados.png', Nrow = 1, Ncol = 2, width = 900, 450) 

# Precipitation
desc_ppt <- descriptors[c(12:20)]
biomains_ppt <- c('Annual prec', 'Prec of wettest month', 'Prec of driest month', 'Prec seasonality', 'Prec of wettest quarter', 'Prec of driest quarter', 'Prec of warmest quarter', 'Prec of coldest quarter', 'Number of consecutive months \n less 100 mm')
grouped_ppt <- grouped[,c(desc_ppt, 'cluster')]
biolabels_ppt <- c(rep('mm', 3), '-', rep('mm',4),'Number months')

contrastPub(descriptors = desc_ppt, grouped = grouped_ppt, biomains = biomains_ppt,
            biolabels = biolabels_ppt, pathGraph = paste0('../_png/_figures/_contrastPub/', run), 
            nameGraph = 'Contrast Plot Precipitation.png', Nrow = 3, Ncol = 3, width = 1000, height = 800)

