
# R options
options(warn = -1)
options(scipen = 999)

# load packages
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(grid))
suppressMessages(library(gtable))

# Initial setup
setwd('W:/_ugandaCoffee/_data/_tbl/_climate/')
source('W:/_ugandaCoffee/_codes/04_00_dual_axis_function.R')

# Current data
# Metrics <- c('min', 'mean', 'max')
metrics <- 'mean'
currentInfo <- lapply(metrics, function(x){
  file <- list.files(path = './_current', pattern = x, full.names = T)
  data <- read.csv(file)
  data <- data[, c("zone", "prec_1", "prec_10", "prec_11", "prec_12", "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8", "prec_9",
                   "tmax_1", "tmax_10", "tmax_11", "tmax_12", "tmax_2", "tmax_3", "tmax_4", "tmax_5", "tmax_6", "tmax_7", "tmax_8", "tmax_9",
                   "tmean_1", "tmean_10", "tmean_11", "tmean_12", "tmean_2", "tmean_3", "tmean_4", "tmean_5", "tmean_6", "tmean_7", "tmean_8", "tmean_9",
                   "tmin_1", "tmin_10", "tmin_11", "tmin_12", "tmin_2", "tmin_3", "tmin_4", "tmin_5", "tmin_6", "tmin_7", "tmin_8", "tmin_9")]
  data$varID  <- x
  data$gcm    <- 'Present'
  data$Period <- 'current'
  return(data)
})
currentInfo <- do.call(rbind, currentInfo)

# Future 2030
gcmList     <- list.files(path = './_2030/', pattern = metrics[1], full.names = F)
gcmList     <- gsub(pattern = 'mean_', replacement = '', x = gcmList, fixed = T)
gcmList     <- gsub(pattern = '_2030.csv', replacement = '', x = gcmList, fixed = T)
gcmList     <- gsub(pattern = '.csv', replacement = '', x = gcmList, fixed = T) # ADD FABIO
fut2030Info <- lapply(metrics, function(x){
  process   <- lapply(gcmList, function(y){
    file    <- list.files(path = './_2030/', pattern = x, full.names = T)
    file    <- file[grep(pattern = paste('_', y, '_2030', sep = ''), x = file)]
    data    <- read.csv(file)
    data    <- data[, c("zone", "prec_1", "prec_10", "prec_11", "prec_12", "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8", "prec_9",
                        "tmax_1", "tmax_10", "tmax_11", "tmax_12", "tmax_2", "tmax_3", "tmax_4", "tmax_5", "tmax_6", "tmax_7", "tmax_8", "tmax_9",
                        "tmean_1", "tmean_10", "tmean_11", "tmean_12", "tmean_2", "tmean_3", "tmean_4", "tmean_5", "tmean_6", "tmean_7", "tmean_8", "tmean_9",
                        "tmin_1", "tmin_10", "tmin_11", "tmin_12", "tmin_2", "tmin_3", "tmin_4", "tmin_5", "tmin_6", "tmin_7", "tmin_8", "tmin_9")]
    data$varID <- x
    data$gcm <- y
    data$Period <- '2030'
    return(data)
  })
  process <- do.call(rbind, process)
  return(process)
})
fut2030Info <- do.call(rbind, fut2030Info) #Join in only one table
head(fut2030Info)

# Future 2050
fut2050Info <- lapply(metrics, function(x){
  process <- lapply(gcmList, function(y){
    file <- list.files(path = './_2050/', pattern = x, full.names = T)
    file <- file[grep(pattern = paste('_', y, '_2050', sep = ''), x = file)]
    data <- read.csv(file)
    data <- data[, c("zone", "prec_1", "prec_10", "prec_11", "prec_12", "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8", "prec_9",
                     "tmax_1", "tmax_10", "tmax_11", "tmax_12", "tmax_2", "tmax_3", "tmax_4", "tmax_5", "tmax_6", "tmax_7", "tmax_8", "tmax_9",
                     "tmean_1", "tmean_10", "tmean_11", "tmean_12", "tmean_2", "tmean_3", "tmean_4", "tmean_5", "tmean_6", "tmean_7", "tmean_8", "tmean_9",
                     "tmin_1", "tmin_10", "tmin_11", "tmin_12", "tmin_2", "tmin_3", "tmin_4", "tmin_5", "tmin_6", "tmin_7", "tmin_8", "tmin_9")]
    data$varID <- x
    data$gcm <- y
    data$Period <- '2050'
    return(data)
  })
  process <- do.call(rbind, process)
  return(process)
})
fut2050Info <- do.call(rbind, fut2050Info)

# all info in just one dataset
all_data <- rbind(currentInfo, fut2030Info, fut2050Info); rm(currentInfo, fut2030Info, fut2050Info, gcmList, metrics)
precData <- all_data[,c('zone', 'varID', 'gcm', 'Period', paste('prec_', 1:12, sep=''))] #data organization
tminData <- all_data[,c('zone', 'varID', 'gcm', 'Period', paste('tmin_', 1:12, sep=''))]
tmeaData <- all_data[,c('zone', 'varID', 'gcm', 'Period', paste('tmean_', 1:12, sep=''))]
tmaxData <- all_data[,c('zone', 'varID', 'gcm', 'Period', paste('tmax_', 1:12, sep=''))]

precData        <- precData %>% gather(month, prec, prec_1:prec_12) #uso of gather
precData$month  <- as.numeric(as.character(gsub(pattern = 'prec_', replacement = '', x = precData$month)))

tminData        <- tminData %>% gather(month, tmin, tmin_1:tmin_12)
tminData$month  <- as.numeric(as.character(gsub(pattern = 'tmin_', replacement = '', x = tminData$month)))

tmeaData        <- tmeaData %>% gather(month, tmean, tmean_1:tmean_12)
tmeaData$month  <- as.numeric(as.character(gsub(pattern = 'tmean_', replacement = '', x = tmeaData$month)))

tmaxData        <- tmaxData %>% gather(month, tmax, tmax_1:tmax_12)
tmaxData$month  <- as.numeric(as.character(gsub(pattern = 'tmax_', replacement = '', x = tmaxData$month)))

all_data2       <- Reduce(function(...) merge(..., by=c('zone', 'varID', 'gcm', 'Period', 'month'), all.x=TRUE), list(precData, tminData, tmeaData, tmaxData))
rm(precData, tminData, tmeaData, tmaxData)
all_data2$month <- factor(x = all_data2$month, levels = 1:12, ordered = TRUE) #conversion to factor to month

# summarize future information by period
precData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(mean(prec))
names(precData)[length(precData)] <- 'prec' #change header
tminData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(mean(tmin))
names(tminData)[length(tminData)] <- 'tmin'
tmeaData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(mean(tmean))
names(tmeaData)[length(tmeaData)] <- 'tmean'
tmaxData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(mean(tmax))
names(tmaxData)[length(tmaxData)] <- 'tmax'

# summarize future information by period sd
sdprecData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(sd(prec))
names(sdprecData)[length(sdprecData)] <- 'sd_prec'
sdprecData$sd_prec[sdprecData$Period=='current'] <- 0

#all_data3       <- Reduce(function(...) merge(..., by=c('zone', 'varID', 'Period', 'month'), all.x=TRUE), list(precData, tminData, tmeaData, tmaxData))
all_data3       <- Reduce(function(...) merge(..., by=c('zone', 'varID', 'Period', 'month'), all.x=TRUE), list(precData, tminData, tmeaData, tmaxData, sdprecData))
#rm(precData, tminData, tmeaData, tmaxData)
rm(precData, tminData, tmeaData, tmaxData, sdprecData)
all_data3$month <- gsub(pattern = 10, replacement = 'Oct', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 11, replacement = 'Nov', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 12, replacement = 'Dec', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 1, replacement = 'Jan', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 2, replacement = 'Feb', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 3, replacement = 'Mar', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 4, replacement = 'Apr', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 5, replacement = 'May', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 6, replacement = 'Jun', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 7, replacement = 'Jul', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 8, replacement = 'Aug', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 9, replacement = 'Sep', x = all_data3$month, fixed = T)
all_data3$month  <- factor(all_data3$month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)
all_data3$Period <- gsub(pattern = 'current', replacement = 'Current', x = all_data3$Period)
all_data3$Period <- factor(x = all_data3$Period, levels = c('Current', '2030', '2050'), ordered = T)

# temperature in original units
all_data3$tmin  <- all_data3$tmin/10
all_data3$tmean <- all_data3$tmean/10
all_data3$tmax  <- all_data3$tmax/10

# load script to do dual axis
max_zone_1 <- all_data3[all_data3$zone & all_data3$varID=='mean',] %>%
  mutate(val_up = prec + sd_prec) %>%
  summarise(max(val_up))
max_zone_temp <- all_data3[all_data3$zone & all_data3$varID=='mean',] %>%
  mutate(val_up = tmax) %>%
  summarise(max(val_up))
all_data4 <- all_data3 %>% filter(Period %in% c('Current', 2030))
df <- filter(all_data4, Period == 'Current') %>% mutate(sd_prec = all_data4[all_data4$Period == '2030',]$sd_prec)
all_data4
df_prec <- filter(all_data4, Period == '2030') %>%
  mutate(max = prec + sd_prec,
         min = prec - sd_prec)
x <- dplyr::select(df_prec, month, max, min, prec)
x <- gather(data = x, key = var, Values, -month)
x <- x %>%
  mutate(zone = 1,
         Period = 2030) %>% 
  dplyr::select(zone, Period, month, var, Values)
x$var <- gsub('prec', 'mean', x$var)

gcms_30 <- filter(all_data2, Period == '2050') %>% tbl_df()
gcms_30$month <- gsub(pattern = 10, replacement = 'Oct', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 11, replacement = 'Nov', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 12, replacement = 'Dec', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 1, replacement = 'Jan', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 2, replacement = 'Feb', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 3, replacement = 'Mar', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 4, replacement = 'Apr', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 5, replacement = 'May', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 6, replacement = 'Jun', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 7, replacement = 'Jul', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 8, replacement = 'Aug', x = gcms_30$month, fixed = T)
gcms_30$month <- gsub(pattern = 9, replacement = 'Sep', x = gcms_30$month, fixed = T)
gcms_30$month  <- factor(gcms_30$month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)

gcms_30_median <- gcms_30 %>%
  group_by(month) %>%
  summarize(median = median(prec))

# write.csv(df_prec, 'D:/values_climatograma.csv', row.names = F)
# Zone 1
#gg <- ggplot(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = prec)) + geom_bar(aes(fill = Period), stat = "identity", position = "dodge")
# gg <- ggplot(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = prec)) + 
#         geom_bar(aes(fill = Period), stat = "identity", position = "dodge") + 
#         geom_errorbar(aes(ymin = prec - sd_prec, ymax = prec + sd_prec, group = Period), width=.2, position=position_dodge(.9))
# gg <- ggplot(data = df[df$zone==1 & df$varID=='mean',], aes(x = month, y = prec)) + 
#         geom_bar(aes(fill = Period), stat = "identity", position = "dodge") + 
#         # geom_errorbar(data = df_prec, aes(ymin = prec - sd_prec, ymax = prec + sd_prec, group = Period), width=.2, position=position_dodge(.9)) +
#         geom_point(data = x, aes(x = month, y = Values, fill = as.character(Period))) 
gg <- ggplot(data = df[df$zone==1 & df$varID=='mean',], aes(x = month, y = prec)) + 
  geom_bar(stat = "identity", position = "dodge", colour = '#99CA3C', fill = '#99CA3C') + 
  # geom_point(data = x, aes(x = month, y = Values, fill = as.character(Period)), colour = 'black')
  geom_point(data = gcms_30, aes(x = month, y = prec, fill = as.character(Period)), colour = 'black') +
  geom_point(data = gcms_30_median, aes(x = month, y = median, size = 13), colour = 'black')
gg <- gg + theme_bw() + xlab('') + ylab('Rainfall (mm)')
gg <- gg + theme(legend.position = "none") + labs(fill = '') #top
gg <- gg + scale_y_continuous(limits = c(0, 300), breaks = c(50,100,150,200,250,300)) #A?adido por Fabio
gg <- gg + theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(),
                 panel.grid.minor.y = element_blank(),
                 panel.border = element_blank())
gg <- gg + ggtitle("At coffee presences") + theme(panel.grid.major = element_line())#theme(panel.grid.major = element_line(colour = "grey"))
# gg <- gg + scale_fill_manual(values = c('#99CA3C', '#CC7D2E', '#295582'))
# gg <- gg + scale_fill_manual(values = c('#F89838'))

# gg2 <- ggplot(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = tmean)) + 
#           geom_line(aes(colour = Period, group = Period))
# gg2 <- gg2 + geom_line(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = tmin, colour = Period, group = Period), linetype = 2)
# gg2 <- gg2 + geom_line(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = tmax, colour = Period, group = Period), linetype = 2)

gg2 <- ggplot(data = all_data4[all_data4$zone==1 & all_data4$varID=='mean',], aes(x = month, y = tmean)) + 
  geom_line(aes(colour = Period, group = Period))
gg2 <- gg2 + geom_line(data = all_data4[all_data4$zone==1 & all_data4$varID=='mean',], aes(x = month, y = tmin, colour = Period, group = Period), linetype = 2)
gg2 <- gg2 + geom_line(data = all_data4[all_data4$zone==1 & all_data4$varID=='mean',], aes(x = month, y = tmax, colour = Period, group = Period), linetype = 2)

gg2 <- gg2 + theme_bw() + xlab('') + ylab(expression("Temperature ("*~degree*C*")"))
gg2 <- gg2 + scale_y_continuous(limits = c(0, 32)) 
gg2 <- gg2 + theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid = element_blank(),
                   panel.border = element_blank(),
                   legend.position = 'top')
# panel.grid.major = element_blank()) #colour = 'green'
gg2 <- gg2 + scale_color_manual(values = c('#99CA3C', '#009933', '#295582'))
gg2 <- gg2 + theme(legend.position = 'none')
# png(filename = '../_png/presen.png', width = 9, height = 8, units = 'in', res = 300)
png(filename = 'climatograma.png', width = 9, height = 8, units = 'in', res = 300)
ggplot_dual_axis(gg, gg2, "y")
dev.off()

