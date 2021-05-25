#########################################################################################
###                          Run FFP on EML Tower 2017-2018                        ###
###                                Code by HGR 2/2021                                 ###
#########################################################################################

### Libraries ###########################################################################
library(sf)
library(raster)
library(data.table)
library(lubridate)
library(doParallel)
library(tidyverse)
#########################################################################################

### Load data ###########################################################################
final.17 <- read.csv('/scratch/hgr7/flux_tower_footprint/data_input/ffp_ready_2017.csv',
                     na.strings = '-999')
final.17 <- data.table(final.17)
#########################################################################################

### Run FFP #############################################################################
### set up data needed for ffp formatting
# point for ec tower location UTM zone 6N
ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
ec_sf <- st_sf(geometry = ec, crs = 32606)
rm(ec)

# raster template
karst_1 <- brick(stack("/scratch/hgr7/output/karst_combined_1_raster_final_1.tif",
                       "/scratch/hgr7/output/karst_combined_1_raster_final_2.tif",
                       "/scratch/hgr7/output/karst_combined_1_raster_final_3.tif"))
crs(karst_1) <- CRS('+init=epsg:32606')
mean.karst <- calc(karst_1, mean, na.rm = TRUE)

filenames <- list.files('/scratch/hgr7/int_output',
                        full.names = TRUE,
                        pattern = '^mtopo15.+9km')

mtopo15 <- brick(stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_1\\.tif$'))],
                     filenames[which(str_detect(filenames, pattern = 'mtopo15.+_2\\.tif$'))],
                     filenames[which(str_detect(filenames, pattern = 'mtopo15.+_3\\.tif$'))]))
mean.mtopo <- calc(mtopo15, mean, na.rm = TRUE)
karst.mtopo.brick <- brick(mean.karst, mean.mtopo)

rm(filenames, karst_1, mean.karst, mtopo15, mean.mtopo)

# load ffp functions
source('/scratch/hgr7/code/flux_tower_footprint/calc_footprint_FFP_EML.R')
source('/scratch/hgr7/code/flux_tower_footprint/flux_footprint_iteration_function.R')


# # run the function on 2017
# Define how many cores you want to use
UseCores <- 28

# split up data
data.rows <- nrow(final.17)
chunk.length <- ceiling(data.rows/UseCores)
final.17.list <- list()
for (i in 1:UseCores) {
  start.row <- (i - 1)*chunk.length + 1
  if (i < UseCores) {
    end.row <- i*chunk.length
  } else {
    end.row <- nrow(final.17)
  }
  final.17.list[[i]] <- final.17[start.row:end.row]
}

cl <- makeCluster(UseCores, outfile = '/scratch/hgr7/flux_tower_footprint/ffp_raw_output/log.txt')
registerDoParallel(cl)

ffp.df.2017 <- foreach(i=1:UseCores, .combine = rbind) %dopar% {
  # load libraries
  library(sf)
  library(raster)
  library(tidyverse)
  
  # calculate ffp
  calc.ffp.loop(final.17.list[[i]], ec_sf, karst.mtopo.brick, seq(10, 90, 10))
  
}

stopCluster(cl)

write.csv(ffp.df.2017,
          '/scratch/hgr7/flux_tower_footprint/ffp_2017.csv',
          row.names = FALSE)
#########################################################################################