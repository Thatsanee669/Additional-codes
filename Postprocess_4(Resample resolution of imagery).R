### The following codes will be used to  resize of the imagery from 5 meter to 10 meters and resample the values.###
### The aim of this task is to do experiental wether 10 meter resolution can still capture the short-lived variations caused by rainfalll ##
### The Venus Satellite will continue functioning until 2021 only. the larger satellites like sentinel 2A and 2 B would have a bit lower resolution ###
### we would like to test whether such lower resolution can still capture short-lived variations caused by Rainfall.###



library(tidyverse)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(dplyr)


## Set working directory

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Study Areas/Tasman/Tif_UTM/Tasman_Single_Band_Good_IM_2018-2020")
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Study Areas/Tasman/Tif_UTM/Tasman_Single_Band_Good_IM_2018-2020"

## Listt all the files in the workfolder (single band images - Tasman)

filenames <- list.files(workfolder, pattern = "*.tif")
filenames



for (i in 1 : length(filenames)) {
  
  
  # load imagery
  
  image <- raster(filenames[i])
  
  # Create a reference raster with new resolution 10, 20, 30 meters
  
  
  s_10 <- raster(crs='+proj=utm +zone=59 +south +datum=WGS84 +units=m +no_defs', xmn=434181.5, xmx=437661.5, ymn=5165322, ymx=5172417, resolution=c(10,10))
  s_20 <- raster(crs='+proj=utm +zone=59 +south +datum=WGS84 +units=m +no_defs', xmn=434181.5, xmx=437661.5, ymn=5165322, ymx=5172417, resolution=c(20,20))
  s_30 <- raster(crs='+proj=utm +zone=59 +south +datum=WGS84 +units=m +no_defs', xmn=434181.5, xmx=437661.5, ymn=5165322, ymx=5172417, resolution=c(30,30))
 
  # resample values from the original imagery
   
  s_10 <- resample(image, s_10, method = 'bilinear') 
  s_20 <- resample(image, s_20, method = 'bilinear') 
  s_30 <- resample(image, s_30, method = 'bilinear') 
  
  # Write the ouputs out
  
  writeRaster(s_10, paste0(workfolder, "/Single_Band 10 meters/", filenames[i]),
              format="GTiff", overwrite=TRUE)
  writeRaster(s_20, paste0(workfolder, "/Single_Band 20 meters/", filenames[i]),
              format="GTiff", overwrite=TRUE)
  writeRaster(s_30, paste0(workfolder, "/Single_Band 30 meters/", filenames[i]),
              format="GTiff", overwrite=TRUE)
  
  print(i) # just to see the progress.
}






## Listt all the files in the workfolder (single band images - Franz Josef)


setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Study Areas/Franz Josef/Tif_UTM/Franz_Josef_Single_Band_Good_IM_2018-2020")
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Study Areas/Franz Josef/Tif_UTM/Franz_Josef_Single_Band_Good_IM_2018-2020"


filenames <- list.files(workfolder, pattern = "*.tif")
filenames



for (i in 1 : length(filenames)) {
  
  
  # load imagery
  
  image <- raster(filenames[i])
  
  # Create a reference raster with new resolution 10, 20, 30 meters
  
  
  s_10 <- raster(crs=crs(image), ext=extent(image), resolution=c(10,10)) # use 'ext' or xmn/xmx, ymn/ymx
  s_20 <- raster(crs=crs(image), ext=extent(image), resolution=c(20,20)) # use 'ext' or xmn/xmx, ymn/ymx
  s_30 <- raster(crs=crs(image), ext=extent(image), resolution=c(30,30)) # use 'ext' or xmn/xmx, ymn/ymx
  
  # resample values from the original imagery
  
  s_10 <- resample(image, s_10, method = 'bilinear') 
  s_20 <- resample(image, s_20, method = 'bilinear') 
  s_30 <- resample(image, s_30, method = 'bilinear') 
  
  # Write the ouputs out
  
  writeRaster(s_10, paste0(workfolder, "/Single_Band 10 meters/", filenames[i]),
              format="GTiff", overwrite=TRUE)
  writeRaster(s_20, paste0(workfolder, "/Single_Band 20 meters/", filenames[i]),
              format="GTiff", overwrite=TRUE)
  writeRaster(s_30, paste0(workfolder, "/Single_Band 30 meters/", filenames[i]),
              format="GTiff", overwrite=TRUE)
  
  print(i) # just to see the progress.
}














