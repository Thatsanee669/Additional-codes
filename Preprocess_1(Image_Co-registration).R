### One concern over using Venus imagery for monitoring glacier velocity is the registration ###
### Every time satellite taking photos, its camera angle and location on the orbit can be slightly different ###
### Co-registration technique is a well-known technique use to change the mis-registered imagery or some time called ###
### "Slave Image" by comparison with known well-registered imagery which is called " Master Image" ###
### This codes will test co-registration among Venus imagery for some period of time July 2019 to March 2020 ###

## Load the relevant library
#library(sf)
library(sp)
library(raster)
library(ggplot2)
library(rgdal)
library(gdalUtils)
library(reshape2)
library(RStoolbox)
library(gdalUtilities)


## set work and output folders.

workfolder  <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM"
outputfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Coregistered_with image 1"

setwd(workfolder)

## list all file names

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==12] # only this length/ filter other junk files.

## read shapfie of the study area.
## when performing image co-registration, the pixel of the imagery will shift abit, and may exeed the study area.
## Before writing the co-registered image, will crop it first. To make sure that its extent is aligned well when measuring velocity. 

tasman <- readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/BBOX/Tasman_UTM.shp")

## Use for loop to do it for all. by comparing with the first image (good image)
for (i in 1: (length(filenames) - 1)) {
  
  j <- i + 1
  
  master <- raster(filenames[1])
  
  slave <- raster(filenames[j])
  
  # co-registration
  
  coreg <- coregisterImages(slave, master = master,
                            nSamples = 1000, reportStats = TRUE)
  
  co <- coreg$coregImg
  
  # the extent of the new raster may move abit based on coregistration so we will crop only the same extent as our polygon.
  
  image_cropped <- crop(co, tasman, snap="in")
  
  # write the output
  
  writeRaster(image_cropped, filename=file.path(outputfolder, substring(filenames[j], 1, 8)), format="GTiff", overwrite=TRUE)
  
  
  print(i)
  
}


