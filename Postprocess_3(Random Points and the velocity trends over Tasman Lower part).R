
library(tidyverse)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(dplyr)


# Set working directory
setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Target_Area_Shapefile/Bbox/Study Area WGS 1984")



Franz_josef <- readOGR("Franz_Josef_BBOX.shp")
Tasman <- readOGR("Tasman_BBOX.shp")

# randomPoints_Tasman <- spsample(Tasman,n=12,"random")

# writeOGR(randomPoints_Tasman , ".", "filename", driver="ESRI Shapefile") 
