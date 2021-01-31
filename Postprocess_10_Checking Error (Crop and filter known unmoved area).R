#### This codes will be used to crop the unmove areas for checking error proposes#### 
## load relevant packages

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(zip)
library(utils)


## The work folder

workfolder <- "E:/Victoria University of Wellington/Venus Project/Tif_whole_scene_2018_2020 UTM"
outputforlder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Imagery of unmoved area"


## load all filenames in the 3 band composite images (tiff)

filenames <- list.files(path = workfolder, pattern = "*.tif")
filenames <- filenames[nchar(filenames)==12]

## crop_area and save to new folder.

tasman_village <- readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/shapfile location of unmoved area/tasman_village_utm.shp")

for (i in 1:length(filenames)) {
  
  # read raster
  
  image <-  stack(paste0(workfolder, "/", filenames[i]), bands=3)
  
  # crop study area 
  
  image_cropped <- crop(image, tasman_village, snap="in")
  writeRaster(image_cropped, filename=file.path(outputforlder, filenames[i]), format="GTiff", overwrite=TRUE)
  
  
  print(i) # to see the progress
}



## The previous lines are for all imagery
## we will only copy the same dates as used studying in the Tasman and Franz Josef glacier only.

## Tasman

tasman_folder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM"
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Imagery of unmoved area"
outputforlder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Tasman"



tasman_files <- list.files(tasman_folder, pattern = ".tif")
tasman_files <- tasman_files[nchar(tasman_files)==12]

filenames <- list.files(workfolder, pattern = ".tif")
filenames <- filenames[nchar(filenames)==12]


for (i in 1: length(tasman_files)) {
  
  file.copy(from=paste0(workfolder, "/", tasman_files[i]), to=paste0(outputforlder, "/", tasman_files[i]))
  print(i)
  
  }
  
  
## franz Josef
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Imagery of unmoved area"
outputforlder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Franz Josef"
franz_files <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Franz_Josef_Single_Band_Jul_2019_Mar_2020"
franz_files <- list.files(franz_files, pattern = ".tif")
franz_files <- franz_files [nchar(franz_files )==12]

for (i in 1: length(franz_files)) {
  
  file.copy(from=paste0(workfolder, "/", franz_files[i]), to=paste0(outputforlder, "/", franz_files[i]))
  print(i)
  
}

