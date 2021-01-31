
### This codes will be use to do preprocessing Satellite imagery from Venus satellite in the older format file.### 
### Since November 2019 onward, the Venus imagery has been stored in different format those will use different codes.### 
## load relevant packages
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(zip)
library(utils)
## Set working directory
setwd("H:/Satellite Imagery/Level 1 C")
## List all the zip files in the folder 
zipfiles <- list.files(path = "./Venus 2018_2019 Old Format", pattern = "*.zip")
## This is to unzip the file - old format of the files 2018 - 2019 and save them in the folder call "Unzipped Old Formatt"
for (i in 1:length(zipfiles)) {
  # zip::unzip(zipfile = paste0("./Venus 2019_2020 Old Format/", zipfiles[i]), exdir = "./Unzipped Old Format")
  untar(tarfile=paste0("./Old Format/", zipfiles[i]), files = NULL, list = FALSE, exdir = "./Unzipped Old Format",
        compressed = NA, extras = NULL, verbose = FALSE,
        restore_times =  TRUE,
        support_old_tars = Sys.getenv("R_SUPPORT_OLD_TARS", FALSE),
        tar = Sys.getenv("TAR"))
  print(i)
  }
# create a composited image containning 3 bands : 11, 7 and 4. This is only from 2018 to 2019  and are in the old format. 
# this 3 composite band will be converted into JPG file later which will be used for analyzing in GIV software. 
for (i in 1:length(zipfiles)) {
  
  new_name <- substring(zipfiles[i], 7, 14)
    image_3bands <- stack(paste0("./Unzipped Old Format/", "VE_VM01_VSC_L1VALD_ZEL01NW2_",new_name,
                               ".DBL.DIR/","VE_VM01_VSC_PDTIMG_L1VALD_ZEL01NW2_",new_name,".DBL.tif"), bands=c(11,7,4))
    writeRaster(image_3bands, filename=file.path("./Tif_whole_scene_2018_2019", paste0(new_name, ".tif")), format="GTiff", overwrite=TRUE)
    print(i) # this is to show the progress one by one because the process is quite long. It is good to see the progress.
  }

# We will crop the tiff in 2018 and 2019 into study areas.
setwd("H:/Satellite Imagery")
# read the shapefile ( study area)
tasman_WGS1984 <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/WGS1984/Tasman.shp")
franz_josef_WGS1984 <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/WGS1984/Franz_josef.shp")
# load all filenames in the 3 band composite images (tiff)
filenames <- list.files(path = "./Level 1 C/Tif_whole_scene_2018_2019", pattern = "*.tif")
for (i in 1:length(filenames)) {
# change projection for imagery into new folder.
  gdalUtilities::gdalwarp(srcfile = file.path("./Level 1 C/Tif_whole_scene_2018_2019", filenames[i]), 
                        dstfile = file.path("./Level 1 C/Tif_whole_scene_2018_2019 WGS84", filenames[i]), 
                        t_srs = "EPSG:4326")
# the reason we change projection is because JPEG format that GIV software requires must be in WGS1984 projection. But we will keep both version. 
# we use stack () function to read a raster that contain more than a single band.
  image <-  stack(paste0("./Level 1 C/Tif_whole_scene_2018_2019 WGS84/", filenames[i]))
  image_cropped <- crop(image, tasman_WGS1984, snap="in")
  image_cropped2 <- crop(image, franz_josef_WGS1984, snap="in")
# write cropped image (Tasman study area)
  writeRaster(image_cropped, filename=file.path("./Tasman/Tif_2018_2020_WGS1984", filenames[i]), format="GTiff", overwrite=TRUE)
# write cropped image (Franz Josef)
  writeRaster(image_cropped2, filename=file.path("./Franz_josef/Tif_2018_2020_WGS1984", filenames[i]), format="GTiff", overwrite=TRUE)
# This to change Tiff to JPEG for both Tasman and Franz Josef.
  gdalUtilities::gdal_translate(src_dataset=file.path("./Tasman/Tif_2018_2020_WGS1984", filenames[i]), 
                                dst_dataset=file.path("./Tasman/JPEG_2018_2020_WGS1984", 
                                paste0(substring(filenames[i], 1, 8), ".jpg")), of="JPEG", ot="Byte", scale = c(0, 1000 , 0, 255), b = c(1,2,3) , projwin = c(170.192832, -43.593637, 170.226867, -43.625256) )
  gdalUtilities::gdal_translate(src_dataset=file.path("./Franz_josef/Tif_2018_2020_WGS1984", filenames[i]), 
                                dst_dataset=file.path("./Franz_josef/JPEG_2018_2020_WGS1984", 
                                paste0(substring(filenames[i], 1, 8), ".jpg")), of="JPEG", ot="Byte", scale = c(0, 800 , 0, 255), b = c(1,2,3), projwin = c(170.190611, -43.47612, 170.232392, -43.494787) )
  print(i) # See the progress
}







# Since old study areas contain shadow areas. I will set new study area in Both Tasman and Franz Josef. Which will be larger and try to avoid shadows. 

# Note that the I change BBOX as well. 



# set working directory

setwd("H:/Satellite Imagery")

# read the shapefile 

tasman_WGS1984 <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/Bbox/Study Area WGS 1984/Tasman_BBOX.shp")
franz_josef_WGS1984 <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/Bbox/Study Area WGS 1984/Franz_Josef_BBOX.shp")

plot(tasman_WGS1984)
plot(franz_josef_WGS1984)

# load all filenames in the 3 band composite images (tiff)

filenames <- list.files(path = "./Level 1 C/Tif_Whole_Scene_2018_2020 WGS84", pattern = "*.tif")
filenames 

for (i in 1:length(filenames)) {
  
  # read raster
  image <-  stack(paste0("./Level 1 C/Tif_whole_scene_2018_2020 WGS84/", filenames[i]))
  
  # crop study area 
  
  image_cropped <- crop(image, tasman_WGS1984, snap="in")
  image_cropped2 <- crop(image, franz_josef_WGS1984, snap="in")
  
  
  writeRaster(image_cropped, filename=file.path("./Study Areas/Tasman/Tif", filenames[i]), format="GTiff", overwrite=TRUE)
  
  writeRaster(image_cropped2, filename=file.path("./Study Areas/Franz Josef/Tif", filenames[i]), format="GTiff", overwrite=TRUE)
  
  
  # Change format from Tiff to JPEG for analysis in the GIV software for both study areas.
  
  gdalUtilities::gdal_translate(src_dataset=file.path("./Study Areas/Tasman/Tif", filenames[i]), 
                                dst_dataset=file.path("./Study Areas/Tasman/JPG", paste0(substring(filenames[i], 1, 8), ".jpg")), of="JPEG", 
                                ot="Byte", scale = c(0, 1000 , 0, 255), b = c(1,2,3) , projwin = c(170.184486, -43.5986, 170.226867, -43.662233) )
  
  gdalUtilities::gdal_translate(src_dataset=file.path("./Study Areas/Franz Josef/Tif", filenames[i]), 
                                dst_dataset=file.path("./Study Areas/Franz Josef/JPG", paste0(substring(filenames[i], 1, 8), ".jpg")), of="JPEG",
                                ot="Byte", scale = c(0, 800 , 0, 255), b = c(1,2,3), projwin = c(170.18633, -43.468375, 170.235704, -43.504661) )
  
  print(i) # to see the progress
}






