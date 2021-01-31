
#### to get a single image band for processing in Imgraft toolbox in the matlab. 
### Since there 3 three bands (11, 7 and 4) from what we extracted and stacked them from zip files using previous codes
### Using raster() function to read the raster would read only the first band - which is band 11. when can change the band to lead as well by modifying the single band to read in that function or in the stack() function in the RStool package.


# When Working with Imgraft toolbox in Matlab. It only requires one - band image with projected coordinate system (UTM)
# So I will provide some codes to crop study area in both study areas, Franz Josef and Tasman by using projected imagery (UTM zone 59 S)

# load revelant packages

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)



# set working directory

setwd("H:/Satellite Imagery")

# read the shapefile 

tasman_UTM <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/Bbox/Study Area UTM/Tasman_BBOX.shp")
franz_josef_UTM <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/Bbox/Study Area UTM/Franz_Josef_BBOX.shp")

# LIST ALL FILE NAMES:

filenames <- list.files(path = "./Level 1 C/Tif_whole_scene_2018_2020 UTM", pattern = "*.tif")

filenames


for (i in 1:length(filenames)) {
  
  
  image <-  raster(paste0("./Level 1 C/Tif_whole_scene_2018_2020 UTM/", filenames[i]))
  
  image_Tasman <- crop(image, tasman_UTM, snap="in")
  image_Franz_Josef <- crop(image, franz_josef_UTM, snap="in")
  
  writeRaster(image_Tasman, filename=file.path("./Study Areas/Tasman/Tif_UTM", paste0(filenames[i])), format="GTiff", overwrite=TRUE)
  
  writeRaster(image_Franz_Josef, filename=file.path("./Study Areas/Franz Josef/Tif_UTM", paste0(filenames[i])), format="GTiff", overwrite=TRUE)
  print(i) # this is just to show the progress when progamming running.
  
}


# To seperate good images from all images. Because we already identify good images in the JPG format.
# This will also apply in the tif format, we will copy the tiff format that have the same names as good images in the JPG format to new folders.
# these folders will contain only good images (geo-tif format) both Tasman and Franz Josef.

good_images1 <- list.files(path = "./Study Areas/Tasman/JPG/Good Images", pattern = "*.jpg")
good_images2 <- list.files(path = "./Study Areas/Franz Josef/JPG/Good Images", pattern = "*.jpg")

good_images1 <- good_images1[nchar(good_images1)==12]
good_images2 <- good_images2[nchar(good_images2)==12]


# Gopy good images from Tasman UTM and Franz Josef folder by using good images name already identified by JPG format

for (i in 1:length(good_images1)) {
  
  file.copy(from=paste0("./Study Areas/Tasman/Tif_UTM/", substring(good_images1[i], 1,8),".tif"), to="./Study Areas/Tasman/Tif_UTM/Tasman_Single_Band_Good_IM_2018-2020" )
  print(i)
  
}


for (i in 1:length(good_images2)) {
  
  file.copy(from=paste0("./Study Areas/Franz Josef/Tif_UTM/", substring(good_images2[i], 1,8),".tif"), to="./Study Areas/Franz Josef/Tif_UTM/Franz_Josef_Single_Band_Good_IM_2018-2020" )
  print(i)
  
}



