
### This codes will be use to do preprocessing Satellite imagery from Venus satellite in the newer format file.### 
### Before November 2019 backward, the Venus imagery was stored in different format. Those will use different codes for processing as well. 

# load revelant packages

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)


## Note that, the author use Linux scripts to extract and stack 3 bands imagery from zip file
## as follow. ## note this must be run in Linux only. 
          #   user-defined variables
              SOURCEDIR=/Volumes/arc_02/REMOTE_SENSING/VENUS/ZEL01NW2 #directory where venus zip files are stored
              DESTDIR=/Volumes/arc_01/eavessh/GIVproject/tasman #project directory
              bbox=(170.162773 -43.577887 170.233498 -43.669049) # bounding box of area of interest

              mkdir tifs
          #   loop through all files in SOURCEDIR (2020 only)
              for file in $SOURCEDIR/*2020*; do
          #   first get the image date from the file name
              zip_date=$(echo "$file" | grep -Eo '[[:digit:]]{8}')
          #   make a temporary directory to store unzipped files
              mkdir $DESTDIR/temp
              TEMPDIR=$DESTDIR/temp
          #   unzip the files
              unzip -a "$file" -d $TEMPDIR
          #   create tif of selected bands B1, B2, etc 743=false color infrared
              gdal_merge.py -o tifs/"$zip_date".tif -separate $TEMPDIR/V*/*B11.tif $TEMPDIR/V*/*B7.tif $TEMPDIR/V*/*B4.tif $TEMPDIR/V*/*B3.tif -co COMPRESS=DEFLATE
          #   reproject to wgs84	
              gdalwarp -t_srs EPSG:4326 tifs/"$zip_date".tif "$TEMPDIR/$zip_date"wgs84.tif
          #   clip to bbox and save as jpeg with image date as file name
              gdal_translate "$TEMPDIR/$zip_date"wgs84.tif "$DESTDIR/$zip_date".jpg -of JPEG -projwin ${bbox[0]} ${bbox[1]} ${bbox[2]} ${bbox[3]} -b 1 -b 2 -b 3 -ot Byte -scale 0 800 0 255
              rm -r $TEMPDIR
              done

# This can be done using R as well. using Unzip () and stack() and writeRaster () in Rgdal packages.
# These linux codes also return three bands imagery for the whole scene as well.
# there are 3 bands composited image for the whole scene got from linux
              
              
# We can use those composite imagery for further processing
# In this case we will clip to study area and convert them into JPEG format in the right projection for Analyzing in GIV

              
## set the working directory folder         

setwd("H:/Satellite Imagery")


filenames <- list.files(path = "./Tasman/From_linux/Tif_whole_scene_2019_2020", pattern = "*.tif")


# We will use the clipped file change their projection and converts them into JPG files.
# when we clip and change the projection later - the edge of the imagery will not be smooth enough.
# We will change projection and clip into study areas. 


tasman_WGS1984 <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/WGS1984/Tasman.shp")
franz_josef_WGS1984 <- readOGR("H:/Satellite Imagery/Target_Area_Shapefile/WGS1984/Franz_josef.shp")


# Change projection 

for (i in 1:length(filenames)) {
  
  gdalUtilities::gdalwarp(srcfile = file.path("./Tasman/From_linux/Tif_whole_scene_2019_2020", 
                         filenames[i]), dstfile = file.path("./Tasman/From_linux/Tif_whole_scene_2019_2020_WGS84", 
                        filenames[i]), t_srs = "EPSG:4326")
  image <-  stack(paste0("./Tasman/From_linux/Tif_whole_scene_2019_2020_WGS84/", filenames[i]))
  image_cropped <- crop(image, tasman_WGS1984, snap="in")
  image_cropped2 <- crop(image, franz_josef_WGS1984, snap="in")
  writeRaster(image_cropped, filename=file.path("./Tasman/From_linux/Tif_study_area_WGS84", 
                                                filenames[i]), format="GTiff", overwrite=TRUE)
  writeRaster(image_cropped2, filename=file.path("./Franz_josef/From_linux/Tif_study_area_WGS84", 
                                                 filenames[i]), format="GTiff", overwrite=TRUE)
  
   gdalUtilities::gdal_translate(src_dataset=file.path("./Tasman/From_linux/Tif_study_area_WGS84", filenames[i]), 
                                dst_dataset=file.path("./Tasman/From_linux/Tif_study_area_WGS84/jpg", 
                                paste0(substring(filenames[i], 1, 8), ".jpg")),
                                of="JPEG", ot="Byte", scale = c(0, 1000 , 0, 255), b = c(1,2,3) , 
                                projwin = c(170.192832, -43.593637, 170.226867, -43.625256) )
  
  gdalUtilities::gdal_translate(src_dataset=file.path("./Franz_josef/From_linux/Tif_study_area_WGS84", 
                              filenames[i]), dst_dataset=file.path("./Franz_josef/From_linux/Tif_study_area_WGS84/jpg",
                              paste0(substring(filenames[i], 1, 8), ".jpg")), of="JPEG", ot="Byte", 
                              scale = c(0, 800 , 0, 255), b = c(1,2,3), projwin = c(170.190611, -43.47612, 170.232392, -43.494787) )
    print(i) # to see the progress.
}



