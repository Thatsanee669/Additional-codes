### Stack Displacement and velocity for error Checking ###

# load the relevant Library

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(RStoolbox)

## Tasman
### Displacement between Scenes
### we will stack the velocity rasters generated from comparing beteen scenes such as 1-2, 2-3, 3-4, 4-5 and so on.


setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Tasman/Displacement of unmoved areas (comparing between scenes)")
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Tasman/Displacement of unmoved areas (comparing between scenes)"
filenames <- list.files(workfolder, pattern = "*.tif")
filenames <- filenames[nchar(filenames)==21]

# assign all variables to read all velocity imagery raster files. 
# There are 115 velocity rasters generated from 155 pairs of imagery. 

for(i in 1:length(filenames)) { 
  
  nam <- paste("pair", i, sep = "_")
  assign(nam, raster(paste0(workfolder, '/', filenames[i])))
}

# create an emty list

list_variables <- vector(mode = "list", length =  length(filenames))

# append variables names to store

for (i in 1 : length(filenames)) {
 
  
  list_variables[[i]] <- eval(parse(text = paste0("pair_", i)))
 
}

## create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

## write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)



### Velocity

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Tasman/Velocity of unmoved areas (comparing between scenes)")
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Tasman/Velocity of unmoved areas (comparing between scenes)"
filenames <- list.files(workfolder, pattern = "*.tif")
filenames <- filenames[nchar(filenames)==21]

# assign all variables to read all velocity imagery raster files. 
# There are 115 velocity rasters generated from 155 pairs of imagery. 

for(i in 1:length(filenames)) { 
  
  nam <- paste("pair", i, sep = "_")
  assign(nam, raster(paste0(workfolder, '/', filenames[i])))
}

# create an emty list

list_variables <- vector(mode = "list", length =  length(filenames))

# append variables names to store

for (i in 1 : length(filenames)) {
  
  
  list_variables[[i]] <- eval(parse(text = paste0("pair_", i)))
  
}

## create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

## write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)




## Franz Josef

## Displacement

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Franz Josef/Displacement of unmoved areas (comparing between scenes)")
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Franz Josef/Displacement of unmoved areas (comparing between scenes)"
filenames <- list.files(workfolder, pattern = "*.tif")
filenames <- filenames[nchar(filenames)==21]

# assign all variables to read all velocity imagery raster files. 
# There are 115 velocity rasters generated from 155 pairs of imagery. 

for(i in 1:length(filenames)) { 
  
  nam <- paste("pair", i, sep = "_")
  assign(nam, raster(paste0(workfolder, '/', filenames[i])))
}

# create an emty list

list_variables <- vector(mode = "list", length =  length(filenames))

# append variables names to store

for (i in 1 : length(filenames)) {
  
  
  list_variables[[i]] <- eval(parse(text = paste0("pair_", i)))
  
}

## create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

## write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)



### Velocity

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Franz Josef/Velocity of unmoved areas (comparing between scenes)")
workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Franz Josef/Velocity of unmoved areas (comparing between scenes)"
filenames <- list.files(workfolder, pattern = "*.tif")
filenames <- filenames[nchar(filenames)==21]

# assign all variables to read all velocity imagery raster files. 
# There are 115 velocity rasters generated from 155 pairs of imagery. 

for(i in 1:length(filenames)) { 
  
  nam <- paste("pair", i, sep = "_")
  assign(nam, raster(paste0(workfolder, '/', filenames[i])))
}

# create an emty list

list_variables <- vector(mode = "list", length =  length(filenames))

# append variables names to store

for (i in 1 : length(filenames)) {
  
  
  list_variables[[i]] <- eval(parse(text = paste0("pair_", i)))
  
}

## create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

## write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)






