#### The following codes will be used to stack and plot de temporal resolution data 5 days, 16 days and 30 days respectively ####

## Load relevant libraries

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(RStoolbox)

## for 5 days

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/5 days"
setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/5 days")

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]

# assign all variables to read all velocity imagery raster files. 

# There are 55 velocity rasters generated from 55 pairs of imagery (56 imagery). 


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

# create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

# write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)


## For 16 days



workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/16 days"
setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/16 days")

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]

# assign all variables to read all velocity imagery raster files. 

# There are 55 velocity rasters generated from 55 pairs of imagery (56 imagery). 


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

# create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

# write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)



## For 30 days



workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/30 days"
setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/30 days")

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]

# assign all variables to read all velocity imagery raster files. 

# There are 55 velocity rasters generated from 55 pairs of imagery (56 imagery). 


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

# create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

# write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)



## The end


