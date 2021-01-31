
### These codes are used to stack all velocity geo-tif files generated from Matlab codes ###
### After Stacking, we will also extract velocity values from the stacked files ###


# load the relevant Libraries

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(RStoolbox)



## 24-Dec-2020 ## Franz_Josef Glaciers ##
## Now we will work on Franz_Josef Glaciers

### we will stack the velocity rasters generated from comparing beteen scenes such as 1-2, 2-3, 3-4, 4-5 and so on.


#### This code will process 10 meter imagert first #####


## Set working directory and define the work folder.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/10 meters")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/10 meters"

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




### Now work with 20 meter resolution data.



## Set working directory and define the work folder.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/20 meters")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/20 meters"

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





## now working with 30 meter resolution data.


## Set working directory and define the work folder.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/30 meters")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/30 meters"

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





## 20-Jan-2021 ## Franz_Josef Glaciers ##
## Now we will work on Franz_Josef Glaciers

### we will stack the velocity rasters generated from comparing beteen scenes such as 1-2, 2-3, 3-4, 4-5 and so on.


#### This code will process 5 meter imagery


## Set working directory and define the work folder.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/5 meters")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Deresolution_Velocity/5 meters"

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










