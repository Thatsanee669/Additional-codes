
### These codes are used to stack all velocity geo-tif files generated from Matlab codes ###
### After Stacking, we will also extract velocity values from the stacked files ###


# load the relevant Library

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(RStoolbox)

## Set working directory and define the work folder.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff"



## list all the files in the workfolder.


filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]


## Use for loop to assign all variables to read all velocity imagery raster files. 

 # There are 115 velocity rasters generated from 115 pairs of imagery. 


for(i in 1:length(filenames)) { 
  
  nam <- paste("pair", i, sep = "_")
  assign(nam, raster(paste0("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff/", filenames[i])))
}




##  create an emty list to contain all assigned variables
 

list_variables <- vector(mode = "list", length =  length(filenames))


## Use for loop to append all variables names to store in the emty list created before.
  # note that all string names are now stores as variables not string anymore.

for (i in 1 : length(filenames)) {
  
  
list_variables[[i]] <- eval(parse(text = paste0("pair_", i)))
  
}
     
        # Contain all the variable names in the list
        # a <- c(eval(parse(text = "pair_1")), eval(parse(text = "pair_2")))
        # a [[1]]
        # velocity_stack <- stack(pair_1, pair_2)
        # stack2 <- stack(a)
        # stack2
        # typeof(a)

## create a stack file of all velocity rasters in the list.
  # note that normally stack() function will work like this stack(raster_1, raster_2, ...)

velocity_stack <- stack(list_variables)

  # check

velocity_stack

## write this layer into a stacked geotif file again for further usage.

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)


 # As checked by ENVI software. raster 1, 50 (only upper part), 57(middle part), 62 (upper part), 101, 108 and 109 does not

## now let's try to read point coordinates. That we would like to extract velocity values from

pointCoordinates=read.csv("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/timeseries_coordinates.csv")

## covert csv into features. 
 # note that we can all read shapefile of the points directly. 


coordinates(pointCoordinates) <- ~ Lon + Lat

##  Extract raster value by points

rasValue=extract(velocity_stack, pointCoordinates)

## to show the first point. # just to check

rasValue[1,]


## Combine raster values with point and save as a CSV file


 combinePointValue=cbind(pointCoordinates,rasValue)

##  now we have point coordinate contains all velocity data.

pointCoordinates

## we will use its attribute data

Attributes <- combinePointValue@data

## We will also change colomn names to only a single date. 


conames <- colnames(Attributes)
conames[1] <- "IDIDIDIDIDIDIDIDID" # change the first column to the same length


## use only the last 11 to 18 digits as new column which will be good when converting to date and long format.

colnames(Attributes) <- paste0(substring(conames, 11, 14), "-", substring(conames, 15, 16), "-", substring(conames, 17, 18))

## check

colnames(Attributes)
Attributes

## save it to the csv file.

write.csv(Attributes,"C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/extracted_raster_velocity.csv", row.names = FALSE)

## write.table(Attributes,file="C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/extracted_raster_velocity.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)







#### Because we generated different types of parings now we will apply similar techniques too. 
### we now stack the velocity rasters generated from comparing beteen scenes such as 1-2, 2-3, 3-4, 4-5 and so on.


setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff (1-2, 2-3..)")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff (1-2, 2-3..)"

filenames <- list.files(workfolder, pattern = "*.tif")


filenames <- filenames[nchar(filenames)==21]


# assign all variables to read all velocity imagery raster files. 

# There are 115 velocity rasters generated from 155 pairs of imagery. 


for(i in 1:length(filenames)) { 
  
  nam <- paste("pair", i, sep = "_")
  assign(nam, raster(paste0("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff (1-2, 2-3..)/", filenames[i])))
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







#### We will use the same code to extract velocity from a shorter period of time. 

### Now we will only look at July 2019 to March 2020. Because this period of time we##########
### notice a haivy rain in the middle (Dec 2019) while there's less rain before and after that events.########
### which is expected to see the changes of the velocity.#########
### Compare between Scenes ####

#setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared with the first scence")

# workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared with the first scence"

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared between scences")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared between scences"

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]


# assign all variables to read all velocity imagery raster files. 

# There are 31 velocity rasters generated from 32 pairs of imagery. 


for(i in 1:length(filenames)) { 
  
  nam <- paste("pair", i, sep = "_")
  assign(nam, raster(paste0(workfolder,"/", filenames[i])))
}


## create an emty list

list_variables <- vector(mode = "list", length =  length(filenames))


## append variables names to store

for (i in 1 : length(filenames)) {
  
  
  list_variables[[i]] <- eval(parse(text = paste0("pair_", i)))
  
}


## create a stack file of all velocity raster.

velocity_stack <- stack(list_variables)

velocity_stack

## write this layer into a geotif file again. into its folder

writeRaster(velocity_stack, filename="velocity_stack", format="GTiff", overwrite=TRUE)










## 24-Dec-2020 ## Franz_Josef Glaciers ##
## Now we will work on Franz_Josef Glaciers

### we will stack the velocity rasters generated from comparing beteen scenes such as 1-2, 2-3, 3-4, 4-5 and so on.


setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Franz_Josef_Single_Band_Jul_2019_Mar_2020/Velocity/Compared between scences")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Franz_Josef_Single_Band_Jul_2019_Mar_2020/Velocity/Compared between scences"

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















