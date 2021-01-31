### This codes will be used for manipulating data and plotting ####

## Check the displacement in meters between image pairs in the Tasman glacier ##

# load the relevant Library

library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(gdalUtilities)
library(RStoolbox)

## Set working directory and define the work folder.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Tasman/Displacement of unmoved areas (comparing between scenes)")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/Matched imagery with Tasman/Displacement of unmoved areas (comparing between scenes)"



## list all the files in the workfolder.


filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]


#### Extract velocity values from this period of times (Jul 2019 - Mar 2020)


## now let's try to read point coordinates and Stacked velocity

displacement_stack <- stack(paste0(workfolder, "/", "velocity_stack.tif"))
pointCoordinates=readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Checking Error/shapfile location of unmoved area/unmoved_location.shp")

## Extract raster value by points

rasValue <- extract(displacement_stack, pointCoordinates)


## Combine raster values with point and save as a CSV file


combinePointValue=cbind(pointCoordinates,rasValue)

## we will use its attribute data

Attributes <- combinePointValue@data

Attributes["Id"] <- c("P 1", "P 2", "P 3")
Attributes[4:6,] <- 0
Attributes[4:6, 1] <- c("T", "m/t", "m/y")
Attributes
Attributes 

im_dates

## We will also change colomn names to only a single date. 

colnames(Attributes)

## use only the last 11 to 18 digits as new column which will be good when converting to date and long format.

colnames(Attributes)[4:34] <- paste0(substring(filenames, 10, 13), "-", substring(filenames, 14, 15), "-", substring(filenames, 16, 17))



library(dplyr)
library(ggplot2)
library(tidyverse)


## create a dataframe that contain date, velocity and sum of rainfall by its number of time interval (days)


## first just remove Lat and Lon column from the Attribute we will only need point ID for now.

Attributes <- Attributes[-c(2, 3)]

## Note that the first imagery date is 2019-07-25 and the last imagery is 2020-03-15. But we excluded the first imagery from the attribute. 
## now we will store all the date of imagery

im_dates <- c("2019-07-25", colnames(Attributes)[-1])

im_dates <- as.Date(im_dates, format="%Y-%m-%d")

for (i in 1:(length(im_dates) - 1 )) {
  
  j <- i + 1
  
  t <- as.numeric(im_dates[j] - im_dates[i])
                  
Attributes[4, j] <- t              
  
}


sum(Attributes[1, 2:32], na.rm = TRUE)
sum(Attributes[2, 2:32], na.rm = TRUE)
sum(Attributes[3, 2:32], na.rm = TRUE)


Attributes[5, 2:32] <- Attributes[2, 2:32]/Attributes[4, 2:32]

Attributes[6, 2:32] <- Attributes[5, 2:32]*365


sum(Attributes[6, 2:32], na.rm = TRUE) # 6132 for p 1, 4722 for point 2

## We will filter the rainfall only this period of time.

Attributes["Id"] 

library(reshape2)

Attributes_long <- melt(Attributes, id.vars="Id")

Attributes_long$variable <- as.Date(Attributes_long$variable, format="%Y-%m-%d")



colnames(Attributes_long) <- c("Variables", "Date", "Values")

## for writing a csv file we can convert to a proper wide format too.

Attributes_wide <- dcast(Attributes_long, Date ~ Variables , value.var="Values")

## filter
Attributes_long <- Attributes_long %>% filter(Variables != "m/y" & Variables != "T" & Variables != "P 3" )
## Now we plot a muiltiple line & bar graph.

pp <- ggplot(data=Attributes_long,aes(x=Date,y=Values,color=Variables )) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = Attributes_long$Date) + 
  theme(axis.text.x = element_text(angle = 90)) 

# 
My_Theme = theme(
  axis.title.x = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 10))

pp + My_Theme 


### Read the velocity data

v <- read.csv("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/TimeSeries_velocity__between_scene_compared_with_rainfall_Jul2019_Mar2020.csv", stringsAsFactors = FALSE)
v <- v[,c(1,3,4,5,6)]

V_long <- gather(v, Varibles, Values, Velocity.Point.1..m.y.:Velocity.Point.4..m.y., factor_key=TRUE)

V_long <- V_long %>% filter(Varibles != "Velocity.Point.4..m.y." & Varibles != "Velocity.Point.3..m.y.")
V_long$Date <- as.Date(V_long$Date, format="%Y-%m-%d")

V_long_re <- V_long
V_long_re[,1] <-  V_long[,2]
V_long_re[,2] <- V_long[,1]

colnames(V_long_re) <- c("Variables", "Date", "Values")

### Modify displacement of unmoved points data

velocity_unmoved <- Attributes

velocity_unmoved[5:7,] <- 0
velocity_unmoved[5:7, 1] <- c("V 1", "V 2", "V 3")
velocity_unmoved[5,2:32] <- velocity_unmoved[1,2:32]/velocity_unmoved[4,2:32]*356
velocity_unmoved[6,2:32] <- velocity_unmoved[2,2:32]/velocity_unmoved[4,2:32]*356
velocity_unmoved[7,2:32] <- velocity_unmoved[3,2:32]/velocity_unmoved[4,2:32]*356

velocity_unmoved <- velocity_unmoved[5:7,]

velocity_unmoved_long <- melt(velocity_unmoved, id.vars="Id")

velocity_unmoved_long$variable <- as.Date(velocity_unmoved_long$variable, format="%Y-%m-%d")

colnames(velocity_unmoved_long) <- c("Variables", "Date", "Values")

velocity_unmoved_long <- velocity_unmoved_long %>% filter(Variables != "V 3")


unmoved_vs_glacier <- rbind(velocity_unmoved_long, V_long_re)



pp <- ggplot(data=unmoved_vs_glacier,aes(x=Date,y=Values,color=Variables )) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = unmoved_vs_glacier$Date) + 
  theme(axis.text.x = element_text(angle = 90)) 

# 
My_Theme = theme(
  axis.title.x = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 10))

pp + My_Theme 



