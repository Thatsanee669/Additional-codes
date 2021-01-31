
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

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared between scences")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared between scences"



## list all the files in the workfolder.


filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]


#### Extract velocity values from this period of times (Jul 2019 - Mar 2020)


## now let's try to read point coordinates and Stacked velocity

velocity_stack <- stack("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared between scences/velocity_stack.tif")

pointCoordinates=readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/Tasman_points2.shp")

## Extract raster value by points

rasValue=extract(velocity_stack, pointCoordinates)


## Combine raster values with point and save as a CSV file


combinePointValue=cbind(pointCoordinates,rasValue)

## we will use its attribute data

Attributes <- combinePointValue@data

Attributes["Id"] <- 1:4

Attributes 
## We will also change colomn names to only a single date. 

colnames(Attributes)

## use only the last 11 to 18 digits as new column which will be good when converting to date and long format.

colnames(Attributes)[4:34] <- paste0(substring(filenames, 10, 13), "-", substring(filenames, 14, 15), "-", substring(filenames, 16, 17))


## read rainfall data: 


rainfall <- read.csv("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Percipation/wgenf.genform1_proc4.csv", stringsAsFactors=FALSE)

## Filter only two columns


rainfall <- rainfall[c("Date.NZST.","Amount.mm.")]

## Convert dates into date format in R

rainfall$Date.NZST. <- base::as.Date(rainfall$Date.NZST., format="%d-%m-%y")

class(rainfall$Date.NZST.) # To check.

## Load dplyr and ggplot2 library for further filtering and ploting. 


library(dplyr)
library(ggplot2)
library(tidyverse)


## create a dataframe that contain date, velocity and sum of rainfall by its number of time interval (days)


## first just remove Lat and Lon column from the Attribute we will only need point ID for now.

Attributes <- Attributes[-c(2, 3)]

## Create Another row called "Rain Fall" for storing summed rain fall based on the date interval among imagery.

Attributes[5,] <- 0

Attributes[5,1] <- "Rain Fall"

## Note that the first imagery date is 2019-07-25 and the last imagery is 2020-03-15. But we excluded the first imagery from the attribute. 
## now we will store all the date of imagery

im_dates <- c("2019-07-25", colnames(Attributes)[-1])

## We will filter the rainfall only this period of time.


rainfall <- rainfall %>% filter(Date.NZST. >= "2019-07-25" & Date.NZST. <= "2020-03-15")

## we will sum rainfall based the time interval. and add that into our velocity matrix named "Attributes" that we created before. 


for (i in 1:(length(im_dates) - 1 )) {
  
  if (i == 1) {  
    
    rain <- rainfall %>% filter(Date.NZST. >= im_dates[i] & Date.NZST. <= im_dates[i+1]) # the first pair we need include the first date of the the first pair "2019-07-25".
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
  } else {
      
    # while the second pair we dont need last date of the pervious pair to sum the data.
    
    rain <- rainfall %>% filter(Date.NZST. > im_dates[i] & Date.NZST. <= im_dates[i+1])
    
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum  # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
    
    }
  
  
  
}

## now we get both Velocity and summed rain data in the same matrix.

line_1 <- Attributes[5,]
as.matrix(Attributes[5,])

as.vector(Attributes[5,])
class(Attributes)

## But we may work abit easier in a long format of the data. We will change wide to long format using this library.

library(reshape2)

Attributes_long <- melt(Attributes, id.vars="Id")

Attributes_long$variable <- as.Date(Attributes_long$variable, format="%Y-%m-%d")



## Change Id names and columns names as well

Attributes_long[Attributes_long$Id==1,]$Id <- "Velocity Point 1 (m/y)"
Attributes_long[Attributes_long$Id==2,]$Id <- "Velocity Point 2 (m/y)"
Attributes_long[Attributes_long$Id==3,]$Id <- "Velocity Point 3 (m/y)"
Attributes_long[Attributes_long$Id==4,]$Id <- "Velocity Point 4 (m/y)"
Attributes_long[Attributes_long$Id=="Rain Fall",]$Id <- "Rain Fall (mm)"

colnames(Attributes_long) <- c("Variables", "Date", "Values")

## for writing a csv file we can convert to a proper wide format too.

Attributes_wide <- dcast(Attributes_long, Date ~ Variables , value.var="Values")

Attributes_long <- Attributes_long %>% filter(Variables != "Rain Fall (mm)") # we will use daily rainfall in stead


## Now we plot a muiltiple line & bar graph.

pp <- ggplot(data=Attributes_long,aes(x=Date,y=Values,color=Variables )) +
  geom_col(data=rainfall, aes(x=Date.NZST., y=Amount.mm.*2), fill="#0f5e9c", color="#0f5e9c", width=0.5) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = Attributes_long$Date) + 
  theme(axis.text.x = element_text(angle = 90)) + 
 
  scale_y_continuous(
   
   # Features of the first axis
   name = "Velocity (m/year)",
   
   # Add a second axis and specify its features
   sec.axis = sec_axis(~./2, name="Rainfall (mm)")
  )

# 
My_Theme = theme(
  axis.title.x = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 10))

pp + My_Theme 



## Write it into a csv file for further anlysis

write.csv(Attributes_wide,"C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/short_lived_between_Scene_compared_with_rainfall.csv", row.names = FALSE)





## 23-Oct-2020 ##

#### Extract velocity values from this period of times (Oct 2018 - Oct 2020) - Tasman glacier 
    ## The velocity is generated in Matlab by comparing among different pairs of imagery between scenes.


workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff (1-2, 2-3..)"
## Read the stacked raster that contains all velocity compared between scences generated by using Matlab - Imgraft Package.

velocity_stack  <- stack("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Velocity_Tiff (1-2, 2-3..)/velocity_stack.tif")

## Also read the the shapefile of the interested points in the lower parts of Tasman glacier.

pointCoordinates=readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/Tasman_points2.shp")

## Extract raster value by points

rasValue=extract(velocity_stack, pointCoordinates)


## Combine raster values with point and save as a CSV file


combinePointValue=cbind(pointCoordinates,rasValue)

## we will use its attribute data

Attributes <- combinePointValue@data

Attributes["Id"] <- c("Velocity Point 1 (m/y)", "Velocity Point 2 (m/y)", "Velocity Point 3 (m/y)", "Velocity Point 4 (m/y)")

Attributes 
## We will also change colomn names to only a single date. 

colnames(Attributes)

## First list all filenames that from a pair of imagery that generated this velocity.

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]

## use only the last 11 to 18 digits as new column which will be good when converting to date and long format.

colnames(Attributes)[-c(1,2,3)] <- paste0(substring(filenames, 10, 13), "-", substring(filenames, 14, 15), "-", substring(filenames, 16, 17))



## read rainfall data: 


rainfall <- read.csv("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Percipation/wgenf.genform1_proc4.csv", stringsAsFactors=FALSE)

## Filter only two columns


rainfall <- rainfall[c("Date.NZST.","Amount.mm.")]

## Convert dates into date format in R

rainfall$Date.NZST. <- base::as.Date(rainfall$Date.NZST., format="%d-%m-%y")

class(rainfall$Date.NZST.) # To check.

## Load dplyr and ggplot2 library for further filtering and ploting. 


library(dplyr)
library(ggplot2)
library(tidyverse)


## create a dataframe that contain date, velocity and sum of rainfall by its number of time interval (days)


## first just remove Lat and Lon column from the Attribute we will only need point ID for now.

Attributes <- Attributes[-c(2, 3)]

## Create Another row called "Rain Fall" for storing summed rain fall based on the date interval among imagery.

Attributes[5,] <- 0

Attributes[5,1] <- "Rain Fall (mm)"

## Note that the first imagery date is "2018-11-05", and the last imagery is 2020-03-15. But we excluded the first imagery from the attribute. 
## now we will store all the date of imagery

im_dates <- c("2018-11-05", colnames(Attributes)[-1])

## We will filter the rainfall only this period of time.


rainfall <- rainfall %>% filter(Date.NZST. >= first(im_dates) & Date.NZST. <= last(im_dates))

## we will sum rainfall based the time interval. and add that into our velocity matrix named "Attributes" that we created before. 


for (i in 1:(length(im_dates) - 1 )) {
  
  if (i == 1) {  
    
    rain <- rainfall %>% filter(Date.NZST. >= im_dates[i] & Date.NZST. <= im_dates[i+1]) # the first pair we need include the first date of the the first pair "2019-07-25".
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
  } else {
    
    # while the second pair we dont need last date of the pervious pair to sum the data.
    
    rain <- rainfall %>% filter(Date.NZST. > im_dates[i] & Date.NZST. <= im_dates[i+1])
    
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum  # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
    
  }
  
  
  
}

## now we get both Velocity and summed rain data in the same matrix.

Attributes[5,] # summed Rainfall based on its time interval.


## But we may work abit easier in a long format of the data. We will change wide to long format using this library.

library(reshape2)

Attributes_long <- melt(Attributes, id.vars="Id")

Attributes_long$variable <- as.Date(Attributes_long$variable, format="%Y-%m-%d")



## Change olumns names as well

colnames(Attributes_long) <- c("Variables", "Date", "Values")



## Filter the date "2018-12-23" to "2020-08-28" only

Attributes_long <- Attributes_long %>% filter(Date >= "2018-12-23" & Date <= "2020-08-28") 


## Now we plot a simple line graph.

ggplot(data=Attributes_long,aes(x=Date,y=Values,color=Variables  )) +
  geom_point() +
  geom_line()

## the result show that from "2018-12-23" to "2020-08-28" show good results

## for writing a csv file we can convert to a proper wide format too.

Attributes_wide <- dcast(Attributes_long, Date ~ Variables , value.var="Values")


## Write it into a csv file for further anlysis

write.csv(Attributes_wide,"C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/TimeSeries_velocity_between_scenes_Oct2018_Oct2020_compared_with_rainfall.csv", row.names = FALSE)










## 24-Oct-2020 ## Franz Josef Glaciers.


#### Extract velocity values from this period of times (Oct 2020 - Oct 2020) - Tasman glacier 
## The velocity is generated in Matlab by comparing among different pairs of imagery between scenes.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Franz_Josef_Single_Band_Good_IM_2018-2020_UTM/Velocity/Compared between scences")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Franz_Josef_Single_Band_Good_IM_2018-2020_UTM/Velocity/Compared between scences"

## Read the stacked raster that contains all velocity compared between scences generated by using Matlab - Imgraft Package.

velocity_stack  <- stack("velocity_stack.tif")

## Also read the the shapefile of the interested points in the lower parts of Tasman glacier.

pointCoordinates=readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/Franz_Josef.shp")

## Extract raster value by points

rasValue <- extract(velocity_stack, pointCoordinates)


## Combine raster values with point and save as a CSV file


combinePointValue <- cbind(pointCoordinates,rasValue)

## we will use its attribute data

Attributes <- combinePointValue@data

Attributes["Id"] <- c("Velocity Point 1 (m/y)", "Velocity Point 2 (m/y)", "Velocity Point 3 (m/y)", "Velocity Point 4 (m/y)")

Attributes 
## We will also change colomn names to only a single date. 

colnames(Attributes)

## First list all filenames that from a pair of imagery that generated this velocity.

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]

## use only the last 11 to 18 digits as new column which will be good when converting to date and long format.

colnames(Attributes)[-1] <- paste0(substring(filenames, 10, 13), "-", substring(filenames, 14, 15), "-", substring(filenames, 16, 17))



## read rainfall data: 


rainfall <- read.csv("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Percipation/wgenf.genform1_proc4.csv", stringsAsFactors=FALSE)

## Filter only two columns


rainfall <- rainfall[c("Date.NZST.","Amount.mm.")]

## Convert dates into date format in R

rainfall$Date.NZST. <- base::as.Date(rainfall$Date.NZST., format="%d-%m-%y")

class(rainfall$Date.NZST.) # To check.

## Load dplyr and ggplot2 library for further filtering and ploting. 


library(dplyr)
library(ggplot2)
library(tidyverse)


## create a dataframe that contain date, velocity and sum of rainfall by its number of time interval (days)


## first just remove Lat and Lon column from the Attribute we will only need point ID for now.

## Attributes <- Attributes[-c(2, 3)]

## Create Another row called "Rain Fall" for storing summed rain fall based on the date interval among imagery.

Attributes[5,] <- 0

Attributes[5,1] <- "Rain Fall (mm)"

## Note that the first imagery date is "2018-11-05", and the last imagery is 2020-03-15. But we excluded the first imagery from the attribute. 
## now we will store all the date of imagery

im_dates <- c("2018-11-05", colnames(Attributes)[-1])

## We will filter the rainfall only this period of time.


rainfall <- rainfall %>% filter(Date.NZST. >= first(im_dates) & Date.NZST. <= last(im_dates))

## we will sum rainfall based the time interval. and add that into our velocity matrix named "Attributes" that we created before. 


for (i in 1:(length(im_dates) - 1 )) {
  
  if (i == 1) {  
    
    rain <- rainfall %>% filter(Date.NZST. >= im_dates[i] & Date.NZST. <= im_dates[i+1]) # the first pair we need include the first date of the the first pair "2019-07-25".
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
               } else {
    
    # while the second pair we dont need last date of the pervious pair to sum the data.
    
    rain <- rainfall %>% filter(Date.NZST. > im_dates[i] & Date.NZST. <= im_dates[i+1])
    
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum }  # add the value to the data.frame that we have. In row 5 and column 2 onward.
  
}



## now we get both Velocity and summed rain data in the same matrix.

Attributes[5,] # summed Rainfall based on its time interval.


## But we may work abit easier in a long format of the data. We will change wide to long format using this library.

library(reshape2)

Attributes_long <- melt(Attributes, id.vars="Id")

Attributes_long$variable <- as.Date(Attributes_long$variable, format="%Y-%m-%d")



## Change olumns names as well

colnames(Attributes_long) <- c("Variables", "Date", "Values")

## Now we would like to look at the data between "2019-07-25" and "2020-03-15" only

Attributes_long <- Attributes_long %>% filter(Date  > "2019-08-22" & Date  <= "2020-03-31", Variables != "Rain Fall (mm)")

rainfall <- rainfall %>%  filter(Date.NZST. >= "2019-07-25" & Date.NZST. <= "2020-03-31")
  
## Now we plot a muiltiple line & bar graph.

pp <- ggplot(data=Attributes_long,aes(x=Date,y=Values,color=Variables )) +
  geom_col(data=rainfall, aes(x=Date.NZST., y=Amount.mm.*4), fill="#0f5e9c", color="#0f5e9c", width=0.5) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = Attributes_long$Date) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Velocity (m/year)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./4, name="Rainfall (mm)")
  )

# 
My_Theme = theme(
  axis.title.x = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 10))

pp + My_Theme 




## for writing a csv file we can convert to a proper wide format too.

Attributes_wide <- dcast(Attributes_long, Date ~ Variables , value.var="Values")


## Write it into a csv file for further anlysis

write.csv(Attributes_wide,"C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/Franz_Josef_TimeSeries_velocity_between_scenes_Aug2018_April2020_compared_with_rainfall.csv", row.names = FALSE)









####### Extract velocity values from this period of times (Jul 2019 - Mar 2020) ################
##### Compared with the first Scene ###############

## now let's try to read point coordinates and Stacked velocity

velocity_stack <- stack("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared with the first scence/velocity_stack.tif")

pointCoordinates=readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/Tasman_points2.shp")

## Extract raster value by points

rasValue=extract(velocity_stack, pointCoordinates)


## Combine raster values with point and save as a CSV file


combinePointValue=cbind(pointCoordinates,rasValue)

## we will use its attribute data

Attributes <- combinePointValue@data

Attributes 

## We will also change colomn names to only a single date. 

colnames(Attributes)

## list all the date files from the folder

filenames <- list.files("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Jul_2019 - March 2020 UTM/Velocity/Compared with the first scence")

filenames <- filenames[nchar(filenames)==21]

## use only the last 11 to 18 digits as new column which will be good when converting to date and long format.

## Change column names to new name which is the last date of its velocity

colnames(Attributes)[4:34] <- paste0(substring(filenames, 10, 13), "-", substring(filenames, 14, 15), "-", substring(filenames, 16, 17))

colnames(Attributes) # check


Attributes

## Provide the proper names in the "ID" columns

Attributes["Id"] <- c("Velocity Point 1 (m/y)", "Velocity Point 2 (m/y)", "Velocity Point 3 (m/y)", "Velocity Point 4 (m/y)")

## first just remove Lat and Lon column from the Attribute we will only need point ID for now.

Attributes <- Attributes[-c(2, 3)]

## Create another row for containing rainfall data.

Attributes[5,1] <- "Rain Fall (mm)"

## read rainfall data: 

rainfall <- read.csv("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Percipation/wgenf.genform1_proc4.csv", stringsAsFactors=FALSE)

## Filter only two columns


rainfall <- rainfall[c("Date.NZST.","Amount.mm.")]

## Convert dates into date format in R

rainfall$Date.NZST. <- base::as.Date(rainfall$Date.NZST., format="%d-%m-%y")


## Get date of all pair imagery to filter the rainfall
# note the firt date is "2019-07-25" which is not included in the column name.

im_dates <- c("2019-07-25", colnames(Attributes)[-1])

## We will filter the rainfall only this period of time.
# Load relevant library

library(dplyr)
library(ggplot2)
library(tidyverse)


rainfall <- rainfall %>% filter(Date.NZST. >= first(im_dates) & Date.NZST. <= last(im_dates))


## we will sum rainfall based the time interval. and add that into our velocity matrix named "Attributes" that we created before. 

for (i in 1:(length(im_dates) - 1 )) {
  if (i == 1) {  
    
    rain <- rainfall %>% filter(Date.NZST. >= im_dates[i] & Date.NZST. <= im_dates[i+1]) # the first pair we need include the first date of the the first pair "2019-07-25".
    sum <- sum(rain$Amount.mm.)
    Attributes[5, i+1] <- sum # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
  } else {
    
    # while the second pair we dont need last date of the pervious pair to sum the data.
    rain <- rainfall %>% filter(Date.NZST. > im_dates[i] & Date.NZST. <= im_dates[i+1])
    sum <- sum(rain$Amount.mm.)
    Attributes[5, i+1] <- sum  # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
  }
}

## Attributes Check

## But we may work abit easier in a long format of the data. We will change wide to long format using this library.

library(reshape2)

Attributes_long <- melt(Attributes, id.vars="Id")
Attributes_long$variable <- as.Date(Attributes_long$variable, format="%Y-%m-%d")

## Change olumns names as well

colnames(Attributes_long) <- c("Variables", "Date", "Values")


## for writing a csv file we can convert to a proper wide format too. and for ploting too

Attributes_wide <- dcast(Attributes_long, Date ~ Variables , value.var="Values")

## Now we plot a simple line graph.
library(RColorBrewer)



pp <- ggplot(data=Attributes_wide,aes(x=Date)) +
  geom_col(data=rainfall, aes(x=Date.NZST., y=Amount.mm./2), fill="#0f5e9c", color="#0f5e9c", width=0.5) +
  geom_point(aes(y=`Velocity Point 1 (m/y)`), size = 1, color="#F8766D") +
  geom_point(aes(y=`Velocity Point 2 (m/y)`), size = 1, color="#7CAE00") +
  geom_point(aes(y=`Velocity Point 3 (m/y)`), size = 1, color="#00BFC4") +
  geom_point(aes(y=`Velocity Point 4 (m/y)`), size = 1, color="#C77CFF") + 
  # geom_point(aes(y=`Rain Fall (mm)`/4), size = 1, color="#F8766D") +
  
  
  geom_line(aes(y=`Velocity Point 1 (m/y)`), size = 0.5, color="#F8766D") +
  geom_line(aes(y=`Velocity Point 2 (m/y)`), size = 0.5, color="#7CAE00") +
  geom_line(aes(y=`Velocity Point 3 (m/y)`), size = 0.5, color="#00BFC4") +
  geom_line(aes(y=`Velocity Point 4 (m/y)`), size = 0.5, color="#C77CFF") + 
  #geom_line(aes(y=`Rain Fall (mm)`/4), size = 0.5, color="#F8766D") + # we will use daily rainfall instead
  scale_x_date(breaks = Attributes_wide$Date) + 
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Velocity (m/year)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*2, name="Rainfall (mm)")
  )

# 
My_Theme = theme(
  axis.title.x = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 10))

pp + My_Theme 


## save it to the csv file. for ploting

write.csv(Attributes_wide,"C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/extracted_short_lived_compared_with_the_first_scene.csv", row.names = FALSE)


















## 19-Jan-2021 ## Franz Josef Glaciers.


#### Extract velocity values from this period of times (Jul 2019 - Marc 2020) - Tasman glacier 
## The velocity is generated in Matlab by comparing among different pairs of imagery between scenes.

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Franz_Josef_Single_Band_Jul_2019_Mar_2020/Velocity/Compared between scences")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Franz_Josef_Single_Band_Jul_2019_Mar_2020/Velocity/Compared between scences"

## Read the stacked raster that contains all velocity compared between scences generated by using Matlab - Imgraft Package.

velocity_stack  <- stack("velocity_stack.tif")

## Also read the the shapefile of the interested points in the lower parts of Tasman glacier.

pointCoordinates=readOGR("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/Franz_Josef.shp")

## Extract raster value by points

rasValue <- extract(velocity_stack, pointCoordinates)


## Combine raster values with point and save as a CSV file


combinePointValue <- cbind(pointCoordinates,rasValue)

## we will use its attribute data

Attributes <- combinePointValue@data

Attributes["Id"] <- c("Velocity Point 1 (m/y)", "Velocity Point 2 (m/y)", "Velocity Point 3 (m/y)", "Velocity Point 4 (m/y)")

Attributes 
## We will also change colomn names to only a single date. 

colnames(Attributes)

## First list all filenames that from a pair of imagery that generated this velocity.

filenames <- list.files(workfolder, pattern = "*.tif")

filenames <- filenames[nchar(filenames)==21]

## use only the last 11 to 18 digits as new column which will be good when converting to date and long format.

colnames(Attributes)[-1] <- paste0(substring(filenames, 10, 13), "-", substring(filenames, 14, 15), "-", substring(filenames, 16, 17))



## read rainfall data: 


rainfall <- read.csv("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Percipation/wgenf.genform1_proc4.csv", stringsAsFactors=FALSE)

## Filter only two columns


rainfall <- rainfall[c("Date.NZST.","Amount.mm.")]

## Convert dates into date format in R

rainfall$Date.NZST. <- base::as.Date(rainfall$Date.NZST., format="%d-%m-%y")

class(rainfall$Date.NZST.) # To check.

## Load dplyr and ggplot2 library for further filtering and ploting. 


library(dplyr)
library(ggplot2)
library(tidyverse)


## create a dataframe that contain date, velocity and sum of rainfall by its number of time interval (days)


## first just remove Lat and Lon column from the Attribute we will only need point ID for now.

## Attributes <- Attributes[-c(2, 3)]

## Create Another row called "Rain Fall" for storing summed rain fall based on the date interval among imagery.

Attributes[5,] <- 0

Attributes[5,1] <- "Rain Fall (mm)"

## Note that the first imagery date is "2018-11-05", and the last imagery is 2020-03-15. But we excluded the first imagery from the attribute. 
## now we will store all the date of imagery

im_dates <- c("2018-12-15", colnames(Attributes)[-1])

## We will filter the rainfall only this period of time.


rainfall <- rainfall %>% filter(Date.NZST. >= first(im_dates) & Date.NZST. <= last(im_dates))

## we will sum rainfall based the time interval. and add that into our velocity matrix named "Attributes" that we created before. 


for (i in 1:(length(im_dates) - 1 )) {
  
  if (i == 1) {  
    
    rain <- rainfall %>% filter(Date.NZST. >= im_dates[i] & Date.NZST. <= im_dates[i+1]) # the first pair we need include the first date of the the first pair "2019-07-25".
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum # add the value to the data.frame that we have. In row 5 and column 2 onward.
    
  } else {
    
    # while the second pair we dont need last date of the pervious pair to sum the data.
    
    rain <- rainfall %>% filter(Date.NZST. > im_dates[i] & Date.NZST. <= im_dates[i+1])
    
    sum <- sum(rain$Amount.mm.)
    
    Attributes[5, i+1] <- sum }  # add the value to the data.frame that we have. In row 5 and column 2 onward.
  
}



## now we get both Velocity and summed rain data in the same matrix.

Attributes[5,] # summed Rainfall based on its time interval.


## But we may work abit easier in a long format of the data. We will change wide to long format using this library.

library(reshape2)

Attributes_long <- melt(Attributes, id.vars="Id")

Attributes_long$variable <- as.Date(Attributes_long$variable, format="%Y-%m-%d")



## Change olumns names as well

colnames(Attributes_long) <- c("Variables", "Date", "Values")

## Now we would like to look at the data between "2019-07-25" and "2020-03-15" only

Attributes_long <- Attributes_long %>% filter( Variables != "Rain Fall (mm)")

## rainfall <- rainfall %>%  filter(Date.NZST. >= "2019-07-25" & Date.NZST. <= "2020-03-31")

## Now we plot a muiltiple line & bar graph.

pp <- ggplot(data=Attributes_long,aes(x=Date,y=Values,color=Variables )) +
  geom_col(data=rainfall, aes(x=Date.NZST., y=Amount.mm.*4), fill="#0f5e9c", color="#0f5e9c", width=0.5) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = Attributes_long$Date) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Velocity (m/year)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./4, name="Rainfall (mm)")
  )

# 
My_Theme = theme(
  axis.title.x = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 10))

pp + My_Theme 




## for writing a csv file we can convert to a proper wide format too.

Attributes_wide <- dcast(Attributes_long, Date ~ Variables , value.var="Values")


## Write it into a csv file for further anlysis

write.csv(Attributes_wide,"C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/TimeSeries Points/Franz_Josef_TimeSeries_velocity_between_scenes_Dec2018_Jul2019_compared_with_rainfall.csv", row.names = FALSE)




















