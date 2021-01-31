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



# 


Attributes <- Attributes[-c(4,5), ]

Attributes[4,] <- 0
Attributes[4, 1] <- "days"

Attributes


im_dates <- as.Date(im_dates , format="%Y-%m-%d")

class(im_dates)


for (i in 1:(length(im_dates)-1)) {
  
  d <- as.numeric(im_dates[i+1]-im_dates[i])
  Attributes[4, 1+i] <- d
 
}


library(reshape2)

Attributes_long <- melt(Attributes, id.vars="Id")

Attributes_long$variable <- as.Date(Attributes_long$variable, format="%Y-%m-%d")


Attributes_long[Attributes_long$Id==1,]$Id <- "Velocity Point 1 (m/y)"
Attributes_long[Attributes_long$Id==2,]$Id <- "Velocity Point 2 (m/y)"
Attributes_long[Attributes_long$Id==3,]$Id <- "Velocity Point 3 (m/y)"

colnames(Attributes_long) <- c("Variables", "Date", "Values")
Attributes_long

Attributes_wide <- dcast(Attributes_long, Date ~ Variables , value.var="Values")

Attributes_wide


plot(Attributes_wide$`Velocity Point 1 (m/y)`, Attributes_wide$days)
plot(Attributes_wide$`Velocity Point 2 (m/y)`, Attributes_wide$days)

plot(Attributes_wide$`Velocity Point 3 (m/y)`, Attributes_wide$days)
