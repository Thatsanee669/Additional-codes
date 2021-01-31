### This is to resample temporal resolution of the imagery. To understand further wether how does it temporal resolution affect its variations cause by rainfall.###
### In this codes, I will try to de temporal resolution from up to 2 days to at least 5 days apart, 15 days, and 30 days. 
### We will use Data in Tasman to do it by using a longer time series imagery. 

### The imagery is between 15-12-2018 to 28-08-2020 


## Set work foldder

setwd("C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Single_Band_Good_IM_2018-2020_UTM")

workfolder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Single_Band_Good_IM_2018-2020_UTM"

filenames <- list.files(workfolder, pattern = "*.tif")
filenames <- filenames[nchar(filenames)==12]

## We will filter all the imagery between 16-01-2019 to 25-03-2020 only.

filenames <- filenames[14:78]
length (filenames)

imdates <- as.Date(
  
  paste0( 
    
    substring(filenames, 1, 4), 
    "-", 
    substring(filenames, 5, 6), 
    "-", 
    substring(filenames, 7, 8)
    )
  
  )

# create empty lists to store time interval and imagery with new revisit times. 

time_interval_between_scenes <- vector()

five_days <- vector()



# create for loop to append each date with the right interval to the empty lists created before.

for (i in 1:length(imdates)-1) {
  
  j <- i + 1
  
 
  time_interval_between_scenes[i] <- as.numeric(imdates[j] - imdates[i])
  
}

# Now we will add data into new datasets that contains different temporal resolution

# sum(1:i) or # consider using remove from the whole files might work as well. 
 
## Now we can get the name of the imagery that has at least 5 days or higher  from its time series imagery.


for (i in 1:length(time_interval_between_scenes)) {
  p <- 5
  
  j <- i + 1
  
  five_days[1] <- filenames[1]
  
  
  if (sum(time_interval_between_scenes[1:i]) >= p) {
    
    five_days[j] <- filenames[j]
    
    time_interval_between_scenes[1:i] <- 0 # after we count we will assign it to 0 to start sum it again.
    
  } else { print(i)} # just to see which imagery was removed
  
 
}


### Remove NA from the 5-days resolution imagery
  # note that when the time interval is less than 5 we will not append any imagery into the empty list. we will get NA and we will remove it now. 

five_days <- five_days[!is.na(five_days)]

  ## The number of imagery using good imagery between  between 15-12-2018 to 28-08-2020  is 97 

length(filenames) 

  ## When reduce its revisit times to at least 5 days. the number would reduce from 97 to 56. 

length(five_days)

## Time interval between image pair before was between 2 to 34 days depending on the cloud cover. 

##  time_interval_between_scenes before

#     10  4  2  6  2  6 14  2  2  6  6  4  2  4 16  2  4  4 10 10  4  2  2  2 26 10  2  4  4 18 28  4 24  2  2 12 10
#     10 12 14  2  2 14  2 12  6  2  4  2 34  2  2  4  2  4  2 10  4  2  2  2  2  2  6

##  time_interval_between_scenes now for at least 5 days now
#   10  6  6  8 14 10  6  6 20  6 14 10  6 30 10  6 22 28 28 16 10 10 12 14 18 14  6  6 36  8  6 12  6  6  8

imdate_5days <- as.Date(
  
  paste0( 
    
    substring(five_days, 1, 4), 
    "-", 
    substring(five_days, 5, 6), 
    "-", 
    substring(five_days, 7, 8)
  )
  
)

time_interval_5days <- vector()

for (i in 1:length(imdate_5days)-1) {
  
  j <- i + 1
  
  
  time_interval_5days [i] <- as.numeric(imdate_5days[j] - imdate_5days[i])
  
}

time_interval_5days

length(imdate_5days)






### now we will get new temporal resolution to new folder.









# save new images into new folders

output_folder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Study Areas/Tasman/Tif_UTM - Deresolution/Detemporal-Resolution/5 days"
input_folder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/ImGraft/Tasman_Single_Band_Good_IM_2018-2020_UTM"

# I will copy the good_images into new folder using "for loop

for (i in 1:length(five_days) ){
  
  image <- five_days[i]
  
  file.copy(from=paste0(input_folder,"/", image), to=paste0(output_folder, "/", image))  
  
  # this is to see the progress
  print(i)
  
}













##### 01-01-2021
###### we get imagery that has at least 16 days time interval and 30 days interval. 

six_teen_days <- vector() # create an empty
thirty_days <- vector() # create an empty

## we will still use the same variables used previously

filenames # all images # name with tif format
imdates  # all dates of images # date format

for (i in 1:length(imdates)-1) {
  
  j <- i + 1
  
  
  time_interval_between_scenes[i] <- as.numeric(imdates[j] - imdates[i])
  
  
}

# we will assign new names to it because when we use for loop we may need to modify it and dont wanna change the original data.

interval_for_16 <- time_interval_between_scenes # time interval data

interval_for_30 <- time_interval_between_scenes # time interval data


#### Now we can get the name of the imagery that has at least 5 days or higher  from its time series imagery.

## for 16 days


for (i in 1:length(interval_for_16)) {
  
  
  
  p <- 16
  
  j <- i + 1
  
  six_teen_days[1] <- filenames[1]
  
  
  if (sum(interval_for_16[1:i]) >= p) {
    
    six_teen_days[j] <- filenames[j]
    
    interval_for_16[1:i] <- 0 # after we count we will assign it to 0 to start sum it again.
    
  } else { print(i)} # just to see which imagery was removed
  
  
}


## for 30 days.


for (i in 1:length(interval_for_30)) {
  
  
  
  p <- 30
  
  j <- i + 1
  
  thirty_days [1] <- filenames[1]
  
  
  if (sum(interval_for_30[1:i]) >= p) {
    
    thirty_days[j] <- filenames[j]
    
    interval_for_30[1:i] <- 0 # after we count we will assign it to 0 to start sum it again.
    
  } else { print(i)} # just to see which imagery was removed
  
  
}



## remove NA

six_teen_days <- six_teen_days[!is.na(six_teen_days)]

thirty_days <- thirty_days[!is.na(thirty_days)]


## the number of imagery now


length(six_teen_days) # from 65 imagery to 20 images now

length(thirty_days) # # from 65 imagery to 13 images now


## check time_intervals for both of them now 

imdate_16 <- as.Date(
  
  paste0( 
    
    substring(six_teen_days, 1, 4), 
    "-", 
    substring(six_teen_days, 5, 6), 
    "-", 
    substring(six_teen_days, 7, 8)
  )
  
)

imdate_30 <- as.Date(
  
  paste0( 
    
    substring(thirty_days, 1, 4), 
    "-", 
    substring(thirty_days, 5, 6), 
    "-", 
    substring(thirty_days, 7, 8)
  )
  
)

time_interval_16 <- vector()

time_interval_30 <- vector()


for (i in 1:length(imdate_16)-1) {
  
  j <- i + 1
  
  
  time_interval_16 [i] <- as.numeric(imdate_16[j] - imdate_16[i])
  
}

for (i in 1:length(imdate_30)-1) {
  
  j <- i + 1
  
  
  time_interval_30 [i] <- as.numeric(imdate_30[j] - imdate_30[i])
  
}

time_interval_16 # 18 24 20 16 18 22 20 16 30 16 22 28 28 16 20 26 18 20 42 16 16 20 22 24 16 22 16 16 22 # 29 pairs.
time_interval_30 # 30 32 30 32 30 30 38 32 40 32 32 62 30 32 36 30 32 30 # 18 pairs



#### save new images into new folders

## 16 DAYS.
output_folder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Study Areas/Tasman/Tif_UTM - Deresolution/Detemporal-Resolution/16 days"
  # we will use the same input folder

## copy the good_images into new folder using "for loop

for (i in 1:length(six_teen_days) ){
  
  image <- six_teen_days[i]
  
  file.copy(from=paste0(input_folder,"/", image), to=paste0(output_folder, "/", image))  
  
  # this is to see the progress
  print(i)
  
}

## 30 days
output_folder <- "C:/Users/Advice/OneDrive - Victoria University of Wellington - STUDENT/GISC 511/Study Areas/Tasman/Tif_UTM - Deresolution/Detemporal-Resolution/30 days"
      # we will use the same input folder

## copy the good_images into new folder using "for loop
for (i in 1:length(thirty_days) ){
  
  image <- thirty_days[i]
  
  file.copy(from=paste0(input_folder,"/", image), to=paste0(output_folder, "/", image))  
  
  # this is to see the progress
  print(i)
  
}


### The end We will use these three temporal resolution to monitor if we can see any variations of the glacier movement using Matlab ###














