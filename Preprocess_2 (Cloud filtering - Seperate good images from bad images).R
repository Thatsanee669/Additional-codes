### these fillowing R codes will be used to automatically saperate useful images from the dataset.###
### All croped images in the dataset are not always good. Some of them are mostly covered by cloud which will not be useful ###
### For glacier velocity measurement using GIV package. ###
### After several experiments,  we have noticed that the JPG files covered heavily by cloud have a much smaller size.###
### Thus, We will try to use R programming to sort the image by size and seperate the bad images from the good ones. ###


## For Franz Josef Glacier.


#set working directory
setwd("H:/Satellite Imagery/Franz_Josef/From_linux/jpg")
# Select only the .jpg file 
files <- list.files(path="./", pattern="*.jpg")
files <- files[nchar(files)==12] 
# get file infor mation especial the size of the file. 
x <- file.info(files)
x <- x[order(-x$size),] # if wanna provide ranking use the following codes.
x <- x[1]
# X is now in list, we will convert it into dataframe
df <- data.frame(image = row.names(x), size = x$size, stringsAsFactors=FALSE)
# largest size if the image
max_size <- max(x$size)
# smallest size of the image
min_size <- min(x$size)
# How about middle size?
mid_size <- (max(x$size) - min(x$size))/2
# As comparison, the image with 10 % smaller than a middle size is considered 
# as bad image mostly covered by cloud 
# or around 40% compare to the maximum size image.
quality_line <- max_size*40/100
# List of the image below quality line is :
df_good_images <- df[which(x$size > quality_line),]
good_images <- df_good_images[,1]
## first check how many bad images are there?
table(files %in% good_images) # 22 images are bad while 56 are good.
# save new images into new folders
output_folder <- "H:/Satellite Imagery/Franz_Josef/From_linux/jpg_good"
input_folder <- "H:/Satellite Imagery/Franz_Josef/From_linux/jpg"
# I will copy the good_images into new folder using "for loop
for (i in 1:length(good_images)) {
  image <- good_images[i]
  file.copy(from=paste0(input_folder,"/", image), to=paste0(output_folder, "/", image))  
  print(i) # this is to see the progress
  }



#### For Tasman Glacier




## set working directory
setwd("H:/Satellite Imagery/Tasman/From_linux/jpg")
# Select only the .jpg file 
files <- list.files(path="./", pattern="*.jpg")
files <- files[nchar(files)==12] 
# get file infor mation especial the size of the file. 
x <- file.info(files)
x <- x[order(-x$size),] # if wanna provide ranking use the following codes.
x <- x[1]
# X is now in list, we will convert it into dataframe
df <- data.frame(image = row.names(x), size = x$size, stringsAsFactors=FALSE)
# largest size if the image
max_size <- max(x$size)
# smallest size of the image
min_size <- min(x$size)
# How about middle size?
mid_size <- (max(x$size) - min(x$size))/2
# In Tasman, which is lower part of glaciers,
# As comparison, the image with 10 % of max size is considered as bad image mostly covered by cloud.
quality_line <- max_size*10/100
# List of the image below quality line is :
df_good_images <- df[which(x$size > quality_line),]
df_good_images
good_images <- df_good_images[,1]
good_images
## first check how many bad images are there?
table(files %in% good_images) # 22 images are bad while 56 are good.
# save new images into new folders
output_folder <- "H:/Satellite Imagery/Tasman/From_linux/jpg_good"
input_folder <- "H:/Satellite Imagery/Tasman/From_linux/jpg"
# I will copy the good_images into new folder using "for loop
for (i in 1:length(good_images)) {
  image <- good_images[i]
  file.copy(from=paste0(input_folder,"/", image), to=paste0(output_folder, "/", image))  
  # this is to see the progress
  print(i)
  }





