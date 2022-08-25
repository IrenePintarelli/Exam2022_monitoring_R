# set the working directory where I downloaded the images from https://earthobservatory.nasa.gov/images/150111/lake-mead-keeps-dropping
setwd("C:/R/")

# load the libraries I will need to elaborate the data
library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)
library(patchwork)

# load the pictures lakemead2000 and lakemead2022
lm2000 <- brick("lakemead2000.jpg")
lm2022 <- brick("lakemead2022.jpg")

# to see the pictures together I need to plot them with ggRGB and use the package patchwork (+ = one next to the other, / = one under the other)
lm00 <- ggRGB(lm2000,1,2,3,stretch="lin")
lm22 <- ggRGB(lm2022,1,2,3,stretch="lin")
lm00 / lm22
lm00 + lm22

dev.off()

# check the info contained and the number of pixels and layers in the first image lm2000
lm2000
# being an elaborated image from Landsat we have 3 layers
## 1 = NIR
## 2 = RED
## 3 = GREEN
# plot the 3 bands (separated)
plot(lm2000)

dev.off()

# create a plot of the image with different classes to distinguish the soil and the lake surface
# I want only one class which highlights the lake so I start with 2 classes and then I see
lm2000_c2 <- unsuperClass(lm2000,nClasses=2)
plot(lm2000_c2$map) # $map is necessary as it's a model otherwise, I cannot plot it if I don't specify map
# part of soil is classified as the lake so try with 3 classes
lm2000_c3 <- unsuperClass(lm2000,nClasses=3)
plot(lm2000_c3$map)
# if I plot this image with the original one, I see there's more correspondance with 1 class only for the lake and two other classes for the soil
par(mfrow=c(2,1))
plotRGB(lm2000,1,2,3,stretch="lin")
plot(lm2000_c3$map)
# check the number of pixels in the different classes
freq(lm2000_c3$map)

# I write down the number of pixels related to the soil summing them together, if (not always like that)
# 3th and 2nd classes: green and yellow <- soil
# 1st class: white <- water
soil <- 13091536 + 9153582
lake <- 3011822

# the total number of pixel (soil + lake)
tot2000 <- 25256940

# calculate the percentages of water surface and soil
perc_lake_2000<- lake / tot2000 * 100
perc_lake_2000
# 11.92473
perc_soil_2000 <- soil / tot2000 * 100
perc_soil_2000
# 88.07527

# in order to compare the images I need to follow the same steps for lm2022

# check the info contained and the number of pixels and layers in the first image lm2022
lm2022
# being an elaborated image from Landsat we have 3 layers
## 1 = NIR
## 2 = RED
## 3 = GREEN
# plot the 3 bands
plot(lm2022)

dev.off ()

# create a plot of the image with different classes to distinguish the soil and the land covered by the lake
lm2022_c3 <- unsuperClass(lm2022,nClasses=3)
plot(lm2022_c3$map)
# plotting the original image and the one unsupervised with 3 classes, like I did with the image related to 2000, I see there are different soil areas considered as lake 
# I try to increase the number of classes 
lm2022_c4 <- unsuperClass(lm2022,nClasses=4)
# change the colours of the map creating a coloRampPalette
cl <- colorRampPalette(c("red","orange","green","blue"))(100)
plot(lm2022_c4$map,col=cl)

dev.off()

# then plot the original image together with the unsupervised one
par(mfrow=c(2,1))
plotRGB(lm2022,1,2,3,stretch="Lin")
plot(lm2022_c4$map,col=cl)

# now I check the number of pixels for each class
freq(lm2022_c4$map)
# 1 : red --> lake
# 2 : orange --> soil
# 3 : green --> soil
# 4 : blue --> soil
# 1+2+4 --> soil

# I sum the pixelrs related to the soil
soil22 <- 6160042 + 6874819 + 10522644
lake22 <- 1699435
# I need the total amount of pixels for the calculation of the percentages
tot22 <- soil22 + lake22
tot22

# calculate the percentages
perc_lake_22 <- lake22 / tot22 * 100
perc_lake_22 
# 6.728586
perc_soil_22 <- soil22 / tot22 * 100
perc_soil_22 
# 93.27141

# biuld a data frame with the differences between 2000 and 2022
# create the columns
class <- c("soil %","lake surface %")
year2000 <- c(88.07527, 11.92473)
year2022 <- c(93.27141,6.728586)
# create the dataframe
lake_mead <- data.frame(class,year2000,year2022)
lake_mead
View(lake_mead)

# build a data frame only with the percentages of the lake dimension
years <- c("2000","2022")
lake_surface <- c(11.92473,6.728586)
lake_variation <- data.frame(years,lake_surface)

# I create a graphic of the variation of the lake surface
ggplot(lake_variation,aes(x=years,y=lake_surface,color=years))+geom_bar(stat="identity",fill="white")


# try to calculate the different percentages only between the lake surfaces in 2000 and 2022
# I consider the lake surface in 2000 as the 100%
# calculate the difference between the two percentages and then how smaller the lake got (100 : x = 11.92473 : diff_perc)
diff_perc <- perc_lake_2000 - perc_lake_22
diff_ls <- 100 * diff_perc / 11.92473
diff_ls
# 43.57452

################################################## end ################################################


# now analyse the area around the lake not just the change in dimensions of the lake surface
# elaborate a PCA analysis of both images and compare if there are differences in the hetereogenity of the entire area
lm2000_pca <- rasterPCA(lm2000)
# to understand which component contain more info
summary(lm2000_pca$model)
# as the proportion of variance is higher in the first band I visualize the map only of the first component
lm2000_pca_c1 <- lm2000_pca$map$PC1
lm2000_pca_c1
# I calculate the standard deviation using a matrix of 3 x 3 and the function focal
lm00_sd <- focal(lm2000_pca_c1,matrix(1/9,3,3),fun=sd)

# let's see the map of the standard deviation of the component 1
c1_2000 <- ggplot()+geom_raster(lm00_sd,mapping=aes(x=x,y=y,fill=layer))+scale_fill_viridis(option="inferno")+ggtitle("Standard Deviation for C1 in 2000")

# if I wanna see the comparison with the original map
lm00 + c1_2000

# even if the hetereogenity is quite low in 2000, I can see how is the result for the 2022 image doing the same passages
lm2022_pca <- rasterPCA(lm2022)
summary(lm2022_pca$model)
# even in this case the proportion of variance is higher for the first component
lm2022_pca_c1 <- lm2022_pca$map$PC1
lm2022_pca_c1
# calculation of the standard deviation using the function focal
lm22_sd <- focal(lm2022_pca_c1,matrix(1/9,3,3),fun=sd)

# let's see the map of the standard deviation of the component 1
library(viridis)
c1_2022 <- ggplot()+geom_raster(lm22_sd,mapping=aes(x=x,y=y,fill=layer))+scale_fill_viridis(option="inferno")+ggtitle("Standard Deviation for C1 in 2022")


# to see the comparison with the original map
lm22 + c1_2022

# to see the comparison between the 2 different years
c1_2000 + c1_2022

# as we can see from the both analysis the hetereogenity is quite low everywhere except for the area near the lake, but there's not big differences between the 2 years
jpeg("Standard_Deviation.jpg")
c1_2000 + c1_2022
dev.off()








