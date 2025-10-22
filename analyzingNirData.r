library(googlesheets4)
library(tidyverse)
library(scales)
library(lubridate)
library(janitor)
library(sf) 
library(spData)
library(terra)
library(geodata)


rawNir <- read_sheet("https://docs.google.com/spreadsheets/d/1dHIhVuh-Sy2clvqRpMiu5mDXpaXXp3W5HgLbzs2rYNo/edit?usp=sharing") %>%
  filter(Latitude != "NULL", 
         Longitude != "NULL")

ggplot(data = rawNir, 
       mapping = aes(x = Latitude, 
                     y = `Average IR`)) + 
  geom_point() + 
  geom_smooth() 

#create spatial data object out of my data 

spatialNirData <- st_as_sf(rawNir,
                           coords = c("Longitude", 
                                      "Latitude"))
spatialNirData <- st_set_crs(spatialNirData, 
                             "+proj=longlat +datum=WGS84")

#Downloading Climate Data

dir.create("./data/worldclim",
           recursive = TRUE)

nirClimateData <- worldclim_global(var = "bio",
                                   res = 10,
                                   path = "./data/worldclim") 

#Transformed my spatial data coordinates to what the worldclim format is 

spatialNirData <- st_transform(spatialNirData,
                               src = st_crs(spatialNirData),
                               crs = crs(nirClimateData))


#code to add a column to spatial data 
#for climate variables that we're gonna use 

names(nirClimateData) <- paste0("bio", 1:19)
spatialNirData$meanTempDriestQuarter <- terra::extract(nirClimateData[["bio9"]],
                                                spatialNirData)$bio9


spatialNirData$isothermality <- terra::extract(nirClimateData[["bio3"]],
                                                spatialNirData)$bio3


#what data should look at (spatialNIR) --> tell it diff ways to visualize that data (scatter, line, etc.) done 
#by adding geoms (variables and plot them)

ggplot(data = spatialNirData,
       mapping = aes(x = meanTempDriestQuarter, 
                     y = `Average IR`)) + 
  geom_point() + 
  geom_smooth()


ggplot(data = spatialNirData, 
       mapping = aes(x = annualMeanTemp, 
                     y = `Average Visible`)) + 
  geom_point() +
  geom_smooth()

ggplot(data = spatialNirData, 
       mapping = aes(x = maxTempWarmestMonth, 
                     y = `Average Visible`)) + 
  geom_point() +
  geom_smooth()

ggplot(data = spatialNirData, 
       mapping = aes(x = maxTempWarmestMonth, 
                     y = `Average IR`)) + 
  geom_point() +
  geom_smooth()

#linear regression model:

ggplot(data = spatialNirData, 
       mapping = aes(x = `Average Visible`,
                     y = `Average IR`)) + 
  geom_point() + 
  geom_smooth() 


cor(spatialNirData$`Average Visible`,
    spatialNirData$`Average IR`, 
    use = "complete.obs")


#linear regression 
visibleAndIRModel <- lm(`Average IR` ~ `Average Visible`, data = spatialNirData,
                    na.action = na.exclude) 
#got na.action = na.exclude from google because it was saying 
#that the resodials has 213 rows and the data has 392, when i was trying to run 
#line 112. Now it's running ok? 

summary(visibleAndIRModel)

#Residuals 

visAndIRResiduals<- residuals(visibleAndIRModel)
spatialNirData$visAndIRRresiduals <- visAndIRResiduals



#plotting vis with residuals (line isn't positive anymore)
ggplot(data = spatialNirData, 
       mapping = aes(x = `Average Visible`, 
                     y = visAndIRRresiduals)) + 
  geom_point() + 
  geom_smooth()


#PCA
#set working directory to the 
#worldclim files first and create raster lines + stack
#used cogwheel icon to print correct setwd path

bioFiles <- list.files(path = "./data/worldclim/climate/wc2.1_10m",
                       pattern = "tif$", full.names = TRUE)

#stack is used to put all the bio data together 
bioClimStack <- terra::rast(bioFiles)

#
randomPoints <- terra::spatSample(bioClimStack[[1]],
                                   size = 10000, method = "random", 
                                   na.rm = TRUE, as.points = TRUE)
bioClimData <- terra::extract(bioClimStack, randomPoints, bind = TRUE) %>%
  as.data.frame() %>%
  drop_na()

str(bioClimData)

pcaResult <- prcomp(bioClimData, center = TRUE, scale = TRUE)
summary(pcaResult)


#solar radiation in the climate data 
#
