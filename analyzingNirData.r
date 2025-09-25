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


#code to add a column to spatial data for climate variables that we're gonna use 
names(nirClimateData) <- paste0("bio", 1:19)
spatialNirData$annualMeanTemp <- terra::extract(nirClimateData[["bio1"]],
                                                spatialNirData)$bio1 


spatialNirData$maxTempWarmestMonth <- terra::extract(nirClimateData[["bio5"]],
                                                spatialNirData)$bio5 
