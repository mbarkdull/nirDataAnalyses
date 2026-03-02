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

rawNir$irPlusVis <- rawNir$`Average IR` + rawNir$`Average Visible`

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


#Plotting Vis and IR:

ggplot(data = spatialNirData, 
       mapping = aes(x = `Average Visible`,
                     y = `Average IR`)) + 
  geom_point() + 
  geom_smooth() 


cor(spatialNirData$`Average Visible`,
    spatialNirData$`Average IR`, 
    use = "complete.obs")


#code to add a column to spatial data 
#for climate variables that we're gonna use 

names(nirClimateData) <- paste0("bio", 1:19)
spatialNirData$meanTempDriestQuarter <- terra::extract(nirClimateData[["bio9"]],
                                                spatialNirData)$bio9


spatialNirData$isothermality <- terra::extract(nirClimateData[["bio3"]],
                                                spatialNirData)$bio3

spatialNirData$tempSeasonality<- terra::extract(nirClimateData[["bio4"]],
                                               spatialNirData)$bio4

spatialNirData$annualMeanTemp <- terra::extract(nirClimateData[["bio1"]],
                                               spatialNirData)$bio1

spatialNirData$maxTempWarmestMonth <- terra::extract(nirClimateData[["bio5"]],
                                                spatialNirData)$bio5

spatialNirData$minTempColdestMonth <- terra::extract(nirClimateData[["bio6"]],
                                               spatialNirData)$bio6

#adding in solar data
dir.create("./data/worldclimSolar",
           recursive = TRUE)

nirSolarData <- worldclim_global(var = "srad",
                                 res = 10,
                                 path = "./data/worldclimSolar") 
spatialNirData <- st_transform(spatialNirData,
                               src = st_crs(spatialNirData),
                               crs = crs(nirSolarData))

spatialNirData$solarRadiationJan <- terra::extract(nirSolarData[["wc2.1_10m_srad_01"]],
                                                   spatialNirData)$wc2.1_10m_srad_01

spatialNirData$solarRadiationFeb <- terra::extract(nirSolarData[["wc2.1_10m_srad_02"]],
                                                   spatialNirData)$wc2.1_10m_srad_02

spatialNirData$solarRadiationMar <- terra::extract(nirSolarData[["wc2.1_10m_srad_03"]],
                                                   spatialNirData)$wc2.1_10m_srad_03

spatialNirData$solarRadiationApr <- terra::extract(nirSolarData[["wc2.1_10m_srad_04"]],
                                                   spatialNirData)$wc2.1_10m_srad_04

spatialNirData$solarRadiationMay <- terra::extract(nirSolarData[["wc2.1_10m_srad_05"]],
                                                   spatialNirData)$wc2.1_10m_srad_05

spatialNirData$solarRadiationJun <- terra::extract(nirSolarData[["wc2.1_10m_srad_06"]],
                                                   spatialNirData)$wc2.1_10m_srad_06

spatialNirData$solarRadiationJul <- terra::extract(nirSolarData[["wc2.1_10m_srad_07"]],
                                                   spatialNirData)$wc2.1_10m_srad_07

spatialNirData$solarRadiationAug <- terra::extract(nirSolarData[["wc2.1_10m_srad_08"]],
                                                   spatialNirData)$wc2.1_10m_srad_08

spatialNirData$solarRadiationSept <- terra::extract(nirSolarData[["wc2.1_10m_srad_09"]],
                                                    spatialNirData)$wc2.1_10m_srad_09

spatialNirData$solarRadiationOct <- terra::extract(nirSolarData[["wc2.1_10m_srad_10"]],
                                                   spatialNirData)$wc2.1_10m_srad_10

spatialNirData$solarRadiationNov <- terra::extract(nirSolarData[["wc2.1_10m_srad_11"]],
                                                   spatialNirData)$wc2.1_10m_srad_11

spatialNirData$solarRadiationDec <- terra::extract(nirSolarData[["wc2.1_10m_srad_12"]],
                                                   spatialNirData)$wc2.1_10m_srad_12


spatialNirData <- spatialNirData %>%
  rowwise() %>%
  mutate(solarMean = mean(c_across(c(starts_with('solarRadiation'))), 
                          na.rm=TRUE))


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
                     y = `Average IR`)) + 
  geom_point() +
  geom_smooth()

ggplot(data = spatialNirData, 
       mapping = aes(x = tempSeasonality, 
                     y = `Average IR`)) + 
  geom_point() +
  geom_smooth()

ggplot(data = spatialNirData, 
       mapping = aes(x = minTempColdestMonth, 
                     y = `Average IR`)) + 
  geom_point() +
  geom_smooth()


#linear models

M1 <- lm(`Average IR` ~ minTempColdestMonth + maxTempWarmestMonth +
           tempSeasonality, data = spatialNirData)
summary(M1)

M2 <- glm(`Average IR` ~ minTempColdestMonth + maxTempWarmestMonth +
           tempSeasonality, data = spatialNirData,
          family = poisson)
summary(M2)


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


#Set up glm model .. don't need?

data <- glm(data = spatialNirData, 
            averageIR ~ annualMeanTemp + tempSeasonality,
            family = poisson)
summary(data)

# Back-transform the intercept
exp(coef(data)[1])

# Back-transform the slope. Change the 2 or 3 for variables.
exp(coef(data)[2])

#geom_line
#simple scatter plot is a no because we have more than 2 dimensons 
ggplot(data = spatialNirData,
       mapping = (aes(x = annualMeanTemp,
                   y = `Average IR`))) + 
  geom_point() + 
  geom_abline(slope = exp(coef(data)[2]),
              intercept = exp(coef(data)[1]))

#Create a function is next to do for code
