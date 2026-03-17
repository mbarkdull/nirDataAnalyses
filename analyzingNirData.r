library(googlesheets4)
library(tidyverse)
library(scales)
library(lubridate)
library(janitor)
library(sf) 
library(spData)
library(terra)
library(geodata)
library(ggplot2)
library(AICcmodavg)


#Create a function is next to do for code
analyzingNIRData <- function(inputGenus) {
  rawNir <- read_sheet("https://docs.google.com/spreadsheets/d/1dHIhVuh-Sy2clvqRpMiu5mDXpaXXp3W5HgLbzs2rYNo/edit?usp=sharing") %>%
    filter(Latitude != "NULL", 
           Longitude != "NULL",
           Genus == inputGenus)
  
  rawNir$irPlusVis <- rawNir$`Average IR` + rawNir$`Average Visible`
  
  #create spatial data object out of my data 
  
  spatialNirData <- st_as_sf(rawNir,
                             coords = c("Longitude", 
                                        "Latitude"), remove = FALSE)
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
  
  
  #Plotting Vis and IR (use this one):
  
  irVisPlot <- ggplot(data = spatialNirData, 
                      mapping = aes(x = `Average Visible`,
                                    y = `Average IR`)) + 
    geom_point(alpha = 0.6) + 
    geom_smooth(method = "lm", color = "black", fill = "lightgreen") +
    annotate("text", x = Inf, y = Inf, 
             label = paste("r =", round(correlation, 4)),
             hjust = 1.1, vjust = 1.5, size = 4) +
    labs(title = "IR vs. Visible Reflectivity",
         x = "% Visible Reflectivity",
         y = "% IR Reflectivity") +
    theme_classic()
  
  saveRDS(object = irVisPlot,
          file = paste(inputGenus,
                       "_irVisPlot.RDS",
                       sep = ""))
  
  
  correlation <- cor(spatialNirData$`Average Visible`,
                     spatialNirData$`Average IR`, 
                     use = "complete.obs")
  correlation
  saveRDS(object = correlation,
          file = paste(inputGenus,
                       "_correlationVisIR.RDS",
                       sep = ""))
  #don't use this one: 
  irVisPlot2 <- ggplot(data = spatialNirData, 
                      mapping = aes(x = `Average Visible`,
                                    y = `Average IR`)) + 
    geom_point() + 
    geom_smooth() +
    labs(title = paste("Correlation of mean IR and\nmean visible reflectance;\ncorrelation = ",
                       correlation,
                       sep = ""))
  
  
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
  
  spatialNirData$mTWQ <- terra::extract(nirClimateData[["bio10"]],
                                                       spatialNirData)$bio10
  
  spatialNirData$mTCQ <- terra::extract(nirClimateData[["bio11"]],
                                                          spatialNirData)$bio11
  
  spatialNirData$percipColdestQ <- terra::extract(nirClimateData[["bio19"]],
                                        spatialNirData)$bio19
  
  
  
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
  
  #linear regression 
  visibleAndIRModel <- lm(`Average IR` ~ `Average Visible`, data = spatialNirData,
                          na.action = na.exclude) 
  #got na.action = na.exclude from google because it was saying 
  #that the resodials has 213 rows and the data has 392, when i was trying to run 
  #line 112. Now it's running ok? 
  
  summary(visibleAndIRModel)
  saveRDS(object = visibleAndIRModel,
          file = paste(inputGenus,
                       "_visibleAndIRModel.RDS",
                       sep = ""))
  
  #Residuals 
  
  visAndIRResiduals<- residuals(visibleAndIRModel)
  spatialNirData$visAndIRRresiduals <- visAndIRResiduals
  
  #linear models
  
  M1 <- lm(visAndIRResiduals ~ mTCQ*tempSeasonality +
             annualMeanTemp, data = spatialNirData)
  summary(M1)
  saveRDS(object = M1,
          file = paste(inputGenus,
                       "_m1Model.RDS",
                       sep = ""))
  M2 <- lm(visAndIRResiduals ~ tempSeasonality*solarMean +
             annualMeanTemp, data = spatialNirData)
  summary(M2)
  saveRDS(object = M2,
          file = paste(inputGenus,
                       "_m2Model.RDS",
                       sep = ""))
  M3 <- lm(visAndIRResiduals ~ mTCQ* solarMean +
             annualMeanTemp, data = spatialNirData)
  summary(M3)
  saveRDS(object = M3,
          file = paste(inputGenus,
                       "_m3Model.RDS",
                       sep = ""))
  M4 <- lm(visAndIRResiduals ~ mTCQ +
             annualMeanTemp, data = spatialNirData)
  summary(M4)
  saveRDS(object = M4,
          file = paste(inputGenus,
                       "_m4Model.RDS",
                       sep = ""))
  
  M5 <- lm(visAndIRResiduals ~ percipColdestQ * tempSeasonality + annualMeanTemp,
           data = spatialNirData)
  summary(M5)
  saveRDS(object = M5, 
          file = paste(inputGenus, 
                       "_m5Model.RDS",
                       sep = ""))
  
  fullModel <- lm(visAndIRResiduals ~ percipColdestQ + tempSeasonality + 
                    solarMean + mTCQ +
                  mTWQ + annualMeanTemp, data = spatialNirData)
  summary(fullModel)
  saveRDS(object = fullModel, 
          file = paste(inputGenus,
                       "_fullModel.RDS",
                       sep = ""))
  
  
  #plotting vis with residuals (line isn't positive anymore)
  ggplot(data = spatialNirData, 
         mapping = aes(x = `Average Visible`, 
                       y = visAndIRRresiduals)) + 
    geom_point() + 
    geom_smooth()
  
  #plot for sampling locations
  ggplot() +
    geom_polygon(data = us_map,
                 aes(x = long, y = lat, group = group),
                 fill = "grey95", color = "grey60") +
    geom_point(data = spatialNirData %>% 
                 group_by(Latitude, Longitude) %>% 
                 mutate(n = n()),
               aes(x = Longitude, y = Latitude, size = n, color = n),
               alpha = 0.6) +
    scale_size_continuous(name = "# Specimens") +
    scale_color_viridis_c() +
    guides(color = "none") +
    coord_fixed(1.3, xlim = c(-125, -66), ylim = c(25, 50)) +
    theme_void() +
    theme(
      plot.margin = margin(t = 20, r = 40, b = 20, l = 20),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = "Prenolepis imparis Sampling Locations",
         x = NULL, y = NULL) 
  
}


#Prenolepis

analyzingNIRData(inputGenus = "Prenolepis")


prenolepisM1Model <- readRDS("Prenolepis_m1Model.RDS")
summary(prenolepisM1Model)
prenolepisM2Model <- readRDS("Prenolepis_m2Model.RDS")
summary(prenolepisM2Model)
prenolepisM3Model <- readRDS("Prenolepis_m3Model.RDS")
summary(prenolepisM3Model)
prenolepisM4Model <- readRDS("Prenolepis_m4Model.RDS")
summary(prenolepisM4Model)
prenolepisM5Model <- readRDS("Prenolepis_m5Model.RDS")
summary(prenolepisM5Model)
prenolepisFullModel <- readRDS("Prenolepis_fullModel.RDS")
summary(prenolepisFullModel)


prenolepisIRVisPlot <- readRDS(file = "Prenolepis_irVisPlot.RDS")
plot(prenolepisIRVisPlot)


prenolepisModelList <- list(
  "Cold" = prenolepisM1Model,
  "Solar Radiation" = prenolepisM2Model,
  "Solar x Cold" = prenolepisM3Model,
  "Warm" = prenolepisM4Model,
  "Precipitation" = prenolepisM5Model,
  "Full Model" = prenolepisFullModel)

aictab(cand.set = prenolepisModelList)


#Tapinoma 

analyzingNIRData(inputGenus = "Tapinoma")

tapinomaIRVisPlot <- readRDS(file = "Tapinoma_irVisPlot.RDS")
plot(tapinomaIRVisPlot)

tapinomaM1Model <- readRDS("Tapinoma_m1Model.RDS")
summary(tapinomaM1Model)
tapinomaM2Model <- readRDS("Tapinoma_m2Model.RDS")
summary(tapinomaM2Model)
tapinomaM3Model <- readRDS("Tapinoma_m3Model.RDS")
summary(tapinomaM3Model)
tapinomaM4Model <- readRDS("Tapinoma_m4Model.RDS")
summary(tapinomaM4Model)
tapinomaM5Model <- readRDS("Tapinoma_m5Model.RDS")
summary(tapinomaM5Model)
tapinomaFullModel <- readRDS("Tapinoma_fullModel.RDS")
summary(tapinomaFullModel)

tapinomaModelList <- list(
  "Cold" = tapinomaM1Model,
  "Solar Radiation" = tapinomaM2Model,
  "Solar x Cold" = tapinomaM3Model,
  "Warm" = tapinomaM4Model,
  "Precipitation" = tapinomaM5Model,
  "Full Model" = tapinomaFullModel)

aictab(cand.set = tapinomaModelList)
