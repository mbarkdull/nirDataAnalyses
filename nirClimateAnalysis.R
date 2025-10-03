library(tidyverse)
library(scales)
library(lubridate)
library(janitor)
library(sf)
library(spData)
library(terra)
library(googlesheets4)
library(geodata)

#### Thoughts/questions to ask team ####
# Which WorldClim variables to explore?
# Models to test for relationship between IR and climate?

#### Analysis ####
# Read in specimen data, select only relevant columns, and filter out missing lat/long:
specimenData <- read_sheet("https://docs.google.com/spreadsheets/d/1dHIhVuh-Sy2clvqRpMiu5mDXpaXXp3W5HgLbzs2rYNo/edit?usp=sharing") %>%
  select(c("Unique identifier",
           "Genus",
           "Species",
           "Country",
           "State/Province",
           "Location",
           "Photographed",
           "Photo Notes",
           "Visible:R:Normalised",
           "Visible:G:Normalised",
           "Visible:B:normalised",
           "ir:R:Normalised",
           "ir:B:normalised",
           "Average Visible",
           "Average IR",
           "Latitude",
           "Longitude",
           "Uncertainty Radius",
           "Uncertainty Radius units",
           "Start Date (Day, Month, Year)",
           "End Date (Day, Month, Year)")) %>%
  filter(!is.na(Latitude),
         Latitude != "NULL",
         !grepl(pattern = "\\*",
              Latitude),
         !is.na(`Average Visible`),
         !is.na(`Average IR`))

# Get a shapefile for the United States:
USA <- tigris::states() 

# Generate an sf object for the specimens:
specimenLocations <- st_as_sf(specimenData, 
                              coords = c("Longitude","Latitude"))
specimenLocations <- st_set_crs(specimenLocations, 
                                "+proj=longlat +datum=WGS84") 

# Download and work with the WorldClim climate data: https://wec.wur.nl/r/spatial/raster-data.html
# Download global worldclim data at 5 minute resolution
dir.create("./data/worldclim",
           recursive = TRUE)
bio <- worldclim_global(var = "bio", # "tmin", "tmax", "tavg", "prec", "wind", "vapr", or "bio"
                        res = 0.5, # resolution: 10, 5, 2.5, or 0.5 (minutes of a degree)
                        path = "./data/worldclim")

# Check the projection of the data:
crs(bio, 
    proj = TRUE, 
    describe = TRUE)

# Update the projection of the specimenLocations and the USA objects to match:
specimenLocations <- st_transform(specimenLocations, 
                                  src = st_crs(specimenLocations),
                                  crs = crs(bio))
USA <- st_transform(USA, 
                    src = st_crs(USA),
                    crs = crs(bio))

# Crop the bio data to just the USA:
bio <- crop(bio, 
            ext(USA))

# Get the climate data variable values at the specimen sampling locations:
specimenLocations$annualMeanTemperature <- terra::extract(bio[["wc2.1_30s_bio_1"]], specimenLocations)$wc2.1_30s_bio_1
specimenLocations$maxTempWarmestMonth <- terra::extract(bio[["wc2.1_30s_bio_5"]], specimenLocations)$wc2.1_30s_bio_5

# Plot the maximum temperature of the warmest month vs. NIR values:
ggplot(data = specimenLocations,
       mapping = aes(x = maxTempWarmestMonth,
                      y = `Average IR`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Maximum temperature of the warmest month",
       y = "Average near-infrared reflectance") +
  theme_bw()

# Plot annual mean temperature vs. NIR values:
ggplot(data = specimenLocations,
       mapping = aes(x = annualMeanTemperature,
                     y = `Average IR`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Annual mean temperature",
       y = "Average near-infrared reflectance") +
  theme_bw()

# See if NIR and Vis reflectance are correlated:
ggplot(data = specimenLocations,
       mapping = aes(x = `Average Visible`,
                     y = `Average IR`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Average visible reflectance",
       y = "Average near-infrared reflectance") +
  theme_bw()

# Since they are, regress them against one another and work with the residuals for subsequent analysis:
irByVis <- lm(data = specimenLocations, `Average IR` ~ `Average Visible`)
summary(irByVis)
irByVisResiduals <- residuals(irByVis)
specimenLocations$irByVisResiduals <- irByVisResiduals

# See if the residuals are now independent of Vis reflectance (they are!):
ggplot(data = specimenLocations,
       mapping = aes(x = `Average Visible`,
                     y = irByVisResiduals)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Visible light reflectance",
       y = "Residuals") +
  theme_bw()

# Plot residuals vs. mean temp of hottest month:
ggplot(data = specimenLocations,
       mapping = aes(x = maxTempWarmestMonth,
                     y = irByVisResiduals)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Maximum temperature of the warmest month",
       y = "Average infrared reflectively\n(controlled by average visible reflectivity)") +
  theme_bw()

residualsByWarmestMonth <- lm(data = specimenLocations, irByVisResiduals ~ maxTempWarmestMonth)
summary(residualsByWarmestMonth)




