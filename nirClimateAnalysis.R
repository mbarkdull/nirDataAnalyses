library(tidyverse)
library(scales)
library(lubridate)
library(janitor)
library(sf)
library(spData)
library(terra)
library(googlesheets4)
library(geodata)

#### Get specimen data ####
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

# See if NIR and Vis reflectance are correlated:
ggplot(data = specimenData,
       mapping = aes(x = `Average Visible`,
                     y = `Average IR`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Average visible reflectance",
       y = "Average near-infrared reflectance") +
  theme_bw()

# Since they are, regress them against one another and work with the residuals for subsequent analysis:
irByVis <- lm(data = specimenData, `Average IR` ~ `Average Visible`)
summary(irByVis)
irByVisResiduals <- residuals(irByVis)
specimenData$irByVisResiduals <- irByVisResiduals

# See if the residuals are now independent of Vis reflectance (they are!):
ggplot(data = specimenData,
       mapping = aes(x = `Average Visible`,
                     y = irByVisResiduals)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Visible light reflectance",
       y = "Residuals") +
  theme_bw()

#### Get climate data for specimens ####
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

# We'll read in the bio variables (bio = bioclimatic variables derived from the tmean, tmin, tmax and prec)
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
specimenLocations$meanDiurnalRange <- terra::extract(bio[["wc2.1_30s_bio_2"]], specimenLocations)$wc2.1_30s_bio_2
specimenLocations$isothermality <- terra::extract(bio[["wc2.1_30s_bio_3"]], specimenLocations)$wc2.1_30s_bio_3
specimenLocations$temperatureSeasonality <- terra::extract(bio[["wc2.1_30s_bio_4"]], specimenLocations)$wc2.1_30s_bio_4
specimenLocations$maxTempWarmestMonth <- terra::extract(bio[["wc2.1_30s_bio_5"]], specimenLocations)$wc2.1_30s_bio_5
specimenLocations$minTempColdest <- terra::extract(bio[["wc2.1_30s_bio_6"]], specimenLocations)$wc2.1_30s_bio_6
specimenLocations$temperatureAnnualRange <- terra::extract(bio[["wc2.1_30s_bio_7"]], specimenLocations)$wc2.1_30s_bio_7
specimenLocations$meanTempWettestQuarter <- terra::extract(bio[["wc2.1_30s_bio_8"]], specimenLocations)$wc2.1_30s_bio_8
specimenLocations$meanTempDriestQuarter <- terra::extract(bio[["wc2.1_30s_bio_9"]], specimenLocations)$wc2.1_30s_bio_9
specimenLocations$meanTempWarmestQuarter <- terra::extract(bio[["wc2.1_30s_bio_10"]], specimenLocations)$wc2.1_30s_bio_10
specimenLocations$meanTempColdestQuarter <- terra::extract(bio[["wc2.1_30s_bio_11"]], specimenLocations)$wc2.1_30s_bio_11
specimenLocations$annualPrecipitation <- terra::extract(bio[["wc2.1_30s_bio_12"]], specimenLocations)$wc2.1_30s_bio_12
specimenLocations$precipitationWettestMonth <- terra::extract(bio[["wc2.1_30s_bio_13"]], specimenLocations)$wc2.1_30s_bio_13
specimenLocations$precipitationDriestMonth <- terra::extract(bio[["wc2.1_30s_bio_14"]], specimenLocations)$wc2.1_30s_bio_14
specimenLocations$precipitationSeasonality <- terra::extract(bio[["wc2.1_30s_bio_15"]], specimenLocations)$wc2.1_30s_bio_15
specimenLocations$precipitationWettestQuarter <- terra::extract(bio[["wc2.1_30s_bio_16"]], specimenLocations)$wc2.1_30s_bio_16
specimenLocations$precipitationDriestQuarter <- terra::extract(bio[["wc2.1_30s_bio_17"]], specimenLocations)$wc2.1_30s_bio_17
specimenLocations$precipitationWarmestQuarter <- terra::extract(bio[["wc2.1_30s_bio_18"]], specimenLocations)$wc2.1_30s_bio_18
specimenLocations$precipitationColdestQuarter <- terra::extract(bio[["wc2.1_30s_bio_19"]], specimenLocations)$wc2.1_30s_bio_19


# Remove specimens without climate data:
specimenLocations <- specimenLocations %>%
  filter(!is.na(maxTempWarmestMonth))

# See how correlated the climate variables are:
# Select only climate variables:
climateVariables <- specimenLocations %>%
  as.data.frame() %>%
  select(c("annualMeanTemperature",
           "meanDiurnalRange",
           "isothermality" ,
           "temperatureSeasonality",
           "maxTempWarmestMonth",
           "minTempColdest",
           "temperatureAnnualRange",
           "meanTempWettestQuarter",
           "meanTempDriestQuarter",
           "meanTempWarmestQuarter",
           "meanTempColdestQuarter",
           "annualPrecipitation",
           "precipitationWettestMonth",
           "precipitationDriestMonth",
           "precipitationSeasonality",
           "precipitationWettestQuarter",
           "precipitationDriestQuarter",
           "precipitationWarmestQuarter",
           "precipitationColdestQuarter")) %>%
  filter(!is.na(annualMeanTemperature)) %>%
  as.data.frame()

# See how correlated the climate variables are:
climateVariablesCorrelation <- Hmisc::rcorr(as.matrix(climateVariables))
# Get R2 values:
climateVariablesCorrelationR2P <- round(climateVariablesCorrelation$r, 2) 
climateVariablesCorrelationR2P
# Get P values
climateVariablesCorrelationP <- round(climateVariablesCorrelation$P, 5) 
climateVariablesCorrelationP

# Can the variables be correlated (can we do a PCA?). Check by getting a Pearsons correlation coefficient for all pairs.
correlation <- cor(as.matrix(climateVariables)) 
# Get the number of observations
numberObservations <- dim(climateVariables)[1] 
# Test for correlation:
psych::cortest.bartlett(correlation, n = numberObservations) # Are the variables correlated? Yes if p < 0.05

# Extract the eigenvalues, components, and loadings matrix of the PCA:
# Extract eigenvalues
eigenValues <- eigen(correlation) 
# Get a vector with the eigenvalues
eigenValuesVector <- eigenValues$vectors 
# Get the components
components <- prcomp(climateVariables, 
                     retx = TRUE, 
                     center = TRUE, 
                     scale. = TRUE) 
# Get the loadings matrix
loadingsMatrix <- components$rotation 

# Generate a scree plot for the components, so that we can select and use only components with an eigenvalue greater than 1:
eigenValuesDataframe <- as.data.frame(eigenValues$values)
eigenValuesDataframe$pcNumber <- as.numeric(rownames(eigenValuesDataframe))
eigenValuesDataframe$group <- "group"

ggplot(data = eigenValuesDataframe,
       mapping = aes(x = pcNumber,
                     y = `eigenValues$values`,
                     group = group)) + 
  geom_line() +
  geom_point(color = "blue") +
  geom_hline(yintercept = 1) +
  theme_bw()

# What is the proportion of variance explained by each component?
summary(components)

# Visualize the loadings matrix, which tells us how each variable is correlated to each principal component:
# In our case,  it looks like lower values on PC1 correspond to cooler and less seasonal environments 
# while lower values on PC2 corresponds to warmer environments with a wider range of daytime temps
# Prep data:
loadingsMatrixDataframe <- setNames(reshape2::melt(loadingsMatrix), 
                                    c('variable', 
                                      'pc', 
                                      'correlation')) %>%
  filter(pc %in% c("PC1", "PC2", "PC3"))

# Plot
ggplot(loadingsMatrixDataframe) +
  geom_tile(mapping = aes(x = pc, 
                          y = variable, 
                          fill = correlation),
            stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_distiller(type = "div")

# Plot the variables loaded onto the data:
# Extract PC axes for plotting
PCAvalues <- data.frame(specimen = filter(specimenLocations, !is.na(maxTempWarmestMonth))$`Unique identifier`, 
                        components$x)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(components$rotation), 
                          components$rotation)

# PC1 vs PC2
ggplot(data = PCAvalues, 
       mapping = aes(x = PC1, 
                     y = PC2)) +
  geom_point(fill = "#648fff", 
             col = "black", 
             pch = 21, 
             size = 2, 
             alpha = 0.4) +
  theme_bw() +
  annotate("text",
           size = 3,
           x = (PCAloadings$PC1 * 11.5),
           y = (PCAloadings$PC2 * 11.5),
           label = (PCAloadings$Variables)) +
  geom_segment(data = PCAloadings[c(1, 2, 3), ],
               mapping = aes(x = 0, 
                             y = 0,
                             xend = (PC1 * 11), 
                             yend = (PC2 * 11)),
    arrow = arrow(length = unit(1 / 2, 
                                "picas")),
    color = "red" ) +
  geom_segment(data = PCAloadings[c(4, 5, 6, 7), ],
               aes(x = 0, 
                   y = 0,
                   xend = (PC1 * 11), 
                   yend = (PC2 * 11)),
    arrow = arrow(length = unit(1 / 2, 
                                "picas")),
    color = "#ffb000") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "gray") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             colour = "gray") +
  ylim(-10, 5) +
  xlim(-7, 5)

# Create a new dataframe with the PC values for each specimen:
pcValues <- PCAvalues[, 1:4]
pcValues$meanIR <- specimenLocations$`Average IR`

# Check to see how the mean infrared reflectance and PCs are correlated:
cor.test(pcValues$PC1, 
         pcValues$meanIR)

cor.test(pcValues$PC2, 
         pcValues$meanIR)

cor.test(pcValues$PC3, 
         pcValues$meanIR)



# Run some linear models to see how IR reflectance and climate variables are related:
residualsByWarmth <- lm(data = specimenLocations, 
                        irByVisResiduals ~ annualMeanTemperature + maxTempWarmestMonth)
summary(residualsByWarmth)

irByWarmth <- lm(data = specimenLocations, 
                 `Average IR` ~ annualMeanTemperature + maxTempWarmestMonth)
summary(irByWarmth)

# Look at infrared reflectance and the two PCs that capture >80% of variation in climate:
irByPCs <- lm(data = pcValues, 
              meanIR ~ PC1 + PC2 + PC3)
summary(irByPCs)

# Plot IR vs. the PCs:
ggplot(data = pcValues,
       mapping = aes(x = PC1, 
                     y = meanIR)) +
  geom_point(color = "orange") +
  theme_bw() +
  xlab("PC1") +
  ylab("Mean infrared reflectance")



