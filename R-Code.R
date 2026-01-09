###############################################################################
##     Practical exam: DSM - Annalena Bauer, Carla Schenk, Emil Unkrig       ##
###############################################################################

# empty workspace
rm(list = ls())

# check directory
getwd()

# loading libraries
library(sp)
library(raster)
library(sf)
library(caret)
library(randomForest)
library (ggplot2)

########################### DATA PREPROCESSING ###############################

# Kovariablen laden, benennen (automatische Benennung hat nicht geklappt) & Koordinatensystem zuweisen
covariates_RS <- stack(list.files("./Covariates/", pattern="\\.tif$", full.names = TRUE))

names(covariates_RS) <- c("Aspect", "Blue", "Catchment_Area", "Channel_Network", "Elevation", "Green", "LS_Factor", "NDVI", 
                          "NIR", "Rainfall", "Red", "Slope", "SWIR1", "SWIR2", "Temperature", "Valley_Depth", "Wetness_Index")
crs(covariates_RS) <- "EPSG:4326"

# Import der Grenzen des Untersuchungsgebiets
study_area <- as(st_read("./GIS/boundary.shp"), "Spatial")
plot(study_area, main = "Untersuchungsgebiet")

# Import der .csv SOIL Daten & Umwandlung in räumliche Daten (Koordinaten + Koordinatensystem)
soil_csv <- read.csv("./Soil/soil.csv", header = TRUE)
coordinates(soil_csv) <- ~ x + y
proj4string(soil_csv) <- CRS("+init=epsg:4326")

head(soil_csv)
plot(soil_csv, add =T, pch = 18, col ="red")


# Extraktion der Kovariaten an den Messpunkten 
cov = extract(covariates_RS, soil_csv, method='bilinear', df=TRUE)

# Kombination der Kovariate mit der CEC (Zielvariable)
cov_soil = cbind(cov[,-1], CEC=soil_csv$CEC)

str(cov_soil)



###################### DESCRIPTIVE STATISTICS ###############################

plot(cov_soil$CEC, cov_soil$NDVI)
plot(cov_soil$CEC)
hist(cov_soil$CEC)

plot(cov_soil$CEC, cov_soil$Elevation)
plot(cov_soil$Temperature)

## Korrelationsanalyse 
## Histogramme 


####################### MODEL ########################################

#Lineare Regression 
#Random Forest mit Regression Kriging 

####################### MODELGÜTE #######################################

#CV metrics; RMSE/MAE/R² 
#Variable Importance 


