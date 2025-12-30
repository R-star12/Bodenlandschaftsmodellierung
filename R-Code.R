###############################################################################
##     Practical exam: DSM - Annalena Bauer, Carla Schenk, Emil Unkrig       ##
###############################################################################

# empty workspace
rm(list = ls())

# check directory
getwd()

# loading libraries
library(sp)
library(sf)
library(caret)
library(ggplot2)
library(mapview)
library(corrplot)
library(rpart)
library(rpart.plot)
library(sp)
library(gstat)
library(raster)
library(gridExtra)
library(randomForest)

########################### DATA PREPROCESSING ###############################



# Covariablen reinladen --> zu einem Stack hochladen; funktioniert nur wenn gleicher Ausschnitt & gleiche Auflösung
covariates_RS <- raster::stack(list.files("./Covariates/", pattern="\\.tif$", full.names = TRUE))
names(covariates_RS) 

files <- list.files("./Covariates/", pattern="\\.tif$", full.names = TRUE)
basename(files)

# import the point soil data from desktop --> X & Y koordinaten von shp-file an diese CSV anfügen
point <- read.csv ("soil.csv", header = TRUE)

# convert soil point data to spatial point data  
coordinates(point) <- ~ x + y

# set projection
proj4string(point) <- CRS("+init=epsg:32639")

# plot the point on the raster
plot(covariates$Nir, main = "Landsat Image + Sample Points")
plot(point, add =T, pch = 19)

# extract covariate values at each point of observation 
cov = extract(covariates, point, method='bilinear', df=TRUE)

# combining covariates and soil properties
cov_soil = cbind(cov[,-1], ec=point$ec)

# remove na values
cov_soil <- cov_soil[complete.cases(cov_soil),]

###################### DESCRIPTIVE STATISTICS ###############################

## Korrelationsanalyse 
## Histogramme 


####################### MODEL ########################################

#Lineare Regression 
#Random Forest mit Regression Kriging 

####################### MODELGÜTE #######################################

#CV metrics; RMSE/MAE/R² 
#Variable Importance 


