###############################################################################
##     Practical exam: DSM - Anna-Lena Bauer, Carla Schenk, Emil Unkrig       ##
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
library(corrplot)
library(car)

########################### DATA PREPROCESSING ###############################

# Kovariablen laden, benennen & Koordinatensystem zuweisen

covariates_RS <- stack(list.files("./Covariates/", pattern="\\.tif$", full.names = TRUE))
names(covariates_RS) <- tools::file_path_sans_ext(basename(list.files("./Covariates/", pattern="\\.tif$", full.names = FALSE)))
covariates_RS  <- projectRaster(covariates_RS, crs = CRS("EPSG:4326"))

plot(covariates_RS)

# Import des Untersuchungsgebiets & Zuweisung Koordinatensystem
study_area <- as(st_read("./GIS/boundary.shp"), "Spatial")
study_area <- spTransform(study_area, CRS("EPSG:4326"))

# Import der Bodendaten & Georeferenzierung
soil_csv <- read.csv("./Soil/soil.csv", header = TRUE)
coordinates(soil_csv) <- ~ x + y
proj4string(soil_csv) <- CRS("EPSG:4326")

head(soil_csv)

# Extraktion der Kovariaten an den Messpunkten & Erstellung eines DF mit Kovariaten + Zielvariable
cov = raster::extract(covariates_RS, soil_csv, method='bilinear', df=TRUE)
cov_soil = cbind(cov[,-1], CEC=soil_csv$CEC)

str(cov_soil)

# Plot der NDVI-Kovariate mit Messpunkten & Grenzen des Untersuchungsgebiets
png("plots/NDVI_Messpunkte_Boundaries_plot.png", width = 800, height = 600) 
plot(covariates_RS$NDVI, main = "NDVI")
plot(study_area, add = T)
plot(soil_csv, pch = 1, add = T, col ="blue")
dev.off()

###################### DESCRIPTIVE STATISTICS ############################### 
######Plots

#Histogram CEC
ggplot(cov_soil, aes(x = CEC)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogramm der CEC-Werte",
       x = "CEC",
       y = "Häufigkeit")

#Boxplot CEC
ggplot(cov_soil, aes(y = CEC)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Boxplot der CEC-Werte",
       y = "CEC")

# Daten ins Long-Format bringen, sodass alle Kovariaten in einer Spalte liegen
cov_long <- tidyr::pivot_longer(cov_soil, cols = everything(), names_to = "Variable", values_to = "Value")

#Histogram of all Variables
ggplot(cov_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Verteilung aller Variablen",
       x = "Wert",
       y = "Häufigkeit")

#Boxplot of all Variables
ggplot(cov_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots aller Variablen",
       x = "",
       y = "Wert")

#Smoothed Histograms of all Variables
ggplot(cov_long, aes(x = Value)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Dichteverteilungen aller Variablen")


#Scatterplots
ggplot(cov_soil, aes(x = NDVI, y = CEC)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

# Daten ins Long-Format bringen, sodass alle Kovariaten in einer Spalte liegen (alle Variablen außer CEC)
cov_longB <- tidyr::pivot_longer(cov_soil, cols = -CEC, names_to = "Variable", values_to = "Value")

#Scatterplots alle Variablen mit CEC
ggplot(cov_longB, aes(x = Value, y = CEC)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Scatterplots von CEC mit allen Kovariaten",
    x = "Kovariate",
    y = "CEC")


############ Correlation #####
cor_mat <- cor(cov_soil, use = "complete.obs", method = "pearson")

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.6)


####################### MODEL ########################################

####################### Lineare Regression ############## 

## Einfaches lineares Model mit allen Variablen
lm_full <- lm(CEC ~ Aspect+Blue+Catchment_Area+Channel_Network+Elevation+Green+LS_Factor+NDVI+NIR+Rainfall+Red+Slope+SWIR1+SWIR2+Temperature+Valley_Depth+Wetness_Index,
                 data=cov_soil)

summary(lm_full)
plot(lm_full)

# alle CEC Werte über 45 entfernen
cov_soil_45 <- cov_soil[cov_soil$CEC <= 45, ]

lm_full_45 <- lm(CEC ~ Aspect+Blue+Catchment_Area+Channel_Network+Elevation+Green+LS_Factor+NDVI+NIR+Rainfall+Red+Slope+SWIR1+SWIR2+Temperature+Valley_Depth+Wetness_Index,
              data=cov_soil_45)

summary(lm_full_45)
plot(lm_full_45)


#Unterteilung Test- und Trainingsdaten 
trainIndex <- createDataPartition(cov_soil_45$CEC, p = 0.8, list = FALSE, times = 1)

# subset the datasets
cov_soil_Train <- cov_soil_45[ trainIndex,]
cov_soil_Test  <- cov_soil_45[-trainIndex,]


#Finale lineare Regression mit Trainingsdaten 

lm_reduced <- lm(CEC ~ Aspect+Temperature+LS_Factor+NDVI+Rainfall+Green+Slope+Wetness_Index,
                 data=cov_soil_Train)
summary(lm_reduced)

# apply the linear model on testing data
CEC_linear_Pred <- predict(lm_reduced, cov_soil_Test)  

# check the plot actual and predicted OC values
plot(cov_soil_Test$CEC, CEC_linear_Pred, main="Linear Regression Model", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", 
     xlim=c(0,45),ylim=c(0,35))
abline(coef = c(0,1),  col="red" )

# calculate correlation
cor_linear <- cor(cov_soil_Test$CEC, CEC_linear_Pred)
cor_linear

# calculate RMSE
RMSE_linear <- sqrt(mean((cov_soil_Test$CEC - CEC_linear_Pred)^2))
RMSE_linear

#calculate R2
R2_linear <- 1 - sum((cov_soil_Test$CEC - CEC_linear_Pred)^2)/sum((cov_soil_Test$CEC - mean(cov_soil_Test$CEC))^2)
R2_linear

#vif (no values over 5, so no multicollinearity)
vif(lm_reduced)


############### RANDOM FOREST MIT REGRESSION KRIGING ##########

#cov_soil_70 <- cov_soil[cov_soil$CEC<70,]

# split the data to training (80%) and testing (20%) sets
trainIndex <- createDataPartition(cov_soil$CEC, p = 0.8, list = FALSE, times = 1)
cov_soil_Train <- cov_soil[ trainIndex,]
cov_soil_Test  <- cov_soil[-trainIndex,]

# inspect the two datasets
str(cov_soil_Train)
str(cov_soil_Test)

############################### Random Forest ###########################

# fit random forest model

rf_full <- randomForest(CEC ~ Aspect+Blue+Catchment_Area+Channel_Network+Elevation+Green+LS_Factor+NDVI+NIR+Rainfall+Red+Slope+SWIR1+SWIR2+Temperature+Valley_Depth+Wetness_Index, 
                       data = cov_soil_Train, ntree = 5000, do.trace = 500) #Cor: 1.7


rf_fit <- randomForest(CEC ~ Aspect+Catchment_Area+Channel_Network+Elevation+Green+Temperature+LS_Factor+NDVI+Rainfall+Slope+SWIR1+Wetness_Index, 
                       data = cov_soil_Train, ntree = 10000) #Cor: 2.1

summary(rf_full)


# variable importance
importance(rf_full)
varImpPlot(rf_full, main = "Variable Importance for RF model")

CEC_rf_full_Pred <- predict(rf_full, cov_soil_Test)

############# Tuning the RF #############
#wird nur schlechter...

ctrl <- trainControl(method = "cv", number = 10)

rfGrid1 <- expand.grid(.mtry = 6)

set.seed(1234)
rf_fit1 <- train(CEC ~ Aspect+Catchment_Area+Channel_Network+Elevation+Green+Temperature+LS_Factor+NDVI+NIR+Rainfall+Slope+SWIR1+Wetness_Index,
                 data = cov_soil_Train,
                 method = "rf",
                 trControl = ctrl,
                 tuneGrid = rfGrid1,
                 ntree = 1000)

rf_fit1$finalModel

CEC_rf_Pred1 <- predict(rf_fit1, cov_soil_Test)

#### check the plot actual and predicted OC values ###################
plot(cov_soil_Test$CEC, CEC_rf_full_Pred, main="RF model", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", xlim=c(0,100),ylim=c(0,40))

abline(coef = c(0,1),  col="red" )

# calculate rf correlation & performance
cor_rf <- cor(cov_soil_Test$CEC, CEC_rf_full_Pred)
cor_rf
RMSE_rf <- sqrt(mean((cov_soil_Test$CEC - CEC_rf_full_Pred)^2))
MAE_rf <- mean(abs(cov_soil_Test$CEC - CEC_rf_full_Pred))
R2_rf <- 1 - sum((cov_soil_Test$CEC - CEC_rf_full_Pred)^2)/sum((cov_soil_Test$CEC - mean(cov_soil_Test$CEC))^2)

# random forest prediction part 
map_rf <- raster::predict(covariates_RS, rf_full)

# plot the RF map
spplot(map_rf, main = "CEC map based on RF model")




####################### REGRESSION KRIGING #######################################

# append residuals
cov_soil$residuals <- cov_soil$CEC - CEC_rf_Pred
names(cov_soil)
summary(cov_soil)

# histogram of the residuals 
hist(cov_soil$residuals, col = "lightblue")

# convert cov_soil to spatial data
cov_soil$x <- soil_csv$x
cov_soil$y <- soil_csv$y
coordinates(cov_soil) <- ~ x + y
proj4string(cov_soil) <- CRS("+init=epsg:4326")

# compute experimental semivariogram of residuals
#install.packages(gstat)
library(gstat)

gstat_res <- gstat(formula = residuals ~ 1, data = cov_soil)
vg_res    <- variogram(gstat_res)
plot(vg_res, plot.nu = FALSE)

# define initial semivariogram model
vg_parameters_res <- vgm(nugget = 70, psill = 110, range = 100, model = "Pen")
plot(vg_res, vg_parameters_res)

# fit semivariogram model
vg_model_res <- fit.variogram(vg_res, vg_parameters_res)
plot(vg_res, vg_model_res)
vg_model_res


# export boundary as a grid  
r_template <- raster(study_area, res = 0.00898)                 # template raster
r_mask     <- rasterize(study_area, r_template, field = 1) # inside polygon = 1, outside = NA
study_area_grid <- as(r_mask, "SpatialPixelsDataFrame")    # grid for kriging


# ordinary kriging of residuals
res_krig <- krige(
  formula   = residuals ~ 1,
  locations = cov_soil,
  newdata   = study_area_grid,
  model     = vg_model_res
)

# plot the residuals map

spplot(res_krig, zcol = "var1.pred", main = "residuals predictions")


# obtain regression kriging prediction
res_krig_raster <- raster::resample(raster(res_krig), map_rf)

RK_map <-     res_krig_raster +  map_rf ##evtl. umdrehen!?

# rk performance 
RK_pred <- raster::extract(RK_map, cov_soil)

RMSE_RK <- sqrt(mean((cov_soil$CEC - RK_pred)^2))
RMSE_RK

MAE_RK <- mean(abs(cov_soil$CEC - RK_pred))
MAE_RK

R2_RK <- 1 - sum((cov_soil$CEC - RK_pred)^2) / sum((cov_soil$CEC - mean(cov_soil$CEC))^2)
R2_RK

# plot the RK map
spplot(map_rf, main = "CEC map based on RF model")
spplot(RK_map, main = "CEC map based on RK model")


######### Comparing the Results ###############

RMSE_models <- c(Linear=RMSE_linear, RF=RMSE_rf)
cor_models <- c(Linear=cor_linear, RF=cor_rf)
R2_models <- c(Linear=R2_linear, RF=R2_rf)

par(mfrow = c(1,3))
barplot(RMSE_models, main="RMSE",col=c("red","blue","green"))
barplot(cor_models, main="Correlation",col=c("red","blue","green"))
barplot(R2_models, main="R2",col=c("red","blue","green"))

par(mfrow = c(1,1))

####################### MODELGÜTE #######################################

### Modellgüte RF #######
obs <- cov_soilA$CEC
pred_rf <- rf_fit$predicted

RMSE_rf <- sqrt(mean((obs - pred_rf)^2))
MAE_rf  <- mean(abs(obs - pred_rf))
R2_rf   <- cor(obs, pred_rf)^2

RMSE_rf
MAE_rf
R2_rf

####### Modellgute RK #######

##residuen an punktstandorten

install.packages("gstat")
library(gstat)

#### convert to spatial data

cov_soilA$x <- soil_csv$x
cov_soilA$y <- soil_csv$y

cov_soil_sp <- cov_soilA

coordinates(cov_soil_sp) <- ~ x + y
proj4string(cov_soil_sp) <- CRS("+init=epsg:4326")

class(cov_soil_sp)


rk_cv <- krige.cv(
  residuals ~ 1,
  locations = cov_soil_sp,
  model     = vg_model_res,
  nfold     = nrow(cov_soil_sp)
)

#RK-vorhersage an punktstandorten
pred_rk_cv <- rf_fit$predicted + rk_cv$var1.pred

#güte
RMSE_rk <- sqrt(mean((obs - pred_rk_cv)^2))
R2_rk   <- cor(obs, pred_rk_cv)^2
RMSE_rk
R2_rk





