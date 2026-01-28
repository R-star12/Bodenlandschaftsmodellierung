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
names(covariates_RS) <- tools::file_path_sans_ext(
  basename(list.files("./Covariates/", pattern="\\.tif$", full.names = FALSE)))

names(covariates_RS)
covariates_RS  <- projectRaster(covariates_RS, crs = CRS("+init=epsg:4326"))

plot(covariates_RS)


# Import der Grenzen des Untersuchungsgebiets
study_area <- as(st_read("./GIS/boundary.shp"), "Spatial")
study_area     <- spTransform(study_area, CRS("+init=epsg:4326"))


# Import der .csv SOIL Daten & Umwandlung in räumliche Daten (Koordinaten + Koordinatensystem)
soil_csv <- read.csv("./Soil/soil.csv", header = TRUE)
coordinates(soil_csv) <- ~ x + y
proj4string(soil_csv) <- CRS("+init=epsg:4326")

head(soil_csv)

png("plots/NDVI_Messpunkte_Boundaries_plot.png", width = 800, height = 600) 
plot(covariates_RS$NDVI, main = "NDVI")
plot(study_area, add = T)
plot(soil_csv, pch = 19, add = T, col ="blue")
dev.off()

# Extraktion der Kovariaten an den Messpunkten 
cov = extract(covariates_RS, soil_csv, method='bilinear', df=TRUE)

# Kombination der Kovariate mit der CEC (Zielvariable)
cov_soil = cbind(cov[,-1], CEC=soil_csv$CEC, Clay = soil_csv$Clay, SOC = soil_csv$SOC, pH = soil_csv$pH)

str(cov_soil)



###################### DESCRIPTIVE STATISTICS ############################### 
#ANNALENA

summary(cov_soil$CEC)


############## Histogramme ###########

library(ggplot2)

ggplot(cov_soil, aes(x = CEC)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogramm der CEC-Werte",
       x = "CEC",
       y = "Häufigkeit")

ggplot(cov_soil, aes(y = CEC)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Boxplot der CEC-Werte",
       y = "CEC")

library(tidyr)

cov_long <- pivot_longer(
  cov_soil,
  cols = everything(),
  names_to = "Variable",
  values_to = "Value"
)

ggplot(cov_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Verteilung aller Variablen",
       x = "Wert",
       y = "Häufigkeit")

ggplot(cov_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots aller Variablen",
       x = "",
       y = "Wert")


ggplot(cov_long, aes(x = Value)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Dichteverteilungen aller Variablen")





############# Korrelation #######

cor_matrix <- cor(cov_soil, use = "complete.obs", method = "pearson")
round(cor_matrix, 2)

library(corrplot)


### korr plot volltändig
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         number.cex = 0.6)

### korr plot vollständig verbessert
cor_mat <- cor(cov_soil, use = "complete.obs", method = "pearson")

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.6
)



#korrelation mit standardisierten werten
cov_soil_scaled <- as.data.frame(scale(cov_soil))
cor_matrix_scaled <- cor(
  cov_soil_scaled,
  use = "complete.obs",
  method = "pearson"
)


round(cor_matrix_scaled, 2)


corrplot(
  cor_matrix_scaled,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.6
)



#### Scatterplots ################


###scatterplot 
ggplot(cov_soil, aes(x = NDVI, y = CEC)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()



library(ggplot2)
library(tidyr)

# Daten ins Long-Format bringen (alle Variablen außer CEC)
cov_long <- pivot_longer(
  cov_soil,
  cols = -CEC,
  names_to = "Variable",
  values_to = "Value"
)

# Scatterplots: CEC vs jede Variable
ggplot(cov_long, aes(x = Value, y = CEC)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Scatterplots von CEC mit allen Kovariaten",
    x = "Kovariate",
    y = "CEC"
  )




####################### MODEL ########################################

#######################Lineare Regression ############## ANNA-LENA

## einfaches lineares Model
lm_full <- lm(CEC ~ ., data = cov_soil_scaled)
lm_full_1 <- lm(CEC ~ Aspect+Blue+Catchment_Area+Channel_Network+Elevation+Green+LS_Factor+NDVI+NIR+Rainfall+Red+Slope+SWIR1+SWIR2+Temprature+Valley_Depth+Wetness_Index,
                 data=cov_soil)
summary(lm_full_1)
summary(lm_full)

# apply the linear model on testing data
CEC_linear_Pred <- predict(lm_full_1, cov_soil)  

# check the plot actual and predicted OC values
plot(cov_soil$CEC, CEC_linear_Pred, main="Linear model", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", 
     xlim=c(0,100),ylim=c(0,100))
abline(coef = c(0,1),  col="red" )

# calculate correlation
cor_linear <- cor(cov_soil$CEC, CEC_linear_Pred)
cor_linear

# calculate RMSE
RMSE_linear <- sqrt(mean((cov_soil$CEC - CEC_linear_Pred)^2))
RMSE_linear

#calculate MAE
MAE_linear <- mean(abs(cov_soil$CEC - CEC_linear_Pred))
MAE_linear

################### reduziertes, einfaches lineare Regression #######
lm_reduced <- lm(
  CEC ~ NDVI + Catchment_Area + Slope + LS_Factor + Valley_Depth,
  data = cov_soil_scaled
)

summary(lm_reduced)

# apply the linear model on testing data
CEC_linear_Pred <- predict(lm_reduced, cov_soil_scaled)  

# check the plot actual and predicted OC values
plot(cov_soil_scaled$CEC, CEC_linear_Pred, main="Linear model", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", 
     xlim=c(0,1),ylim=c(0,1))
abline(coef = c(0,1),  col="red" )

## Multikollinearität checken --> VIF über 5 problematisch, über 10 schlecht
library(car)
vif(lm_full)

## anhand analyse auszuschließen: Blue, Channel Network, Elevation, green, NDVI, NIR, Red, Slope, SWIR1+2, Temperature


##reduzierte Model
lm_red <- lm(CEC ~ NDVI + Wetness_Index + Elevation, data = cov_soil)
summary(lm_red)

par(mfrow = c(2, 2))
plot(lm_red)
par(mfrow = c(1, 1))


##### VIF #######
predictors <- cov_soil[, -which(names(cov_soil) == "CEC")]

R2_values <- sapply(names(predictors), function(v) {
  others <- predictors[, names(predictors) != v]
  summary(lm(predictors[[v]] ~ ., data = others))$r.squared
})

VIF_like <- 1 / (1 - R2_values)
sort(VIF_like, decreasing = TRUE)


################## GAM - BESSERES MODELL ############################

library(nlme)
library(mgcv)

gam_cec <- gam(
  CEC ~ 
    s(NDVI, k = 6) +
    s(Catchment_Area, k = 6) +
    s(Slope, k = 6) +
    s(LS_Factor, k = 6) +
    s(Valley_Depth, k = 6),
  data = cov_soil,
  method = "REML"
)

summary(gam_cec)

gam.check(gam_cec)

par(mfrow = c(2, 3))
plot(gam_cec, shade = TRUE, pages = 1, seWithMean = TRUE)
par(mfrow = c(1, 1))

CEC_GAM_Pred <- predict(gam_cec, cov_soil)  
plot(cov_soil$CEC, CEC_GAM_Pred, main="GAM", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", 
     xlim=c(0,100),ylim=c(0,100))
abline(coef = c(0,1),  col="red" )


<<<<<<< HEAD





#Random Forest mit Regression Kriging #EMIL
=======
# calculate correlation
cor_GAM <- cor(cov_soil$CEC, CEC_GAM_Pred)
cor_GAM
>>>>>>> 0a76d071157e8e2762e88e3132a0aafff97d154c




############################### Random Forest ###########################

#Random Forest  #EMIL

# split the data to training (80%) and testing (20%) sets

#cov_soil <- cov_soil[cov_soil$CEC<45,] #Test!

trainIndex <- createDataPartition(cov_soil$CEC, p = 0.8, list = FALSE, times = 1)

# subset the datasets
cov_soil_Train <- cov_soil[ trainIndex,]
cov_soil_Test  <- cov_soil[-trainIndex,]

# inspect the two datasets
str(cov_soil_Train)
str(cov_soil_Test)

# fit random forest model



rf_fit <- randomForest(CEC ~ Aspect+Blue+Catchment_Area+Channel_Network+Elevation+Green+LS_Factor+NDVI+NIR+Rainfall+Red+Slope+SWIR1+SWIR2+Temperature+Valley_Depth+Wetness_Index, 
                       data = cov_soil_Train, ntree = 1000, do.trace = 50) #Cor: 1.7


rf_fit <- randomForest(CEC ~ Aspect+Catchment_Area+Channel_Network+Elevation+Green+Temperature+LS_Factor+NDVI+NIR+Rainfall+Slope+SWIR1+Wetness_Index, 
                       data = cov_soil_Train, ntree = 10000) #Cor: 2.1

summary(rf_fit)

######### Ausprobieren -> SOC modellieren #########

trainIndexX <- createDataPartition(cov_soil$SOC, p = 0.8, list = FALSE, times = 1)

# subset the datasets
cov_soil_TrainX <- cov_soil[ trainIndex,]
cov_soil_TestX  <- cov_soil[-trainIndex,]


rf_fitX <- randomForest(SOC ~ Aspect+Blue+Catchment_Area+Channel_Network+Elevation+Green+LS_Factor+NDVI+NIR+Rainfall+Red+Slope+SWIR1+SWIR2+Temperature+Valley_Depth+Wetness_Index, 
                       data = cov_soil_TrainX, ntree = 1000, do.trace = 50) #Cor: 0.41


importance(rf_fitX)
varImpPlot(rf_fitX, main = "Variable Importance for RF model")

CEC_rf_PredX <- predict(rf_fitX, cov_soil_TestX)

cor_rfX <- cor(cov_soil_TestX$SOC, CEC_rf_PredX)
cor_rfX
###########################

# variable importance
importance(rf_fit)
varImpPlot(rf_fit, main = "Variable Importance for RF model")

CEC_rf_Pred <- predict(rf_fit, cov_soil_Test)


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
plot(cov_soil_Test$CEC, CEC_rf_Pred, main="Tree model", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", xlim=c(0,100),ylim=c(0,100))

abline(coef = c(0,1),  col="red" )

# calculate correlation
cor_rf <- cor(cov_soil_Test$CEC, CEC_rf_Pred)
cor_rf


# rf performance 
RMSE_RF <- sqrt(mean((cov_soil_Test$CEC - CEC_rf_Pred)^2))
RMSE_RF

MAE_RF <- mean(abs(cov_soil_Test$CEC - CEC_rf_Pred))
MAE_RF

R2_RF <- 1 - sum((cov_soil_Test$CEC - CEC_rf_Pred)^2)/sum((cov_soil_Test$CEC - mean(cov_soil_Test$CEC))^2)
R2_RF

# random forest prediction part 
map_rf <- raster::predict(covariates_RS, rf_fit)

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
RK_pred <- extract(RK_map, cov_soil)

RMSE_RK <- sqrt(mean((cov_soil$CEC - RK_pred)^2))
RMSE_RK

MAE_RK <- mean(abs(cov_soil$CEC - RK_pred))
MAE_RK

R2_RK <- 1 - sum((cov_soil$CEC - RK_pred)^2) / sum((cov_soil$CEC - mean(cov_soil$CEC))^2)
R2_RK

# plot the RK map
spplot(map_rf, main = "CEC map based on RF model")
spplot(RK_map, main = "CEC map based on RK model")





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





