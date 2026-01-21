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



###################### DESCRIPTIVE STATISTICS ############################### #ANNALENA

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

## nur mit CEC korreliert
corrplot(matrix(cor_CEC, ncol = 1),
         is.corr = FALSE,
         tl.cex = 0.8)
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


#### signifikanz der korrelationen (p-wert)
cor_test_results <- lapply(names(cov_soil)[-ncol(cov_soil)], function(var) {
  test <- cor.test(cov_soil[[var]], cov_soil$CEC, method = "pearson")
  data.frame(
    Variable = var,
    Correlation = test$estimate,
    p_value = test$p.value
  )
})

cor_test_results <- do.call(rbind, cor_test_results)
cor_test_results ### bedeutet: Aspect, NIR, LS_Factor ist signifikant korreliert mit CEC


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
lm_full <- lm(CEC ~ ., data = cov_soil)
lm_full_1 <- lm(CEC ~ Aspect+Blue+Catchment_Area+Channel_Network+Elevation+Green+LS_Factor+NDVI+NIR+Rainfall+Red+Slope+SWIR1+SWIR2+Temperature+Valley_Depth+Wetness_Index,
                 data=cov_soil)
summary(lm_full_1)

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

################### reduziertes, einfaches lineare Regression #######
lm_reduced <- lm(
  CEC ~ NDVI + Catchment_Area + Slope + LS_Factor + Valley_Depth,
  data = cov_soil
)

summary(lm_reduced)

# apply the linear model on testing data
CEC_linear_Pred <- predict(lm_reduced, cov_soil)  

# check the plot actual and predicted OC values
plot(cov_soil$CEC, CEC_linear_Pred, main="Linear model", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", 
     xlim=c(0,100),ylim=c(0,100))
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


#Random Forest mit Regression Kriging #EMIL

# Erstellen eines neuen Datensatzes zum üben

cov_soilA <- cov_soil


# fit random forest model

rf_fit <- randomForest(CEC ~ ., data = cov_soilA, ntree = 1000)
summary(rf_fit)

# random forest prediction part 
map_rf <- raster::predict(covariates_RS, rf_fit)

# plot the RF map
spplot(map_rf, main = "CEC map based on RF model")


# append residuals
cov_soilA$residuals <- cov_soilA$CEC - rf_fit$predicted
names(cov_soilA)
summary(cov_soilA)

# histogram of the residuals 
hist(cov_soilA$residuals, col = "lightblue")

# convert cov_soil to spatial data
cov_soilA$x <- soil_csv$x
cov_soilA$y <- soil_csv$y
coordinates(cov_soilA) <- ~ x + y
proj4string(cov_soilA) <- CRS("+proj=utm +zone=39 +datum=WGS84 +units=m +no_defs")

# compute experimental semivariogram of residuals
install.packages(gstat)
library(gstat)

gstat_res <- gstat(formula = residuals ~ 1, data = cov_soilA)
vg_res    <- variogram(gstat_res)
plot(vg_res, plot.nu = FALSE)

# define initial semivariogram model
vg_parameters_res <- vgm(nugget = 70, psill = 100, range = 1.5, model = "Pen")
plot(vg_res, vg_parameters_res)

# fit semivariogram model
vg_model_res <- fit.variogram(vg_res, vg_parameters_res)
plot(vg_res, vg_model_res)
vg_model_res


# export boundary as a grid (approx. 90 m) 
res(covariates_RS) #Rastergröße etwa 1 km + 1 km
r_template <- raster(study_area, res = 0.009)                 # template raster
r_mask     <- rasterize(study_area, r_template, field = 1) # inside polygon = 1, outside = NA
study_area_grid <- as(r_mask, "SpatialPixelsDataFrame")    # grid for kriging

crs(cov_soilA) <- "EPSG:4326"

# ordinary kriging of residuals
res_krig <- krige(
  formula   = residuals ~ 1,
  locations = cov_soilA,
  newdata   = study_area_grid,
  model     = vg_model_res
)

# plot the residuals map
spplot(res_krig, zcol = "var1.pred", main = "residuals predictions")



# obtain regression kriging prediction
res_krig_raster <- raster::resample(raster(res_krig), map_rf)

RK_map <-     res_krig_raster +  map_rf ##evtl. umdrehen!?

# plot the RK map
spplot(RK_map, main = "CEC map based on RK model")






####################### MODELGÜTE #######################################

#CV metrics; RMSE/MAE/R² #CARLA

#Variable Importance #CARLA


