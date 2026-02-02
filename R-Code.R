###############################################################################
##     Practical exam: DSM - Anna-Lena Bauer, Carla Schenk, Emil Unkrig      ##
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
library(gstat)

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



####cov_soil###################### DESCRIPTIVE STATISTICS ############################### 
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

cov_soil_unab <- cov_soil[ , -which(names(cov_soil) %in% c("NIR", "SWIR1","Catchment_Area", "Channel_Network","Green", "Red", "Blue", "Temperature", "SWIR2", "Valley_Depth"))]

cor_mat <- cor(cov_soil_unab, use = "complete.obs", method = "pearson")

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.6)




############### Applying different Models ##########

set.seed(123) ##Optional, so wird der Trainingssplit jedes mal gleich geteilt --> Bessere Vergleichbarkeit

# 80/20 Split (einmal, für alle Modelle)
trainIndex <- createDataPartition(cov_soil_unab$CEC, p = 0.8, list = FALSE)

cov_soil_Train <- cov_soil_unab[trainIndex, ]
cov_soil_Test  <- cov_soil_unab[-trainIndex, ]


############################### Random Forest ###########################

rf <- randomForest(CEC ~ ., data = cov_soil_Train, ntree = 10000) 
summary(rf)

importance(rf)  # variable importance
varImpPlot(rf, main = "Variable Importance for RF model")

CEC_rf_Pred <- predict(rf, cov_soil_Test)

# check the plot actual and predicted OC values 
plot(cov_soil_Test$CEC, CEC_rf_Pred, main="RF model", 
     col="blue",xlab="Actual CEC", ylab="Predicted CEC", xlim=c(0,100),ylim=c(0,100))

abline(coef = c(0,1),  col="red" )

cor_rf <- cor(cov_soil_Test$CEC, CEC_rf_Pred)  # calculate rf correlation & performance
cor_rf

# random forest prediction part 
#map_rf <- raster::predict(covariates_RS, rf)

# plot the RF map
spplot(map_rf, main = "CEC map based on RF model")

RMSE_rf <- sqrt(mean((cov_soil_Test$CEC - CEC_rf_Pred)^2))
MAE_rf <- mean(abs(cov_soil_Test$CEC - CEC_rf_Pred))
R2_rf <- 1 - sum((cov_soil_Test$CEC - CEC_rf_Pred)^2)/sum((cov_soil_Test$CEC - mean(cov_soil_Test$CEC))^2)


############ NEU: Regression Kriging #############

# RF-Vorhersage im TRAIN (nicht im test, wie oben)
CEC_rf_train <- predict(rf, cov_soil_Train)

### Residuen nur im TRAIN
cov_soil_RK_train <- cov_soil_Train
cov_soil_RK_train$residuals <- cov_soil_RK_train$CEC - CEC_rf_train

### Spatial Object: konsistente Indizes
cov_soil_RK_train$x <- soil_csv$x[trainIndex]
cov_soil_RK_train$y <- soil_csv$y[trainIndex]
coordinates(cov_soil_RK_train) <- ~ x + y
proj4string(cov_soil_RK_train) <- CRS("EPSG:4326")

## Variogramm der Residuen
vg_res <- variogram(residuals ~ 1, cov_soil_RK_train)
vg_mod <- fit.variogram(vg_res, vgm("Sph"))
plot(vg_res, vg_mod)


# histogram of the residuals 
#hist(cov_soil_RK$residuals, col = "lightblue")

###RK-Prediction für TEST
cov_soil_Test$x <- soil_csv$x[-trainIndex]
cov_soil_Test$y <- soil_csv$y[-trainIndex]
coordinates(cov_soil_Test) <- ~ x + y
proj4string(cov_soil_Test) <- CRS("EPSG:4326")

res_krig_test <- krige(
  residuals ~ 1,
  locations = cov_soil_RK_train,
  newdata   = cov_soil_Test,
  model     = vg_mod
)

### Endgültige RK-Vorhersage
CEC_rf_test <- predict(rf, cov_soil_Test)
CEC_RK_pred <- CEC_rf_test + res_krig_test$var1.pred

### Modellgüte RK
RMSE_RK <- sqrt(mean((cov_soil_Test$CEC - CEC_RK_pred)^2))
RMSE_RK
MAE_RK  <- mean(abs(cov_soil_Test$CEC - CEC_RK_pred))
MAE_RK
R2_RK   <- 1 - sum((cov_soil_Test$CEC - CEC_RK_pred)^2) /
  sum((cov_soil_Test$CEC - mean(cov_soil_Test$CEC))^2)
R2_RK


####karte erstellen
vg_res_full <- variogram(residuals ~ 1, cov_soil_RK)
vg_res_full <- fit.variogram(vg_res_full, vgm("Sph"))
res_krig_map <- krige(
  residuals ~ 1,
  locations = cov_soil_RK,
  newdata   = study_area_grid,
  model     = vg_res_full
)

RK_map <- map_rf + raster(res_krig_map)

spplot(RK_map, main = "CEC – Regression Kriging")



################ Ordinary Kriging ####################
cov_soil_OK <- cov_soil_unab

cov_soil_OK$x <- soil_csv$x
cov_soil_OK$y <- soil_csv$y

coordinates(cov_soil_OK) <- ~ x + y
proj4string(cov_soil_OK) <- CRS("EPSG:4326")

gstat_OK <- gstat(formula = CEC ~ 1, data = cov_soil_OK)

vg_OK <- variogram(gstat_OK)

plot(vg_OK, plot.nu = FALSE,
     main = "Empirisches Variogramm von CEC")

####Variogramm 
vg_init_OK <- vgm(
  nugget = 70,
  psill  = 110,
  range  = 100,
  model  = "Sph"
)

vg_model_OK <- fit.variogram(vg_OK, vg_init_OK)

plot(vg_OK, vg_model_OK,
     main = "Gefittetes Variogramm")
vg_model_OK

###leave one out cross validation
cv_OK <- krige.cv(
  CEC ~ 1,
  locations = cov_soil_OK,
  model     = vg_model_OK,
  nfold     = nrow(cov_soil_OK)   # LOOCV --> für jede Zeile wird einmal ein Punkt ausgelassen, dann geschaut wie er sich verhalt und das wird dann für jeden sample gemacht
)

##modellgüte
res_OK <- cv_OK$residual

RMSE_OK <- sqrt(mean(res_OK^2))
MAE_OK  <- mean(abs(res_OK))
R2_OK   <- 1 - sum(res_OK^2) /
  sum((cov_soil_OK$CEC - mean(cov_soil_OK$CEC))^2)

RMSE_OK
MAE_OK
R2_OK

#####Karte
#Grid
r_template <- raster(study_area, res = 0.00898)
r_mask     <- rasterize(study_area, r_template, field = 1)
study_area_grid <- as(r_mask, "SpatialPixelsDataFrame")

##Jetzt kommt eigentliches Ordinary kriging
CEC_OK_map <- krige(
  formula   = CEC ~ 1,
  locations = cov_soil_OK,
  newdata   = study_area_grid,
  model     = vg_model_OK
)

#plot
spplot(
  CEC_OK_map,
  zcol = "var1.pred",
  main = "CEC – Ordinary Kriging"
)


######### Comparing the Results ###############

##Barplots
RMSE_models <- c(RF = RMSE_rf, RK = RMSE_RK, OK = RMSE_OK)
MAE_models  <- c(RF = MAE_rf,  RK = MAE_RK,  OK = MAE_OK)
R2_models   <- c(RF = R2_rf,   RK = R2_RK,   OK = R2_OK)

par(mfrow = c(1,3))

barplot(RMSE_models, main="RMSE", col=c("steelblue","darkgreen","orange"))
barplot(MAE_models,  main="MAE",  col=c("steelblue","darkgreen","orange"))
barplot(R2_models,   main=expression(R^2), col=c("steelblue","darkgreen","orange"))

par(mfrow = c(1,1))

### Prediction vs observed Plots
# Beobachtungen (Testdaten)
obs_test <- cov_soil_Test$CEC
#predictions
pred_RF <- CEC_rf_Pred
pred_RK <- CEC_RK_pred
obs_OK  <- cov_soil_OK$CEC
pred_OK <- cov_soil_OK$CEC - res_OK

par(mfrow = c(1,3))

# RF
plot(obs_test, pred_RF,
     main = "Random Forest",
     xlab = "Observed CEC",
     ylab = "Predicted CEC")
abline(0, 1, col = "red", lwd = 2)

# RK
plot(obs_test, pred_RK,
     main = "Regression Kriging",
     xlab = "Observed CEC",
     ylab = "Predicted CEC")
abline(0, 1, col = "red", lwd = 2)

# OK (LOOCV)
plot(obs_OK, pred_OK,
     main = "Ordinary Kriging",
     xlab = "Observed CEC",
     ylab = "Predicted CEC")
abline(0, 1, col = "red", lwd = 2)

par(mfrow = c(1,1))


#### alle Modellgütemaße auf einmal anzeigen lassen
RMSE <- function(o, p) sqrt(mean((o - p)^2))
MAE  <- function(o, p) mean(abs(o - p))
R2   <- function(o, p) {
  1 - sum((o - p)^2) / sum((o - mean(o))^2)
}

metrics <- data.frame(
  Model = c("RF", "RK", "OK"),
  
  RMSE = c(
    RMSE(obs_test, pred_RF),
    RMSE(obs_test, pred_RK),
    RMSE(obs_OK,  pred_OK)
  ),
  
  MAE = c(
    MAE(obs_test, pred_RF),
    MAE(obs_test, pred_RK),
    MAE(obs_OK,  pred_OK)
  ),
  
  R2 = c(
    R2(obs_test, pred_RF),
    R2(obs_test, pred_RK),
    R2(obs_OK,  pred_OK)
  )
)

metrics


### Variogramme vergleichen

#RK
plot(vg_res, vg_mod, main="Fitted Variogramm RK")
vg_mod

#OK
plot(vg_OK, vg_model_OK,
     main = "Fitted Variogramm OK")
vg_model_OK



############ Alte plots
# plot the maps
par(mfrow = c(2,2))
spplot(map_lin, main = "CEC map based on Linear model")
spplot(map_rf_red, main = "CEC map based on RF model")
spplot(RK_map, main = "CEC map based on RK model")
ssplot(OK_map, main= "CEC map based on OK")


#library(gridExtra)
#p1 <- spplot(map_lin, main = "CEC: Linear Model", col.regions = viridis::viridis(100))
#p2 <- spplot(map_rf, main = "CEC: Random Forest", col.regions = viridis::viridis(100))
#p3 <- spplot(RK_map, main = "CEC: Regression Kriging", col.regions = viridis::viridis(100))
#grid.arrange(p1, p2, p3, ncol = 1, nrow = 3)


RMSE_models <- c(Linear=RMSE_linear, RF=RMSE_rf) #, RK=RMSE_RK)
cor_models <- c(Linear=cor_linear, RF=cor_rf)
R2_models <- c(Linear=R2_linear, RF=R2_rf) #, RK=R2_RK)

par(mfrow = c(1,3))
barplot(RMSE_models, main="RMSE",col=c("red","blue","green"))
barplot(cor_models, main="Correlation",col=c("red","blue","green"))
barplot(R2_models, main="R2",col=c("red","blue","green"))

par(mfrow = c(1,1))

# nochmal in hübscher!
models_df <- data.frame(RMSE = RMSE_models, Correlation = cor_models, R2 = R2_models, Model = names(RMSE_models))

models_long <- models_df %>% tidyr::pivot_longer(cols = c(RMSE, Correlation, R2), names_to = "Metric", values_to = "Value")

ggplot(models_long, aes(x = Model, y = Value, fill = Model)) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.3) +
  geom_text(aes(label = round(Value, 3)), 
            vjust = -0.3, hjust = 0.5,      
            color = "black", fontface = "bold", size = 3.5) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  labs(title = "Modellvergleich: RMSE, Korrelation, R²",
       x = "Modelle", y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


