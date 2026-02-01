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


<<<<<<< HEAD

############### RANDOM FOREST MIT REGRESSION KRIGING ##########

#cov_soil_70 <- cov_soil[cov_soil$CEC<70,]

# split the data to training (80%) and testing (20%) sets
trainIndex <- createDataPartition(cov_soil$CEC, p = 0.8, list = FALSE, times = 1)
cov_soil_Train <- cov_soil[ trainIndex,]
cov_soil_Test  <- cov_soil[-trainIndex,]

=======
>>>>>>> 1cd80d1e7b24b8c2b0084a4945eabf2fae69f428
############################### Random Forest ###########################

trainIndex <- createDataPartition(cov_soil_unab$CEC, p = 0.8, list = FALSE, times = 1)
cov_soil_Train <- cov_soil_unab[ trainIndex,]
cov_soil_Test  <- cov_soil_unab[-trainIndex,]

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
map_rf <- raster::predict(covariates_RS, rf)

# plot the RF map
spplot(map_rf, main = "CEC map based on RF model")

RMSE_rf <- sqrt(mean((cov_soil_Test$CEC - CEC_rf_Pred)^2))
MAE_rf <- mean(abs(cov_soil_Test$CEC - CEC_rf_Pred))
R2_rf <- 1 - sum((cov_soil_Test$CEC - CEC_rf_Pred)^2)/sum((cov_soil_Test$CEC - mean(cov_soil_Test$CEC))^2)

####################### REGRESSION KRIGING #######################################

# Hier müsste man entscheiden ob man alle Werte oder nur den Train/ Test Datensatz nutzt!
CEC_rf_Pred <- predict(rf, cov_soil_unab)

# append residuals
cov_soil_RK <- cov_soil_unab
cov_soil_RK$residuals <- cov_soil_RK$CEC - CEC_rf_Pred 
names(cov_soil_RK)
summary(cov_soil_RK)

# histogram of the residuals 
hist(cov_soil_RK$residuals, col = "lightblue")

# convert cov_soil to spatial data

cov_soil_RK$x <- soil_csv$x
cov_soil_RK$y <- soil_csv$y
coordinates(cov_soil_RK) <- ~ x + y
proj4string(cov_soil_RK) <- CRS("EPSG:4326")

# compute experimental semivariogram of residuals
gstat_res <- gstat(formula = residuals ~ 1, data = cov_soil_RK)
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
  locations = cov_soil_RK,
  newdata   = study_area_grid,
  model     = vg_model_res)

# plot the residuals map
spplot(res_krig, zcol = "var1.pred", main = "residuals predictions")

# obtain regression kriging prediction
res_krig_raster <- raster::resample(raster(res_krig), map_rf)

RK_map <-     res_krig_raster +  map_rf 

# rk performance 
RK_pred <- raster::extract(RK_map, cov_soil_RK)

RMSE_RK <- sqrt(mean((cov_soil$CEC - RK_pred)^2))
MAE_RK <- mean(abs(cov_soil$CEC - RK_pred))
R2_RK <- 1 - sum((cov_soil$CEC - RK_pred)^2) / sum((cov_soil$CEC - mean(cov_soil$CEC))^2)
R2_RK

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


################ Ordinary Kriging ####################
cov_soil_OK <- cov_soil_unab

cov_soil_OK$x <- soil_csv$x
cov_soil_OK$y <- soil_csv$y
coordinates(cov_soil_OK) <- ~ x + y
proj4string(cov_soil_OK) <- CRS("EPSG:4326")

gstat_OK <- gstat(formula = CEC ~ 1, data = cov_soil_OK)
vg_OK    <- variogram(gstat_OK)

plot(vg_OK, plot.nu = FALSE)

####Variogramm: Which model? Pen or SPH?
vg_init_OK <- vgm(
  nugget = 70,
  psill  = 110,
  range  = 100,
  model  = "Sph")

vg_model_OK <- fit.variogram(vg_OK, vg_init_OK)
plot(vg_OK, vg_model_OK)
vg_model_OK

#Grid 
r_template <- raster(study_area, res = 0.00898)
r_mask     <- rasterize(study_area, r_template, field = 1)
study_area_grid <- as(r_mask, "SpatialPixelsDataFrame")

#Ordinary Kriging von CEC
CEC_OK <- krige(
  formula   = CEC ~ 1,
  locations = cov_soil_OK,
  newdata   = study_area_grid,
  model     = vg_model_OK)

CEC_OK

#map
OK_map <- spplot(CEC_OK, zcol = "var1.pred", main = "CEC – Ordinary Kriging")
OK_map

#modellgüte
CEC_OK_pred <- raster::extract(raster(CEC_OK), cov_soil_OK)

RMSE_OK <- sqrt(mean((cov_soil$CEC - CEC_OK_pred)^2))
MAE_OK  <- mean(abs(cov_soil$CEC - CEC_OK_pred))
R2_OK   <- 1 - sum((cov_soil$CEC - CEC_OK_pred)^2) / sum((cov_soil$CEC - mean(cov_soil$CEC))^2)

RMSE_OK
MAE_OK
R2_OK


######### Comparing the Results ###############

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
