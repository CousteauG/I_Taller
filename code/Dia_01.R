library(raster)
library(maps)
library(mapdata)
library(sdmpredictors)
library(rerddap)
library(dplyr)
library(ncdf4)


# -------------------------------------------------------------------------
# WORLDCLIM ---------------------------------------------------------------
# -------------------------------------------------------------------------
# https://www.worldclim.org/data/bioclim.html


# Descarga de variable particular [1] -------------------------------------
elevation <- raster::getData(name = "worldclim", var = "alt", res = 5, path = "data/worldclim/")

# Explorar objeto
elevation

# Plot simple
plot(elevation)

# Seleccionar un área particular
getData('ISO3') #contiene los códigos de los países (ID)
peru <- raster::getData("GADM", country = "PER", level = 0)
peru
plot(peru)

# crop
elevation2 <- raster::crop(x = elevation, y = extent(peru))
plot(elevation2)
plot(peru, add = TRUE, lwd = 2)

# mask
elevation3 <- raster::mask(x = elevation2, mask = peru)
plot(elevation3)
plot(peru, add = TRUE, lwd = 2)




# Descarga de variable particular [2] -------------------------------------
tmean <- raster::getData(name = "worldclim", var = "tmean", res = 2.5, path = "data/worldclim/")
tmean

plot(tmean)

tmean[] <- tmean[]/10
tmean
plot(tmean)

enero <- tmean$tmean1
plot(enero)


# Caso particular de la resolución mas fina (30 s) ------------------------
prec <- raster::getData(name = 'worldclim', var = 'prec', res = 0.5, 
                            lon = -90, lat = -20, path = "data/worldclim/")
prec
plot(prec$prec1_33)

# Cortar un area particular
e <- raster::extent(-80, -70, -10, -5)
new_climate <- raster::crop(x = prec, y = e)
plot(new_climate$prec1_33)




# Descarga variables bioclimáticas ----------------------------------------
bio <- raster::getData(name = "worldclim", var = "bio", res = 2.5, path = "data/worldclim/")
bio

plot(bio$bio1, main = "Annual Mean Temperature")

# crop
annual_mean_temperature <- bio$bio1
annual_mean_temperature[] <- annual_mean_temperature[]/10
annual_mean_temperature2 <- raster::crop(x = annual_mean_temperature, y = extent(peru))
plot(annual_mean_temperature2)
plot(peru, add = TRUE, lwd = 2)

# mask
annual_mean_temperature3 <- raster::mask(x = annual_mean_temperature2, mask = peru)
plot(annual_mean_temperature3)
plot(peru, add = TRUE, lwd = 2)



# ¿Qué pasa si tenemos raster de diferente resolución?
elevation3
annual_mean_temperature3

# Quiero que el raster de temperatura tenga la resolución del raster de elevación
annual_mean_temperature3_resize <- raster::resample(annual_mean_temperature3, elevation3)
annual_mean_temperature3_resize

plot(annual_mean_temperature3)
plot(annual_mean_temperature3_resize)



# Lectura de archivos GeoTiff (.tif)
altitud <- raster("data/worldclim/wc2.1_5m_elev.tif")

altitud



# Descarga de clima futuro ------------------------------------------------
# climate <- raster::getData(name = "CMIP5",  model = "BC", rcp = 26, year = 50, var = "bio", res = 2.5, path = "data/worldclim/")
# climate



# Guardar raster ----------------------------------------------------------
writeRaster(x = bio, filename = "output/bioclimatic_variables_worldclim.grd", format = "raster")

writeRaster(x = bio, filename = paste0("output/", names(bio)), bylayer = TRUE, format = "GTiff")

writeRaster(x = annual_mean_temperature3, filename = "output/annual_mean_temperature.grd", format = "raster")


# Leer raster -------------------------------------------------------------
stack("output/bioclimatic_variables_worldclim.grd")

raster("output/bio1.tif")



# -------------------------------------------------------------------------
# BIO-ORACLE --------------------------------------------------------------
# -------------------------------------------------------------------------

# Descargar desde Bioracle en linea datos "presentes" --------------------
list_layers(datasets = "Bio-ORACLE")$layer_code

predictors <- load_layers(layercodes = c("BO_calcite", "BO_chlomean", "BO_nitrate", 
                                         "BO_ph", "BO_salinity", "BO_sstmean"), 
                          datadir = "data/bio-oracle/")

predictors

plot(predictors)



# Capas futuras -----------------------------------------------------------
future <- list_layers_future(terrestrial = FALSE)

# Escenarios disponibles
unique(future$scenario)

unique(future$year)

# Información
get_future_layers(c("BO2_tempmin_ss", "BO2_tempmean_ss", "BO2_tempmax_ss"), 
                  scenario = "RCP26", 
                  year = 2050)$layer_code 


future_predictors <- load_layers(layercodes = c("BO2_RCP26_2050_tempmax_ss", 
                                                "BO2_RCP26_2050_tempmean_ss", 
                                                "BO2_RCP26_2050_tempmin_ss"), 
                                 datadir = "data/bio-oracle/")

future_predictors



# Guardar raster ----------------------------------------------------------
writeRaster(x = predictors, filename = "output/bio_oracle.grd", format = "raster", overwrite = TRUE)

writeRaster(x = predictors, filename = paste0("output/", names(predictors)), bylayer = TRUE, format = "GTiff")

writeRaster(x = future_predictors, filename = "output/bio_oracle_future.grd", format = "raster")

writeRaster(x = future_predictors, filename = paste0("output/", names(future_predictors)), bylayer = TRUE, format = "GTiff")
