# Fijar el directorio de trabajo:
setwd('C:/Users/moroncog/Documents/GitHub/I_Taller')

# Cargar librerias:
library(raster)
library(maps)
library(mapdata)
library(sdmpredictors)
library(rerddap)
library(dplyr)
library(ncdf4)
library(ggplot2)

# Funciones de ayuda:
source('code/aux_functions.R')


# -------------------------------------------------------------------------

# Caso 1: ENCONTRAR EL PROMEDIO EN UNA ZONA DADA

# Descarga de variable particular -------------------------------------
elevation = raster::getData(name = "worldclim", var = "alt", res = 2.5, path = "data/worldclim/")

# Graficar:
windows()
plot(elevation)

# Cortar un area particular
e = raster::extent(-80, -70, -12, -3)
new_climate = raster::crop(x = elevation, y = e)
plot(new_climate$alt)

# Obtener datos:
lat = yFromRow(new_climate)
lon = xFromCol(new_climate)
selvar = matrix(getValues(new_climate), ncol = length(lon),
                nrow = length(lat), byrow = TRUE)

# Definir el area de estudio:
savePol = locator(6)
base_polygon = data.frame(x = savePol$x, y = savePol$y)

# Ver el area de estudio definida por usuario:
lines(c(base_polygon$x, base_polygon$x[1]), 
      c(base_polygon$y, base_polygon$y[1]), lwd = 2)

# Funcion para hallar la media de la variable ambiental en esa area:
outMean = get_variable2(base_polygon = base_polygon, Lon = lon, Lat = lat, Var = selvar)

# Exploramos el objeto:
outMean

# Verificamos si lo ha hecho bien:
points(outMean$Positions$x, outMean$Positions$y, col = 4, pch = 19, cex = 0.5)


# -------------------------------------------------------------------------

# Caso 2: INFORMACION ESPACIAL, EXTRAER VALORES DE VARIABLES AMBIENTALES

myData = read.csv('data/sampleData.csv')

# Exploramos los datos:
plot(myData$lon, myData$lat)

# Descarga variables bioclimaticas ----------------------------------------
bio = raster::getData(name = "worldclim", var = "bio", res = 2.5, path = "data/worldclim/")

# Temperatura promedio:
annual_mean_temperature = bio$bio1
annual_mean_temperature[] = annual_mean_temperature[]/10

# Cortar un area particular
e = raster::extent(-80, -70, -12, -3)
new_climate = raster::crop(x = annual_mean_temperature, y = e)
plot(new_climate$bio1)

# Anadir los puntos de muestreo:
points(myData$lon, myData$lat, pch = 19)

# Extraer info de raster:
lat = yFromRow(new_climate)
lon = xFromCol(new_climate)
selvar = matrix(getValues(new_climate), ncol = length(lon),
                nrow = length(lat), byrow = TRUE)

# Encontrar la variable ambiental correspondiente a cada punto de muestreo
varData = get_variable(base_data = myData[,c('lon', 'lat')], 
                       Lon = lon, Lat = lat, Var = selvar)

# Explorar objeto de salida:
varData

# Unir a mis datos:
myData$var = varData$var

# Verificar:
plot(varData$lon_env, varData$lat_env, type = 'p', pch = 19)
points(myData$lon, myData$lat, pch = 19, col = 2)

# Verificar ver2:
ggplot(myData, aes(x = lon, y = lat, color = var)) + 
        geom_point() + 
        scale_color_gradient(low="blue", high="red") +
        theme_bw()

# -------------------------------------------------------------------------

# Caso 3: INFORMACION ESPACIAL Y TEMPORAL, EXTRAER VALORES DE VARIABLES AMBIENTALES

# Leer datos:
myData = read.csv('data/sampleData2.csv')

# Formato tiempo:
myData$time = as.POSIXct(x = myData$time, tz = 'GMT')
myData$time = format(myData$time, format = '%Y-%m-%d')

# Obtener y guardar archivo netCDF 
griddap(x = "ncdcOisst21Agg_LonPM180", 
        time = range(myData$time),
        longitude = c(-85, -70),
        latitude = c(-20, -2), 
        fields = "sst", 
        store = disk("output/erddap/"))

# Leer archivo nc descargado:
envNC = nc_open(filename = 'output/erddap/d9d00b2fd01a93bd839ae465e0436f53.nc')

# Extraer informacion:
lon = ncvar_get(envNC, varid = 'longitude')
lat = ncvar_get(envNC, varid = 'latitude')
sst = ncvar_get(envNC, varid = 'sst')
time = ncvar_get(envNC, varid = 'time')
timetmp = as.POSIXct(time, origin="1970-01-01", tz="GMT")
time2 = format(timetmp, format = '%Y-%m-%d')

# Plotear variable ambiental:
image.plot(lon, sort(lat), sst[,order(lat),1])
points(myData$lon, myData$lat)

# Loop en el tiempo:
myData$var = NA 
for(i in seq_along(myData$time)){
        
        seltime = myData$time[i]
        indtime = which(seltime == time2)
        
        varData = get_variable(base_data = myData[myData$time == seltime,
                                                  c('lon', 'lat')], 
                               Lon = lon, Lat = lat, Var = sst[,,indtime])
        myData$var[myData$time == seltime] = varData$var
        
}

# Verificar:
ggplot(myData, aes(x = lon, y = lat, color = var)) + 
        geom_point() + 
        scale_color_gradient(low="blue", high="red") +
        theme_bw() +
        facet_wrap(~ time)
