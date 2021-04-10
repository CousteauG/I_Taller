

# Leer archivos netCDF usando paquete ncdf4 I -----------------------------

# Cargar paquete
require(ncdf4)

# Mostrar metadata de netCDF de topografía
nc_open(filename = "data/erddap/etopo180_5ed7_aa5a_84f0.nc")

# Revisar nombre y características de variable(s)
# Revisar nombres y características de dimensiones
# Revisar atributos del archivo


# Guardar información de netCDF en objeto
informacion_nc <- nc_open(filename = "data/erddap/etopo180_5ed7_aa5a_84f0.nc")

# Obtener valores de una variable
topografia <- ncvar_get(nc = informacion_nc, varid = "altitude")

# Obtener dimensiones
dim(topografia)

# Gráfico rápido de verificación
image(topografia)

# Crear lista XYZ
topoXYZ <- list(x = informacion_nc$dim$longitude$vals,
                y = informacion_nc$dim$latitude$vals,
                z = topografia)

# Convertir a objeto raster
require(raster)

topoRaster <- raster(x = topoXYZ)

class(topoRaster)

# Gráfico rápido de verificación
plot(topoRaster)

# Cerrar enlace con metadata
nc_close(nc = informacion_nc)

# topografia <- ncvar_get(nc = informacion_nc, varid = "altitude")


# Leer archivos netCDF usando paquete ncdf4 II ----------------------------

# https://upwell.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg_LonPM180.graph

# Cargar paquete
require(ncdf4)

# Obtener metadata de netCDF 
informacion_nc <- nc_open(filename = "data/erddap/ncdcOisst21Agg_LonPM180_cb4f_b0b2_6eee.nc")

# Obtener valores de una variable
temperatura <- ncvar_get(nc = informacion_nc, varid = "sst")

# Obtener valores de fecha-hora
informacion_nc$dim$time$vals
as.POSIXct(x = informacion_nc$dim$time$vals, 
           origin = as.POSIXct("1970-1-1 00:00:00"))
as.Date(as.POSIXct(x = informacion_nc$dim$time$vals, 
                   origin = as.POSIXct("1970-1-1 00:00:00")))

# Obtener valores de fecha
valoresTiempo <- as.Date(as.POSIXct(x = informacion_nc$dim$time$vals, 
                                    origin = as.POSIXct("1970-1-1 00:00:00")))

# Dimensiones de arreglo de temperatura
dim(temperatura)

# Crear lista XYZ para fecha en particular
miFecha <- as.Date("2010-02-22")

TSM_XYZ <- list(x = informacion_nc$dim$longitude$vals,
                y = informacion_nc$dim$latitude$vals,
                z = temperatura[,,valoresTiempo == miFecha])

# Convertir a objeto raster
require(raster)

TSM_raster <- raster(x = TSM_XYZ)

# Gráfico rápido de verificación
plot(TSM_raster)

# Cerrar enlace con metadata
nc_close(nc = informacion_nc)


# Leer archivos netCDF usando paquete ncdf4 III ---------------------------

# Cargar paquete
require(ncdf4)

# ncFile <- nc_open(filename = "F:/SatelliteData/CDF/MUR_SST/2002-06-01.nc")
# ncData <- ncvar_get(nc = ncFile, varid = "analysed_sst")
# nc_close(nc = ncFile)

# Definir límites espaciales/temporales para la lectura
xlim <- c(-76, -70)
ylim <- c(-19, -14)
timelim <- c("2010-4-1", "2010-8-30")

# Mostrar metadata de netCDF de topografía
informacion_nc <- nc_open(filename = "data/erddap/erdSoda331oceanmday_LonPM180_375e_0002_193a.nc")

# Obtener índices para límites en longitud
index_x <- cut(x = xlim,
               breaks = c(informacion_nc$dim$longitude$vals, Inf),
               labels = seq(informacion_nc$dim$longitude$len))
index_x <- as.numeric(as.character(index_x)) + c(0, 1)

# Obtener índices para límites en latitud
index_y <- cut(x = ylim,
               breaks = c(informacion_nc$dim$latitude$vals, Inf),
               labels = seq(informacion_nc$dim$latitude$len))
index_y <- as.numeric(as.character(index_y)) + c(0, 1)

# Obtener índices para límites en tiempo
valoresTiempo <- as.Date(as.POSIXct(x = informacion_nc$dim$time$vals,
                                    origin = "1970-1-1 00:00:00"))
index_time <- cut(x = as.Date(timelim),
                  breaks = c(valoresTiempo, Inf),
                  labels = seq(informacion_nc$dim$time$len))
index_time <- as.numeric(as.character(index_time))

# Read nc depth data
temp <- ncvar_get(nc = informacion_nc, 
                    varid = "temp", 
                    start = c(index_x[1], index_y[1], 1, index_time[1]),
                    count = c(diff(index_x) + 1, 
                              diff(index_y) + 1,
                              -1,
                              diff(index_time + 1)))

# Ver dimendiones de datos extraídos
dim(temp)

# Convertir a un objeto raster
require(raster)

temp_raster <- raster(list(x = informacion_nc$dim$longitude$vals[seq(index_x[1], index_x[2])],
                           y = informacion_nc$dim$latitude$vals[seq(index_y[1], index_y[2])],
                           z = temp[,,1,1]))

# Plot de verificación
plot(temp_raster)

# Close nc file
nc_close(nc = ncFile)



# Leer archivos netCDF con paquete raster ---------------------------------

# Cargar paquete
require(raster)

# ETOPO
topografia <- raster(x = "data/erddap/etopo180_5ed7_aa5a_84f0.nc")

plot(topografia)

# AVHRR
temperatura <- brick(x = "data/erddap/ncdcOisst21Agg_LonPM180_cb4f_b0b2_6eee.nc")

plot(temperatura)

# SODA
soda <- brick(x = "data/erddap/erdSoda331oceanmday_LonPM180_375e_0002_193a.nc",
              varname = "temp", level = 1)

plot(soda)


# rerddap -----------------------------------------------------------------

# https://coastwatch.pfeg.noaa.gov/erddap/index.html
# https://upwell.pfeg.noaa.gov/erddap/index.html

require(rerddap)


# AVHRR, ID: ncdcOisst21Agg_LonPM180
info(datasetid = "ncdcOisst21Agg_LonPM180")

# Obtener y guardar archivo netCDF 
griddap(x = "ncdcOisst21Agg_LonPM180", 
        time = c("2010-1-1", "2010-03-31"),
        longitude = c(-85, -70),
        latitude = c(-20, -2), 
        fields = "sst", 
        store = disk("output/erddap/"))


# SODA, ID: erdSoda331oceanmday_LonPM180
info(datasetid = "erdSoda331oceanmday_LonPM180")

# Obtener y guardar archivo netCDF 
soda <- griddap(x = "erdSoda331oceanmday_LonPM180", 
                time = c("2010-1-1", "2010-12-31"),
                longitude = c(-85, -70),
                latitude = c(-20, -2), 
                fields = c("temp", "salt", "u", "v"), 
                store = disk("output/erddap/"))

# Obtener nombre temporal de archivo
soda$summary$filename

# Cambiar nombre de archivo
file.rename(from = soda$summary$filename, 
            to = "output/erddap/SODA_85W-70W_20S-2S_2010.nc")


# Copernicus --------------------------------------------------------------

# https://marine.copernicus.eu/
# https://github.com/clstoulouse/motu-client-python

# Definir servidor
server <- "http://nrt.cmems-du.eu/motu-web/Motu"

# Range of dates
startDate <- "2019-1-1"
endDate <- "2019-4-1"
by <- "month"

usr_psw <- list(user = "INSERTE-SU-USUARIO-AQUI",
                password = "INSERTE-SU-CONTRASEÑA-AQUI")

suffixOutputName <- ""

# Range of space
lonRange <- c(-85, -70)
latRange <- c(-10, 0)

# Product ID
# productID <- "GLOBAL-ANALYSIS-FORECAST-PHY-001-024"
productInfo <- list(service_ID = "GLOBAL_REANALYSIS_PHY_001_030-TDS", 
                    product_ID = "global-reanalysis-phy-001-030-daily")

productInfo <- list(service_ID = "GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS",
                    product_ID = "global-analysis-forecast-phy-001-024")

# List of variables
variables <- c("thetao", "so", "uo", "vo")

# Range of depths
depthRange <- c(0, 500)

# Indicates the folder for downloading
outFolder <- "output/copernicus/"

# Do you want to overwrite the existing files? (If FALSE, the script will skip
# those already existing files)
overwrite <- TRUE


# COMBINAR PARÁMETROS PARA DECARGA  

# Definir rango de fechas
depthRange <- gsub(x = format(depthRange, nsmall = 3), pattern = " ", replacement = "")

# Obtener vector de fechas
allDates <- seq(from = as.Date(startDate), to = as.Date(endDate), by = by)

newPaths <- file.path("data/MercatorOcean/", unique(format(x = allDates, format = "%Y")))
lapply(newPaths, dir.create, showWarnings = FALSE, recursive = TRUE)

prevs <- paste("python -m motuclient --motu",
               sprintf("'%s'", server),
               "--service-id", 
               productInfo$service_ID, 
               "--product-id", 
               productInfo$product_ID, 
               "--longitude-min",
               lonRange[1], 
               "--longitude-max", 
               lonRange[2], 
               "--latitude-min", 
               latRange[1], 
               "--latitude-max",
               latRange[2], 
               "--depth-min", 
               depthRange[1], 
               "--depth-max", 
               depthRange[2], 
               paste(paste("--variable", variables), collapse = " "), 
               "--user",
               usr_psw$user,
               "--pwd",
               usr_psw$password,
               "--out-dir", 
               sprintf('"%s"', outFolder))

overwrite <- isTRUE(overwrite)

for(i in seq(2, length(allDates))){
  
  tempDates <- c(allDates[i - 1], allDates[i] - 1)
  
  cat("\n------- Start to download ", as.character(tempDates[1]), " [", i - 1, "/", length(allDates) - 1, "] -------", sep = "")  
  
  outName <- paste0(suffixOutputName, 
                    format(tempDates[1], format = "%Y-%m-%d"), ".nc")
  
  if(file.exists(file.path(outFolder, outName)) && isTRUE(overwrite)){
    cat("\nSkipping ", as.character(tempDates[1]), ", the file alread exists.\n", sep = "")
    next
  }
  
  toCMD <- paste(prevs[1], 
                 "--date-min",
                 format(x = tempDates[1], format = "%F 12:00:00"), 
                 "--date-max",
                 format(x = tempDates[2], format = "%F 12:00:00"),
                 "--out-name",
                 outName)
  
  system(command = toCMD)
  
  Sys.sleep(3)
}
