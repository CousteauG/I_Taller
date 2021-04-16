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
elevation2
plot(elevation2)
plot(peru, add = TRUE, lwd = 2)

# mask
elevation3 <- raster::mask(x = elevation2, mask = peru)
elevation3
plot(elevation3)
plot(peru, add = TRUE, lwd = 2)


# Mapa inicial ------------------------------------------------------------
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0))

# Obtener los códigos de los países ---------------------------------------
cs <- ccodes()
head(cs)

# Filtros de paises -------------------------------------------------------
# cs <- subset(cs, continent == "South America")
# cs <- subset(cs, UNREGION2 == "Americas")
# cs <- subset(cs, UNREGION1 == "South America")
cs <- subset(cs, NAME %in% c("Peru", "Ecuador", "Chile"))
cs$ISO3
cs$NAME

# Descargar países --------------------------------------------------------
for (i in 1:nrow(cs)){
  raster::getData("GADM", country = cs$ISO3[i], level = 0, path = "data/gadm/")
}


# Agregar los países seleccionados ----------------------------------------
for (i in 1:nrow(cs)){
  country <- raster::getData("GADM", country = cs$ISO3[i], level = 0, path = "data/gadm/")
  plot(country, add = TRUE)
}


# Variar la escala de colores ---------------------------------------------
# Colores predefinidos ----------------------------------------------------
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = terrain.colors(255, rev = TRUE))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = heat.colors(255, rev = TRUE))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = topo.colors(255, rev = TRUE))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = cm.colors(255, rev = TRUE))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = rainbow(255))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = gray.colors(255, rev = TRUE))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = rev(bpy.colors(255)))


# Crear paleta de colores -------------------------------------------------
jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = jet.colors(255))

colores <- colorRampPalette(c("blue", "white", "red"))
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = colores(255))


# Cambiar posición de la leyenda ------------------------------------------
plot(elevation, xlim = c(-100, -70), ylim = c(-30, 0), col = colores(255), horizontal=T)


elevation

# Rango de valores para la extensión total de elevation -------------------
range_value <- range(elevation[], na.rm = TRUE)


# crop --------------------------------------------------------------------
elevation4 <- raster::crop(x = elevation, y = extent(c(-100, -70, -30, 0)))
elevation4
plot(elevation4)
# al haber delimitadoun área, ya no es necesario especificar los límites

# Rango de valores para el área seleccionada ------------------------------
range_value <- range(elevation4[], na.rm = TRUE)
range_value


# Modificar los intervalos de la leyenda ----------------------------------
breaks = c(range_value[1], seq(0, 5000, by = 1000), range_value[2])
breaks
plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)))

# Intervalos no regulares -------------------------------------------------
breaks = c(range_value[1], 0, 1000, 2500, 5000, range_value[2])
breaks
plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)))

plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)), lab.breaks = c("", c(0, 1000, 2500, 5000), ""))

plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)), horizontal = TRUE)


# Leyenda dentro del área del mapa ----------------------------------------
plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)), legend = FALSE)
plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)), legend.only = TRUE, smallplot = c(.13, .15, .2, .5), legend.args = list( text = "Elevación", col = "magenta", cex = 1.2, side = 3, line = 1.5))


# Modificando los ejes ----------------------------------------------------
plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)), legend = FALSE, axes = FALSE)
plot(elevation4, xlim = c(-100, -70), ylim = c(-30, 0), breaks = breaks, col = rev(bpy.colors(length(breaks)-1)), legend.only = TRUE, smallplot = c(.13, .15, .2, .5), legend.args = list( text = "Elevación", col = "magenta", cex = 1.2, side = 3, line = 1.5))

axis(1, at = seq(-100, -70, by = 5), labels = paste0(seq(100, 70, by = -5), "°W"))
axis(2, at = seq(-30, 0, by = 5), labels = c(paste0(seq(30, 5, by = -5), "°S"), "0°"))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



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


# Seleccionamos BO_sstmean ------------------------------------------------
predictors[[6]]
plot(predictors[[6]])



# Seleccionando un área específica ----------------------------------------
BO_sstmean <- raster::crop(x = predictors[[6]], y = extent(c(-122, -104, 20, 35)))
BO_sstmean
plot(BO_sstmean)


library(maps)
library(mapdata)
library(maptools)

plot(BO_sstmean, xlim = c(-122, -104), ylim = c(20, 35), xlab = "", ylab = "")
map("worldHires", col = "gray90", fill = TRUE, add = TRUE)

G <- data.frame(x = c(-117,-113,-115), y = c(22,26,29))

title(xlab = "Longitud (O°)", ylab = "Latitud (N°)", cex.lab = 1.2, cex.axis = 1.2)
mtext(text = "", side = 3, line = +1, font = 2,
      adj = 0.5, cex = 2, col = "violet")
text(x = -114,y = 23,labels = "Océano Pacífico", srt = -38, col = "black",cex = .85)
text(x = -111.,y = 26.9,labels = "Golfo de California", srt = -38, col = "black",cex = .7)
text(x = -108,y = 29,labels = "México", col = "black",cex = .8)
text(x = -112,y =34,labels = "E.U.A.", col = "black",cex = .8)
points(x = G$Lon,y = G$Lat, pch = 1, cex = 0.5, col="blue")
box()
require(prettymapr)
addnortharrow(pos = "bottomleft", padin = c(0.10, 0.10), scale = 0.45,lwd = 1,
              border = "black", cols = c("white", "black"),text.col = "black")


# Usando ggplot -----------------------------------------------------------
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)


G <- data.frame(x = c(-117,-113,-115), y = c(22,26,29))



# Convirtiendo el raster en data.frame
# BO_sstmean_df <- as.data.frame(predictors[[6]], xy = TRUE)
BO_sstmean_df <- as.data.frame(BO_sstmean, xy = TRUE)

world <- ne_countries(scale = "medium", returnclass = "sf")


ggplot() +
  geom_raster(data = BO_sstmean_df,
              aes(x = x, y = y, fill = BO_sstmean)) +
  scale_fill_gradientn(colours = heat.colors(255, rev = TRUE)) +
  geom_point(data = G, aes(x = x, y = y), color = "blue") +
  geom_sf(data = world) +
  annotate(geom="text", x=-114, y=23, label="Océano Pacífico", srt = -38, size=4) +
  annotate(geom="text", x=-111, y=26.9, label="Golfo de California", srt = -38, size=2) +
  annotate(geom="text", x=-108, y=29, label="México", size=3)+
  annotate(geom="text", x=-112, y=34, label="E.U.A.", size=3)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-118,-106), ylim = c(20,35)) +
  xlab("Longitud (O°)") +
  ylab("Latitud (N°)") +
  ggtitle("Mapa") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(size = 20, hjust = 0.5, colour = "violet"))



my_col <- terrain.colors(3)

ggplot() +
  geom_raster(data = BO_sstmean_df,
              aes(x = x, y = y, fill = BO_sstmean)) +
  scale_fill_gradientn(colours = my_col) +
  geom_point(data = G, aes(x = x, y = y), color = "blue") +
  geom_sf(data = world) +
  annotate(geom="text", x=-114, y=23, label="Océano Pacífico", srt = -38, size=4) +
  annotate(geom="text", x=-111, y=26.9, label="Golfo de California", srt = -38, size=2) +
  annotate(geom="text", x=-108, y=29, label="México", size=3)+
  annotate(geom="text", x=-112, y=34, label="E.U.A.", size=3)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-118,-106), ylim = c(20,35)) +
  xlab("Longitud (O°)") +
  ylab("Latitud (N°)") +
  ggtitle("Mapa") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue"),
        plot.title = element_text(size = 20, hjust = 0.5, colour = "violet"))



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


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


