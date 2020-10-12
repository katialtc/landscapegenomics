# landscapegenomics
#Paquetes necesarios instalar y llamar 
library(knitr)
library(rgl)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(rasterVis)
library(maptools)
library(rgdal)
library(sp)
install.packages("rgeos")
library(rgeos)
library(ggplot2)

#Cargar mapa recorte de cada polígono###
map_dirmarta <- "MAPA TESIS/martawgs844326.shp"
map_vecmarta <- rgdal::readOGR(dsn = map_dirmarta, layer = "martawgs844326")
plot(map_vecmarta)

paths_capas <- list.files("MAPA TESIS/BioClim(Tiff)",
                          pattern = "*.tif$",full.names = TRUE)
#Esta opción es si utilizara todas las variables 
#stackangela<- raster::stack(paths_capas)

#Otra opción es llamar capa por capa y hacer un stack, esta es la opción que utilicé ya que solo nos quedamos con las siguientes variables

bio1 <- raster("MAPA TESIS/BioClim(Tiff)/bio01.tif")
bio2 <- raster("MAPA TESIS/BioClim(Tiff)/bio02.tif")
bio3 <- raster("MAPA TESIS/BioClim(Tiff)/bio03.tif")
bio4 <- raster("MAPA TESIS/BioClim(Tiff)/bio04.tif")
bio7 <- raster("MAPA TESIS/BioClim(Tiff)/bio07.tif")
bio12 <- raster("MAPA TESIS/BioClim(Tiff)/bio12.tif")
bio15 <- raster("MAPA TESIS/BioClim(Tiff)/bio15.tif")


stackangela <- stack(bio1, bio12, bio15, bio2, bio3, bio4, bio7, layers=NULL)

Martabiocrop <-  crop(stackangela, extent(map_vecmarta))


plot(Martabiocrop, add=T)


dir.create("marta_BioStack7/")

lapply(names(Martabiocrop), function(x){
  writeRaster(Martabiocrop[[x]], paste0("marta_BioStack7//",
                                         x,".tif"),overwrite=TRUE)})

#Repetir para Peña Nevada
map_dirpenanevada <- "MAPA TESIS/penanevadawgs844326.shp"
map_vecpenanevada <- rgdal::readOGR(dsn = map_dirpenanevada, layer = "penanevadawgs844326")
plot(map_vecpenanevada)


PenaNevadabiocrop <-  crop(stackangela, extent(map_vecpenanevada))


plot(PenaNevadabiocrop, add=T)


dir.create("penanevada_BioStack7/")

lapply(names(PenaNevadabiocrop), function(x){
  writeRaster(PenaNevadabiocrop[[x]], paste0("penanevada_BioStack7//",
                                        x,".tif"),overwrite=TRUE)})
                                        
#Repetir para El Potosí
map_dirpotosi <- "MAPA TESIS/potosiwgs844326.shp"
map_vecpotosi <- rgdal::readOGR(dsn = map_dirpotosi, layer = "potosiwgs844326")
plot(map_vecpotosi)


Potosibiocrop <-  crop(stackangela, extent(map_vecpotosi))

plot(Potosibiocrop, add=T)

dir.create("potosi_BioStack7/")

lapply(names(Potosibiocrop), function(x){
  writeRaster(Potosibiocrop[[x]], paste0("potosi_BioStack7//",
                                              x,".tif"),overwrite=TRUE)})
                                              
                                              
#Primero hice un stack de cada conjunto de capas bioclimáticas recortadas en formato tif 

tiffilespathmarta <- list.files("marta_BioStack7//", 
                           pattern = "*.tif$",full.names = TRUE)
tifmarta<- raster::stack(tiffilespathmarta)
plot(tifmarta)

tiffilespathpenanevada <- list.files("penanevada_BioStack7//", 
                           pattern = "*.tif$",full.names = TRUE)
tifpenanevada<- raster::stack(tiffilespathpenanevada)
plot(tifpenanevada)

tiffilespathpotosi <- list.files("potosi_BioStack7//", 
                           pattern = "*.tif$",full.names = TRUE)
tifpotosi<- raster::stack(tiffilespathpotosi)
plot(tifpotosi)


#EXTRACCIÓN DE DATOS CULMINICOLA / para cada localidad  

#Leer un archivo csv y convertirlo en un objeto para llamarlo más fácil 
puntosculminicola <- read.csv("Pinusculminicola puntos.csv")
#Para verificar que estén bien los datos, me di cuenta que tiene que ir primero acomodado en la matriz longitud y luego latitud, usualmente la base de datos lo acomoda al revés 
head(puntosculminicola)

#Plotear y extraer los puntos LA MARTA
plot(tifmarta)
points(puntosculminicola$decimalLongitude, puntosculminicola$decimalLatitude, pch=".",col="blue") 
pinusculminicolamarta_extract <- data.frame(extract(tifmarta,puntosculminicola))
pinusculminicolamarta_extract <- na.omit(pinusculminicolamarta_extract)
knitr::kable(head(pinusculminicolamarta_extract))
knitr::kable(round(cor(pinusculminicolamarta_extract),2))
#Plotear y extraer los puntos PEÑA NEVADA
plot(tifpenanevada)
points(puntosculminicola$decimalLongitude, puntosculminicola$decimalLatitude, pch=".",col="blue") 
pinusculminicolapenanevada_extract <- data.frame(extract(tifpenanevada,puntosculminicola))
pinusculminicolapenanevada_extract <- na.omit(pinusculminicolapenanevada_extract)
knitr::kable(head(pinusculminicolapenanevada_extract))
knitr::kable(round(cor(pinusculminicolapenanevada_extract),2))
#Plotear y extraer los puntos EL POTOSÍ
plot(tifpotosi)
points(puntosculminicola$decimalLongitude, puntosculminicola$decimalLatitude, pch=".",col="blue") 
pinusculminicolapotosi_extract <- data.frame(extract(tifpotosi,puntosculminicola))
pinusculminicolapotosi_extract <- na.omit(pinusculminicolapotosi_extract)
knitr::kable(head(pinusculminicolapotosi_extract))
knitr::kable(round(cor(pinusculminicolapotosi_extract),2))

#EXTRACCIÓN DE DATOS HARTWEGII / para cada localidad 
#Plotear y extraer los puntos LA MARTA
plot(tifmarta)
puntoshartwegii <- read.csv("Pinushartwegii puntos.csv")
head(puntoshartwegii)
points(puntoshartwegii$decimalLongitude, puntoshartwegii$decimalLatitude, pch=".",col="blue") 
pinushartwegiimarta_extract <- data.frame(extract(tifmarta,puntoshartwegii))
pinushartwegiimarta_extract <- na.omit(pinushartwegiimarta_extract)
knitr::kable(head(pinushartwegiimarta_extract))
knitr::kable(round(cor(pinushartwegiimarta_extract),2))
#Plotear y extraer los puntos PEÑA NEVADA
plot(tifpenanevada)
puntoshartwegii <- read.csv("Pinushartwegii puntos.csv")
head(puntoshartwegii)
points(puntoshartwegii$decimalLongitude, puntoshartwegii$decimalLatitude, pch=".",col="blue") 
pinushartwegiipenanevada_extract <- data.frame(extract(tifpenanevada,puntoshartwegii))
pinushartwegiipenanevada_extract <- na.omit(pinushartwegiipenanevada_extract)
knitr::kable(head(pinushartwegiipenanevada_extract))
knitr::kable(round(cor(pinushartwegiipenanevada_extract),2))
#Plotear y extraer los puntos EL POTOSÍ
plot(tifpotosi)
puntoshartwegii <- read.csv("Pinushartwegii puntos.csv")
head(puntoshartwegii)
points(puntoshartwegii$decimalLongitude, puntoshartwegii$decimalLatitude, pch=".",col="blue") 
pinushartwegiipotosi_extract <- data.frame(extract(tifpotosi,puntoshartwegii))
pinushartwegiipotosi_extract <- na.omit(pinushartwegiipotosi_extract)
knitr::kable(head(pinushartwegiipotosi_extract))
knitr::kable(round(cor(pinushartwegiipotosi_extract),2))

write.csv(pinusculminicolamarta_extract, "MAPA TESIS/Pinusculminicolamarta.csv")
write.csv(pinusculminicolapenanevada_extract, "MAPA TESIS/Pinusculminicolapenanevada.csv")
write.csv(pinusculminicolapotosi_extract, "MAPA TESIS/Pinusculminicolapotosi.csv")
write.csv(pinushartwegiimarta_extract, "MAPA TESIS/Pinushartwegiimarta.csv")
write.csv(pinushartwegiipenanevada_extract, "MAPA TESIS/Pinushartwegiipenanevada.csv")
write.csv(pinushartwegiipotosi_extract, "MAPA TESIS/Pinushartwegiipotosi.csv")

#correlacion Pearson de las variables ambientales
library(raster)    
estadisticas <-raster::layerStats(Potosibiocrop, 'pearson', na.rm=T)
Pearson <-estadisticas$'pearson correlation coefficient'
#exportar a csv
write.csv(Pearson, "Pearsonbiostack.csv")
install.packages("RStoolbox")
library(RStoolbox)
PCA<-rasterPCA (Potosibiocrop, nSamples = NULL, nComp =16, spca = TRUE)
PCA_sum<-summary(PCA$model)
PCA_load<-loadings(PCA$model)
PCA_compon<-PCA$map
plot(PCA_compon, add= T)

#exportar matriz PCA loadings
write.csv(PCA_load, "PCA_loadbiostack.csv")


PCACSV <- "PCA_loadbiostack.csv"


library(readr)
PCA_loadbiostack <- read_csv("PCA_loadbiostack.csv")
View(PCA_loadbiostack)
