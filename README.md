# Correlaci-n-diaria-de-SSM-promedios-en-base-al-NDVI-y-NDVI
Correlación entre la humedad superficial (SSM) y el índice de vegetación normalizado (NDVI). Para formar el cubo de información se creo un data frace y un csv, que contiene el pixel, las coordenadas y el valor de SSM, NDVI, y los 4 grupos topográficos 

### Cargar librería raster 
> `library(raster)
> library(maps)
> library(dichromat)
> library(spatialEco)
> library(mapview)
> library(rasterVis)
> library(RColorBrewer)`
### Kmeans 
> `library("raster")  
> library("cluster")
> library("randomForest")`
### Regresión lineal
> `library(moonBook)
> library(ggiraphExtra)
> library(devtools)`

# VARIABLE TOPOGRAFICA

##### KmeasnTopo muestra los 3 grupos topograficos
> kmeansTopo <- raster("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Georfometria R_SAGA GIS/4 ATRIBUTOS TOPOGRAFICOS/kmeans_3gruposTopograficos_bueno.tif")
>
>limn <- getData('GADM', country='ESP', level=2)
> Km_Topografic <- mask(kmeansTopo,limn)
> plot(Km_Topografic)
> ![image](https://user-images.githubusercontent.com/78845785/117947678-bad06400-b310-11eb-90f2-11cb98b52ad9.png)


# Dias del MES DE ENERO

### Ubicación del directorio de trabajo NDVI
> dia1ndvienero <- raster("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Vegetación/NDVI_1km_v2_2020/1_Enero_NDVI_1km_v2_2020/3 NDVI_ESPAÑA/Dia1_enero2020ndvi.tif")
> 
![image](https://user-images.githubusercontent.com/78845785/117945686-d9cdf680-b30e-11eb-8fca-beb69f01c9c6.png)

> dia11ndvienero <- raster("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Vegetación/NDVI_1km_v2_2020/1_Enero_NDVI_1km_v2_2020/3 NDVI_ESPAÑA/Dia11_enero2020ndvi.tif")
> 
> dia21ndvienero <- raster("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Vegetación/NDVI_1km_v2_2020/1_Enero_NDVI_1km_v2_2020/3 NDVI_ESPAÑA/Dia21_enero2020ndvi.tif")

### DATOS de diarios (3 dias) SSM diaspromedio ENERO

> dia1SSMpromerdioenero <- raster ("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Humedad/SSM_Bir  2020/1_Enero_SSM_Bir_2020/Imagenes correctas/DiaSSM27dic2019_1ene2020na_true_6dias.tif")
> ![image](https://user-images.githubusercontent.com/78845785/117945982-287b9080-b30f-11eb-8a58-0c5e7ff8379c.png)


> dia11SSMpromedioenero <- raster ("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Humedad/SSM_Bir  2020/1_Enero_SSM_Bir_2020/Imagenes correctas/DíaSSM_6_11_enero_6dias.tif")
> 
> dia21SSMpromedioenero <- raster ("C:/Users/Usuario/Documents/Análisis de Tesis en Rstudio y SAGA GIS/Variables/Humedad/SSM_Bir  2020/1_Enero_SSM_Bir_2020/Imagenes correctas/DíaSSM_16_21_enero_6dias.tif")

# METODO ESTADISTICO DE CORRELACIÓN

###### Primero se Ajustan los datos para el mismo mes (cambio de tamaño de pixel a uno mismo), es la misma capa Ajuste_NDVIenero, para que quede en el mismo tamaño ssm'ngb' este metodo se refiere que ajusta a los vecinos cercanos 

> Ajuste_NDVI_1Enero <- resample(dia1ndvienero, dia1SSMpromerdioenero, method='ngb')
> 
> plot(Ajuste_NDVI_1Enero)


###### Raster correlation (s es el tamaño de la ventana móvil para la correlación, x=SSM; y=NDVI)
correlaciónSSMNDVI1enero <-rasterCorrelation(dia1SSMpromerdioenero, Ajuste_NDVI_1Enero,  s = 9, type = "pearson")
###### Eliminar los infinitos
correlaciónSSMNDVI1enero[!is.finite(correlaciónSSMNDVI1enero)] <- NA
#
> plot(correlaciónSSMNDVI1enero)
![image](https://user-images.githubusercontent.com/78845785/117946310-798b8480-b30f-11eb-90c7-4a10985cab80.png)

> df <- na.omit(as.data.frame(stack(dia1SSMpromerdioenero, Ajuste_NDVI_1Enero)))
![image](https://user-images.githubusercontent.com/78845785/117946473-a049bb00-b30f-11eb-8810-e700faf40336.png)
###### Hay un total de 664805 pixels 

> writeRaster(correlaciónSSMNDVI1enero, file='correlacionenero1SSMNDVIs9.tif')


# Union  de 3 Variables para formar el cubo de información y luego hacer matriz de correlaciones 
###### Unificación 3 variables hace falta incluir las coberturas vegetales.
##### NDVI Re- Proyectada
Ajust_NDVI_1Enero <- mask(Ajuste_NDVI_1Enero,limn)
NDVI <- projectRaster(Ajust_NDVI_1Enero, Km_Topografic)

###### Humedad re- proyectada
dia1SSMprom_enero <- mask(dia1SSMpromerdioenero,limn)
ssm <- projectRaster(dia1SSMprom_enero,Km_Topografic)

Unión3variable1enero <- stack(ssm, NDVI, Km_Topografic)
plot(Unión3variable1enero)

![image](https://user-images.githubusercontent.com/78845785/117946860-f6b6f980-b30f-11eb-8322-1fc77d04861b.png)


df <- na.omit(as.data.frame(Unión3variable1enero, xy=TRUE))


![image](https://user-images.githubusercontent.com/78845785/117947573-9c6a6880-b310-11eb-8b09-25c13da2ecb8.png)

#Tablas para el cubo de información, Y el tiempo y como ha cambiado y entre grupos.
write.csv(df, file='tableUnionssm_ndvi_topo_primerdiaEnero.csv')
