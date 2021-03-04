# GeoReqHid

## Introducción

El requerimiento de riego es un indicador que permite estimar la lámina de agua requerida por un cultivo. Para el cálculo del requerimiento de riego, se requieren dos parámetros: la evapotranspiración y la Precipitación efectiva.

La evapotranspiración se define como la pérdida de humedad del suelo a traves de dos procesos (Evaporación y Transpiración). La evaporación es la pérdida de humedad del suelo directamente hacia la atmósfera y la transpiración representa la pérdida de humedad del suelo ingresando por la planta y siendo liberado en forma de vapor hacia la atmósfera. 

La precipitación efectiva representa la cantidad de agua que quedará en el suelo despues de una precipitación.  

## Funciones

### Global_RR
- Función que realiza la operación completa generando toda la información solicitada.

### Zona_estudio
 - La función carga un vectorial para definir la zona de estudio. 
 - Cambia la proyección a EPSG: 4326.
 
### MODIS
- Solicita un rango de fechas para descargar el producto MOD16A2 (Evapotranspiración).
- Descarga el producto MOD16A2 recordado al área de estudio.
- Aplica el factor de conversión transformando los valores radiometricos a valores de evapotranspiración en mm/8 días.
- Transforma los valores de relleno a NA. 
- Guarda los archivos raster procesado.

### Modis_mes
- Suma los datos de evapotranspiración a mm/mes.
- Guarda los archivos raster procesado.

### Datos_Precipitacion
- Descarga datos de precipitación de climate worldclim.org,
- Delimta los datos a la zona de estudio.
- Guarda los archivos raster procesado.

### Datos_Precipitacion_efectiva
- Estima la precipitación efectiva delimitado a la zona de estudio.
- Guarda los archivos raster procesado.

### Datos_Escorrentia
- Estima la escorrentía basado en la precipitación y precipitación efectiva.
- Guarda los archivos raster procesado.

### Requerimiento
- Solicita la forma de procesamiento para considerar los valores de KC: cargar un archivo excel con valores mensuales de KC, proporcionar un valor alto de KC, realizar las operaciones sin el valor de KC.
- Genera un reporte en excel con los datos mensuales en mm.
- Guarda los archivos raster procesado.

### Volumen
- Proporciona los valores de mm a un área determinada convertiendose en m^3. 

## Instalación:

devtools::install_github("Leugimxw9/GeoReqHid")

### Consola

- Si se desea realizar todo el proceso completo:

 Global_RR()
 
- Si se desea realizar por pasos se recomienda:

  Zona<-Zona_estudio()
  
  Modis<-MODIS(Zona)
  
  ET<-Modis_mes(Zona, Modis)
  
  Prec<-Datos_Precipitacion(Zona)
  
  Prec_Efec<-Datos_Precipitacion_efectiva(Prec,Zona)
  
  Escor<-Datos_Escorrentia(Prec,Prec_Efec,Zona)
  
  RR<-Requerimiento(ET, Prec_Efec, Zona)
  
  Vol_ET<-Volumen(ET, Zona)
