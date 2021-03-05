#' @title Delimitardor a zona de estudio
#' @description Lee un archivo vectorial de la zona de estudio, verifica que la proyección de coordendas sean geográficas (EPSG:4326).
#' @details Lee un archivo vectorial, verifica el sistema de proyección de coordenadas en EPSG:4626, en caso de no coincidir se realiza la proyección correspondiente.
#' @return Devuelve la zona de estudio cargada en R.
#' @export

Zona_estudio<-function(){



  cat("\n*** Cargando un vectorial de la zona de estudio ***\n")
  Area<-readOGR(choose.files(default="",caption="Seleccione el archivo vectorial de la zona de estudio:"))
  #Area2<-readOGR("C:/Users/leugi/Documents/Datos geoespaciales/Sinaloa/Culiacan22.shp")
  Area_proj<-crs(Area)
  WGS84_4326<-CRS("+init=epsg:4326")
  #WGS84_4326<-CRS("EPSG:4326")
  if(projection(WGS84_4326)==projection(Area_proj)){cat("Proyección correcta.\n")}else{
    cat("\nCambiando proyección a EPSG:4326.\n")
    Area<-spTransform(Area, WGS84_4326)
    crs(Area)}
  return(Area)
}

