#' @title  Delimitar a la zona de estudio.
#' @description Carga un vectorial y devuelve un vectorial para ser usado en otras funciones.
#' @details Esta función es requerida para ser usada en otras funciones, debido a que limita a la zona de estudio los datos geoespaciales.
#' @keywords Vectorial Area
#' @return Devuelve la zona vectorial.
#' @concept **Zona estudio** se refiere al área geográfica de donde se realizará el proceso geoespacial.
#' @export
Zona_estudio<-function(){
  cat("\n*** Cargando un vectorial de la zona de estudio ***\n")
  Area<-rgdal::readOGR(utils::choose.files(default="",caption="Seleccione el archivo vectorial de la zona de estudio:"))
  #Area2<-readOGR("C:/Users/leugi/Documents/Datos geoespaciales/Sinaloa/Culiacan22.shp")
  Area_proj<-raster::crs(Area)
  WGS84_4326<-raster::crs("+init=epsg:4326")
  #WGS84_4326<-CRS("EPSG:4326")
  if(raster::projection(WGS84_4326)==raster::projection(Area_proj)){cat("Proyección correcta.\n")}else{
    cat("\nCambiando proyección a EPSG:4326.\n")
    Area<-sp::spTransform(Area, WGS84_4326)
    raster::crs(Area)}
  return(Area)
}

