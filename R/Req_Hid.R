#' @title Estimación de requerimiento de riego
#' @description Realiza la descarga los datos y el procesamiento de las imágenes para obtener el requerimiento hídrico de un cultivo basado en el rango temporal del ciclo vegetativo.
#' @export
Req_Hid<-function(){
  Zona<-Zona_estudio()
  Descarga_MODIS(Zona)
  Lectura_MODIS(Zona)
  ET_mes(Zona)
  Precipitacion(Zona)
  Precipitacion_efectiva()
  Escorrentia()
  Requerimiento(Zona)
}
