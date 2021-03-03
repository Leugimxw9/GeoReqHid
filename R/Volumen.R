#' @title Volumen
#' @description Realiza la estimación del volumen
#' @details Estima el volumen en un área determinada.
#' @param mm Raster con valores en mm de evapotranspiración.
#' @param Area Vectorial de la zona de estudio.
#' @return Devuelve un raster stack con datos de requerimiento de riego limitado a la zona de estudio.
#' @export
Volumen<-function(mm, Area){
  if(dir.exists(paste0("~/_Descarga_Datos/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  Nombre<-names(mm)
  AreaTerreno<-area(Area)
  Volumen<-(mm/1000000)*AreaTerreno
  Volumen<-as.data.frame(Volumen)
  colnames(Volumen)<-Nombre
  #rownames(Volumen)<-"ET (M^3)"
  write_xlsx(Volumen, paste0("~/_Descarga_Datos/Reporte_Volumen m3.xlsx"))
  write.csv(Volumen, file = paste0("~/_Descarga_Datos/Reporte_Volumen m3.csv"), row.names = TRUE, col.names = TRUE)


}
#' @title Volumen
#' @description Realiza la estimación del volumen
#' @details Estima el volumen en un área determinada.
#' @param mm Raster con valores en mm de evapotranspiración.
#' @param Area Vectorial de la zona de estudio.
#' @return Devuelve un raster stack con datos de requerimiento de riego limitado a la zona de estudio.
#' @export
Volumen<-function(mm, Area){
  if(dir.exists(paste0("~/_Descarga_Datos/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  Nombre<-names(mm)
  AreaTerreno<-area(Area)
  Volumen<-(mm/1000000)*AreaTerreno
  Volumen<-as.data.frame(Volumen)
  colnames(Volumen)<-Nombre
  #rownames(Volumen)<-"ET (M^3)"
  write_xlsx(Volumen, paste0("~/_Descarga_Datos/Reporte_Volumen m3.xlsx"))
  write.csv(Volumen, file = paste0("~/_Descarga_Datos/Reporte_Volumen m3.csv"), row.names = TRUE, col.names = TRUE)


}
