#' @title Descarga de MOD16A2 (Evapotranspiración).
#' @description Descarga datos de evapotranspiración
#' @details La función utiliza como parámetro de entrada un vectorial para delimitar los datos a descargar dentro de un rango de fechas que corresponden al ciclo vegetativo del cultivo.
#' @param Zona Es el área de estudio cargado con la función Zona_estudio.
#' @export
Descarga_MODIS<-function(Zona){
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(), sep=" "))==FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(), sep=" "))==FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(), sep=" "),recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date(), sep=" "))==FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date(), sep=" "),recursive=TRUE)
  }


  if(dir.exists("C:/OSGeo4W64/bin/")==FALSE){
    if(dir.exists("C:/Program Files/QGIS 3.18/bin/")==FALSE){
      stop(svDialogs::winDialog("ok","Debe instalar OSGEO4W para las liberías de GDAL/OGR o QGIS."))}else{cat("GDAL/OGR instalado...")
         GDALPATH<-"C:/Program Files/QGIS 3.*/bin/"}
    }else{cat("GDAL/OGR instalado...")
         GDALPATH<-"C:/OSGeo4W64/bin/"}

  cat("Continuando procesamiento...\n")

  Ruta<-"~/_Descarga_Datos/MODIS/"
  setwd("~/_Descarga_Datos/MODIS/")


  MODIS::MODISoptions(localArcPath = Ruta,
               outDirPath = Ruta,
               gdalPath = GDALPATH,
               MODISserverOrder = c("LPDAAC", "LAADS"))

  producto <- "MOD16A2.060"
  bandas <- "001"


  # Rango de fechas
  Fecha1<-svDialogs::dlgInput("Ingrese la fecha inicial de descarga (Anio-Mes-Dia): ")$res
  Fecha1<-as.Date(Fecha1, format="%Y-%m-%d")
  Verificar<-is.na(Fecha1)
  cat(paste0("\nFecha inicial de descarga:  ", Fecha1,"\n"))
  while (Verificar==TRUE) {
    svDialogs::winDialog("ok","Error en el formato de fecha, ingrese en formato:
            Año-Mes-Día.")
    Fecha1<-svDialogs::dlgInput("Ingrese la fecha inicial de descarga (Año-Mes-Día): ")$res
    Fecha1<-as.Date(Fecha1, format="%Y-%m-%d")
    Verificar<-is.na(Fecha1)
    cat(paste0("\nFecha inicial de descarga:  ", Fecha1,"\n"))
  }

  Fecha2<-svDialogs::dlgInput("Ingrese la fecha final de descarga (Anio-Mes-Dia): ")$res
  Fecha2<-as.Date(Fecha2, format="%Y-%m-%d")
  Verificar<-is.na(Fecha2)
  cat(paste0("Fecha final de descarga:  ",Fecha2,"\n"))
  while (Verificar==TRUE) {
    svDialogs::winDialog("ok","Error en el formato de fecha, ingrese en formato:
            Año-Mes-Día.")
    Fecha2<-svDialogs::dlgInput("Ingrese la fecha inicial de descarga (Año-Mes-Día): ")$res
    Fecha2<-as.Date(Fecha1, format="%Y-%m-%d")
    Verificar<-is.na(Fecha2)
    cat(paste0("Fecha final de descarga:  ",Fecha2,"\n"))
  }

  Fecha1<-MODIS::transDate(begin = Fecha1)
  #Fecha1
  Fecha2<-MODIS::transDate(end = Fecha2)

  A<-MODIS::getTile(Zona)

  MODIS::EarthdataLogin(usr=getPass::getPass("Usuario Earthdata: "), pwd = getPass::getPass("Contraseña Earthdata: "))

  MODIS::runGdal(job=paste0(Sys.Date(), sep=""),
          product=producto,
          collection = "006",
          #product=Producto,
          extent=A,
          begin=Fecha1$beginDOY,
          end=Fecha2$endDOY,
          SDSstring = bandas,
          outProj= "+init=epsg:4326")

}
