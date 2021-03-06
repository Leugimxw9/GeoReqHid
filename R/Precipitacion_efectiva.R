#' @title Precipitación efectiva
#' @description Estima la precipitación efectiva.
#' @details Lee los archivos de precipitación creados anteriormente con la función Precipitacion.
#' @return Devuelve un raster stack de datos de precipitación efectiva.
#' @export
Precipitacion_efectiva<-function(){
  #cat("\n*** Cargando un vectorial de la zona de estudio ***\n")
  if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
  }

  if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(),"/raster/"))==FALSE){
    stop(utils::winDialog("ok",paste0("No existe el directorio: ~/_Descarga_Datos/MODIS/",Sys.Date())))
  }

  setwd(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(),"/raster/"))

  Meses<-(c("Enero", "Febrero", "Marzo", "Abril",
            "Mayo", "Junio", "Julio", "Agosto",
           "Septiembre", "Octubre", "Noviebre", "Diciembre"))

  Precipitacion<-list.files(pattern = "*.tif")
  Precipitacion<-raster::stack(Precipitacion)
  #Precipitacion
  precipitacion_efectiva<-function(P){
    ifelse(P<251, (P*(125-0.2*P))/125, 125+0.1*P)
  }

  cat("\nCalculando la precipitación efectiva...\n")
  Pre_efec<-raster::calc(Precipitacion, fun=precipitacion_efectiva)
  Pre_efec[Pre_efec < 0]<-0

  names(Pre_efec)

  col_RB<-grDevices::colorRampPalette(c("Red", "Yellow", "Blue"))
  i=0
  while(i <= raster::nlayers(Pre_efec)){
    i<-i+1
    if(i <= raster::nlayers(Pre_efec)){
      cat("Datos restantes: ",(raster::nlayers(Pre_efec)-i), "\n")
      raster::writeRaster(Pre_efec[[i]], filename = file.path(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date()),paste0("PE_",names(Pre_efec[[i]]))), suffix=Meses[i], format="GTiff", overwrite=TRUE)
      grDevices::png(filename=paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/", Sys.Date(),"/",i,"_",Meses[i],"_Precipitacion Efectiva.png"), width = 1200, height=1200, units="px")
      raster::plot(Pre_efec[[i]], col=col_RB(raster::maxValue(Pre_efec[[i]])), main="Precipitación Efectiva", sub=paste0(Meses[i]),
           cex.main=3, cex.sub=2, cex.lab=20)
      grDevices::dev.off()
    }
  }


  return(Pre_efec)
}

