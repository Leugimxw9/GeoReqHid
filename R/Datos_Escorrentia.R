#' @title Escorrentía
#' @description Realiza la estimación de la esconrrentía superficial
#' @details La ecuación utilizada representa la diferencia entre la precipitación y la precipitación efectiva.
#' @param Precipitacion Raster stack de Precipitación.
#' @param Precipitacion_efectiva Raster stack de precipitación efectiva.
#' @return Devuelve un raster stack con datos de escorrentia  limitado a la zona de estudio.
#' @export

Datos_Escorrentia<-function(Precipitacion, precipitacion_efectiva){
  cat("Creando directorios...\n")
  if(dir.exists(paste0("~/_Descarga_Datos/Escorrentia/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Escorrentia/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/Escorrentia/Raster/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Escorrentia/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
  }


  Escorrentia2<-function(Prec, PE){
    Prec-PE
  }

  cat("\nCalculando la escorrentia generada...\n")

  Escor<-Escorrentia2(Precipitacion, precipitacion_efectiva)

  Meses<-(c("Enero", "Febrero", "Marzo", "Abril",
            "Mayo", "Junio", "Julio", "Agosto",
            "Septiembre", "Octubre", "Noviembre", "Diciembre"))

  names(Escor)<-Meses
  cat("\nGuardando raster de escorrentía...\n")

  col_RB<-colorRampPalette(c("Red", "Yellow", "Blue"))
  i=0
  while(i <= nlayers(Escor)){
    i<-i+1
    if(i <= nlayers(Escor)){
      cat("Datos restantes: ",(nlayers(Escor)-i), "\n")
      writeRaster(Escor[[i]], filename = paste0("~/_Descarga_Datos/Escorrentia/Raster/", Sys.Date(),"/"), paste0(i,"_", Meses[i]), suffix=Meses[i], format="GTiff", overwrite=TRUE)
      #png(filename=paste0("~/_Descarga_Datos/Escorrentia/Imagenes/", Sys.Date(),"/", i,"_",Meses[i],"_Escorrentia.png"), width = 1200, height=1200, units="px")
      #plot(Escor[[i]], col=col_RB(maxValue(Escor[[i]])), main="Escorrentia", sub=paste0(Meses[i]),
      #     cex.main=3, cex.sub=2, cex.lab=20)
      #dev.off()
    }

  }
  return(Escor)
}
