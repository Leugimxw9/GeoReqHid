#' @title Precipitación Efectiva
#' @description Realiza la estimación de precipitación efectiva
#' @details La ecuación utilizada es obtenida del software Cropwat, compara el valor de precipitación y aplica la ecuación correspondiente.
#' Si P<251... PE= P(PE x 125-0.2 x P)/125
#' Si => 251... PE= 125+0.1 x P
#' @param Precipitacion Raster stack de Precipitación.
#' @return Devuelve un raster stack con datos de precipitación  limitado a la zona de estudio.
#' @export

Datos_Precipitacion_efectiva<-function(Precipitacion){
  cat("\n*** Cargando un vectorial de la zona de estudio ***\n")
  if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
  }


  Meses<-(c("Enero", "Febrero", "Marzo", "Abril",
            "Mayo", "Junio", "Julio", "Agosto",
            "Septiembre", "Octubre", "Noviembre", "Diciembre"))


  precipitacion_efectiva<-function(P){
    ifelse(P<251, (P*(125-0.2*P))/125, 125+0.1*P)
  }


  cat("\nCalculando la precipitación efectiva...\n")
  Pre_efec<-calc(Precipitacion, fun=precipitacion_efectiva)
  names(Pre_efec)<-Meses
  col_RB<-colorRampPalette(c("Red", "Yellow", "Blue"))
  i=0
  while(i <= nlayers(Pre_efec)){
    i<-i+1
    if(i <= nlayers(Pre_efec)){
      cat("Datos restantes: ",(nlayers(Pre_efec)-i), "\n")
      writeRaster(Pre_efec[[i]], filename = file.path(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date()),paste0(i,"_", Meses[i])), suffix=Meses[i], format="GTiff", overwrite=TRUE)
      #png(filename=paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/", Sys.Date(),"/",i,"_",Meses[i],"_Precipitacion Efectiva.png"), width = 1200, height=1200, units="px")
      #plot(Pre_efec[[i]], col=col_RB(maxValue(Pre_efec[[i]])), main="Precipitación Efectiva", sub=paste0(Meses[i]),
      #     cex.main=3, cex.sub=2, cex.lab=20)
      #dev.off()
    }
  }


  return(Pre_efec)
}
