Escorrentia<-function(){
  cat("Creando directorios...\n")
  if(dir.exists(paste0("~/_Descarga_Datos/Escorrentia/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Escorrentia/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/Escorrentia/Raster/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Escorrentia/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
  }

  Precipitacion<- list.files(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(),"/raster/"),pattern = "tif")
  Precipitacion<-raster::stack(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(),"/raster/", Precipitacion))
  Prec_efectiva<-list.files(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date()),pattern = "tif")
  Prec_efectiva<-raster::stack(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(),"/",Prec_efectiva))

  Escorrentia2<-function(Prec, PE){
    Prec-PE
  }

  cat("\nCalculando la escorrentia generada...\n")

  Escor<-Escorrentia2(Precipitacion, Prec_efectiva)

  Meses<-(c("Enero", "Febrero", "Marzo", "Abril",
            "Mayo", "Junio", "Julio", "Agosto",
            "Septiembre", "Octubre", "Noviembre", "Diciembre"))

  names(Escor)<-Meses
  cat("\nGuardando raster de escorrentía...\n")

  col_RB<-grDevices::colorRampPalette(c("Red", "Yellow", "Blue"))
  i=0
  while(i <= raster::nlayers(Escor)){
    i<-i+1
    if(i <= raster::nlayers(Escor)){
      cat("Datos restantes: ",(raster::nlayers(Escor)-i), "\n")
      raster::writeRaster(Escor[[i]], filename = paste0("~/_Descarga_Datos/Escorrentia/Raster/", Sys.Date(),"/"), paste0(i,"_", Meses[i]), suffix=Meses[i], format="GTiff", overwrite=TRUE)
      grDevices::png(filename=paste0("~/_Descarga_Datos/Escorrentia/Imagenes/", Sys.Date(),"/", i,"_",Meses[i],"_Escorrentia.png"), width = 1200, height=1200, units="px")
      raster::plot(Escor[[i]], col=col_RB(if(maxValue(Escor[[i]])<0){1}else{maxValue(Escor[[i]])}), main="Escorrentia", sub=paste0(Meses[i]),
           cex.main=3, cex.sub=2, cex.lab=20)
      grDevices::dev.off()
    }

  }
  return(Escor)


}
