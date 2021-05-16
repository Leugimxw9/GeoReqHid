#' @title Composición mensual de Evapotranspiración
#' @description Crea composiciones mensuales de los datos geoespaciales.
#' @details Carga los datos de evapotranspiración generados con la función de ET_datos. Crea composiciones raster de 1 mes acorde al rango de fecha de los productos procesados.
#' @param Zona Vectorial cargado de la función Zona_estudio().
#' @return Devuelve un raster stack de evapotranspiración mensual.
#' @export
Modis_mes<-function(Zona){
  cat("\n*** Composición mensual de datos modis ***\n")
  cat("\nCargando datos...\n")
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/",Sys.Date()))==FALSE){
    stop(utils::winDialog("ok",paste0("No existe el directorio: ~/_Descarga_Datos/MODIS/",Sys.Date())))
  }
  setwd(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(),"/"))
  Modis_datos<- list.files(pattern = ".tif")
  Modis_datos<-raster::stack(Modis_datos)
  Nombre<-names(Modis_datos)
  F_inicial<-substr(Nombre[1],start=10, stop=16)
  F_inicial<-MODIS::transDate(F_inicial)
  F_inicial<-as.Date(F_inicial$begin, formar="%Y-%m-%d")
  F_2<-substr(Nombre[2],start=10, stop=16)
  F_2<-MODIS::transDate(F_2)
  F_2<-as.Date(F_2$begin, formar="%Y-%m-%d")
  F_final<-substr(Nombre[raster::nlayers(Modis_datos)],start=10, stop=16)
  F_final<-MODIS::transDate(F_final)
  F_final<-as.Date(F_final$begin, formar="%Y-%m-%d")
  F_dif<-F_2-F_inicial
  #F_dif
  #day(F_inicial)<-1
  #day(F_final)<-1
  Rangotemporal<-seq.Date(as.Date(F_inicial), as.Date(F_final), F_dif)
  #Rangotemporal
  if(raster::nlayers(Modis_datos)!=length(Rangotemporal)){
    for (i in 1:length(Rangotemporal)) {
      if (i!=1) {
        if(lubridate::year(Rangotemporal[i-1]) != lubridate::year(Rangotemporal[i])){
          while (i<=length(Rangotemporal)) {
            lubridate::day(Rangotemporal[i])<-lubridate::day(Rangotemporal[i])-3
            i=i+1
          }

        }}
    }
    RT<-Rangotemporal[length(Rangotemporal)]
    lubridate::day(RT)<-lubridate::day(Rangotemporal[length(Rangotemporal)])+as.numeric(F_dif)
    Rangotemporal<-c(Rangotemporal,RT)
  }
  names(Modis_datos)<-Rangotemporal
  lubridate::day(F_inicial)<-1
  Rangomensual<-seq(as.Date(F_inicial),as.Date(F_final), "1 month")
  #Rangomensual
  indices <- format(as.Date(names(Modis_datos), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
  ET_mes<- raster::stackApply(Modis_datos, indices, fun = sum)
  col_RB<-grDevices::colorRampPalette(c("Blue", "Yellow", "Red"))
  NL<-(raster::nlayers(ET_mes))
  ET_mes[ET_mes < 0 | ET_mes == 0]<-NA

  Area_extension<-raster::extent(sp::bbox(Zona))
  ET_mes@extent<-raster::extent(Area_extension)

  indice<- format(as.Date(names(ET_mes), format = "X%Y.%m.%d"), format="%B/%Y")
  names(ET_mes)<-indice
  i=0
  while(i<=NL){
    i<-i+1
    if(i<=NL){
      cat("Datos restantes: ",(NL-i), "\n")
      raster::writeRaster(ET_mes[[i]],
                  filename= paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date(),"/", paste0(Rangomensual[i])),
                  format="GTiff", overwrite=TRUE)
      #}
      grDevices::png(filename=paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(),"/", Rangomensual[i],".png"), width = 1200, height=1200, units="px")
      raster::plot(ET_mes[[i]], col=col_RB(raster::maxValue(ET_mes[[i]])), main="Evapotranspiración mensual", sub=paste0(Rangomensual[i]),
           cex.main=3, cex.sub=2, cex.lab=4)
      grDevices::dev.off()
    }
  }

  #utils::winDialog("ok","Procesamiento de datos MODIS terminado.")
  names(ET_mes)<-Rangomensual
  return(ET_mes)

}
