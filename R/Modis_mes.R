#' @title Composición mensual
#' @description Suma los datos acorde al mes para obtener Evapotranspiración mensual
#' @param Modis_datos Raster stack de Evapotranspiración.
#' @param Area Vectorial de la zona de estudio.
#' @return Devuelve un raster stack con datos de requerimiento de riego limitado a la zona de estudio.
#' @export

Modis_mes<-function(Area, Modis_datos){
  cat("\n*** Composición mensual de datos modis ***\n")
  #setwd(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(),"/"))
  cat("\nCargando datos...\n")
  #Modis_datos<- list.files(pattern = "tif")
  #Modis_datos<-stack(Modis_datos)
  Nombre<-names(Modis_datos)
  #Nombre[1]
  F_inicial<-substr(Nombre[1],start=10, stop=16)
  F_inicial
  F_inicial<-transDate(F_inicial)
  F_inicial<-as.Date(F_inicial$begin, formar="%Y-%m-%d")
  F_2<-substr(Nombre[2],start=10, stop=16)
  F_2<-transDate(F_2)
  F_2<-as.Date(F_2$begin, formar="%Y-%m-%d")
  F_final<-substr(Nombre[nlayers(Modis_datos)],start=10, stop=16)
  F_final<-transDate(F_final)
  F_final<-as.Date(F_final$begin, formar="%Y-%m-%d")
  F_dif<-F_2-F_inicial
  #F_dif
  #day(F_inicial)<-1
  #day(F_final)<-1
  Rangotemporal<-seq.Date(as.Date(F_inicial), as.Date(F_final), F_dif)
  #Rangotemporal
  if(nlayers(Modis_datos)!=length(Rangotemporal)){
    for (i in 1:length(Rangotemporal)) {
      if (i!=1) {
        if(year(Rangotemporal[i-1]) != year(Rangotemporal[i])){
          while (i<=length(Rangotemporal)) {
            day(Rangotemporal[i])<-day(Rangotemporal[i])-3
            i=i+1
          }

        }}
    }
    RT<-Rangotemporal[length(Rangotemporal)]
    day(RT)<-day(Rangotemporal[length(Rangotemporal)])+as.numeric(F_dif)
    Rangotemporal<-c(Rangotemporal,RT)
  }
  #Rangotemporal
  names(Modis_datos)<-Rangotemporal
  day(F_inicial)<-1
  Rangomensual<-seq(as.Date(F_inicial),as.Date(F_final), "1 month")
  #Rangomensual
  indices <- format(as.Date(names(Modis_datos), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
  ET_mes<- stackApply(Modis_datos, indices, fun = sum)
  col_RB<-colorRampPalette(c("Blue", "Yellow", "Red"))
  NL<-(nlayers(ET_mes))
  ET_mes[ET_mes < 0 | ET_mes == 0]<-NA

  Area_extension<-extent(bbox(Area))
  ET_mes@extent<-extent(Area_extension)

  indice<- format(as.Date(names(ET_mes), format = "X%Y.%m.%d"), format="%B/%Y")
  names(ET_mes)<-indice
  i=0
  while(i<=NL){
    i<-i+1
    if(i<=NL){
      cat("Datos restantes: ",(NL-i), "\n")
      writeRaster(ET_mes[[i]],
                  filename= paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date(),"/", paste0(Rangomensual[i])),
                  format="GTiff", overwrite=TRUE)
      #}
      #png(filename=paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(),"/", Rangomensual[i],".png"), width = 1200, height=1200, units="px")
      #plot(ET_mes[[i]], col=col_RB(maxValue(ET_mes[[i]])), main="Evapotranspiración mensual", sub=paste0(Rangomensual[i]),
      #     cex.main=3, cex.sub=2, cex.lab=4)
      #dev.off()
    }
  }

  winDialog("ok","Procesamiento de datos MODIS terminado.")
  names(ET_mes)<-Rangomensual
  return(ET_mes)

}

