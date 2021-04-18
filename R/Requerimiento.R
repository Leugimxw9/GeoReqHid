#' @title Requerimiento de riego
#' @description Estima el requerimiento de riego con los datos de evapotranspiración de MODIS y Precipitación de worldclim.org.
#' @details Descarga datos geoespaciales de precipitación del portal worldclim.org, posteriormente es procesado a la zona de estudio.
#' @param Zona Es el archivo vectorial cargado anteriormente con la función ZOna_estudio.
#' @return Devuelve un raster stack de requerimiento de riego.
#' @export
Requerimiento<-function(Zona){
  cat("\nCalculando el requerimiento de riego...\n")

  if(dir.exists(paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/Requerimiento/Raster/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Requerimiento/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
  }

  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/",Sys.Date()))==FALSE){
    stop(svDialogs::winDialog("ok",paste0("No existe el directorio: ~/_Descarga_Datos/MODIS/",Sys.Date(),"\nDatos de evapotranspiración inexistentes.")))
  }

  if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date()))==FALSE){
    stop(utils::winDialog("ok",paste0("No existe el directorio: ~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(),"\nDatos de precipitación efectiva inexistentes.")))
  }


  ET<- list.files(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date()), pattern = ".tif")
  ET<- raster::stack(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date(),"/",ET))#ET
  PE<-list.files(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date()),pattern = "tif")
  PE<-raster::stack(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(),"/",PE))

  #PE<-Prec_Efec
  #PE<-Prec_datos
  Area<-Zona
  if (raster::res(ET)!=raster::res(PE)) {
    PE<-raster::resample(PE,ET, method="bilinear")
  }
  Meses_ET<-names(ET)
  tempo<-Sys.Date()
  #tempo
  lubridate::month(tempo)<-lubridate::month(1)
  #tempo
  lubridate::day(tempo)<-1

  indice<- format(as.Date(names(ET), format = "X%Y.%m.%d"), format="%B %Y")
  indices <- format(as.Date(names(ET), format = "X%Y.%m.%d"), format = "%m")
  #indices
  indices<- as.numeric(indices)
  names(ET)<-indices
  #ET
  Meses<-seq(as.Date(tempo), by = "month", length.out = 12)
  indice2<-format(as.Date(Meses, format = "X%Y.%m.%d"), format = "%m")
  indice2<-as.numeric(indice2)
  #indice2
  names(PE)<-indice2
  #PE
  RespG<-utils::winDialog("yesnocancel","¿Dispone de datos de coeficiente de cultivo?")

  #ET<-projectRaster(ET, crs= crs("+init=epsg:4326"))
  #PE<-projectRaster(PE, crs= crs("+init=epsg:4326"))

  PE1<-ET
  RR<-ET
  ETc<-ET
  Area_terreno<-raster::area(Area)
  if(RespG=="YES"){
    KC=NULL
    while (is.data.frame(KC)==FALSE) {
      try(KC<- readxl::read_excel(file.choose()), silent=TRUE)
    }
    KC<-as.list(KC[,2])
    KC<-as.numeric(unlist(KC))
    names(KC)<-indices
    for (i in 1:raster::nlayers(ET)) {
      for (j in 1:raster::nlayers(PE)) {
        RT<-names(ET[[i]])==names(PE[[j]])
        if(RT==TRUE)
        {
          cat("Dato restante: ", paste0(raster::nlayers(ET)-i),"\n")
          #print(names(PE[[j]]))
          #print(paste0(KC[i]))
          ETc[[i]]<-ET[[i]]*KC[i]
          RR[[i]]<- ETc[[i]]-PE[[j]]
          PE1[[i]]<-PE[[j]]
        }
      }
    }
  }

  #RR
  ET
  if (RespG=="NO"){
    RespT<-utils::winDialog("yesno","¿Desea ingresar un valor de coeficiente de cultivo máximo?")
    if(RespT=="YES"){
      KC=NULL
      KC<-svDialogs::dlgInput("¿Desea ingrese un valor de coeficiente de cultivo máximo? : ")$res
      for (i in 1:raster::nlayers(ET)) {
        for (j in 1:raster::nlayers(PE)) {
          RT<-names(ET[[i]])==names(PE[[j]])
          if(RT==TRUE)
          {
            cat("Dato restante: ", paste0(raster::nlayers(ET)-i),"\n")
            #print(names(PE[[j]]))
            ETc[[i]]<-ET[[i]]*KC
            RR[[i]]<-ETc[[i]]-PE[[j]]
            PE1[[i]]<-PE[[j]]
          }
        }
      }
    }else{
      for (i in 1:raster::nlayers(ET)) {
        for (j in 1:raster::nlayers(PE)) {
          RT<-names(ET[[i]])==names(PE[[j]])
          if(RT==TRUE)
          {
            cat("Dato restante: ", paste0(raster::nlayers(ET)-i),"\n")
            #print(names(PE[[j]]))
            RR[[i]]<-ET[[i]]-PE[[j]]
            PE1[[i]]<-PE[[j]]
          }
        }
      }
    }
  }

  if (RespG=="CANCEL"){
    for (i in 1:raster::nlayers(ET)) {
      for (j in 1:raster::nlayers(PE)) {
        RT<-names(ET[[i]])==names(PE[[j]])
        #cat("\n",paste0(RT))
        if(RT==TRUE)
        {
          cat("Dato restante: ", paste0(raster::nlayers(ET)-i),"\n")
          RR[[i]]<-ET[[i]]-PE[[j]]
          PE1[[i]]<-PE[[j]]
        }
      }
    }
  }
  PE[PE1<0]<-0
  names(RR)<-indice
  RR[RR < 0]<-0
  names(PE1)<-indice
  names(ET)<-indice
  names(ETc)<-indice
  #RR<-(RR/1000000)
  #RR
  #PE1<-(PE1/1000000)
  #ET<-(ET/1000000)
  #ETc<-(ETc/1000000)
  RR2<-(RR/1000000)*Area_terreno
  R_RR<-as.data.frame(raster::cellStats(RR, stat="mean", na.rm=TRUE))
  colnames(R_RR)<-"Requerimiento de riego (mm)"
  R_RR2<-as.data.frame(raster::cellStats(RR2, stat="mean", na.rm=TRUE))
  colnames(R_RR2)<-"Requerimiento de riego (m^3)"
  R_ET<-data.frame(raster::cellStats(ET, stat="mean", na.rm=TRUE))
  colnames(R_ET)<-"Evapotranspiracion (mm)"
  R_ETc<-data.frame(raster::cellStats(ETc, stat="mean", na.rm=TRUE))
  colnames(R_ETc)<-"Evapotranspiracion referencia (mm)"
  R_PE<-data.frame(raster::cellStats(PE1, stat="mean", na.rm=TRUE))
  colnames(R_PE)<-"Precipitacion efectiva (mm)"
  indice<-as.data.frame(indice)
  colnames(indice)<-"Mes"
  Reporte<-as.data.frame(c(indice, R_ET, R_ETc, R_PE, R_RR, R_RR2))
  #max(Reporte$Evapotranspiracion.referencia)
  cat("\nGuardando gráfico de balance...\n")
  grDevices::png("~/_Descarga_Datos/Balance.png", width = 2500, height = 2000, res = 250)
  plot(Reporte$Evapotranspiracion.referencia..mm., ylim=c(0, max(Reporte$Evapotranspiracion.referencia..mm.)), type="b", lwd=2,axes=FALSE,
       col="red", xlab="Meses", ylab="mm", main="Requerimiento de riego")
  graphics::lines(Reporte$Precipitacion.efectiva..mm., type="b", lwd=2,col="blue")
  graphics::lines(Reporte$Requerimiento.de.riego..mm., type="b", lwd=2, col="green")
  graphics::text(Reporte$Evapotranspiracion.referencia..mm., labels=round(Reporte$Evapotranspiracion.referencia..mm.,1), cex=0.75, pos=1, offset = 0.75)
  graphics::text(Reporte$Precipitacion.efectiva..mm., labels=round(Reporte$Precipitacion.efectiva..mm.,1), cex=0.75, pos=1, offset = 0.75)
  graphics::text(Reporte$Requerimiento.de.riego..mm., labels=round(Reporte$Requerimiento.de.riego..mm.,1), cex=0.75, pos=1, offset = 0.75)
  graphics::legend("bottomleft", col=c("red", "blue", "green"),
         legend=c("Evapotranspiración de referencia", "Precipitación Efectiva", "Requerimiento de Riego"),
         lwd=1, bty="n", inset=c(0,1), xpd=TRUE, horiz=TRUE)
  graphics::box()
  graphics::axis(1, las=1, at=1:length(Reporte$Mes),lab=Reporte$Mes)
  graphics::axis(2, las=1, at=0:round(max(Reporte$Evapotranspiracion.referencia..mm.)))
  grDevices::dev.off()
  cat("\nGuardando datos en excel...\n")
  writexl::write_xlsx(Reporte, "~/_Descarga_Datos/Reporte.xlsx")
  ########################

  cat("\nGuardando raster de Requerimiento de riego...\n")
  col_RB<-grDevices::colorRampPalette(c("#FFFFCC", "#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494"))
  col_RB(raster::maxValue(RR[[i]]))
  i=0
  while(i <= raster::nlayers(RR)){
    i<-i+1
    if(i <= raster::nlayers(RR)){
      cat("Datos restantes: ",raster::nlayers(RR)-i, "\n")
      raster::writeRaster(RR[[i]], filename = paste0("~/_Descarga_Datos/Requerimiento/Raster/", Sys.Date(),"/",i,"_", names(RR[[i]])), suffix=indice[i,], format="GTiff", overwrite=TRUE)
      grDevices::png(filename=paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(),"/",i, indice[i,],"_Requerimiento.png"), width = 1200, height=1200, units="px")
      raster::plot(RR[[i]], col=col_RB(raster::maxValue(RR[[i]])), main="Requerimiento de riego", sub=paste0(indice[i,]),cex.main=3, cex.sub=2, cex.lab=20)
      grDevices::dev.off()
    }
  }

  return(RR)
}
