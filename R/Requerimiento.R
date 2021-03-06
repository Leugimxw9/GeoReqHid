#' @title Requerimiento de riego
#' @description Realiza la estimación del requerimiento de riego
#' @details Realiza una homogenización de la resolución espacial entre los datos de entrada.
#' Solicita un archivo en Excel con valores de KC, en caso de no disponerlo puede ingresar un valor máximo, o dejarlo solo con la evapotranspiración de referencia para el cálculo.
#' Genera un reporte y gráfico de Evapontraspiración de referencia, Precipitación efectiva y requerimiento de riego.
#' @param ET Raster stack de Evapotranspiración.
#' @param PE Raster stack de precipitación efectiva.
#' @param Area Vectorial de la zona de estudio.
#' @return Devuelve un raster stack con datos de requerimiento de riego limitado a la zona de estudio.
#' @export

Requerimiento<-function(ET,PE,Area){
  cat("\nCalculando el requerimiento de riego...\n")

  if(dir.exists(paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/Requerimiento/Raster/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Requerimiento/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
  }

  #ET
  #PE<-Prec_Efec
  #Area<-Zona
  #PE<-Prec_datos
  if (res(ET)!=res(PE)) {
    PE<-resample(PE,ET, method="bilinear")
  }
  Meses_ET<-names(ET)
  tempo<-Sys.Date()
  #tempo
  month(tempo)<-month(1)
  #tempo
  day(tempo)<-1

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
  RespG<-winDialog("yesnocancel","¿Dispone de datos de coeficiente de cultivo?")

  #ET<-projectRaster(ET, crs= crs("+init=epsg:4326"))
  #PE<-projectRaster(PE, crs= crs("+init=epsg:4326"))

  PE1<-ET
  RR<-ET
  ETc<-ET
  Area_terreno<-area(Area)
  if(RespG=="YES"){
    KC=NULL
    while (is.data.frame(KC)==FALSE) {
      try(KC<- read_excel(file.choose()), silent=TRUE)
    }
    KC<-as.list(KC[,2])
    KC<-as.numeric(unlist(KC))
    names(KC)<-indices
    for (i in 1:nlayers(ET)) {
      for (j in 1:nlayers(PE)) {
        RT<-names(ET[[i]])==names(PE[[j]])
        if(RT==TRUE)
        {
          cat("Dato restante: ", paste0(nlayers(ET)-i),"\n")
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
  if (RespG=="NO"){
    RespT<-winDialog("yesno","¿Desea ingresar un valor de coeficiente de cultivo máximo?")
    if(RespT=="YES"){
      KC=NULL
      KC<-dlgInput("¿Desea ingrese un valor de coeficiente de cultivo máximo? : ")$res
      for (i in 1:nlayers(ET)) {
        for (j in 1:nlayers(PE)) {
          RT<-names(ET[[i]])==names(PE[[j]])
          if(RT==TRUE)
          {
            cat("Dato restante: ", paste0(nlayers(ET)-i),"\n")
            #print(names(PE[[j]]))
            ETc[[i]]<-ET[[i]]*KC
            RR[[i]]<-ETc[[i]]-PE[[j]]
            PE1[[i]]<-PE[[j]]
          }
        }
      }
    }else{
      for (i in 1:nlayers(ET)) {
        for (j in 1:nlayers(PE)) {
          RT<-names(ET[[i]])==names(PE[[j]])
          if(RT==TRUE)
          {
            cat("Dato restante: ", paste0(nlayers(ET)-i),"\n")
            #print(names(PE[[j]]))
            RR[[i]]<-ET[[i]]-PE[[j]]
            PE1[[i]]<-PE[[j]]
          }
        }
      }
    }
  }

  if (RespG=="CANCEL"){
    for (i in 1:nlayers(ET)) {
      for (j in 1:nlayers(PE)) {
        RT<-names(ET[[i]])==names(PE[[j]])
        #cat("\n",paste0(RT))
        if(RT==TRUE)
        {
          cat("Dato restante: ", paste0(nlayers(ET)-i),"\n")
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
  R_RR<-as.data.frame(cellStats(RR, stat="sum", na.rm=TRUE))
  colnames(R_RR)<-"Requerimiento de riego (mm)"
  R_RR2<-as.data.frame(cellStats(RR2, stat="sum", na.rm=TRUE))
  colnames(R_RR2)<-"Requerimiento de riego (m^3)"
  R_ET<-data.frame(cellStats(ET, stat="sum", na.rm=TRUE))
  colnames(R_ET)<-"Evapotranspiracion (mm)"
  R_ETc<-data.frame(cellStats(ETc, stat="sum", na.rm=TRUE))
  colnames(R_ETc)<-"Evapotranspiracion referencia (mm)"
  R_PE<-data.frame(cellStats(PE1, stat="sum", na.rm=TRUE))
  colnames(R_PE)<-"Precipitacion efectiva (mm)"
  indice<-as.data.frame(indice)
  colnames(indice)<-"Mes"
  Reporte<-as.data.frame(c(indice, R_ET, R_ETc, R_PE, R_RR, R_RR2))
  max(Reporte$Evapotranspiracion.referencia)
  cat("\nGuardando gráfico de balance...\n")
  png("~/_Descarga_Datos/Balance.png", width = 2500, height = 2000, res = 250)
  plot(Reporte$Evapotranspiracion.referencia..mm., ylim=c(0, max(Reporte$Evapotranspiracion.referencia..mm.)), type="b", lwd=2,axes=FALSE,
  plot(Reporte$Evapotranspiracion.referencia..mm., ylim=c(0, max(Reporte$Evapotranspiracion.referencia)), type="b", lwd=2,axes=FALSE,
       col="red", xlab="Meses", ylab="mm", main="Requerimiento de riego")
  lines(Reporte$Precipitacion.efectiva..mm., type="b", lwd=2,col="blue")
  lines(Reporte$Requerimiento.de.riego..mm., type="b", lwd=2, col="green")
  text(Reporte$Evapotranspiracion.referencia..mm., labels=round(Reporte$Evapotranspiracion.referencia,1), cex=0.75, pos=1, offset = 0.75)
  text(Reporte$Precipitacion.efectiva..mm., labels=round(Reporte$Precipitacion.efectiva..mm.,1), cex=0.75, pos=1, offset = 0.75)
  text(Reporte$Requerimiento.de.riego..mm., labels=round(Reporte$Requerimiento.de.riego..mm.,1), cex=0.75, pos=1, offset = 0.75)
  legend("bottomleft", col=c("red", "blue", "green"),
         legend=c("Evapotranspiración de referencia", "Precipitación Efectiva", "Requerimiento de Riego"),
         lwd=1, bty="n", inset=c(0,1), xpd=TRUE, horiz=TRUE)
  box()
  axis(1, las=1, at=1:length(Reporte$Mes),lab=Reporte$Mes)
  axis(2, las=1, at=0:round(max(Reporte$Evapotranspiracion.referencia)))
  dev.off()
  cat("\nGuardando datos en excel...\n")
  write_xlsx(Reporte, "~/_Descarga_Datos/Reporte.xlsx")
  write.csv(Reporte, file = "~/_Descarga_Datos/Reporte.csv", row.names = TRUE, col.names = TRUE)
  ########################
  #
  cat("\nGuardando raster de Requerimiento de riego...\n")
  col_RB<-colorRampPalette(c("#FFFFCC", "#C7E9B4", "#7FCDBB", "#41B6C4", "#2C7FB8", "#253494"))

  i=0
  while(i <= nlayers(RR)){
    i<-i+1
    if(i <= nlayers(RR)){
      cat("Datos restantes: ",nlayers(RR)-i, "\n")
      writeRaster(RR[[i]], filename = paste0("~/_Descarga_Datos/Requerimiento/Raster/", Sys.Date(),"/",i,"_", names(RR[[i]])), suffix=indice[i], format="GTiff", overwrite=TRUE)
      if(maxValue(RR[[i]])== 0){n <- 1}else{n<-maxValue(RR[[i]])}

      #png(filename=paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(),"/",i, indice[i,],"_Requerimiento.png"), width = 1200, height=1200, units="px")
      #plot(RR[[i]], col=col_RB(n),main="Precipitación", sub=paste0(indice[i,]),cex.main=3, cex.sub=2, cex.lab=20)
      #dev.off()
    }
  }

  return(RR)
}
