#' @title Precipitación
#' @description Descarga datos de precipitación de worldclim.org y delimitado a la zona de estudio.
#' @details Descarga los datos de precipitación
#' @param Area Vectorial de la zona de estudio con la proyección EPSG: 4326.
#' @return Devuelve un raster stack con datos de precipitación limitado a la zona de estudio.
#' @export
Datos_Precipitacion<-function(Area){

  winDialog("ok","Comenzando a descargar y procesamiento de datos de precipitación. Fuente: worldclim.org")
  cat("\n*** Cargando un vectorial de la zona de estudio ***\n")
  if(dir.exists("~/_Descarga_Datos/Precipitacion/Datos/") == FALSE){
    dir.create("~/_Descarga_Datos/Precipitacion/Datos/", recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(), sep=" "), recursive=TRUE)
  }


  #RespG<-winDialog("yesno","¿Desea guardar las imágenes raster procesadas?")

  cat("\nCargando un vectorial de la zona de estudio... ***\n")

  cat("\nProcesando datos mundiales de precipitación...\n")

  #setwd("~/_Descarga_Datos/Precipitacion/Datos/")

  Archivo<-file.exists("~/_Descarga_Datos/Precipitacion/Datos/wc2.1_30s_prec_01.tif")
  if(Archivo==FALSE){
    Archivo<-file.exists("Precipitacion.zip")
    if(Archivo==TRUE){
      Prec_datos<- list.files(pattern="*.zip")
      cat("\nDescomprimiendo archivo...\n")
      unzip(Prec_datos[1], overwrite = TRUE)
    }else{
      cat("\nDescargando archivo...\n")
      download.file("http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_prec.zip", dest="Precipitacion.zip")
      Prec_datos<- "Precipitacion.zip"
      cat("\nDescomprimiendo archivo...\n")
      unzip(Prec_datos, overwrite = TRUE)
    }
  }


  cat("\nCargando datos de precipitación a R...\n")
  Prec_datos<- list.files(path="~/_Descarga_Datos/Precipitacion/Datos/" ,pattern = "tif")
  Prec_datos<- stack(Prec_datos)
  #Prec_datos
  #names(Prec_datos)
  Meses<-(c("Enero", "Febrero", "Marzo", "Abril",
            "Mayo", "Junio", "Julio", "Agosto",
            "Septiembre", "Octubre", "Noviembre", "Diciembre"))
  names(Prec_datos)<-Meses
  #Prec_datos
  col_RB<-colorRampPalette(c("Red", "Yellow", "Blue"))
  cat("\nDelimitando la precipitación...\n")
  Dimen<-dim(Prec_datos)
  Prec_datos<-crop(Prec_datos,extent(Area))
  if(Dimen[1] & Dimen[2] != 1){
    Prec_datos<-mask(Prec_datos, Area)
  }
  Area_extension<-extent(bbox(Area))
  Prec_datos@extent<-extent(Area_extension)
  #setwd()
  #col_RB<-colorRampPalette(c("Red", "Yellow", "Blue"))
  i=0
  while(i <= nlayers(Prec_datos)){
    i<-i+1
    if(i <= nlayers(Prec_datos)){
      cat("Datos restantes: ",(nlayers(Prec_datos)-i), "\n")
      #png(filename=paste0(i,"_",Meses[i],"_Precipitacion.png"), width = 1200, height=1200, units="px")
      writeRaster(Prec_datos[[i]], filename = paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(),i,"_", Meses[i]), suffix=Meses[i], format="GTiff", overwrite=TRUE)
      #plot(Prec_datos[[i]], col=col_RB(maxValue(Prec_datos[[i]])), main="Precipitación", sub=paste0(Meses[i]),
      #     cex.main=3, cex.sub=2, cex.lab=4)
      #dev.off()
    }
  }
  return(Prec_datos)
}

