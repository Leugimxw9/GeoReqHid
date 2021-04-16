Lectura_MODIS<-function(Zona){
  cat("\n*** LECTURA Y PROCESAMIENTO DE EVAPOTRANSPIRACIÓN ***\n")

  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/",Sys.Date()))==FALSE){
      stop(utils::winDialog("ok",paste0("No existe el directorio: ~/_Descarga_Datos/MODIS/",Sys.Date())))
      }


  setwd(paste0("~/_Descarga_Datos/MODIS/",Sys.Date()))
  cat("\nCargando archivos tif...\n")
  Modis_datos<- list.files(pattern = ".tif")
  Modis_datos<-raster::stack(Modis_datos)
  Nombre<-names(Modis_datos)
  cat("\nAplicando Máscara...\n")
  Dimen<-dim(Modis_datos)
  #Modis_datos<-raster::crop(Modis_datos,raster::extent(Zona))
  if(Dimen[1] & Dimen[2] != 1){
    Modis_datos<-raster::mask(Modis_datos, Area)
  }

  cat("\nConvirtiendo valores de relleno a NA...\n")
  Modis_datos[Modis_datos > 32000]<-NA
  Modis_datos[Modis_datos < 0]<-NA

  cat("\nCalculando factor de conversión...\n")
  Factor_modis<-function(x){
    x*0.1
  }

  Modis_datos<-raster::calc(Modis_datos, fun=Factor_modis)

  Interpolacion<-function(Modis_datos, Area){
    MD<-as.data.frame(Modis_datos, xy=TRUE)
    names(MD)<-c("x","y","ET")
    MD<-MD[!is.na(MD$ET),]
    tempo<-dim(Modis_datos)
    Area_DIM<-Area@bbox
    x.range<-as.numeric(range(MD$x))
    y.range<-as.numeric(range(MD$y))
    x_seq<-seq(x.range[1], x.range[2], length.out = tempo[2])
    y_seq<-seq(y.range[1], y.range[2], length.out = tempo[1])
    grd<-expand.grid(x_seq,y_seq)
    sp::coordinates(MD)= ~x+y
    sp::coordinates(grd)= ~Var1+Var2
    sp::gridded(grd)<-TRUE
    sp::fullgrid(grd)<-TRUE
    #gridded(MD)<-TRUE
    sp::proj4string(MD)<-sp::CRS("+init=epsg:4326")
    sp::proj4string(grd)<sp::CRS("+init=epsg:4326")
    idw_model<-gstat::gstat(formula= ET~1, data= MD, nmax=length(MD$ET), set= list(idp=2))
    modelo<-stats::predict(object = idw_model, newdata=grd)
    modelo<-raster::raster(modelo)
    Area_extension<-raster::extent(bbox(Area))
    modelo@extent<-raster::extent(Area_extension)
    modelo<-raster::mask(modelo, Area)
    return(modelo)
  }



  cat("\nCreando Mapas...\n")

  names(Modis_datos)<-Nombre
  col_RB<-grDevices::colorRampPalette(c("Blue", "Yellow", "Red"))

  NL<-(raster::nlayers(Modis_datos))

  #plot(Modis_datos, col=col_RB(maxValue(Modis_datos)))
  i=0
  #RespG<-utils::utils::winDialog("yesno","¿Desea guardar las imágenes raster procesadas?")
  Modis_interpol<-Modis_datos
  while(i<=NL){
    i<-i+1
    if(i<=NL){
      cat("Datos restantes: ",(NL-i), "\n")
      Dimen<-dim(Modis_datos[[i]])
      if(Dimen[1] & Dimen[2] == 1){
        if (is.na(values(Modis_datos[[i]]))==TRUE) {
          if (i==1){
            i2<-i+1
            if (is.na(values(Modis_datos[[i2]]))==FALSE) {
              Valor<-values(Modis_datos[[i2]])
              as.numeric(valor)
              values(Modis_datos[[i]])<-valor
              Modis_interpol[[i]]<-Modis_datos[[i]]
            }}else{
              y1<-as.numeric(values(Modis_datos[[i-1]]))
              y2<-as.numeric(values(Modis_datos[[i+1]]))
              y<- y1+((8/16))*(y2-y1)
              values(Modis_datos[[i]])<-y
              Modis_interpol[[i]]<-Modis_datos[[i]]
            }
        }else{Modis_interpol[[i]]<-Modis_datos[[i]]}
      }
      else{Modis_interpol[[i]]<-Interpolacion(Modis_datos[[i]],Zona)}
      raster::writeRaster(Modis_interpol[[i]], filename= paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(),"/", paste0(Nombre[i])), format="GTiff", overwrite=TRUE)
      grDevices::png(filename=paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(),"/", Nombre[i],".png"), width = 1200, height=1200, units="px")
      raster::plot(Modis_interpol[[i]], col=col_RB(maxValue(Modis_interpol[[i]])), main="Evapotranspiración", sub=paste0(Nombre[i]),
           cex.main=3, cex.sub=2, cex.lab=4)
      grDevices::dev.off()
    }
  }

  #Modis_datos
  return(Modis_datos)
}
