#' @title Datos MODIS de Evapotranspiración
#' @description Descarga el producto MOD16A2, procesa las imagenes a una zona de estudio.
#' @details Inicia solicitando el rango de fechas, verificando que las fechas sean correctas.
#' Realiza una validación que se encuentre instalado OSGEO para el uso de las librerias OGR/GDAL.
#' Solicita usuario y contraseña USGS para la descarga de imagenes MOD16A2.
#' Descarga y realiza el procesamiento de MOD16A2.
#' Realiza una interpolación para los datos faltantes.
#' Finaliza con la creación de imagenes y archivos raster.
#' @param Area Vectorial de la zona de estudio con la proyección EPSG: 4326.
#' @return Devuelve un raster stack de imagenes de evapotranspiración.
#' @export


MODIS<-function(Area){

  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(), sep=" "))==FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(), sep=" "))==FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(), sep=" "),recursive=TRUE)
  }
  if(dir.exists(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date(), sep=" "))==FALSE){
    dir.create(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/Mensual/",Sys.Date(), sep=" "),recursive=TRUE)
  }

  if(dir.exists("C:/OSGeo4W64/bin/")==FALSE){
    stop(winDialog("ok","Debe instalar OSGEO4W para las liberías de GDAL/OGR:
       https://trac.osgeo.org/osgeo4w/"))}else{cat("GDAL/OGR instalado...")}
  cat("Continuando procesamiento...\n")


  Sys.which("C:/OSGeo4W64/bin/")
  GDALPATH<-"C:/OSGeo4W64/bin/"

  setwd("~/_Descarga_Datos/MODIS/")
  Ruta<-"~/_Descarga_Datos/MODIS/"

  Fecha1<-dlgInput("Ingrese la fecha inicial de descarga (Anio-Mes-Dia): ")$res
  Fecha1<-as.Date(Fecha1, format="%Y-%m-%d")
  Verificar<-is.na(Fecha1)
  cat(paste0("\nFecha inicial de descarga:  ", Fecha1,"\n"))
  while (Verificar==TRUE) {
    winDialog("ok","Error en el formato de fecha, ingrese en formato:
            Año-Mes-Día.")
    Fecha1<-dlgInput("Ingrese la fecha inicial de descarga (Año-Mes-Día): ")$res
    Fecha1<-as.Date(Fecha1, format="%Y-%m-%d")
    Verificar<-is.na(Fecha1)
    cat(paste0("\nFecha inicial de descarga:  ", Fecha1,"\n"))
  }

  Fecha2<-dlgInput("Ingrese la fecha final de descarga (Anio-Mes-Dia): ")$res
  Fecha2<-as.Date(Fecha2, format="%Y-%m-%d")
  Verificar<-is.na(Fecha2)
  cat(paste0("Fecha final de descarga:  ",Fecha2,"\n"))
  while (Verificar==TRUE) {
    winDialog("ok","Error en el formato de fecha, ingrese en formato:
            Año-Mes-Día.")
    Fecha2<-dlgInput("Ingrese la fecha inicial de descarga (Año-Mes-Día): ")$res
    Fecha2<-as.Date(Fecha1, format="%Y-%m-%d")
    Verificar<-is.na(Fecha2)
    cat(paste0("Fecha final de descarga:  ",Fecha2,"\n"))
  }

  winDialog("ok", "Comenzando el procesamiento de datos MOD16A2.")

  if(dir.exists("C:/OSGeo4W64/bin/")==FALSE){
    stop(winDialog("ok","Debe instalar OSGEO4W para las liberías de GDAL/OGR:
       https://trac.osgeo.org/osgeo4w/"))}else{cat("GDAL/OGR instalado...")}
  cat("Continuando procesamiento...\n")

  Sys.which("C:/OSGeo4W64/bin/")
  GDALPATH<-"C:/OSGeo4W64/bin/"

  setwd("~/_Descarga_Datos/MODIS/")
  Ruta<-"~/_Descarga_Datos/MODIS/"

  # Login Earthdata
  EarthdataLogin(usr=getPass::getPass("Usuario Earthdata: "),pwd=getPass::getPass("Contraseña Earthdata: "))


  # Parametros MODIS
  MODISoptions(localArcPath = Ruta,
               outDirPath = Ruta,
               gdalPath = GDALPATH,
               MODISserverOrder = c("LPDAAC", "LAADS"))

  Fecha1<-transDate(begin = Fecha1)
  #Fecha1
  Fecha2<-transDate(end = Fecha2)

  A<-getTile(Area)
  runGdal(job=paste0(Sys.Date(), sep=""),
          product="MOD16A2",
          #product=Producto,
          extent=A,
          begin=Fecha1$beginDOY,
          end=Fecha2$endDOY,
          SDSstring = "001",
          outProj= "+init=epsg:4326")

  # Procesamiento de Mod16A2 ------------------------------------------------
  cat("\n*** LECTURA Y PROCESAMIENTO DE EVAPOTRANSPIRACIÓN ***\n")
  setwd(paste0("~/_Descarga_Datos/MODIS/",Sys.Date()))
  cat("\nCargando archivos tif...\n")
  Modis_datos<- list.files(pattern = ".tif")
  Modis_datos<-stack(Modis_datos)
  Nombre<-names(Modis_datos)
  #Nombre
  cat("\nAplicando Máscara...\n")
  #crs(Area)
  #Modis_datos

  Dimen<-dim(Modis_datos)
  Modis_datos<-crop(Modis_datos,extent(Area))
  if(Dimen[1] & Dimen[2] != 1){
    Modis_datos<-mask(Modis_datos, Area)
  }

  cat("\nConvirtiendo valores de relleno a NA...\n")
  Modis_datos[Modis_datos > 32000]<-NA
  Modis_datos[Modis_datos < 0]<-NA

  cat("\nCalculando factor de conversión...\n")
  Factor_modis<-function(x){
    x*0.1
  }

  Modis_datos<-calc(Modis_datos, fun=Factor_modis)

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
    coordinates(MD)= ~x+y
    coordinates(grd)= ~Var1+Var2
    gridded(grd)<-TRUE
    fullgrid(grd)<-TRUE
    #gridded(MD)<-TRUE
    proj4string(MD)<-CRS("+init=epsg:4326")
    proj4string(grd)<-CRS("+init=epsg:4326")
    idw_model<-gstat(formula= ET~1, data= MD, nmax=length(MD$ET), set= list(idp=2))
    modelo<-predict(object = idw_model, newdata=grd)
    modelo<-raster(modelo)
    Area_extension<-extent(bbox(Area))
    modelo@extent<-extent(Area_extension)
    modelo<-mask(modelo, Area)
    return(modelo)
  }



  cat("\nCreando Mapas...\n")

  names(Modis_datos)<-Nombre
  col_RB<-colorRampPalette(c("Blue", "Yellow", "Red"))

  NL<-(nlayers(Modis_datos))

  #plot(Modis_datos, col=col_RB(maxValue(Modis_datos)))
  i=0
  #RespG<-winDialog("yesno","¿Desea guardar las imágenes raster procesadas?")
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
      else{Modis_interpol[[i]]<-Interpolacion(Modis_datos[[i]],Area)}
      writeRaster(Modis_interpol[[i]], filename= paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(),"/", paste0(Nombre[i])), format="GTiff", overwrite=TRUE)
      #png(filename=paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(),"/", Nombre[i],".png"), width = 1200, height=1200, units="px")
      #plot(Modis_interpol, col=col_RB(maxValue(Modis_interpol)), main="Evapotranspiración", sub=paste0(Nombre[i]),
      #     cex.main=3, cex.sub=2, cex.lab=4)
      #dev.off()
    }
  }

  #Modis_datos
  return(Modis_datos)
}




