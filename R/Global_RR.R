#' @title Requerimiento hidrico
#' @description Realiza la estimación del requerimiento de riego
#' @details Realiza la estimación del requerimiento de riego, seleccionando área de estudio, descarga y procesado de datos MOD16A2, descarga y procesado de precipitación, estimación de la precipitación efectiva y estimación del requerimiento de riego.
#' Genera un reporte y gráfico de Evapontraspiración de referencia, Precipitación efectiva y requerimiento de riego.
#' @export


# Creacion de carpetas ----------------------------------------------------
Global_RR<-function(){


cat("Creando directorios...\n")
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
if(dir.exists("~/_Descarga_Datos/Precipitacion/Datos/") == FALSE){
  dir.create("~/_Descarga_Datos/Precipitacion/Datos/", recursive=TRUE)
}
if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(), sep=" ")) == FALSE){
  dir.create(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date(), sep=" "), recursive=TRUE)
}
if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
  dir.create(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
}
if(dir.exists(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(), sep=" ")) == FALSE){
  dir.create(paste0("~/_Descarga_Datos/Precipitacion_Efectiva/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
}
if(dir.exists(paste0("~/_Descarga_Datos/Escorrentia/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
  dir.create(paste0("~/_Descarga_Datos/Escorrentia/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
}
if(dir.exists(paste0("~/_Descarga_Datos/Escorrentia/Raster/",Sys.Date(), sep=" ")) == FALSE){
  dir.create(paste0("~/_Descarga_Datos/Escorrentia/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
}
if(dir.exists(paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(), sep=" ")) == FALSE){
  dir.create(paste0("~/_Descarga_Datos/Requerimiento/Imagenes/",Sys.Date(), sep=" "), recursive=TRUE)
}
if(dir.exists(paste0("~/_Descarga_Datos/Requerimiento/Raster/",Sys.Date(), sep=" ")) == FALSE){
  dir.create(paste0("~/_Descarga_Datos/Requerimiento/Raster/",Sys.Date(), sep=" "), recursive=TRUE)
}



  if(dir.exists("C:/OSGeo4W64/bin/")==FALSE){
    stop(winDialog("ok","Debe instalar OSGEO4W para las liberías de GDAL/OGR:
       https://trac.osgeo.org/osgeo4w/"))}else{cat("GDAL/OGR instalado...")}
  cat("Continuando procesamiento...\n")

  Sys.which("C:/OSGeo4W64/bin/")
  GDALPATH<-"C:/OSGeo4W64/bin/"

  setwd("~/_Descarga_Datos/MODIS/")
  Ruta<-"~/_Descarga_Datos/MODIS/"

  cat("\n*** Cargando un vectorial de la zona de estudio ***\n")
  Area<-readOGR(choose.files(default="",caption="Seleccione el archivo vectorial de la zona de estudio:"))
  #Area<-readOGR("C:/Users/leugi/Documents/Datos geoespaciales/Sinaloa/Culiacan22.shp")
  Area_proj<-crs(Area)
  WGS84_4326<-crs("EPSG:4326")
  #WGS84_4326<-CRS("EPSG:4326")
  if(projection(WGS84_4326)==projection(Area_proj)){cat("Proyección correcta.\n")}else{
    cat("\nCambiando proyección a ESPG:4326.\n")
    Area<-spTransform(Area, WGS84_4326)
    crs(Area)}


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
      if(Dimen[1] == 1 & Dimen[2] == 1){
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
        }
      }
      else{Modis_interpol[[i]]<-Interpolacion(Modis_datos[[i]],Area)}
      writeRaster(Modis_interpol[[i]], filename= paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(),"/", paste0(Nombre[i])), format="GTiff", overwrite=TRUE)
      #png(filename=paste0("~/_Descarga_Datos/MODIS/Procesamiento/Imagenes/",Sys.Date(),"/", Nombre[i],".png"), width = 1200, height=1200, units="px")
      #plot(Modis_interpol, col=col_RB(maxValue(Modis_interpol)), main="Evapotranspiración", sub=paste0(Nombre[i]),
      #     cex.main=3, cex.sub=2, cex.lab=4)
      #dev.off()
    }
  }



  cat("\n*** Composición mensual de datos modis ***\n")
  setwd(paste0("~/_Descarga_Datos/MODIS/Procesamiento/Raster_procesados/",Sys.Date(),"/"))
  #cat("\nCargando datos...\n")
  Modis_datos<- list.files(pattern = "tif")
  Modis_datos<-stack(Modis_datos)
  Nombre<-names(Modis_datos)
  #Nombre
  F_inicial<-substr(Nombre[1],start=10, stop=16)
  F_inicial
  F_inicial<-transDate(F_inicial)
  F_inicial<-as.Date(F_inicial$begin, formar="%Y-%m-%d")
  F_2<-substr(Nombre[2],start=10, stop=16)
  #Nombre[2]
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
  names(ET_mes)<-Rangomensual
  #winDialog("ok","Procesamiento de datos MODIS terminado.")


# Precipitacion -----------------------------------------------------------


  #winDialog("ok","Comenzando a descargar y procesamiento de datos de precipitación. Fuente: worldclim.org")

  #RespG<-winDialog("yesno","¿Desea guardar las imágenes raster procesadas?")

  cat("\nCargando un vectorial de la zona de estudio... ***\n")

  cat("\nProcesando datos mundiales de precipitación...\n")

  setwd("~/_Descarga_Datos/Precipitacion/Datos/")

  Archivo<-file.exists("wc2.1_30s_prec_01.tif")
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
  Prec_datos<- list.files(pattern = "tif")
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
  setwd(paste0("~/_Descarga_Datos/Precipitacion/Procesamiento/",Sys.Date()))
  #col_RB<-colorRampPalette(c("Red", "Yellow", "Blue"))
  i=0
  while(i <= nlayers(Prec_datos)){
    i<-i+1
    if(i <= nlayers(Prec_datos)){
      cat("Datos restantes: ",(nlayers(Prec_datos)-i), "\n")
      png(filename=paste0(i,"_",Meses[i],"_Precipitacion.png"), width = 1200, height=1200, units="px")
      writeRaster(Prec_datos[[i]], filename = paste0(i,"_", Meses[i]), suffix=Meses[i], format="GTiff", overwrite=TRUE)
      #plot(Prec_datos[[i]], col=col_RB(maxValue(Prec_datos[[i]])), main="Precipitación", sub=paste0(Meses[i]),
      #     cex.main=3, cex.sub=2, cex.lab=4)
      #dev.off()
    }
  }

#####################################
#                                   #
#    Precipitaci?n efectiva         #
#                                   #
#####################################

#setwd(paste0("~/_Descarga_Datos/Precipitacion Efectiva/Procesamiento/Imagenes/", Sys.Date())



  Meses<-(c("Enero", "Febrero", "Marzo", "Abril",
            "Mayo", "Junio", "Julio", "Agosto",
            "Septiembre", "Octubre", "Noviembre", "Diciembre"))


  precipitacion_efectiva<-function(P){
    ifelse(P<251, (P*(125-0.2*P))/125, 125+0.1*P)
  }


  cat("\nCalculando la precipitación efectiva...\n")
  Pre_efec<-calc(Prec_datos, fun=precipitacion_efectiva)
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




# Escorrent?a -------------------------------------------------------------

  Escorrentia2<-function(Prec, PE){
    Prec-PE
  }

  cat("\nCalculando la escorrentia generada...\n")

  Escor<-Escorrentia2(Prec_datos, Pre_efec)

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


  cat("\nCalculando el requerimiento de riego...\n")
  ET<-ET_mes
  PE<-Pre_efec

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
  R_RR<-as.data.frame(cellStats(RR, stat="mean", na.rm=TRUE))
  colnames(R_RR)<-"Requerimiento de riego (mm)"
  R_RR2<-as.data.frame(cellStats(RR2, stat="mean", na.rm=TRUE))
  colnames(R_RR2)<-"Requerimiento de riego (m^3)"
  R_ET<-data.frame(cellStats(ET, stat="mean", na.rm=TRUE))
  colnames(R_ET)<-"Evapotranspiracion (mm)"
  R_ETc<-data.frame(cellStats(ETc, stat="mean", na.rm=TRUE))
  colnames(R_ETc)<-"Evapotranspiracion referencia (mm)"
  R_PE<-data.frame(cellStats(PE1, stat="mean", na.rm=TRUE))
  colnames(R_PE)<-"Precipitacion efectiva (mm)"
  indice<-as.data.frame(indice)
  colnames(indice)<-"Mes"
  Reporte<-as.data.frame(c(indice, R_ET, R_ETc, R_PE, R_RR, R_RR2))
  #max(Reporte$Evapotranspiracion.referencia)
  cat("\nGuardando gráfico de balance...\n")
  png("~/_Descarga_Datos/Balance.png", width = 2500, height = 2000, res = 250)
  plot(Reporte$Evapotranspiracion.referencia..mm., ylim=c(0, max(Reporte$Evapotranspiracion.referencia..mm.)), type="b", lwd=2,axes=FALSE,
       col="red", xlab="Meses", ylab="mm", main="Requerimiento de riego")
  lines(Reporte$Precipitacion.efectiva..mm., type="b", lwd=2,col="blue")
  lines(Reporte$Requermiento.de.riego..mm., type="b", lwd=2, col="green")
  text(Reporte$Evapotranspiracion.referencia..mm., labels=round(Reporte$Evapotranspiracion.referencia..mm.,1), cex=0.75, pos=1, offset = 0.75)
  text(Reporte$Precipitacion.efectiva..mm., labels=round(Reporte$Precipitacion.efectiva..mm.,1), cex=0.75, pos=1, offset = 0.75)
  text(Reporte$Requerimiento.de.riego..mm., labels=round(Reporte$Requerimiento.de.riego..mm.,1), cex=0.75, pos=1, offset = 0.75)
  legend("bottomleft", col=c("red", "blue", "green"),
         legend=c("Evapotranspiración de referencia", "Precipitación Efectiva", "Requerimiento de Riego"),lwd=1, bty="n", inset=c(0,1), xpd=TRUE, horiz=TRUE)
  box()
  axis(1, las=1, at=1:length(Reporte$Mes),lab=Reporte$Mes)
  axis(2, las=1, at=0:round(max(Reporte$Evapotranspiracion.referencia)))
  dev.off()
  cat("\nGuardando datos en excel...\n")
  write_xlsx(Reporte, "~/_Descarga_Datos/Reporte.xlsx")
  write.csv(Reporte, file = "~/_Descarga_Datos/Reporte.csv", row.names = TRUE, col.names = TRUE)
  ########################

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

  winDialog("ok",paste0("Proceso Terminado. Archivos guardados en: ~/_Descarga_Datos/MODIS/",Sys.Date()))


}

