#' @title Requerimiento de riego
#' @description Estima el requerimiento de riego con los datos de evapotranspiración de MODIS y Precipitación de worldclim.org.
#' @details Descarga datos geoespaciales de precipitación del portal worldclim.org, posteriormente es procesado a la zona de estudio.
#' @param Zona Es el archivo vectorial cargado anteriormente con la función ZOna_estudio.
#' @return Devuelve un raster stack de requerimiento de riego.
#' @export
KC<-function(Zona){
  if(dir.exists(paste0("~/_Descarga_Datos/KC/",Sys.Date(), sep=" ")) == FALSE){
    dir.create(paste0("~/_Descarga_Datos/KC/",Sys.Date(), sep=" "), recursive=TRUE)
  }

  if(dir.exists("C:/OSGeo4W64/bin/")==FALSE){
    if(dir.exists("C:/Program Files/QGIS 3.18/bin/")==FALSE){
      stop(svDialogs::winDialog("ok","Debe instalar OSGEO4W para las liberías de GDAL/OGR o QGIS."))}else{cat("GDAL/OGR instalado...")
        GDALPATH<-"C:/Program Files/QGIS 3.*/bin/"}
  }else{cat("GDAL/OGR instalado...")
    GDALPATH<-"C:/OSGeo4W64/bin/"}

  cat("Continuando procesamiento...\n")

  Ruta<-"~/_Descarga_Datos/KC/"
  setwd("~/_Descarga_Datos/KC/")

  MODIS::MODISoptions(localArcPath = Ruta,
                      outDirPath = Ruta,
                      gdalPath = GDALPATH,
                      MODISserverOrder = c("LPDAAC", "LAADS"))

  producto <- "MOD16A2.060"
  bandas1 <- "001"
  bandas2 <- "1"

  Fecha1<-svDialogs::dlgInput("Ingrese la fecha inicial de descarga (Anio-Mes-Dia): ")$res
  Fecha1<-as.Date(Fecha1, format="%Y-%m-%d")
  Verificar<-is.na(Fecha1)
  cat(paste0("\nFecha inicial de descarga:  ", Fecha1,"\n"))
  while (Verificar==TRUE) {
    svDialogs::winDialog("ok","Error en el formato de fecha, ingrese en formato:
            Año-Mes-Día.")
    Fecha1<-svDialogs::dlgInput("Ingrese la fecha inicial de descarga (Año-Mes-Día): ")$res
    Fecha1<-as.Date(Fecha1, format="%Y-%m-%d")
    Verificar<-is.na(Fecha1)
    cat(paste0("\nFecha inicial de descarga:  ", Fecha1,"\n"))
  }

  Fecha2<-svDialogs::dlgInput("Ingrese la fecha final de descarga (Anio-Mes-Dia): ")$res
  Fecha2<-as.Date(Fecha2, format="%Y-%m-%d")
  Verificar<-is.na(Fecha2)
  cat(paste0("Fecha final de descarga:  ",Fecha2,"\n"))
  while (Verificar==TRUE) {
    svDialogs::winDialog("ok","Error en el formato de fecha, ingrese en formato:
            Año-Mes-Día.")
    Fecha2<-svDialogs::dlgInput("Ingrese la fecha inicial de descarga (Año-Mes-Día): ")$res
    Fecha2<-as.Date(Fecha1, format="%Y-%m-%d")
    Verificar<-is.na(Fecha2)
    cat(paste0("Fecha final de descarga:  ",Fecha2,"\n"))
  }

  Fecha1<-MODIS::transDate(begin = Fecha1)
  #Fecha1
  Fecha2<-MODIS::transDate(end = Fecha2)

  A<-MODIS::getTile(Zona)

  MODIS::EarthdataLogin(usr=getPass::getPass("Usuario Earthdata: "), pwd = getPass::getPass("Contraseña Earthdata: "))

  MODIS::runGdal(job=paste0(Sys.Date(),"/ETc", sep=""),
                                               product=producto,
                                               collection = "006",
                                               #product=Producto,
                                               extent=A,
                                               begin=Fecha1$beginDOY,
                                               end=Fecha2$endDOY,
                                               SDSstring = bandas1,
                                               outProj= "+init=epsg:4326")


  MODIS::runGdal(job=paste0(Sys.Date(),"/ETo", sep=""),
                   product=producto,
                   collection = "006",
                   #product=Producto,
                   extent=A,
                   begin=Fecha1$beginDOY,
                   end=Fecha2$endDOY,
                   SDSstring = bandas2,
                   outProj= "+init=epsg:4326")

  ETo_DIR<-paste0("~/_Descarga_Datos/MODIS/",Sys.Date(),"/ETo")
  ETc_DIR<-paste0("~/_Descarga_Datos/MODIS/",Sys.Date(),"/ETc")

  print("\nEstimando el coeficiente de cultivo...\n")

  KC<-function(ETo_DIR, ETc_DIR){
    setwd(ETo_DIR)
    ETo<-list.files( pattern = ".tif")
    ETo<-raster::stack(ETc)

    setwd(ETc_DIR)
    ETc<-list.files( pattern = ".tif")
    ETc<-raster::stack(ETc)

    Nombre<-names(ETc)
    F_inicial<-substr(Nombre[1],start=10, stop=16)
    F_inicial<-MODIS::transDate(F_inicial)
    F_inicial<-as.Date(F_inicial$begin, formar="%Y-%m-%d")
    F_2<-substr(Nombre[2],start=10, stop=16)
    F_2<-MODIS::transDate(F_2)
    F_2<-as.Date(F_2$begin, formar="%Y-%m-%d")
    F_final<-substr(Nombre[raster::nlayers(ETc)],start=10, stop=16)
    F_final<-MODIS::transDate(F_final)
    F_final<-as.Date(F_final$begin, formar="%Y-%m-%d")
    F_dif<-F_2-F_inicial
    #F_dif
    #day(F_inicial)<-1
    #day(F_final)<-1
    Rangotemporal<-seq.Date(as.Date(F_inicial), as.Date(F_final), F_dif)
    Fechas<-as.data.frame(Rangotemporal)


    KC<-ETc/ETo
    KC
    Fechas<-as.data.frame(names(KC))
    KC<-as.data.frame(raster::cellStats(KC, stat="mean", na.rm=TRUE))
    KC_R<-as.data.frame(c(Fechas, KC))

    colnames(KC_R)<-c("Fechas", "Coeficiente de cultivo")
    writexl::write_xlsx(KC_R, "~/_Descarga_Datos/KC.xlsx")



  }

  print("Estimando el coeficiente de cultivo mensual...")
  ETm<-function(Dir){
    setwd(Dir)
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
                            filename= paste0(Dir,"/", paste0(Rangomensual[i])),
                            format="GTiff", overwrite=TRUE)
        #}
        }
    }

    #utils::winDialog("ok","Procesamiento de datos MODIS terminado.")
    names(ET_mes)<-Rangomensual
    return(ET_mes)

  }

  KC(ETo_DIR, ETc_DIR)
  ETo<-ETm(ETo_DIR)
  ETc<-ETm(ETc_DIR)
  KC<-ETc/ETo
  KC_R<-as.data.frame(raster::cellStats(KC, stat="mean", na.rm=TRUE))
  colnames(KC_R)<-"Coeficiente de cultivo"
  writexl::write_xlsx(KC_R, "~/_Descarga_Datos/KC_mensual.xlsx")

  ETm(ETo_DIR)



}


