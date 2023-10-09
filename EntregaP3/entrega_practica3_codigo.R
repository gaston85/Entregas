rm(list = ls())
#Cargo los datos de las estaciones y les asigno nombre a las estaciones

datos_Aero <- read.table("AEROPARQUE.txt",col.names = c("Codigo identificacion","Fecha","Temperatura","Temp_de_Rocio","Presion"))

datos_Azul <- read.table("AZUL.txt",col.names = c("Codigo identificacion","Fecha","Temperatura","Temp_de_Rocio","Presion"))

datos_Catamarca <- read.table("CATAMARCA.txt",col.names = c("Codigo identificacion","Fecha","Temperatura","Temp_de_Rocio","Presion"))

datos_Chilecito <- read.table("CHILECITO.txt",col.names = c("Codigo identificacion","Fecha","Temperatura","Temp_de_Rocio","Presion"))

datos_Iguazu <- read.table("IGUAZU.txt",col.names = c("Codigo identificacion","Fecha","Temperatura","Temp_de_Rocio","Presion"))

datos_Mendoza <- read.table("MENDOZA.txt",col.names = c("Codigo identificacion","Fecha","Temperatura","Temp_de_Rocio","Presion"))

ubicacion_estaciones<-read.table("estaciones.txt",col.names =c("Nombres","Latitud","Longitud","Altura") )

#Faltan los datos de Mendoza, busco de forma manual.
ubicacion_mendoza<-data.frame("MENDOZA",-32.9,-68.8,746) 
colnames(ubicacion_mendoza)<-c("Nombres","Latitud","Longitud","Altura")

ubicacion_estaciones<-rbind(ubicacion_estaciones,ubicacion_mendoza)

rm(ubicacion_mendoza)

#Junto los datos de las estaciones con los datos de ubicacion de cada una
datos_Aero<-list("Datos"=datos_Aero,"Ubicacion"=ubicacion_estaciones[ubicacion_estaciones$Nombres==toupper("Aeroparque"),])
datos_Azul<-list("Datos"=datos_Azul,"Ubicacion"=ubicacion_estaciones[ubicacion_estaciones$Nombres==toupper("Azul"),])
datos_Catamarca<-list("Datos"=datos_Catamarca,"Ubicacion"=ubicacion_estaciones[ubicacion_estaciones$Nombres==toupper("Catamarca"),])
datos_Chilecito<-list("Datos"=datos_Chilecito,"Ubicacion"=ubicacion_estaciones[ubicacion_estaciones$Nombres==toupper("Chilecito"),])
datos_Iguazu<-list("Datos"=datos_Iguazu,"Ubicacion"=ubicacion_estaciones[ubicacion_estaciones$Nombres==toupper("Iguazu"),])
datos_Mendoza<-list("Datos"=datos_Mendoza,"Ubicacion"=ubicacion_estaciones[ubicacion_estaciones$Nombres==toupper("Mendoza"),])

#Genero una lista con las listas de las estaciones
Estaciones<-list("Aeroparque"=datos_Aero,"Azul"=datos_Azul,"Catamarca"=datos_Catamarca,"Chilecito"=datos_Chilecito,"Iguazu"=datos_Iguazu,"Mendoza"=datos_Mendoza)

#A los datos faltantes les asigno NA

for (i in names(Estaciones)) {
  Estaciones[[i]][["Datos"]][Estaciones[[i]][["Datos"]][]==9999.9]<-NA
}
#Convierto las temperaturas de grados fahrenheit a Celsius

for (i in names(Estaciones)) {
  Estaciones[[i]][["Datos"]][3:4]<-(Estaciones[[i]][["Datos"]][3:4] - 32) * (5/9)
}


Resumen_Lista<-function(lista){
  #Parametros: Recibe listas
  #Valor de retorno: Devuelve resumen de la lista ingresada
  
  #Crea vectores en los que se les ingresara los valores de cada estacion
  #para luego unirlos en una lista
  
  nombres<-c()
  media_T<-c()
  media_Tr<-c()
  desv_est_T<-c()
  desv_est_Tr<-c()
  max_T<-c()
  min_T<-c()
  max_Tr<-c()
  min_Tr<-c()
  fecha_I<-c()
  fecha_F<-c()
  for (i in 1:length(lista)) {
    for (j in 1:length(lista[[i]][])) {
      for (k in names(lista[[i]][[j]][])) {
        if(k=="Fecha"){
          fecha_I[i]<-lista[[i]][[j]][[k]][1]
          fecha_F[i]<-lista[[i]][[j]][[k]][length(Estaciones[[i]][[j]][[k]])]
        }else if (k=="Temperatura"){
          max_T[i]<-max(lista[[i]][[j]][[k]][],na.rm = T)
          min_T[i]<-min(lista[[i]][[j]][[k]][],na.rm = T)
          media_T[i]<-mean(lista[[i]][[j]][[k]][],na.rm = T)
          desv_est_T[i]<-sd(lista[[i]][[j]][[k]][],na.rm = T)
        }else if(k=="Temp_de_Rocio"){
          max_Tr[i]<-max(lista[[i]][[j]][[k]][],na.rm = T)
          min_Tr[i]<-min(lista[[i]][[j]][[k]][],na.rm = T)
          media_Tr[i]<-mean(lista[[i]][[j]][[k]][],na.rm = T)
          desv_est_Tr[i]<-sd(lista[[i]][[j]][[k]][],na.rm = T)
        }else if(k=="Nombres"){
          nombres[i]<-lista[[i]][[j]][[k]][]
        }
      }
    }
  }
  resumen<-data.frame("Nombres"=nombres,"Max_T"=max_T,"Min_T"=min_T,"Media_T"=media_T,
                      "Desvio_Est_T"=desv_est_T,"Max_Tr"=max_Tr,"Min_T"=min_T,
                      "Media_Tr"=media_Tr,"Desvio_Est_Tr"=desv_est_Tr,
                      "Fecha_Inicial"=fecha_I,"Fecha_Final"=fecha_F)
  return(resumen)
}

Resumen_Lista()

Localizacion<-function(lista,long_max,long_min,lat_max,lat_min){
  #Parametros: Recibe una lista, y valores numericos de longitud y latitud
  #Valor de Retorno: Mensaje con el aviso.
  
  #Genera un vector vacio, en los que se ingresara mensajes 
  #que avisan las estaciones que estan dentro 
  #del rango de los valores ingresados de longitud y latitud
  
  estaciones_en_la_region<-c()
  t<-1
  for (i in names(lista)) {
    if(lista[[i]][[2]][["Latitud"]][]>lat_min & lista[[i]][[2]][["Latitud"]][]<lat_max & lista[[i]][[2]][["Longitud"]][]<long_max & lista[[i]][[2]][["Longitud"]][]>long_min){
    estaciones_en_la_region[t]<-paste("La estacion",i,"se encuentra dentro de la region.")
    t<-t+1
    }
  }
  if(length(estaciones_en_la_region)==0){
    estaciones_en_la_region<-"No hay estaciones cercanas"
  }
  return(estaciones_en_la_region)
}
Localizacion()

save(Estaciones,file = "Datos Estaciones.Rdata")
