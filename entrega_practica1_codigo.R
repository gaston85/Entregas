rm(list = ls())

nombre<-readline("Ingrese su nombre: ") #Pido por pantalla el nombre
apellido<-readline("Ingrese su apellido: ")
libreta<-readline("Ingrese su libreta (N/AA): ") #Pido la libreta universitaria
puesto<-substr(libreta,1,nchar(libreta)-3) #Obtengo el puesto de la libreta
anio_libreta<-substr(libreta,nchar(libreta)-1,nchar(libreta)) #Obtengo el año de la libreta
mat_aprobadas<-as.numeric(readline("Cuantas materias tiene aprobadas? "))
mat_faltantes<-20-mat_aprobadas #Calculo las materias que le fatan al estudiante
print(paste("El alumno/a",nombre,apellido,"se inscribio como alumno de Exactas en el puesto",puesto,paste("en el año 20",anio_libreta),"y debe aprobar",mat_faltantes,"materias para obtener el titulo de grado"))