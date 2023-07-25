################################################################
# Problem Set 3: Predicting Poverty
# Authors: por Stiven Peralta, Jazmine Galdos, Andrea Clavijo, 
##Sergio Jiménez, Nicolás Barragán 
################################################################

rm(list = ls()) 
# Loading Libraries -------------------------------------------------------
install.packages("pacman")
install.packages("httr")
library("pacman") # para cargar paquetes
p_load("GGally","psych","rpart.plot","ROCR","gamlr","modelsummary","gtsummary","naniar","PerformanceAnalytics","pastecs",
       "writexl","dplyr","httr","tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer",
       "readr","AER","MLmetrics","smotefamily","pROC","smotefamily","rpart","randomForest","rpart", "Metrics",
       "rattle")
# Importing Dataset -------------------------------------------------------

# Se importan los 4 archivos a usar

test_hogar <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_3/stores/test_hogar.rds")
train_hogar <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_3/stores/train_hogar.rds")
test_persona <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_3/stores/test_persona.rds")
train_persona <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_3/stores/train_persona.rds")
sample_sub <- read_csv("GitHub/Problem_set_3/stores/sample_submission.csv")


# Unimos la base de datos de personas y hogares con merge usando el id del hogar
m_test <- merge(test_hogar, test_persona, by = "id")
m_train <- merge(train_hogar, train_persona, by = "id")
#rm(test_hogares, test_personas,train_hogares, train_personas)

##Ahora verificamos los valores únicos de cada set (train y test)
##Lo cual evidencia que no existen duplicados

length(unique(m_test$id))
length(unique(m_train$id))

##Una vez se han unido el los datos a nivel de persona a hogar-------------------------------------------------------
#Procedemos con la creación y eliminación de variables, aprovechamos la información
##de personas para crear variables agragadas al hogar que permitan predecir pobreza e ingreso

##Para empezar, empezamos por la variable de si vive en cabecera o en 
##zona urbana: convertir variable "Clase" del dataset original

#dicotómica Cabecera = 1 si vive en una cabecera municipal, 
##0 si vive en la zona rural

m_test$v.cabecera <- ifelse(m_test$Clase.x == 1, 1, 
                            ifelse(m_test$Clase.x == 2, 0, 0
                            ))

m_test <- m_test %>%
  mutate(v.cabecera = factor(m_test$v.cabecera, 
                             levels = c(0,1),
                             labels = c("Rural", "Vive en cabecera")))


m_train$v.cabecera <- ifelse(m_train$Clase.x == 1, 1, 
                             ifelse(m_train$Clase.x == 2, 0, 0
                             ))

m_train <- m_train %>%
  mutate(v.cabecera = factor(m_train$v.cabecera, 
                             levels = c(0,1),
                             labels = c("Rural", "Vive en cabecera")))
### Ahora vamos a crear las variables relacionadas con condiciones del hogar

## Primero: tipo de vivienda

m_test <- m_test %>%
  mutate(P5090 = factor(m_test$P5090, 
                        levels = c(1,2,3,4,5,6),
                        labels = c("Propia totalmente pagada", "Propia, la están pagando", "Arriendo o subarriendo", "Usufructo", "posesión sin título", "Otra")))

m_train <- m_train %>%
  mutate(P5090 = factor(m_train$P5090, 
                        levels = c(1,2,3,4,5,6),
                        labels = c("Propia totalmente pagada", "Propia, la están pagando", "Arriendo o subarriendo", "Usufructo", "posesión sin título", "Otra")))

m_test <- rename(m_test, Tipodevivienda = P5090)
m_train <- rename(m_train, Tipodevivienda = P5090)

##segundo, Hacinamiento: se determina que hay hacinamiento cuando residen más de 3.4 
##personas por habitación, nos sirve como proxy de la variable de vivienda
##usada en las mediciones de pobreza
##se saca la proporción entre personas en el hogar y la cantidad de cuartos

m_test$PersonaxCuarto <-  m_test$Nper / m_test$P5010
m_train$PersonaxCuarto <-  m_train$Nper / m_train$P5010

##Variables relacionadas con educación y mercado laboral

##nivel educativo como variable categorica.

m_test <- m_test %>%
  mutate(P6210 = factor(m_test$P6210, 
                        levels = c(1,2,3,4,5,6,9),
                        labels = c("Ninguno", "Preescolar", "Básica primaria", "Básica secundaria", "Media", "Universitaria", "No sabe, no informa")))

m_train <- m_train %>%
  mutate(P6210 = factor(m_train$P6210, 
                        levels = c(1,2,3,4,5,6,9),
                        labels = c("Ninguno", "Preescolar", "Básica primaria", "Básica secundaria", "Media", "Universitaria", "No sabe, no informa")))

m_test <- rename(m_test, Nivel_educativo = P6210)
m_train <- rename(m_train, Nivel_educativo = P6210)


##Expresamos la variable de educación en número de años estudiados

# Variable años de educación en función de max grado alcanzado

m_test$P6210[is.na(m_test$P6210)] <- 0 # los NA en teoría corresponden a personas que no reportan grado, los ajustamos a cero.
m_train$P6210[is.na(m_train$P6210)] <- 0

m_train$Max_grado <- ifelse(m_train$P6210 == 3, 5, 
                           ifelse(m_train$P6210  == 4, 9, 
                                  ifelse(m_train$P6210  == 5, 11, 
                                         ifelse(m_train$P6210  == 6, 16, 
                                                ifelse(m_train$P6210  == 9, 0, 0)))))

m_test$Max_grado <- ifelse(m_test$P6210 == 3, 5, 
                          ifelse(m_test$P6210  == 4, 9, 
                                 ifelse(m_test$P6210  == 5, 11, 
                                        ifelse(m_test$P6210  == 6, 16, 
                                               ifelse(m_test$P6210  == 9, 0, 0)))))


##Antiguedad en el actual trabajo

m_train <- rename(m_train, Antiguedad_trabajo = P6426)
m_test <- rename(m_test, Antiguedad_trabajo = P6426)

# Renombramos la variable categórica P6430 "Tipo de trabajo" a dicótoma

m_train$P6430[m_train$Oc == 0] <- 0
m_test$P6430[m_test$Oc == 0] <- 0

m_test <- m_test %>%
  mutate(P6430 = factor(P6430, 
                        levels = c(0,1,2,3,4,5,6,7,8,9),
                        labels = c("No trabaja",
                                   "Empleado empresa particular",
                                   "Empleado del Gobierno",
                                   "Empleado doméstico",
                                   "Cuenta propia",
                                   "Empleador",
                                   "Trabajador familiar sin remuneración",
                                   "Trabajador sin remuneración en empresas",
                                   "Jornalero o peón",
                                   "Otro")))

m_train <- m_train %>%
  mutate(P6430 = factor(P6430, 
                        levels = c(0,1,2,3,4,5,6,7,8,9),
                        labels = c("No trabaja",
                                   "Empleado empresa particular",
                                   "Empleado del Gobierno",
                                   "Empleado doméstico",
                                   "Cuenta propia",
                                   "Empleador",
                                   "Trabajador familiar sin remuneración",
                                   "Trabajador sin remuneración en empresas",
                                   "Jornalero o peón",
                                   "Otro")))


m_train <- rename(m_train, Tipo_de_trabajo = P6430)
m_test <- rename(m_test, Tipo_de_trabajo = P6430)

#Ahora, vamos a crear una variable que muestra si el Jefe de hogar es mujer,  si sí = 1, de lo contrario
#0

m_test$Jefe_mujer <- ifelse(m_test$P6050 == 1 & m_test$P6020 == 2, 1, 0)
m_train$Jefe_mujer <- ifelse(m_train$P6050 == 1 & m_train$P6020 == 2, 1, 0)


m_test <- m_test %>%
  mutate(Jefe_mujer = factor(m_test$Jefe_mujer, 
                             levels = c(0,1),
                             labels = c("Jefe de hogar no es mujer", "Jefe de hogar es mujer")))

m_train <- m_train %>%
  mutate(Jefe_mujer = factor(m_train$Jefe_mujer, 
                             levels = c(0,1),
                             labels = c("Jefe de hogar no es mujer", "Jefe de hogar es mujer")))

# Para tener una proxy de salud, validamos regimen contributivo & subsidiado
##Asignamos valores de 0 a respuestas de 2 y 9 cuyas respuestas son: no
#está afiliado,no es cotizante o no es beneficiario de alguna entidad 
#de seguridad social en salud y no sabe no responde, respectivamente.

m_test$P6100[m_test$P6090 == 2 | m_test$P6090 == 9] <- 0
m_train$P6100[m_train$P6090 == 2 | m_train$P6090 == 9] <- 0

m_test$Regimen_salud <- ifelse(m_test$P6100 == 1 | m_test$P6100 == 2 , 1, 0)
m_train$Regimen_salud <- ifelse(m_train$P6100 == 1 | m_train$P6100 == 2 , 1, 0)

m_test <- m_test %>%
  mutate(Regimen_salud = factor(m_test$Regimen_salud, 
                                levels = c(0,1),
                                labels = c("No Pertenece al régimen contributivo o especial", "Pertenece al régimen contributivo o especial")))

m_train <- m_train %>%
  mutate(Regimen_salud = factor(m_train$Regimen_salud, 
                                levels = c(0,1),
                                labels = c("No Pertenece al régimen contributivo o especial", "Pertenece al régimen contributivo o especial")))


###Cantidad de personas ocupadas en el hogar (proporcion)
###Primero reemplazamos los valores de NA en ocupados por O

m_test$Oc[is.na(m_test$Oc)] <- 0
m_train$Oc[is.na(m_train$Oc)] <- 0

### Creamos duplicado de la variable Ocupados, para hacer reemplazos
### en recuperación de missing values

m_test$Oc2<-m_test$Oc
m_train$Oc2<-m_train$Oc

m_test <- m_test %>%
  group_by(id) %>%
  mutate(Suma_ocupados = sum(Oc))
m_test$Porcentaje_ocupados <-  m_test$Suma_ocupados / m_test$Nper

m_train <- m_train %>%
  group_by(id) %>%
  mutate(Suma_ocupados = sum(Oc))
m_train$Porcentaje_ocupados <-  m_train$Suma_ocupados / m_train$Nper


# Creamos la nueva variable que suma los años para cada valor repetido de id
###para sacar eeducación promedio por hogar

m_test <- m_test %>%
  group_by(id) %>%
  mutate(suma_anos = sum(Max_grado))
m_test$Educacion_promedio <-  m_test$suma_anos / m_test$Nper

m_train <- m_train %>%
  group_by(id) %>%
  mutate(suma_anos = sum(Max_grado))
m_train$Educacion_promedio <-  m_train$suma_anos / m_train$Nper

#####Agregamos la variable Ingpcug y pobre a test_hogares
##son las variables que vamos a predecir#####

m_test <- m_test %>%
  mutate(Ingpcug = ".")

m_test <- m_test %>%
  mutate(Pobre = ".")


##Renomabramos variables

m_train <- rename(m_train, pet= Pet, ocupado= Oc2, desocupado= Des, inactivo= Ina, fex_c= Fex_c.y,
                  depto= Depto.y, fex_dpto=Fex_dpto.y,cuartos_hog= P5000, cuartos_dorm= P5010, arr_hip= P5130,
                  arriendo= P5140, nper=Nper, npersug= Npersug,sexo = P6020, edad = P6040, cotizante= P6090, seg_soc= P6100, 
                  ing_hor_ext= P6510, prima= P6545, bonif= P6580, sub_trans= P6585s2,
                  subsid_fam= P6585s3, subsid_educ= P6585s4, alim_trab = P6590, viv_pag_trab = P6600,
                  ing_esp= P6620, bonif_anual= P6630s6, otro_trab= P7040,deseo_hor= P7090,hor_trab_sem= P6800,hor_trab_seg_sem= P7045, 
                  din_otr_per= P7505,ingr_trab_d= P7472,pagos_arr_pen= P7495,fondo_pensiones= P6920,IngresoPerCapita = Ingpcug)

m_test <- rename(m_test, pet= Pet, ocupado= Oc2, desocupado= Des, inactivo= Ina, fex_c= Fex_c.x,
                 depto= Depto.x, fex_dpto=Fex_dpto.x,cuartos_hog= P5000, cuartos_dorm= P5010, arr_hip= P5130,
                 arriendo= P5140, nper=Nper, npersug= Npersug,sexo = P6020, edad = P6040, cotizante= P6090, seg_soc= P6100, 
                 ing_hor_ext= P6510, prima= P6545, bonif= P6580, sub_trans= P6585s2,
                 subsid_fam= P6585s3, subsid_educ= P6585s4, alim_trab = P6590, viv_pag_trab = P6600,
                 ing_esp= P6620, bonif_anual= P6630s6, otro_trab= P7040,deseo_hor= P7090,hor_trab_sem= P6800,hor_trab_seg_sem= P7045, 
                 din_otr_per= P7505,ingr_trab_d= P7472,pagos_arr_pen= P7495,fondo_pensiones= P6920,IngresoPerCapita = Ingpcug)

length(unique(m_test$id))  #validamos nuevamente que no hallamos perdido hogares en el proceso
length(unique(m_train$id)) #validamos nuevamente que no hallamos perdido hogares en el proceso

## Ya tenemos todas las variables, las operaciones que provienen de personas las asignamos para todo el hogar, ######
##especificamente para el Jefe de Hogar, por lo tanto, procedemos a generar las data por hogar nuevamente.

m_train <- rename(m_train, Jefe_hogar = P6050)
m_test <- rename(m_test, Jefe_hogar = P6050)
m_train <- m_train %>% filter(Jefe_hogar == 1)
m_test <- m_test %>% filter(Jefe_hogar == 1)

##Sumamos variable arriendos,para ellos se cambian
#los valores NA de arriendo y arr_hip por 0 para poder sumarlos ###

m_test<- m_test %>%
  mutate(arr_hip = ifelse(is.na(arr_hip), 0, arr_hip),
         arriendo = ifelse(is.na(arriendo), 0, arriendo))

m_train<- m_train %>%
  mutate(arr_hip = ifelse(is.na(arr_hip), 0, arr_hip),
         arriendo = ifelse(is.na(arriendo), 0, arriendo))

### Creamos una dummy de si el hogar paga arriendo o no ###

m_train <- m_train %>%
  mutate(d_arriendo = ifelse(arriendo > 0, "1", "0"))

m_test <- m_test %>%
  mutate(d_arriendo = ifelse(arriendo > 0, "1", "0"))

m_train <- m_train %>%
  mutate(arriendo= arr_hip + arriendo)

m_test <- m_test %>%
  mutate(arriendo= arr_hip + arriendo)

##Seleccionamos únicamente las variables de interes para cada set de datos
train_final <-subset(m_train, select = c("id","Porcentaje_ocupados","v.cabecera","cuartos_hog","cuartos_dorm",
                                          "nper","npersug","Li", "Lp", "fex_c","depto","fex_dpto",   
                                         "d_arriendo","Jefe_mujer","Jefe_hogar","PersonaxCuarto",
                                         "Tipodevivienda","Regimen_salud","Educacion_promedio",
                                         "sexo", "edad","seg_soc", "Nivel_educativo", "Max_grado" ,                        
                                         "Antiguedad_trabajo" , "Tipo_de_trabajo", 
                                         "ing_hor_ext","prima", "bonif", "sub_trans","subsid_fam",
                                         "subsid_educ","alim_trab","viv_pag_trab",
                                         "ing_esp","bonif_anual","fondo_pensiones","otro_trab",          
                                         "hor_trab_sem","deseo_hor","ingr_trab_d",
                                         "pagos_arr_pen","din_otr_per","pet","ocupado", 
                                         "desocupado", "inactivo","Pobre","IngresoPerCapita"))
                                          
##Seleccionamos únicamente las variables de interes para cada set de datos
##Seleccionamos únicamente las variables de interes para cada set de datos
test_final <-subset(m_test, select = c("id","Porcentaje_ocupados","v.cabecera","cuartos_hog","cuartos_dorm",
                                         "nper","npersug","Li", "Lp", "fex_c","depto","fex_dpto",   
                                       "d_arriendo","Jefe_mujer","Jefe_hogar","PersonaxCuarto",
                                         "Tipodevivienda","Regimen_salud","Educacion_promedio",
                                         "sexo", "edad","seg_soc",  "Nivel_educativo", "Max_grado" ,                        
                                         "Antiguedad_trabajo" , "Tipo_de_trabajo", 
                                         "ing_hor_ext","prima", "bonif", "sub_trans","subsid_fam",
                                         "subsid_educ","alim_trab","viv_pag_trab",
                                         "ing_esp","bonif_anual","fondo_pensiones","otro_trab",          
                                         "hor_trab_sem","deseo_hor","ingr_trab_d",
                                         "pagos_arr_pen","din_otr_per","pet","ocupado",
                                       "desocupado", "inactivo","Pobre","IngresoPerCapita"))

length(unique(test_final$id))  #validamos nuevamente que no hallamos perdido hogares en el proceso
length(unique(train_final$id)) #validamos nuevamente que no hallamos perdido hogares en el proceso


# Identificamos los NA para las bases de datos
missing_countrain <- colSums(is.na(train_final))
print(missing_countrain) #s

missing_counttest <- colSums(is.na(test_final))
print(missing_counttest) #s

###Empezamos con la imputación de datos####

###Depuramos NA en variables de Desocupados, inactivos y ocupados

test_final$desocupado <- ifelse(is.na(test_final$desocupado), 0, test_final$desocupado)
train_final$desocupado <- ifelse(is.na(train_final$desocupado), 0, train_final$desocupado)

test_final$inactivo<- ifelse(is.na(test_final$inactivo), 0, test_final$inactivo)
train_final$inactivo <- ifelse(is.na(train_final$inactivo), 0, train_final$inactivo)

names(train_final)

##Todos aquellos que reportaron no estar ocupados, en las variables de ingreso (categ+oricas)
##se les imputó un 0, Adicionalmente, aquellos que reportaron no estar ocupados en antiguedad trabajo
##se les imputó con 0. 

train_final$Antiguedad_trabajo <- ifelse(is.na(train_final$Antiguedad_trabajo) & train_final$ocupado == "0", "0", train_final$Antiguedad_trabajo)
train_final$ing_hor_ext <- ifelse(is.na(train_final$ing_hor_ext) & train_final$ocupado == "0", "2", train_final$ing_hor_ext)
train_final$prima <- ifelse(is.na(train_final$prima) & train_final$ocupado == "0", "2", train_final$prima) 
train_final$bonif <- ifelse(is.na(train_final$bonif) & train_final$ocupado =="0", "2", train_final$bonif) 
train_final$sub_trans <- ifelse(is.na(train_final$sub_trans) & train_final$ocupado == "0", "2", train_final$sub_trans) 
train_final$subsid_fam <- ifelse(is.na(train_final$subsid_fam) & train_final$ocupado == "0", "2", train_final$subsid_fam) 
train_final$subsid_educ <- ifelse(is.na(train_final$subsid_educ) & train_final$ocupado == "0", "2", train_final$subsid_educ) 
train_final$alim_trab <- ifelse(is.na(train_final$alim_trab) & train_final$ocupado == "0", "2", train_final$alim_trab) 
train_final$viv_pag_trab <- ifelse(is.na(train_final$viv_pag_trab) & train_final$ocupado == "0", "2", train_final$viv_pag_trab) 
train_final$ing_esp <- ifelse(is.na(train_final$ing_esp) & train_final$ocupado == "0", "2", train_final$ing_esp) 
train_final$otro_trab <- ifelse(is.na(train_final$otro_trab) & train_final$ocupado == "0", "2", train_final$otro_trab) 
train_final$hor_trab_sem <- ifelse(is.na(train_final$hor_trab_sem) & train_final$ocupado == "0", "0", train_final$hor_trab_sem)
train_final$deseo_hor <- ifelse(is.na(train_final$deseo_hor) & train_final$ocupado == "0", "2", train_final$deseo_hor)
train_final$ingr_trab_d <- ifelse(is.na(train_final$ingr_trab_d) & train_final$ocupado == "0" & train_final$inactivo == "1", "2", train_final$ingr_trab_d)
train_final$bonif_anual <- ifelse(is.na(train_final$bonif_anual) & train_final$ocupado == "0" & train_final$inactivo == "1", "2", train_final$bonif_anual)                        

#Ahora en test

test_final$Antiguedad_trabajo <- ifelse(is.na(test_final$Antiguedad_trabajo) & test_final$ocupado == "0", "0", test_final$Antiguedad_trabajo)
test_final$ing_hor_ext <- ifelse(is.na(test_final$ing_hor_ext) & test_final$ocupado == "0", "2", test_final$ing_hor_ext)
test_final$prima <- ifelse(is.na(test_final$prima) & test_final$ocupado == "0", "2", test_final$prima) 
test_final$bonif <- ifelse(is.na(test_final$bonif) & test_final$ocupado =="0", "2", test_final$bonif) 
test_final$sub_trans <- ifelse(is.na(test_final$sub_trans) & test_final$ocupado == "0", "2", test_final$sub_trans) 
test_final$subsid_fam <- ifelse(is.na(test_final$subsid_fam) & test_final$ocupado == "0", "2", test_final$subsid_fam) 
test_final$subsid_educ <- ifelse(is.na(test_final$subsid_educ) & test_final$ocupado == "0", "2", test_final$subsid_educ) 
test_final$alim_trab <- ifelse(is.na(test_final$alim_trab) & test_final$ocupado == "0", "2", test_final$alim_trab) 
test_final$viv_pag_trab <- ifelse(is.na(test_final$viv_pag_trab) & test_final$ocupado == "0", "2", test_final$viv_pag_trab) 
test_final$ing_esp <- ifelse(is.na(test_final$ing_esp) & test_final$ocupado == "0", "2", test_final$ing_esp) 
test_final$otro_trab <- ifelse(is.na(test_final$otro_trab) & test_final$ocupado == "0", "2", test_final$otro_trab) 
test_final$hor_trab_sem <- ifelse(is.na(test_final$hor_trab_sem) & test_final$ocupado == "0", "0", test_final$hor_trab_sem)
test_final$deseo_hor <- ifelse(is.na(test_final$deseo_hor) & test_final$ocupado == "0", "2", test_final$deseo_hor)
test_final$ingr_trab_d <- ifelse(is.na(test_final$ingr_trab_d) & test_final$ocupado == "0" & test_final$inactivo == "1", "2", test_final$ingr_trab_d)
test_final$bonif_anual <- ifelse(is.na(test_final$bonif_anual) & test_final$ocupado == "0" & test_final$inactivo == "1", "2", test_final$bonif_anual)                        

names(test_final)

##Primero imputamos los NA con la media del grado de educación para
##sacar el promedio de educación por el hogar

#train_final$Educacion_promedio <- as.numeric(train_final$Educacion_promedio )
#test_final$Educacion_promedio <- as.numeric(test_final$Educacion_promedio )
#media_educ <- mean(train_final$Educacion_promedio, na.rm = TRUE)
#media_educ <- round(media_educ, 0)
#train_final$Educacion_promedio <- ifelse(is.na(train_final$Educacion_promedio), media_educ, train_final$Educacion_promedio)
#test_final$Educacion_promedio <- ifelse(is.na(test_final$Educacion_promedio), media_educ, test_final$Educacion_promedio)


##Para la variable "tipo de trabajo" hacemos imputación de "otro"
##pues el jefe del hogar contestó que está desocupado e inactivo y
##adicionalmente reporta 0 en antiguedad laboral

train_final$Tipo_de_trabajo <- ifelse(is.na(train_final$Tipo_de_trabajo) & train_final$ocupado == "0" & train_final$Antiguedad_trabajo == "0", "9", train_final$Tipo_de_trabajo)
test_final$Tipo_de_trabajo <- ifelse(is.na(test_final$Tipo_de_trabajo) & test_final$ocupado == "0" & test_final$Antiguedad_trabajo == "0", "9", test_final$Tipo_de_trabajo)

length(unique(test_final$id))  #validamos nuevamente que no hallamos perdido hogares en el proceso
length(unique(train_final$id)) #validamos nuevamente que no hallamos perdido hogares en el proceso

# Grabamos las bases de datos sin imputaciones en variables
## categoricas en formato rds
setwd("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_3")
saveRDS(train_final, file = "../stores/train_final.rds")
saveRDS(test_final, file = "../stores/test_final.rds")

###Como se siguen teniendo NAs en las variables catégoricas,

# Evalúo variables con missing values para imputar

missing_valuesTrain <- colSums(is.na(train_final)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_valuesTrain), Missing_Values = missing_valuesTrain) # lo reflejo en un data.frame
missing_table

###Hago imputación de esas variables que me dan missing values, siguiendo la recomendación de: 
##imputar por la MODA de cada una de ellas. Para más información, véase:
##https://rstudio-pubs-static.s3.amazonaws.com/788490_4a1cd5ddda6a4e2b9c242766b2923351.html

##Excluyo "ingr_trab_d" porque más del 70% son missing y estaríamos
##creando sesgo al imputar casi todo el 100% de la muestra. Por esa razón se excluye

#Primero en train, calculo la moda

train_imp <- select(train_final, ing_hor_ext, prima, bonif,
                    sub_trans, subsid_fam, subsid_educ, alim_trab,
                    viv_pag_trab, ing_esp, bonif_anual,fondo_pensiones,
                    pagos_arr_pen, din_otr_per, hor_trab_sem) 

calcular_moda <- function(x) {
  tabla_frecuencias <- table(x)
  moda <- names(tabla_frecuencias)[tabla_frecuencias == max(tabla_frecuencias)]
  return(moda)
}
modas1 <- sapply(train_imp, calcular_moda)

###Ahora hago la imputación de la moda en cada una de esas variables categóricas
##con missing en train

for (col in names(train_imp)) {
  # Calcular la moda para la columna actual
  moda_actual <- calcular_moda(train_imp[[col]])
  # Reemplazar los valores NA con la moda correspondiente utilizando ifelse()
  train_imp[[col]] <- ifelse(is.na(train_imp[[col]]), moda_actual, train_imp[[col]])
}

#Segundo en test, calculo la moda
test_imp <- select(test_final, ing_hor_ext, prima, bonif,
                    sub_trans, subsid_fam, subsid_educ, alim_trab,
                    viv_pag_trab, ing_esp, bonif_anual,fondo_pensiones,
                    pagos_arr_pen, din_otr_per, hor_trab_sem) 

modas2 <- sapply(test_imp, calcular_moda)

###Ahora hago la imputación de la moda en cada una de esas variables categóricas
##con missing en test

for (col in names(test_imp)) {
  # Calcular la moda para la columna actual
  moda_actual <- calcular_moda(test_imp[[col]])
  # Reemplazar los valores NA con la moda correspondiente utilizando ifelse()
  test_imp[[col]] <- ifelse(is.na(test_imp[[col]]), moda_actual, test_imp[[col]])
}

train_final$ing_hor_ext <- train_imp$ing_hor_ext
db_new_flt$banos <- imputed_data$banos


cantidad_nas <- sum(is.na(train_final$ing_hor_ext))
cantidad_nas




