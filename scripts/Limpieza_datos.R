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
#Procedemos con la creación y eliminación de variables

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

m_test <- rename(m_test, Grado_edu = P6210s1)
m_train <- rename(m_train, Grado_edu = P6210s1)

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

m_test$Oc[is.na(m_test$Oc)] <- 0
m_train$Oc[is.na(m_train$Oc)] <- 0

m_test <- m_test %>%
  group_by(id) %>%
  mutate(Suma_ocupados = sum(Oc))
m_test$Porcentaje_ocupados <-  m_test$Suma_ocupados / m_test$Nper

m_train <- m_train %>%
  group_by(id) %>%
  mutate(Suma_ocupados = sum(Oc))
m_train$Porcentaje_ocupados <-  m_train$Suma_ocupados / m_train$Nper

# Creamos la nueva variable que suma los años para cada valor repetido de id
m_test <- m_test %>%
  group_by(id) %>%
  mutate(suma_anos = sum(Grado_edu))
m_test$Educación_promedio <-  m_test$suma_anos / m_test$Nper

m_train <- m_train %>%
  group_by(id) %>%
  mutate(suma_anos = sum(Grado_edu))
m_train$Educación_promedio <-  m_train$suma_anos / m_train$Nper

#####Agregamos la variable Ingpcug y pobre a test_hogares
##son las variables que vamos a predecir#####

m_test <- m_test %>%
  mutate(Ingpcug = ".")

m_test <- m_test %>%
  mutate(Pobre = ".")


##Renomabramos variables

m_train <- rename(m_train, pet= Pet, ocupado= Oc, desocupado= Des, inactivo= Ina, fex_c= Fex_c.y,
                  depto= Depto.y, fex_dpto=Fex_dpto.y,cuartos_hog= P5000, cuartos_dorm= P5010, arr_hip= P5130,
                  arriendo= P5140, nper=Nper, npersug= Npersug,sexo = P6020, edad = P6040, cotizante= P6090, seg_soc= P6100, 
                  ing_hor_ext= P6510, prima= P6545, bonif= P6580, sub_trans= P6585s2,
                  subsid_fam= P6585s3, subsid_educ= P6585s4, alim_trab = P6590, viv_pag_trab = P6600,
                  ing_esp= P6620, bonif_anual= P6630s6, otro_trab= P7040,deseo_hor= P7090,hor_trab_seg_sem= P7045, 
                  din_otr_per= P7505,ingr_trab_d= P7472,pagos_arr_pen= P7495,fondo_pensiones= P6920,IngresoPerCapita = Ingpcug)

m_test <- rename(m_test, pet= Pet, ocupado= Oc, desocupado= Des, inactivo= Ina, fex_c= Fex_c.x,
                 depto= Depto.x, fex_dpto=Fex_dpto.x,cuartos_hog= P5000, cuartos_dorm= P5010, arr_hip= P5130,
                 arriendo= P5140, nper=Nper, npersug= Npersug,sexo = P6020, edad = P6040, cotizante= P6090, seg_soc= P6100, 
                 ing_hor_ext= P6510, prima= P6545, bonif= P6580, sub_trans= P6585s2,
                 subsid_fam= P6585s3, subsid_educ= P6585s4, alim_trab = P6590, viv_pag_trab = P6600,
                 ing_esp= P6620, bonif_anual= P6630s6, otro_trab= P7040,deseo_hor= P7090,hor_trab_seg_sem= P7045, 
                 din_otr_per= P7505,ingr_trab_d= P7472,pagos_arr_pen= P7495,fondo_pensiones= P6920,IngresoPerCapita = Ingpcug)

length(unique(m_test$id))  #validamos nuevamente que no hallamos perdido hogares en el proceso
length(unique(m_train$id)) #validamos nuevamente que no hallamos perdido hogares en el proceso

## Ya tenemos todas las variables, las operaciones que provienen de personas las asignamos para todo el hogar, 
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
                                         "Tipodevivienda","Regimen_salud","Educación_promedio",
                                         "sexo", "edad","Jefe_hogar","seg_soc",  "Nivel_educativo", "Grado_edu" ,                        
                                         "Antiguedad_trabajo" , "Tipo_de_trabajo", 
                                         "ing_hor_ext","prima", "bonif", "sub_trans","subsid_fam",
                                         "subsid_educ","subsid_educ","alim_trab","viv_pag_trab",
                                         "ing_esp","bonif_anual","fondo_pensiones","otro_trab",          
                                         "hor_trab_seg_sem","deseo_hor","ingr_trab_d",
                                         "pagos_arr_pen","din_otr_per","pet", "Pobre","IngresoPerCapita"))
                                          
##Seleccionamos únicamente las variables de interes para cada set de datos
##Seleccionamos únicamente las variables de interes para cada set de datos
test_final <-subset(m_test, select = c("id","Porcentaje_ocupados","v.cabecera","cuartos_hog","cuartos_dorm",
                                         "nper","npersug","Li", "Lp", "fex_c","depto","fex_dpto",   
                                       "d_arriendo","Jefe_mujer","Jefe_hogar","PersonaxCuarto",
                                         "Tipodevivienda","Regimen_salud","Educación_promedio",
                                         "sexo", "edad","Jefe_hogar","seg_soc",  "Nivel_educativo", "Grado_edu" ,                        
                                         "Antiguedad_trabajo" , "Tipo_de_trabajo", 
                                         "ing_hor_ext","prima", "bonif", "sub_trans","subsid_fam",
                                         "subsid_educ","subsid_educ","alim_trab","viv_pag_trab",
                                         "ing_esp","bonif_anual","fondo_pensiones","otro_trab",          
                                         "hor_trab_seg_sem","deseo_hor","ingr_trab_d",
                                         "pagos_arr_pen","din_otr_per","pet","Pobre","IngresoPerCapita"))

length(unique(test_final$id))  #validamos nuevamente que no hallamos perdido hogares en el proceso
length(unique(train_final$id)) #validamos nuevamente que no hallamos perdido hogares en el proceso


# Identificamos los NA para las bases de datos
missing_countrain <- colSums(is.na(train_final))
print(missing_countrain) #s

missing_counttest <- colSums(is.na(test_final))
print(missing_counttest) #s

###Empezamos con la imputación de datos

###Aquí quede
train_final<- train_final %>%
  mutate(
    ocupado = ifelse(is.na(ocupado), "0", "1"),
    desocupado = ifelse(is.na(desocupado), "0", "1"),
    inactivo = ifelse(is.na(inactivo), "0", "1"),
  )

train_final  <- train_final %>%
  mutate(tiempo_empresa = ifelse(is.na(tiempo_empresa) & ocupado == "0", "0", tiempo_empresa)) %>%
  mutate(ing_hor_ext = ifelse(is.na(ing_hor_ext) & ocupado == "0", "2", ing_hor_ext)) %>%
  mutate(prima = ifelse(is.na(prima) & ocupado == "0", "2", prima)) %>%
  mutate(bonif = ifelse(is.na(bonif) & ocupado == "0", "2", bonif)) %>%
  mutate(sub_trans = ifelse(is.na(sub_trans) & ocupado == "0", "2", sub_trans)) %>%
  mutate(subsid_fam = ifelse(is.na(subsid_fam) & ocupado == "0", "2", subsid_fam)) %>%
  mutate(subsid_educ = ifelse(is.na(subsid_educ) & ocupado == "0", "2", subsid_educ)) %>%
  mutate(alim_trab = ifelse(is.na(alim_trab) & ocupado == "0", "2", alim_trab)) %>%
  mutate(viv_pag_trab = ifelse(is.na(viv_pag_trab) & ocupado == "0", "2", viv_pag_trab)) %>%
  mutate(tr_empr = ifelse(is.na(tr_empr) & ocupado == "0", "2", tr_empr)) %>%
  mutate(ing_esp = ifelse(is.na(ing_esp) & ocupado == "0", "2", ing_esp)) %>%
  mutate(hor_trab_sem = ifelse(is.na(hor_trab_sem) & ocupado == "0", "0", hor_trab_sem)) %>%
  mutate(otro_trab = ifelse(is.na(otro_trab) & ocupado == "0", "2", otro_trab)) %>%
  mutate(hor_trab_seg_sem = ifelse(is.na(hor_trab_seg_sem) & ocupado == "0", "0", hor_trab_seg_sem)) %>%
  mutate(deseo_hor = ifelse(is.na(deseo_hor) & ocupado == "0", "2", deseo_hor)) %>%
  mutate(ingr_trab_d = ifelse(is.na(ingr_trab_d) & desocupado == "0" & inactivo == "0", "2", ingr_trab_d))


