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
#Procedemos con la creación de variables

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
##se encuentra que el 4% de los datos refleja 
##missing values. #DECIDIR QUE HACER CON ESTO o si dejamos esto en 
##términos de años

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

length(unique(m_test$id))  #validamos nuevamente que no hallamos perdido hogares en el proceso
length(unique(m_train$id)) #validamos nuevamente que no hallamos perdido hogares en el proceso












##renombramos la variable Nper correspondientes al número de 
##personas por hogar

m_test <- rename(m_test, Num.personashogar = Nper)
m_train <- rename(m_train, Num.personashogar = Nper)
