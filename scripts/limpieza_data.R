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

##Una vez se han unido el los datos a nivel de persona a hogar
#Procedemos con la creación de variables

##Para empezar, empezamos por la variable de su vive en cabecera o en 
##zona urbana: convertir variable "Clase" del dataset original


#dicotómica Cabecera = 1 si vive en una cabecera municipal, 
##0 si vive en la zona rural

m_test$v.cabecera <- ifelse(m_test$Clase.x == 1, 1, 
                                ifelse(m_test$Clase.x == 2, 0, 0
                                ))

m_test <- m_test %>%
  mutate(v.cabecera = factor(m_test$v.cabecera, 
                                 levels = c(0,1),
                                 labels = c("Urbana", "Vive en cabecera")))



m_train$v.cabecera <- ifelse(m_train$Clase.x == 1, 1, 
                                 ifelse(m_train$Clase.x == 2, 0, 0
                                 ))

m_train <- m_train %>%
  mutate(v.cabecera = factor(m_train$v.cabecera, 
                                 levels = c(0,1),
                                 labels = c("Urbana", "Vive en cabecera")))


