############################## Problem Set 3 ###################################
# Autores: David Peralta
# fecha: 018/07/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, rio, tidyverse, skimr, caret, 
               rvest, magrittr, rstudioapi, stargazer, 
               boot, readxl, knitr, kableExtra,
               glmnet, sf, tmaptools, leaflet,
               tokenizers, stopwords, SnowballC,
               stringi, dplyr, stringr, sp, hunspell,
               car,
               parallel, # conocer los cores de mi pc
               doParallel, # maximizar el procesamiento en r en función de los cores de mi pc
               rattle) # graficar los árgoles) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()
rm(path_folder, path_script)

# maximizo el procesamiento de r
detectCores() # detecta los cores del computador
registerDoParallel(6) # 6 de 8 cores de mi computador para R
getDoParWorkers() # verifico el número de cores usados por R

# importar datos ----------------------------------------------------------

#train data
train_hogar<-read_csv("../../../Problems Sets/Problem set 3/train_hogares.csv")
train_persona<-read_csv("../../../Problems Sets/Problem set 3/train_personas.csv")

test_hogar<-read_csv("../../../Problems Sets/Problem set 3/test_hogares.csv")
test_persona<-read_csv("../../../Problems Sets/Problem set 3/test_personas.csv")

train_hogar<-train_hogar %>% mutate(sample_level="train_hogar")
train_persona<-train_persona %>% mutate(sample_level="train_persona")

test_hogar<-test_hogar %>% mutate(sample_level="test_hogar")
test_persona<-test_persona %>% mutate(sample_level="test_persona")

summary(test_hogar, train_hogar)

# guardo las bases en formato rds
saveRDS(train_hogar, file = "../stores/train_hogar.rds")
saveRDS(test_hogar, file = "../stores/test_hogar.rds")
saveRDS(train_persona, file = "../stores/train_persona.rds")
saveRDS(test_persona, file = "../stores/test_persona.rds")

# para importar las bases utilizo readRDS()
train_hogar <- readRDS("../stores/train_hogar.rds")
