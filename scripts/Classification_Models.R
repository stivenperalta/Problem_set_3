#Classification Models

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, tidyverse, caret) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

#vemos que hay en el directorio de stores
dir("../stores")

hogares<-read_csv("../stores/test_hogares.csv")
personas<-read_csv("../stores/test_personas.csv")

#vemos variables
names(hogares)
names(personas)

#para facilidad cambiamos nombres de variables
