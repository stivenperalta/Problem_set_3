# Estadísticas descriptivas -----------------------------------------------

### Estadisticas descriptivas base de train###

library(psych) # cargar paquete psych para usar la función describe()
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  dplyr,
  expss,
  sf,
  gtsummary
)


###Cargamos las bases de datos imputadas:

test_final <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_3/stores/test_final.rds")
train_final <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_3/stores/train_final.rds")

# Seleccionamos variables para hacer descripción de datos
train_final_desc <- train_final[, c("Porcentaje_ocupados", "v.cabecera", "cuartos_hog", "cuartos_dorm",
                                    "depto", "d_arriendo", "Jefe_mujer", "PersonaxCuarto", "Lp",
                                    "Tipodevivienda", "Regimen_salud","Educacion_promedio", "sexo",
                                    "edad","seg_soc","Nivel_educativo","Antiguedad_trabajo","Tipo_de_trabajo",
                                    "ing_hor_ext","prima", "bonif","sub_trans","subsid_fam","subsid_educ",
                                    "alim_trab","viv_pag_trab","ing_esp","bonif_anual","fondo_pensiones",
                                    "otro_trab", "hor_trab_sem","deseo_hor","pagos_arr_pen","din_otr_per",
                                    "ocupado", "desocupado","inactivo","Pobre","IngresoPerCapita")]

# Calcular estadísticas descriptivas con la función describe()
describe(train_final_desc)

###Diseñando tabla para exportar

train_final<-train_final %>% select (Porcentaje_ocupados, v.cabecera, cuartos_hog, cuartos_dorm,
                                     depto, d_arriendo, Jefe_mujer, PersonaxCuarto, Lp,
                                     Tipodevivienda, Regimen_salud,Educacion_promedio, sexo,
                                     edad,seg_soc,Nivel_educativo,Antiguedad_trabajo,Tipo_de_trabajo,
                                     ing_hor_ext,prima, bonif,sub_trans,subsid_fam,subsid_educ,
                                     alim_trab,viv_pag_trab,ing_esp,bonif_anual,fondo_pensiones,
                                     otro_trab, hor_trab_sem,deseo_hor,pagos_arr_pen,din_otr_per,
                                     ocupado, desocupado,inactivo,Pobre,IngresoPerCapita) #seleccionamos variables que queremos usar para tabla descriptiva

#Aplicamos un theme para la tabla
theme_gtsummary_journal(journal = "qjecon")
theme_gtsummary_compact()

tbl<- tbl_summary(train_final,
                  statistic = list(all_continuous() ~ "{mean} ({sd})", # estadisticas para valores continuos
                                  all_categorical() ~ "{n}/{N} ({p}%)", # estadisticas para variables categoricas
                                   all_dichotomous() ~ "{p}%"),
                  digits = all_continuous() ~ 1,
            
                  missing = "no",
                  label = list(
                    Porcentaje_ocupados ~ "Porcentaje de ocupados",
                    v.cabecera ~ "Cabecera",
                    cuartos_hog ~ "Cuartos por hogar",
                    cuartos_dorm ~ "Domirtorios",
                    depto ~ "Departamento",
                    d_arriendo ~ "Arriendo",
                    Jefe_mujer ~ "Jefe_mujer",
                    PersonaxCuarto ~ "Hacinamiento",
                    Lp ~ "Línea de pobreza",
                    Tipodevivienda ~ "Tipo de vivienda",
                    Regimen_salud ~ "Regimen de salud",
                    Educacion_promedio ~ "Eduación promedio por hogar",
                    sexo ~ "Sexo",
                    edad ~ "Edad",
                    seg_soc ~ "Seguridad social",
                    Nivel_educativo ~ "Nivel educativo",
                    Antiguedad_trabajo ~ "Antiguedad trabajo",
                    Tipo_de_trabajo ~ "Tipo de trabajo",
                    ing_hor_ext ~ "Horas extra",
                    prima ~ "Prima",
                    bonif ~ "Delitos violencia intrafamiliar 2019-2021",
                    sub_trans ~ "Distancia centro comercial",
                    subsid_fam ~ "Distancia colegios")
) %>%
  modify_header(label ~ "**Característica**") %>%
  modify_caption("**Tabla 1. Estadísticas Descriptivas**")

tbl
