# Estadísticas descriptivas -----------------------------------------------

### Estadisticas descriptivas base de train###

library(psych) # cargar paquete psych para usar la función describe()

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


library(ggplot2)