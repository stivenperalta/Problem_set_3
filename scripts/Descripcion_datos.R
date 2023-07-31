# Estadísticas descriptivas -----------------------------------------------

rm(list = ls()) 

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

descriptivos <- train_final[, c("Porcentaje_ocupados", "v.cabecera", "cuartos_hog", "cuartos_dorm",
                                    "d_arriendo", "Jefe_mujer", "PersonaxCuarto", "Lp",
                                    "Tipodevivienda", "Regimen_salud","Educacion_promedio", "sexo",
                                    "edad","seg_soc","Nivel_educativo","Tipo_de_trabajo",
                                    "ing_hor_ext","prima", "bonif","sub_trans","subsid_fam","subsid_educ",
                                    "alim_trab","viv_pag_trab","ing_esp","bonif_anual","fondo_pensiones",
                                    "otro_trab", "hor_trab_sem","deseo_hor","pagos_arr_pen","din_otr_per",
                                    "ocupado", "desocupado","inactivo","Pobre","IngresoPerCapita")]

descriptivos2 <- test_final[, c("Porcentaje_ocupados", "v.cabecera", "cuartos_hog", "cuartos_dorm",
                                "d_arriendo", "Jefe_mujer", "PersonaxCuarto", "Lp",
                                "Tipodevivienda", "Regimen_salud","Educacion_promedio", "sexo",
                                "edad","seg_soc","Nivel_educativo","Tipo_de_trabajo",
                                "ing_hor_ext","prima", "bonif","sub_trans","subsid_fam","subsid_educ",
                                "alim_trab","viv_pag_trab","ing_esp","bonif_anual","fondo_pensiones",
                                "otro_trab", "hor_trab_sem","deseo_hor","pagos_arr_pen","din_otr_per",
                                "ocupado", "desocupado","inactivo","Pobre","IngresoPerCapita")]

tabla_estadisticas <- tibble()  # Creo una tibble vacía para almacenar los resultados

for (variable in colnames(descriptivos))
if (is.numeric(descriptivos[[variable]])) {
    estadisticas <- descriptivos %>%
      summarise(n_observaciones = sum(!is.na(.data[[variable]])),
                promedio = mean(.data[[variable]], na.rm = TRUE),
                desviacion = sd(.data[[variable]], na.rm = TRUE),
                mín = min(.data[[variable]], na.rm = TRUE),
                max = max(.data[[variable]], na.rm = TRUE)) %>%
      add_column(variable = variable, .before = 1)
  } else {
    estadisticas <- descriptivos %>%
      summarise(n_observaciones = sum(!is.na(.data[[variable]])),
                moda = names(which.max(table(.data[[variable]])))) %>%
      add_column(variable = variable, .before = 1)
  
  tabla_estadisticas <- bind_rows(tabla_estadisticas, estadisticas)  # Agregar las estadísticas a la tabla
}
  
tabla_estadisticas <- tabla_estadisticas[-2, ] 
tabla_estadisticas <- tabla_estadisticas %>% 
  select(-2, -3)
tabla_estadisticas[, 2:ncol(tabla_estadisticas)] <- round(tabla_estadisticas[, 2:ncol(tabla_estadisticas)], 2)
  # Creamos la tabla en formato APA
  tabla_01 <- flextable(tabla_estadisticas)
  tabla_01 <- theme_booktabs(tabla_01)
  tabla_01 <- autofit(tabla_01)
  tabla_01
  
  par(mfrow = c(1, 2))
  
  
###Generamos gráficos para hacer análisis #######

  library(ggplot2)
  
# Gráfico de barras para variables categóricas
## Aquí la idea fue validar cómo se comportan cada una de las variables
## en cada hogar que se clasifica como pobre o no pobre

##Si paga arriendo 
barplot(table(train_final$d_arriendo, train_final$Pobre), beside = TRUE,
          col = c("darkslategray", "darkslategray2" ), main = "Vivir en arriendo vs. Pobreza",
          xlab = "Pobre", ylab = "Frecuencia",
          par(xpd = TRUE),  # Permite que la leyenda se dibuje fuera del área del gráfico
          legend("bottomright", legend = c("Vive en arriendo", "No vive en arriendo"),
        fill = c("darkslategray", "darkslategray2"), title = "Clasificación",
        horiz = TRUE),inset = c(0, -0.15))
  

##Si la jefe del hogar es mujer
barplot(table(train_final$Jefe_mujer, train_final$Pobre), beside = TRUE,
        col = c("darkslategray", "darkslategray2" ), main = "Vivir en arriendo vs. Pobreza",
        xlab = "Pobre", ylab = "Frecuencia",
        par(xpd = TRUE),  # Permite que la leyenda se dibuje fuera del área del gráfico
        legend("bottomright", legend = c("Jeje_mujer", "Jefe hogar no es mujer"),
               fill = c("darkslategray", "darkslategray2"), title = "Clasificación",
               horiz = TRUE),inset = c(0, -0.15))  

  
  
  
  
  
  
  
  
  ggplot(train_final, aes(x = v.cabecera)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Distribución de cabecera o rural")
  
  ggplot(train_final, aes(x = Pobre)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Pobre")
  
  
  ggplot(train_final, aes(x = Jefe_mujer)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Distribución de Jefe_mujer")
  
  ggplot(train_final, aes(x = Tipodevivienda)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Distribución de tipo de vivienda")
  

  barplot(table(train_final$d_arriendo, train_final$Pobre), beside = TRUE, legend: TRUE,
          col = c("darkslategray", "darkslategray2" ), main = "Vivir en arriendo vs. Pobreza",
          xlab = "Pobre", ylab = "Frecuencia")+
          legend("bottom", legend = c("Pobre", "No pobre"), col = c("darkslategray", "darkslategray2"))
  
          
          
         
  
  ggplot(train_final, aes(x = Regimen_salud)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Distribución de Regimen_Salud")
  
  ggplot(train_final, aes(x = Tipo_de_trabajo)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Distribución de TipoDeTrabajo")
  
  ggplot(train_final, aes(x = Pobre)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Distribución de Pobre")
  
  ggplot(train_final, aes(x = Nivel_educativo)) +
    geom_bar(fill = "darkslategray") +
    labs(title = "Distribución de nivel educativo")
  
  # Histograma para variables numéricas
  ggplot(train_final, aes(x = Porcentaje_ocupados)) +
    geom_histogram(fill = "darkslategray", bins = 20) +
    labs(title = "Distribución de PorcentajeOcupados")
  
  ggplot(train_final, aes(x = Antiguedad_trabajo)) +
    geom_histogram(fill = "darkslategray", bins = 20) +
    labs(title = "Distribución de AntiguedadTrabajo")
  
  ggplot(train_final, aes(x = Lp)) +
    geom_histogram(fill = "darkslategray", bins = 20) +
    labs(title = "Distribución de Lp")
  
  ggplot(train_final, aes(x = IngresoPerCapita)) +
    geom_histogram(fill = "darkslategray", bins = 20) +
    labs(title = "Distribución de IngresoPerCapita")
  
 
  # Boxplot para variables numéricas
  ggplot(train_final, aes(y = PersonaPorCuarto)) +
    geom_boxplot(fill = "darkslategray") +
    labs(title = "Boxplot de PersonaPorCuarto")
  
  # Gráfico de densidad para variables numéricas
  ggplot(train_final, aes(x = PersonaPorCuarto)) +
    geom_density(fill = "darkslategray") +
    labs(title = "Distribución de hacinamiento")
  

  
###Otros gráficos que complementan el análisis

  # Gráfico de barras
  barplot(table(train_final$Nivel_educativo, train_final$Pobre), beside = TRUE,
          col = c("darkslategray","turquoise3", "darkslategray2","turquoise4","darkslategray4","lightblue2" ), main = "Relación nivel educativo vs. Clasificación pobreza",
          xlab = "Nivel Educativo", ylab = "Frecuencia",
          legend("bottom", legend = train_final$Nivel_educativo, fill = c("darkslategray","turquoise3", "darkslategray2","turquoise4","darkslategray4","lightblue2" ), ncol = length(train_final$Nivel_educativo)))
  
  
  
  
  
  
  
