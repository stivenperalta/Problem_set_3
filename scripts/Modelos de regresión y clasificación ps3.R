######## Problem Set 3 - modelos de Regresión y Clasificación ##################
# Autores: David Peralta
# fecha: 26/07/2023

# Preparación de la script -----------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, #gráficas
               tidyverse, #limpieza
               caret, #modelos
               parallel, # conocer los cores de mi pc
               doParallel, # maximizar el procesamiento en r en función de los cores de mi pc
               ranger,
               dplyr, tidyr, glmnet, pROC, randomForest) # Cargar paquetes requeridos

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

#vemos que hay en el directorio de stores
dir("../stores")

#Importamos las bases
test<-readRDS("../stores/test_final.rds")
train<-readRDS("../stores/train_final.rds")

#renombramos variable Pobre a pobre (para poder cargarla en kaggle)
test <- test %>%
  rename(pobre = Pobre)
train <- train %>%
  rename(pobre=Pobre)

prop.table(table(train$pobre)) #los datos estan desbalanceados

#Mutación de factores (tenemos que hacerlo por niveles/levels)
test$pobre <- factor(test$pobre, levels = c("0", "1"),
                                  labels = c("No", "Si"))
train$pobre <- factor(train$pobre, levels = c("0", "1"),
                      labels = c("No", "Si"))
#convertimos en factor
train$Regimen_salud <- as.factor(train$Regimen_salud)
test$Regimen_salud <- as.factor(test$Regimen_salud)

# Evalúo la correlación de las variables (Matríz de correlación)
numeric_train <- train %>% select_if(is.numeric) #separamos las numericas
numeric_train <- ungroup(numeric_train) %>% select(-id)
cor_matrix <- cor(numeric_train) #calculamos correlacion
print(cor_matrix)
library(corrplot)
corrplot(cor_matrix, method = "circle", tl.col = "black")
rm(cor_matrix, numeric_train)

# Selecciono las variables para emplear en el modelo
names(train)
summary(train)

# selecciono variables de mayor interés
train <- select(train, c(1:4, 6, 13:14, 16, 17, 19:23, 26, 44, 47, 48, 8, 9))
test <- select(test, c(1:4, 6, 13:14, 16, 17, 19:23, 26, 44, 47, 48, 8, 9))
summary(train)

# convierto la variable pobre en factor y la ajusto con valores de 1 y 0
train$pobre <- as.factor(train$pobre)
test$pobre <- as.factor(test$pobre)

#train$pobre <- factor(train$pobre, levels = c(0, 1))
#test$pobre <- factor(test$pobre, levels = c(0, 1))

#Creo las bases para poder hacer las predicciones
test_relevant <- test %>%
  ungroup() %>%
  select(Porcentaje_ocupados, v.cabecera, cuartos_hog, nper,
         d_arriendo, Jefe_mujer, PersonaxCuarto, Tipodevivienda,
         Educacion_promedio, sexo, edad, seg_soc, Nivel_educativo,
         Tipo_de_trabajo, ocupado, IngresoPerCapita, pobre)
train_prueba <- train %>%
  ungroup() %>%
  select(Porcentaje_ocupados, v.cabecera, cuartos_hog, nper,
         d_arriendo, Jefe_mujer, PersonaxCuarto, Tipodevivienda,
         Educacion_promedio, sexo, edad, seg_soc, Nivel_educativo,
         Tipo_de_trabajo, ocupado, IngresoPerCapita, pobre)

# balanceo los datos para reducir el sesgo en los resultados -------------------

#Balanceo la muestra para nivelar con el valor de menor frecuencia
set.seed(201718234)
down_train <- downSample(x = train[, -ncol(train)],
                         y = train$pobre)
table(down_train$pobre)

#Balanceo la muestra para nivelar con el valor de mayor frecuencia
set.seed(201718234)
up_train <- upSample(x = train[, -ncol(train)],
                         y = train$pobre)
table(up_train$pobre)

# Creo los parámetros e hiperparámetros de ajuste del modelo -------------------
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE, # guardar probabilidades
                     summaryFunction = twoClassSummary) # calcular métricas para accuracy

set.seed(201718234)
down_outside <- train(pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
                        d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
                        sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
                      data = down_train, 
                      method = "treebag",
                      nbagg = 50,
                      metric = "Accuracy",
                      trControl = ctrl)

pacman::p_load("adabag") # Boosting (adaboost)

set.seed(201718234)
mod_adaboost_1 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = train,
  metric = "Accuracy",
  method = "adaboost.M1", 
  trControl = ctrl,
  tuneGrid = expand.grid(
#    mfinal = c(135,145), # número de árboles que hará (iteraciones)
 #   maxdepth = c(25, 26), # profundidad de los árboles
    coeflearn = c("Breiman", "Freund") # tipo de coeficiente de aprendizaje / el paquete usa por default el de Breiman
  )
)

set.seed(201718234)
mod_adaboost_2 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = down_train,
  metric = "Accuracy",
  method = "adaboost.M1", 
  trControl = ctrl,
  tuneGrid = expand.grid(
    #    mfinal = c(135,145), # número de árboles que hará (iteraciones)
    #   maxdepth = c(25, 26), # profundidad de los árboles
    coeflearn = c("Breiman", "Freund") # tipo de coeficiente de aprendizaje / el paquete usa por default el de Breiman
  )
)

set.seed(201718234)
mod_adaboost_3 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = up_train,
  metric = "Accuracy",
  method = "adaboost.M1", 
  trControl = ctrl,
  tuneGrid = expand.grid(
    #    mfinal = c(135,145), # número de árboles que hará (iteraciones)
    #   maxdepth = c(25, 26), # profundidad de los árboles
    coeflearn = c("Breiman", "Freund") # tipo de coeficiente de aprendizaje / el paquete usa por default el de Breiman
  )
)

# Creo control por valicación cruzadamod_fr_1$bestTune-------------------------
cv<-trainControl(method="cv",
                 number=3)

                 classProbs=TRUE, #retorna la probabilidad de cada una de las clases
                 verbose=TRUE, #
                 savePredictions=T) #que guarde las predicciones

#creo la grilla
tunegrid_rf <- expand.grid(
  min.node.size = seq(c(135,145,length.out=5)), # controla la profundidad del árbol
  mtry = c(25, 26), #sqrt de variables # es el número de predictores (si los toma todos es bagging; solo un subconjunto es random forest)
  splitrule = "gini" # empleamos el índice de gini como regla de partición
)

# modelos de elastic net ------------------------------------------------------

# Elastic Net regresión
mod_en_1 <- train(
IngresoPerCapita ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
  sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = train,
  method = "glmnet", 
  trControl = cv
)

mod_en_1$bestTune # Evalúo los mejores hiperparámetros para ajustar la grilla

#Evalúo la predicción dentro de muestra
train_prueba$ingreso <- predict(mod_en_1, newdata = train_prueba)
train_prueba$pred_pobre <- ifelse(train_prueba$ingreso>train$Li, 0, 1)
train_prueba$pobre <- ifelse(train_prueba$pobre == "Si", 1, 0)
train_prueba$pred_pobre <- factor(train_prueba$pred_pobre, levels = c(0, 1))
train_prueba$pobre <- factor(train_prueba$pobre, levels = c(0, 1))
conf_matrix <- confusionMatrix(train_prueba$pred_pobre, train_prueba$pobre)
print(conf_matrix) # observo la matriz de confusión


# Elastic Net clasificación
mod_en_2 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = down_train,
  method = "glmnet", 
  trControl = cv,
  metric = "Accuracy"#
)#,
  tuneGrid = expand.grid(alpha = seq(0.50, 0.60, length.out =7),
                         lambda = seq(0.002000000, 0.003005342, length.out =3)) # bestTune = alpha  0.55 lambda 0.002705342
)

mod_en_2$bestTune # Evalúo los mejores hiperparámetros para ajustar la grilla

#Evalúo la predicción dentro de muestra
train_prueba$pred_pobre_1 <- predict(mod_en_2, newdata = train_prueba)
train_prueba$pred_pobre_1 <- ifelse(train_prueba$pred_pobre_1 == "Si", 1, 0)
train_prueba$pred_pobre_1 <- factor(train_prueba$pred_pobre_1, levels = c(0, 1))
train_prueba$pobre <- factor(train_prueba$pobre, levels = c(0, 1))
conf_matrix <- confusionMatrix(train_prueba$pred_pobre_1, train_prueba$pobre)
print(conf_matrix) # observo la matriz de confusión


# Realizar la predicción
test$pobre_balance_data <- predict(mod_en_1, newdata = test_relevant)
test$pobre <- ifelse(test$IngresoPerCapita>test$Li, 0, 1)
test1_EN <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
head(test1_EN) #evalúo que la base esté correctamente creada
write.csv(test1_EN,"../stores/regresion_en_1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

#predicción balance data
test$pobre_balance_data <- predict(mod_en_2, newdata = test_relevant)
test$pobre_balance_data <- ifelse(test$pobre_balance_data == "Si", 1, 0)
#test$pobre <- ifelse(test$IngresoPerCapita>test$Li, 0, 1)
test2_EN_bd <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id, pobre_balance_data)
test2_EN_bd <- test2_EN_bd %>% 
  rename(pobre=pobre_balance_data)
head(test2_EN_bd) #evalúo que la base esté correctamente creada
write.csv(test2_EN_bd,"../stores/regresion_en_bd_2.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle






# random forest clasificación ------------------------------------------------

mod_rf_1 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = train,
  method = "ranger", 
  trControl = cv,
  maximize = TRUE,
  metric = "Accuracy"
) # BestTune: mrty 2, splitrule extratrees, min.node.size 1

mod_rf_1$metric

#Evalúo la predicción dentro de muestra Random forest
train_prueba$pobre_rf <- predict(mod_rf_1, newdata = train_prueba)
train_prueba$pobre_rf <- ifelse(train_prueba$pred_pobre_1 == "Si", 1, 0)
train_prueba$pobre_rf <- factor(train_prueba$pred_pobre_1, levels = c(0, 1))
conf_matrix <- confusionMatrix(train_prueba$pred_pobre_1, train_prueba$pobre)
print(conf_matrix) # observo la matriz de confusión

table(train_prueba$pobre_rf)

mod_fr_1$bestTune


tunegrid_rf <- expand.grid(
  min.node.size = seq(c(135,145,length.out=5)), # inicial c(3000, 6000, 9000, 12000)
  mtry = c(25, 26), #sqrt de variables #inicial c(6, 12, 18)
  splitrule = c("variance")
)

# Verificar el balance de clases después del submuestreo
table(down_train$pobre)

####################
# Creo el modelo 11 de predicciónCreo con random forest
modelo11rf <- train(
  price ~ .,
  data = train,
  method = "ranger", 
  trControl = fitcontrol_localidad,
  maximize = F,
  metric = "MAE",
  tuneGrid = tunegrid_rf # bestTune = alpha  0.55 lambda 31446558
)

round(modelo11rf$results$MAE[which.min(modelo11rf$results$mtry)],3) #Evalúo el error de predicción de ese lambda
modelo11rf$bestTune # evaluar el mejor alpha y lambda
plot(modelo11rf, xvar = "lambda") # Grafico el error MAE
plot(modelo1rf) # observo gráficamente los resultados
fancyRpartPlot(modelo11rf$finalModel) # grafico el árbol (librería rattle)

#predigo el resultado en mi test
test_data$price <- predict(modelo11rf, newdata = test_data)
test9 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test9) #evalúo que la base esté correctamente creada
write.csv(test9,"../stores/spatial_random_forest.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Creo el modelo 14 de predicciónCreo con random forest
modelo14rf_barrio <- train(
  price ~ .,
  data = train,
  method = "ranger", 
  trControl = fitcontrol_barrio,
  maximize = F,
  metric = "MAE",
  tuneGrid = tunegrid_rf 
)


# Creo el modelo 12 de predicción con boosting

#creo la grilla
tunegrid_boosting <- expand.grid(
  mtry = c(25),
  splitrule = c("extratrees"),
  min.node.size = c(135, 136)
)

modelo12boosting <- train(
price ~ .,
data = train,
method = "ranger", 
trControl = fitcontrol_barrio,
maximize = F,
metric = "MAE",
tuneGrid = tunegrid_boosting # bestTune = alpha  0.55 lambda 31446558
)

round(modelo12boosting$results$MAE[which.min(modelo12boosting$results$MAE)],3) #Evalúo el error de predicción de ese lambda
mean(train$price)
modelo12boosting$bestTune # evaluar el mejor alpha y lambda
plot(modelo12boosting, xvar = "lambda") # Grafico el error MAE
plot(modelo12boosting) # observo gráficamente los resultados
fancyRpartPlot(modelo12boosting$finalModel) # grafico el árbol (librería rattle)

#predigo el resultado en mi test
test_data$price <- predict(modelo12boosting, newdata = test_data)
test10_1 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test10_1) #evalúo que la base esté correctamente creada
write.csv(test10_1,"../stores/spatial_boosting_1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# modelo 13 bagging
library(ipred)

modelo_bagging <- bagging(
  formula = price ~ .,
  data = train,
  nbagg = 1000,
  trControl = tunegrid_rf,
  coob = TRUE
)

mean(train$price)
modelo_bagging$err # evaluar el err
y_hat_price_bagging <- predict(modelo_bagging, test_data) # Predicción del precio con el modelo1
require("Metrics")
mae(y_hat_price_bagging, train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida

