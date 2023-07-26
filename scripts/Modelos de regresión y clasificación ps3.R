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
               dplyr, tidyr, glmnet, pROC) # Cargar paquetes requeridos

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

glimpse(train) # visualizamos los datos

#renombramos variable Pobre a pobre (para poder cargarla en kaggle)
test <- test %>%
  rename(pobre = Pobre)
train <- train %>%
  rename(pobre=Pobre)

table(train$pobre) #los datos estan desbalanceados

#Mutación de factores (tenemos que hacerlo por niveles/levels)
train$pobre <- factor(train$pobre, levels = c("0", "1"),
                                    labels = c("No", "Si"))
test$pobre <- factor(test$pobre, levels = c("0", "1"),
                                  labels = c("No", "Si"))

# selecciono variables de mayor interés
train_1 <- select(train, c(1:9, 13:23, 25:48))
test_1 <- select(test, c(1:9, 13:23, 25:48))


# modelos de bagging, Random Forest y boosting ---------------------------------

# Creo control por valicación cruzada
cv<-trainControl(method="cv",
                 number=5,
                 classProbs=TRUE, #retorna la probabilidad de cada una de las clases
                 verbose=TRUE, #
                 savePredictions=T) #que guarde las predicciones

#creo la grilla
tunegrid_rf <- expand.grid(
  min.node.size = seq(c(135,145,length.out=5)), # controla la profundidad del árbol
  mtry = c(25, 26), #sqrt de variables # es el número de predictores (si los toma todos es bagging; solo un subconjunto es random forest)
  splitrule = "gini" # empleamos el índice de gini como regla de partición
)

mod_rf_1 <- train(
  pobre ~ . - id - IngresoPerCapita,
  data = train_1,
  method = "ranger", 
  trControl = cv,
  maximize = F,
  metric = "Accuracy"
)











#creo la grilla
tunegrid_rf <- expand.grid(
  min.node.size = seq(c(135,145,length.out=5)), # inicial c(3000, 6000, 9000, 12000)
  mtry = c(25, 26), #sqrt de variables #inicial c(6, 12, 18)
  splitrule = c("variance")
)

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

