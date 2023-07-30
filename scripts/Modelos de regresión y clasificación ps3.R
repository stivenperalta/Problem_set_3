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
               ada, # Adaboost para clasificación,
               e1071,
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
                         y = train$pobre,
                         prop = 0.6)
table(down_train$pobre)

#Balanceo la muestra para nivelar con el valor de mayor frecuencia
set.seed(201718234)
up_train <- upSample(x = train[, -ncol(train)],
                         y = train$pobre)
table(up_train$pobre)

#Balanceo la muestra con ROSE
pacman::p_load(ROSE)

# Creo los parámetros e hiperparámetros de ajuste del modelo -------------------

# Creo control por valicación cruzada para clasificación
ctrl <- trainControl(method = "repeatedcv", # CV para clasificación
                     repeats = 5,
                     verboseIter = TRUE,
                     classProbs = TRUE, # guardar probabilidades
                     summaryFunction = twoClassSummary) # calcular métricas para accuracy

# defino la grilla
grid <- expand.grid(
  iter = seq(c(10,150,length.out=20)),          # Number of boosting iterations
  maxdepth = c(15),        # Maximum tree depth
  nu = c(0.1)           # Shrinkage parameter (learning rate)
)

# Creo control por valicación cruzada para regresión
ctrl2<-trainControl(method="cv",
                    number=5,
                    verboseIter = TRUE,
                    savePredictions= TRUE) #que guarde las predicciones

#creo la grilla para random forest
tunegrid_rf <- expand.grid(
  min.node.size = seq(c(10,150,length.out=5)), # controla la profundidad del árbol
  mtry = c(5, 8, 15), #sqrt de variables # es el número de predictores (si los toma todos es bagging; solo un subconjunto es random forest)
  splitrule = "gini" # empleamos el índice de gini como regla de partición
)

set.seed(201718234)
mod_adaboost_1 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = train,
  metric = "Accuracy",
  method = "ada", 
  trControl = ctrl,
  tuneGrid = grid
)

set.seed(201718234)
#mod_adaboost_2 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = down_train,
  metric = "Accuracy",
  method = "ada", 
  trControl = ctrl,
  tuneGrid = grid
)

set.seed(201718234)
#mod_adaboost_3 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = up_train,
  metric = "Accuracy",
  method = "ada", 
  trControl = ctrl,
  tuneGrid = grid
)

# modelos de elastic net ------------------------------------------------------

# Elastic Net regresión con data desbalanceada
mod_en_1 <- train(
  IngresoPerCapita ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = train,
  method = "glmnet", 
  trControl = ctrl2, 
  metric = "MAE",
  tuneGrid = expand.grid(alpha = seq(0.6, 0.9, length.out =5),
                         lambda = seq(862.3046, 867.3046, length.out =10))
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

# Realizar la predicción
test$IngresoPerCapita <- predict(mod_en_1, newdata = test_relevant)
test$pobre <- ifelse(test$IngresoPerCapita>test$Lp, 0, 1)
test1_EN <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
head(test1_EN) #evalúo que la base esté correctamente creada
write.csv(test1_EN,"../stores/regresion_en_3.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Elastic Net regresión con data balanceada (upsample)
mod_en_3 <- train(
  IngresoPerCapita ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = up_train,
  method = "glmnet", 
  trControl = ctrl2, 
  metric = "MAE",
  tuneGrid = expand.grid(alpha = seq(0.4, 0.7, length.out =5),
                         lambda = seq(70519.15, 74519.15, length.out =10))
)
prop.table(table(train$pobre))
mod_en_3$bestTune # Evalúo los mejores hiperparámetros para ajustar la grilla
varImp(mod_en_3)

# Omito variables menos relevantes
mod_en_4 <- train(
  IngresoPerCapita ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper + seg_soc +
    d_arriendo + Tipodevivienda + sexo + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = up_train,
  method = "glmnet", 
  trControl = ctrl2, 
  metric = "MAE",
  tuneGrid = expand.grid(alpha = seq(0.4, 0.7, length.out =5),
                         lambda = seq(70519.15, 74519.15, length.out =10))
)

mod_en_4$bestTune # Evalúo los mejores hiperparámetros para ajustar la grilla
varImp(mod_en_4)

#Evalúo la predicción dentro de muestra
train_prueba$ingreso_en_4 <- predict(mod_en_4, newdata = train_prueba)
train_prueba$pred_pobre_1 <- ifelse(train_prueba$ingreso_en_4>train$Li, 0, 1)
train_prueba$pobre <- ifelse(train_prueba$pobre == "Si", 1, 0)
train_prueba$pred_pobre_1 <- factor(train_prueba$pred_pobre_1, levels = c(0, 1))
train_prueba$pobre <- factor(train_prueba$pobre, levels = c(0, 1))
conf_matrix <- confusionMatrix(train_prueba$pred_pobre_1, train_prueba$pobre)
print(conf_matrix) # observo la matriz de confusión

# Realizar la predicción
test$IngresoPerCapita_en_3 <- predict(mod_en_3, newdata = test_relevant)
test$pobre <- ifelse(test$IngresoPerCapita_en_3>test$Li, 0, 1)
test1_EN <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
head(test1_EN) #evalúo que la base esté correctamente creada
write.csv(test1_EN,"../stores/regresion_uptrain_en_3.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Elastic Net clasificación con balance downsample
mod_en_2 <- train(
  pobre ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = down_train,
  method = "glmnet", 
  trControl = cv,
  metric = "Accuracy"#
)#,
#  tuneGrid = expand.grid(alpha = seq(0.50, 0.60, length.out =7),
#                        lambda = seq(0.002000000, 0.003005342, length.out =3)) # bestTune = alpha  0.55 lambda 0.002705342
#)

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

# Segundo modelo de regresión
pacman::p_load("e1071", "ranger", "dplyr")
gridreg <- expand.grid( # defino la grilla del modelo
  min.node.size = seq(c(10,150,length.out=5)), 
  mtry = c(5, 10), #sqrt de variables #inicial c(6, 12, 18)
  splitrule = c("variance")
)
train$IngresoPerCapita <- as.numeric(train$IngresoPerCapita) # defino el tipo correcto de la variable Y numérica
mod_rf_reg2 <- train(
  IngresoPerCapita ~ Porcentaje_ocupados + v.cabecera + cuartos_hog + nper +
    d_arriendo + Jefe_mujer + PersonaxCuarto + Tipodevivienda + Educacion_promedio +
    sexo + edad + seg_soc + Nivel_educativo + Tipo_de_trabajo + ocupado,
  data = train,
  method = "ranger", 
  trControl = ctrl2,
  maximize = F,
  metric = "MAE",
  tuneGrid = gridreg # bestTune = alpha  0.55 lambda 31446558
)

mod_rf_reg2$bestTune

#Evalúo la predicción dentro de muestra Random forest
train_prueba$pobre_rf <- predict(mod_rf_reg2, newdata = train_prueba)
train_prueba$pobre_rf <- ifelse(train_prueba$pobre_rf>train$Lp, 0, 1)
train_prueba$pobre <- ifelse(train_prueba$pobre == "Si", 1, 0)
train_prueba$pobre_rf <- factor(train_prueba$pobre_rf, levels = c(0, 1))
train_prueba$pobre <- factor(train_prueba$pobre, levels = c(0, 1))
conf_matrix <- confusionMatrix(train_prueba$pobre_rf, train_prueba$pobre)
print(conf_matrix) # observo la matriz de confusión
table(train_prueba$pobre_rf)

# Realizar la predicción en test
test$pobre_rf <- predict(mod_rf_reg2, newdata = test_relevant)
test$pobre <- ifelse(test$pobre_rf>test$Li, 0, 1)
test1_rf <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
head(test1_rf) #evalúo que la base esté correctamente creada
write.csv(test1_rf,"../stores/regresion_rf_1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

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
=======

# RESULTADOS --------------------------------------------------------------

#Elastic net con data desbalanceada
alpha   lambda
50   0.9 867.3046

Confusion Matrix and Statistics

Reference
Prediction      0      1
0 124360  19032
1   7576  13992

Accuracy : 0.8387          
95% CI : (0.8369, 0.8405)
No Information Rate : 0.7998          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.421           

Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9426          
            Specificity : 0.4237          
         Pos Pred Value : 0.8673          
         Neg Pred Value : 0.6487          
             Prevalence : 0.7998          
         Detection Rate : 0.7539          
   Detection Prevalence : 0.8693          
      Balanced Accuracy : 0.6831          
                                          
       'Positive' Class : 0  ' 

#Elastic net con data balanceada (UPSAMPLE)

alpha   lambda
40 0.625 74519.15

glmnet variable importance

only 20 most important variables shown (out of 34)

Overall
Porcentaje_ocupados                  100.0000
Nivel_educativoUniversitaria          83.3226
Tipo_de_trabajo3                      62.5434
seg_soc2                              51.9053
seg_soc1                              41.4927
Tipo_de_trabajo6                      26.3309
seg_soc3                              21.1317
cuartos_hog                           10.3566
nper                                  10.2279
d_arriendo                             5.8454
v.cabeceraVive en cabecera             5.3428
Nivel_educativoBásica primaria         4.9496
Tipo_de_trabajo5                       4.2550
TipodeviviendaArriendo o subarriendo   3.0775
Educacion_promedio                     1.9954
edad                                   0.6333
Jefe_mujerJefe de hogar es mujer       0.0000
TipodeviviendaOtra                     0.0000
Nivel_educativoMedia                   0.0000
Nivel_educativoPreescolar              0.0000

Confusion Matrix and Statistics

Reference
Prediction      0      1
0 155364      0
1   9596      0

Accuracy : 0.9418         
95% CI : (0.9407, 0.943)
No Information Rate : 1              
P-Value [Acc > NIR] : 1              

Kappa : 0              

Mcnemar's Test P-Value : <2e-16         
                                         
            Sensitivity : 0.9418         
            Specificity :     NA         
         Pos Pred Value :     NA         
         Neg Pred Value :     NA         
             Prevalence : 1.0000         
         Detection Rate : 0.9418         
   Detection Prevalence : 0.9418         
      Balanced Accuracy :     NA         
                                         
       'Positive' Class : 0              
                                ' 
