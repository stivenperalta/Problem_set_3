#Classification Models

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, tidyverse, caret, dplyr, tidyr, glmnet, pROC) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

#vemos que hay en el directorio de stores
dir("../stores")

test<-readRDS("../stores/test_final.rds")
train<-readRDS("../stores/train_final.rds")

#vemos variables
names(train)

#renombramos variable Pobre a pobre
test <- test %>%
  rename(pobre = Pobre)
train <- train %>%
  rename(pobre=Pobre)

table(train$pobre) #los datos estan desbalanceados
glimpse(train)

#Mutación de factores (tenemos que hacerlo por niveles/levels)
train$pobre <- factor(train$pobre, levels = c("0", "1"), labels = c("No", "Si"))
test$pobre <- factor(test$pobre, levels = c("0", "1"), labels = c("No", "Si"))


# LOGIT  -------------------------------------------------------------------

#Logit
ctrl<- trainControl(method = "cv", #controla el entrenamiento, la validacion cruzada.
                    number = 10, #mejor 10. no sirve para dato espaciales
                    classProbs = TRUE, #probabilidad de las clases en lugar de raw predicciones
                    verbose=FALSE,
                    savePredictions = T) #que guarde las predicciones

ctrl2<- trainControl(method = "cv", #controla el entrenamiento, la validacion cruzada.
                     number = 10, #mejor 10. no sirve para dato espaciales
                     classProbs = TRUE, #probabilidad de las clases en lugar de raw predicciones
                     verbose=FALSE,
                     savePredictions = T,
                     summaryFunction = twoClassSummary
                     )

set.seed(2023)

#hacemos la grilla para los hiperparámetros
hyperparameter_grid <- expand.grid(alpha = seq(0.85, 0.87, 0.01), # iremos variando los valores
                                   lambda = seq(0, 0.1, 0.01)) # iremos variando los valores


colnames(hyperparameter_grid) <- c("alpha", "lambda")

logit1 <- train(pobre~cuartos_hog+ cuartos_dorm + nper+ npersug+Li
                + d_arriendo + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
                + Educacion_promedio + sexo +edad+ seg_soc+ Nivel_educativo+ otro_trab
                +ocupado + desocupado+ inactivo, #especifico mi formula. primero utilizaremos todos los predictores "."
                data = train,
                metric="Accuracy", #metrica de performance
                method = "glmnet", #logistic regression with elastic net regularization
                trControl = ctrl,
                tuneGrid = hyperparameter_grid,
                family= "binomial"
)

#para tune logit1
plot(logit1$results$lambda,
     logit1$results$Accuracy,
     xlab="lambda",
     ylab="Accuracy")


logit2 <- train(pobre~cuartos_hog+ cuartos_dorm + nper+ npersug+Li
                + d_arriendo + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
                + Educacion_promedio + sexo +edad+ seg_soc+ Nivel_educativo+ otro_trab
                +ocupado + desocupado+ inactivo,
                data = train,
                metric="ROC",
                method = "glmnet",
                trControl = ctrl2,
                tuneGrid = hyperparameter_grid,
                tuneLength= 10,
                family= "binomial",
)

#para tune logit2
plot(logit2$results$lambda,
     logit2$results$Accuracy,
     xlab="lambda",
     ylab="Accuracy")


# LOGIT BESTUNES ----------------------------------------------------------

#Adaptamos hiperparámetros en base a esto
logit1$bestTune
logit2$bestTune

logit1
logit2



# Cambiando cortes para Logit ---------------------------------------------



predictTest_logit <- data.frame(
  obs = train$pobre,                    ## observed class labels
  predict(logit1, type = "prob"),         ## predicted class probabilities
  pred = predict(logit1, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)


predictTest_logit2 <- data.frame(
  obs = train$pobre,                    ## observed class labels
  predict(logit2, type = "prob"),         ## predicted class probabilities
  pred = predict(logit2, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)



head(predictTest_logit)
head(predictTest_logit2)

confusionMatrix(data = predictTest_logit$hat_default, reference=predictTest_logit$Default)
confusionMatrix(data = predictTest_logit2$hat_default, reference=predictTest_logit2$Default)


# Predicción Kaggle LOGIT -------------------------------------------------


# Exporto la predicción en csv para cargar en Kaggle
test$pobre <- predict(logit1, newdata = test) #adaptamos 
test_logit1 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_logit1$pobre <- ifelse(test_logit1$pobre == "No", 0, 1)
head(test_logit1) #evalúo que la base esté correctamente creada
write.csv(test_logit1,"../stores/test_logit1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Exporto la predicción en csv para cargar en Kaggle
test$pobre <- predict(logit2, newdata = test) #adaptamos 
test_logit2 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_logit2$pobre <- ifelse(test_logit2$pobre == "No", 0, 1)
head(test_logit2) #evalúo que la base esté correctamente creada
write.csv(test_logit2,"../stores/logit2.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle




# LDA -------------------------------------
lda_fit = train(Default~duration+amount+installment+age, 
                data=train, 
                method="lda",
                trControl = ctrl)

lda_fit


head(credit)



# QDA-----------------------------------------------------------

qda_fit= train(Default~duration+amount+installment+age,
               data=train,
               method="qda",
               trControl= ctrl)

qda_fit #empeoró el accuracy

# KNN ---------------------------------------------------------------------

set.seed(2009)
mylogit_knn <- train(pobre~cuartos_hog+ cuartos_dorm + nper+ npersug+Li
                     + d_arriendo + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
                     + Educacion_promedio + sexo +edad+ seg_soc+ Nivel_educativo+ otro_trab
                     +ocupado + desocupado+ inactivo, 
                     data = train, 
                     method = "knn",
                     trControl = ctrl,
                     tuneGrid = expand.grid(k=c(3,5,7,9,11)))


mylogit_knn

# Exporto la predicción en csv para cargar en Kaggle
test$pobre <- predict(mylogit_knn, newdata = test) #adaptamos 
test_knn <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_knn$pobre <- ifelse(test_knn$pobre == "No", 0, 1)
head(test_knn) #evalúo que la base esté correctamente creada
write.csv(test_knn,"../stores/knn1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle



# Resultados de tune grid -------------------------------------------------

#LOGIT
alpha lambda
37  0.86      0


#KNN
Accuracy was used to select the optimal model using the largest value.
The final value used for the model was k = 11.