#Classification Models

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, tidyverse, caret, dplyr, tidyr, glmnet, pROC,reshape2, klaR) # Cargar paquetes requeridos

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
train$otro_trab <- factor(train$otro_trab, levels = c("0", "1"), labels = c("No", "Si"))
test$otro_trab <- factor(test$otro_trab, levels = c("0", "1"), labels = c("No", "Si"))

#Correlation matrix
numeric_columns <- sapply(train, is.numeric)
cor_matrix <- cor(train[, numeric_columns])

cor_melted <- melt(cor_matrix)
ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# TRAIN CONTROL  -------------------------------------------------------------------

#Logit
ctrl<- trainControl(method = "cv", #controla el entrenamiento, la validacion cruzada.
                    number = 10, #mejor 10. no sirve para dato espaciales
                    classProbs = TRUE, #probabilidad de las clases en lugar de raw predicciones
                    verbose=FALSE,
                    savePredictions = T) #que guarde las predicciones

ctrl2<- trainControl(method = "cv", #controla el entrenamiento, la validacion cruzada.
                     number = 100, #mejor 10. no sirve para dato espaciales
                     classProbs = TRUE, #probabilidad de las clases en lugar de raw predicciones
                     verbose=FALSE,
                     savePredictions = T
                     )

set.seed(2023)

# Grilla ------------------------------------------------------------------

 
#hacemos la grilla para los hiperparámetros
hyperparameter_grid <- expand.grid(alpha = seq(0.855, 0.856, 0.01), # iremos variando los valores
                                   lambda = seq(0, 0.0005, 0.0001)) # iremos variando los valores


colnames(hyperparameter_grid) <- c("alpha", "lambda")


# LOGIT -------------------------------------------------------------------


#LOGIT1
set.seed(2023)
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

#LOGIT2
set.seed(2023)
logit2 <- train(pobre~Porcentaje_ocupados+ + v.cabecera+ cuartos_hog + nper+ npersug
                + d_arriendo + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
                + Educacion_promedio +edad+ seg_soc+ Nivel_educativo+ Tipo_de_trabajo+otro_trab
                + fondo_pensiones +ocupado, #especifico mi formula. primero utilizaremos todos los predictores "."
                data = train,
                metric="Accuracy", #metrica de performance
                method = "glmnet", #logistic regression with elastic net regularization
                trControl = ctrl,
                tuneGrid = hyperparameter_grid,
                family= "binomial"
)

#para tune logit2
plot(logit2$results$lambda,
     logit2$results$Accuracy,
     xlab="lambda",
     ylab="Accuracy")

#LOGIT3

#creo iteracciones
#creando interacciones
train$int1<- interaction(train$v.cabecera,train$Jefe_mujer)
train$int2<- interaction(train$d_arriendo,train$Jefe_mujer)
train$int3<- interaction(train$Tipodevivienda, train$Jefe_mujer)
train$int4<- interaction(train$edad, train$Jefe_mujer)
train$int5<- interaction(train$Nivel_educativo, train$Jefe_mujer)
train$int6<- interaction(train$otro_trab, train$Jefe_mujer)
train$int7<- interaction(train$fondo_pensiones, train$Jefe_mujer)
train$int8<- interaction(train$ocupado, train$Jefe_mujer)

set.seed(2023)
logit3 <- train(pobre~Porcentaje_ocupados+ + v.cabecera +cuartos_hog + nper
                + d_arriendo + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
                + Educacion_promedio +edad+ seg_soc+ Nivel_educativo+ Tipo_de_trabajo
                +otro_trab + fondo_pensiones +ocupado + int1 + int2 +int3 +int4
                +int5 +int6+ int7 + int8, #especifico mi formula. primero utilizaremos todos los predictores "."
                data = train,
                metric="Accuracy", #metrica de performance
                method = "glmnet", #logistic regression with elastic net regularization
                trControl = ctrl,
                tuneGrid = hyperparameter_grid,
                family= "binomial"
)


#para tune logit3
plot(logit3$results$lambda,
     logit3$results$Accuracy,
     xlab="lambda",
     ylab="Accuracy")

#LOGIT4
set.seed(2023)
down_train <- downSample(x = train[, -ncol(train)], #hacemos esto para balancear las muestras
                         y = train$pobre)

set.seed(2023)
logit4 <- train(pobre~Porcentaje_ocupados+ + v.cabecera +cuartos_hog + nper
                + d_arriendo + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
                + Educacion_promedio +edad+ seg_soc+ Nivel_educativo+ Tipo_de_trabajo
                +otro_trab + fondo_pensiones +ocupado + int1 + int2 +int3 +int4
                +int5 +int6+ int7,
                data = down_train,
                metric="Accuracy", #metrica de performance
                method = "glmnet", #logistic regression with elastic net regularization
                trControl = ctrl2,
                tuneGrid = hyperparameter_grid,
                family= "binomial"
)

#para tune logit4
plot(logit4$results$lambda,
     logit4$results$Accuracy,
     xlab="lambda",
     ylab="Accuracy")

# LOGIT BESTUNES ----------------------------------------------------------

#Adaptamos hiperparámetros en base a esto
logit1$bestTune
logit2$bestTune
logit3$bestTune
logit4$bestTune

logit1
logit2
logit3
logit4

# LOGIT Confusion Matrix y cambiando cutoffs ---------------------------------------------


#Logit1
predictTest_logit <- data.frame(
  obs = train$pobre,                    ## observed class labels
  predict(logit1, type = "prob"),         ## predicted class probabilities
  pred = predict(logit1, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)

head(predictTest_logit)

confusionMatrix(data = predictTest_logit$pred, reference=predictTest_logit$obs)

#Logit2
predictTest_logit2 <- data.frame(
  obs = train$pobre,                    ## observed class labels
  predict(logit2, type = "prob"),         ## predicted class probabilities
  pred = predict(logit2, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)

head(predictTest_logit2)

confusionMatrix(data = predictTest_logit2$pred, reference=predictTest_logit2$obs)

#Evaluando los cortes/thresholds
roc_data <- roc(predictTest_logit2$obs, predictTest_logit2$Si)
plot(roc_data, main = "ROC Curve", col = "purple", lwd = 2) #vemos nuestra curva ROC. Estamos muy alto en sensitivity y bajo en specificity
mycoords <- coords(roc_data, "all")

plot(mycoords$threshold, mycoords$sensitivity, type = "l", col = "red",
     xlab = "Cutoff", ylab = "Sensitivity", main = "Sensitivity vs. Cutoff")
lines(mycoords$threshold, mycoords$specificity, col = "blue")
legend("bottomright", legend = c("Sensitivity", "Specificity"), col = c("red", "blue"), lwd = 2)

#Nueva matiz
predicted_probabilities <- predictTest_logit2$Si
new_cutoff<-0.35
predictTest_logit2$new_thres <- factor(ifelse(predicted_probabilities > new_cutoff, "Si", "No"))

confusionMatrix(data = predictTest_logit2$new_thres, reference=predictTest_logit2$obs)

#Logit3

predictTest_logit3 <- data.frame(
  obs = train$pobre,                    ## observed class labels
  predict(logit3, type = "prob"),         ## predicted class probabilities
  pred = predict(logit3, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)

head(predictTest_logit3)

confusionMatrix(data = predictTest_logit3$pred, reference=predictTest_logit3$obs)

#Logit4

predictTest_logit4 <- data.frame(
  obs = up_train$pobre,                    ## observed class labels
  predict(logit4, type = "prob"),         ## predicted class probabilities
  pred = predict(logit4, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)

head(predictTest_logit4)

confusionMatrix(data = predictTest_logit4$pred, reference=predictTest_logit4$obs)

#Evaluando los cortes/thresholds
roc_data <- roc(predictTest_logit4$obs, predictTest_logit4$Si)
plot(roc_data, main = "ROC Curve", col = "purple", lwd = 2) #vemos nuestra curva ROC. Estamos muy alto en sensitivity y bajo en specificity
mycoords <- coords(roc_data, "all")

plot(mycoords$threshold, mycoords$sensitivity, type = "l", col = "red",
     xlab = "Cutoff", ylab = "Sensitivity", main = "Sensitivity vs. Cutoff")
lines(mycoords$threshold, mycoords$specificity, col = "blue")
legend("bottomright", legend = c("Sensitivity", "Specificity"), col = c("red", "blue"), lwd = 2)

#Nueva matiz
predicted_probabilities <- predictTest_logit4$Si
new_cutoff<-0.50
predictTest_logit4$new_thres <- factor(ifelse(predicted_probabilities > new_cutoff, "Si", "No"))

confusionMatrix(data = predictTest_logit4$new_thres, reference=predictTest_logit4$obs)




#Calculando Youden J statistic
#Youden's J = Sensitivity + Specificity - 1
#youden_j <- mycoords$sensitivities + mycoords$specificities - 1
#optimal_threshold <- mycoords$thresholds[which.max(youden_j)]


# Predicción Kaggle LOGIT -------------------------------------------------


# Logit1: Exporto la predicción en csv para cargar en Kaggle
test$pobre <- predict(logit1, newdata = test) #adaptamos 
test_logit1 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_logit1$pobre <- ifelse(test_logit1$pobre == "No", 0, 1)
head(test_logit1) #evalúo que la base esté correctamente creada
write.csv(test_logit1,"../stores/test_logit1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Logit2: Exporto la predicción en csv para cargar en Kaggle
test$pobre <- predict(logit2, newdata = test) #adaptamos 
test_logit2 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_logit2$pobre <- ifelse(test_logit2$pobre == "No", 0, 1)
head(test_logit2) #evalúo que la base esté correctamente creada
write.csv(test_logit2,"../stores/logit2.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Logit2.1: Exporto prediccion con logit 2 pero con corte de 2.5
test$pobre <- predict(logit2, newdata = test, type="prob") #adaptamos 
test$pobre <- test$pobre$Si
test$pobre <- as.factor(ifelse(test$pobre > 0.35, "Si", "No"))
test_logit2_1 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_logit2_1$pobre <- ifelse(test_logit2_1$pobre == "No", 0, 1)
head(test_logit2_1) #evalúo que la base esté correctamente creada
write.csv(test_logit2_1,"../stores/logit2_1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Logit3: Exporto la predicción en csv para cargar en Kaggle

#interacciones para test
test$int1<- interaction(test$v.cabecera,test$Jefe_mujer)
test$int2<- interaction(test$d_arriendo,test$Jefe_mujer)
test$int3<- interaction(test$Tipodevivienda, test$Jefe_mujer)
test$int4<- interaction(test$edad, test$Jefe_mujer)
test$int5<- interaction(test$Nivel_educativo, test$Jefe_mujer)
test$int6<- interaction(test$otro_trab, test$Jefe_mujer)
test$int7<- interaction(test$fondo_pensiones, test$Jefe_mujer)
test$int8<- interaction(test$ocupado, test$Jefe_mujer)


test$pobre <- predict(logit3, newdata = test) #adaptamos 
test_logit3 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_logit3$pobre <- ifelse(test_logit3$pobre == "No", 0, 1)
head(test_logit3) #evalúo que la base esté correctamente creada
write.csv(test_logit3,"../stores/logit3.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Logit3: Exporto la predicción en csv para cargar en Kaggle

test$pobre <- predict(logit4, newdata = test) #adaptamos 
test_logit4 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_logit4$pobre <- ifelse(test_logit4$pobre == "No", 0, 1)
head(test_logit4) #evalúo que la base esté correctamente creada
write.csv(test_logit4,"../stores/logit4.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle



# LDA -------------------------------------

cuartos_dorm + nper+Li
+ d_arriendo
+ocupado + desocupado+ inactivo

#LDA1
set.seed(1234)
lda_1 = train(pobre~cuartos_hog+ nper+Li #saco npersug y cuartos hogar porque tiene muy alta correlacion con nper
              + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
              + Educacion_promedio +edad+ seg_soc+ Nivel_educativo+ otro_trab
              +ocupado, 
              data=train, 
              method="lda",
              trControl = ctrl,
              metric="Accuracy")

lda_1

#LDA2
set.seed(2023)
lda_2 <- train(pobre ~ cuartos_hog+ nper +Porcentaje_ocupados + v.cabecera
               + Jefe_mujer+ Tipodevivienda + Educacion_promedio +edad
               + seg_soc+ Nivel_educativo+ otro_trab +ocupado + Tipo_de_trabajo
               + v.cabecera*Jefe_mujer + Tipodevivienda*Jefe_mujer+ edad*Jefe_mujer
               + Nivel_educativo*Jefe_mujer+ otro_trab*Jefe_mujer+ ocupado*Jefe_mujer,
              #se sacan fondo_pens y otro_trab porque tienen near zero variance (constant_vars <- nearZeroVar(train, saveMetrics = TRUE) )
               data = train,
               method = "lda",
               trControl = ctrl,
               metric = "Accuracy")
lda_2

# Exporto la predicción en csv para cargar en Kaggle

#LDA1
test$pobre <- predict(lda_1, newdata = test) #adaptamos 
test_lda <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_lda$pobre <- ifelse(test_lda$pobre == "No", 0, 1)
head(test_lda) #evalúo que la base esté correctamente creada
write.csv(test_lda,"../stores/lda1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

#LDA2
test$pobre <- predict(lda_2, newdata = test) #adaptamos 
test_lda2 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_lda2$pobre <- ifelse(test_lda2$pobre == "No", 0, 1)
head(test_lda2) #evalúo que la base esté correctamente creada
write.csv(test_lda2,"../stores/lda2.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle



# QDA ---------------------------------------------------------------------
#QDA1
set.seed(1234)
qda_1 = train(pobre~cuartos_hog+ nper+Li #saco npersug y cuartos hogar porque tiene muy alta correlacion con nper
              + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
              + Educacion_promedio +edad+ seg_soc+ Nivel_educativo+ otro_trab
              +ocupado, 
              data=train, 
              method="qda",
              trControl = ctrl,
              metric="Accuracy")

qda_1

#QDA2
set.seed(2023)
qda_2 <- train(pobre ~ cuartos_hog+ nper +Porcentaje_ocupados + v.cabecera
               + Jefe_mujer+ Tipodevivienda + Educacion_promedio +edad
               + seg_soc+ Nivel_educativo+ otro_trab +ocupado + Tipo_de_trabajo
               +v.cabecera*Jefe_mujer + Tipodevivienda*Jefe_mujer + edad*Jefe_mujer
               + otro_trab*Jefe_mujer + ocupado*Jefe_mujer,
               #se sacan fondo_pens y otro_trab porque tienen near zero variance (constant_vars <- nearZeroVar(train, saveMetrics = TRUE) )
               data = train,
               method = "qda",
               trControl = ctrl,
               metric = "Accuracy")
qda_2


# Exporto la predicción en csv para cargar en Kaggle

#QDA1
test$pobre <- predict(qda_1, newdata = test) #adaptamos 
test_qda <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_qda$pobre <- ifelse(test_qda$pobre == "No", 0, 1)
head(test_qda) #evalúo que la base esté correctamente creada
write.csv(test_qda,"../stores/qda1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

#QDA2
test$pobre <- predict(qda_2, newdata = test) #adaptamos 
test_qda2 <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_qda2$pobre <- ifelse(test_qda2$pobre == "No", 0, 1)
head(test_qda2) #evalúo que la base esté correctamente creada
write.csv(test_qda2,"../stores/qda2.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle



# KNN ---------------------------------------------------------------------

set.seed(2009)
mylogit_knn <- train(pobre~cuartos_hog+ cuartos_dorm + nper+ npersug+Li
                     + d_arriendo + Jefe_mujer+ PersonaxCuarto+ Tipodevivienda
                     + Educacion_promedio + sexo +edad+ seg_soc+ Nivel_educativo+ otro_trab
                     +ocupado + desocupado+ inactivo, 
                     data = train, 
                     method = "knn",
                     trControl = ctrl,
                     tuneGrid = expand.grid(k=c(5,10,11))) #el mejor fue el 11


mylogit_knn

# Exporto la predicción en csv para cargar en Kaggle
test$pobre <- predict(mylogit_knn, newdata = test) #adaptamos 
test_knn <- test %>% #organizo el csv para poder cargarlo en kaggle
  select(id,pobre)
test_knn$pobre <- ifelse(test_knn$pobre == "No", 0, 1)
head(test_knn) #evalúo que la base esté correctamente creada
write.csv(test_knn,"../stores/knn1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle


# Naive Bayes -------------------------------------------------------------

hist(train$cuartos_dorm, main = "Histograma de Número cuartos", xlab = "Número de cuartos", col = "skyblue", border = "white")
hist(train$nper, main = "Histograma de Número de personas", xlab = "Número de personas", col = "skyblue", border = "white")
hist(train$Porcentaje_ocupados, main = "Histograma de Porcentaje ocupados", xlab = "Porcentaje ocupados", col = "skyblue", border = "white")
hist(train$edad, main = "Histograma de edad", xlab = "Edad", col = "skyblue", border = "white")




set.seed(2023)
nb1 <- train(pobre ~
              otro_trab +ocupado + Tipo_de_trabajo + seg_soc + cuartos_dorm +nper
             +Porcentaje_ocupados +edad + sexo, 
              data = train, 
              method = "nb",
              trControl = ctrl,
              tuneGrid=expand.grid(fL=seq(0,1,0.1), #fl es la correción de laplace, trata de satisface el problema, warning de las probabilidades=0 (ajusta probabilidades)
                                  usekernel=TRUE, #otro parámetro
                                  adjust=seq(0,1,0.1)))
nb1

v.cabecera
Jefe_mujer
Tipodevivienda
Nivel_educativo

# Resultados de tune grid -------------------------------------------------

#LOGIT

#Logit 1
alpha lambda
37  0.86      0

Confusion Matrix and Statistics

Reference
Prediction     No     Si
No 125412  21862
Si   6524  11162

Accuracy : 0.8279          
95% CI : (0.8261, 0.8297)
No Information Rate : 0.7998          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.3494          

Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9506          
            Specificity : 0.3380          
         Pos Pred Value : 0.8516          
         Neg Pred Value : 0.6311          
             Prevalence : 0.7998          
         Detection Rate : 0.7603          
   Detection Prevalence : 0.8928          
      Balanced Accuracy : 0.6443          
                                          
       'Positive' Class : No  ' 

#Logit2

> logit2$bestTune
   alpha lambda
53 0.865  0.001

Confusion Matrix and Statistics

Reference
Prediction     No     Si
No 124452  16688
Si   7484  16336

Accuracy : 0.8535          
95% CI : (0.8518, 0.8552)
No Information Rate : 0.7998          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.489           

Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9433          
            Specificity : 0.4947          
         Pos Pred Value : 0.8818          
         Neg Pred Value : 0.6858          
             Prevalence : 0.7998          
         Detection Rate : 0.7544          
   Detection Prevalence : 0.8556          
      Balanced Accuracy : 0.7190          
                                          
       'Positive' Class : No  ' 

#Logit 2.1- con cutoff en 3.5
Confusion Matrix and Statistics

Reference
Prediction     No     Si
No 116367  10977
Si  15569  22047

Accuracy : 0.8391          
95% CI : (0.8373, 0.8408)
No Information Rate : 0.7998          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.5224          

Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.8820          
            Specificity : 0.6676          
         Pos Pred Value : 0.9138          
         Neg Pred Value : 0.5861          
             Prevalence : 0.7998          
         Detection Rate : 0.7054          
   Detection Prevalence : 0.7720          
      Balanced Accuracy : 0.7748          
                                          
       'Positive' Class : No     ' 

#Logit3

> logit3$bestTune
alpha lambda
23  0.86  0.001

Confusion Matrix and Statistics

Reference
Prediction     No     Si
No 124467  16613
Si   7469  16411

Accuracy : 0.854           
95% CI : (0.8523, 0.8557)
No Information Rate : 0.7998          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.4913          

Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.9434          
            Specificity : 0.4969          
         Pos Pred Value : 0.8822          
         Neg Pred Value : 0.6872          
             Prevalence : 0.7998          
         Detection Rate : 0.7545          
   Detection Prevalence : 0.8552          
      Balanced Accuracy : 0.7202          
                                          
       'Positive' Class : No     '

#LOGIT4- con 100 folds y downtrain. no tuvo mejora en accuracy asi que no se subio a kaggle
> logit4$bestTune
alpha lambda
3 0.855  2e-04

Confusion Matrix and Statistics

Reference
Prediction     No     Si
No 104386  23624
Si  27550 108312

Accuracy : 0.8061          
95% CI : (0.8046, 0.8076)
No Information Rate : 0.5             
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.6121          

Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.7912          
            Specificity : 0.8209          
         Pos Pred Value : 0.8155          
         Neg Pred Value : 0.7972          
             Prevalence : 0.5000          
         Detection Rate : 0.3956          
   Detection Prevalence : 0.4851          
      Balanced Accuracy : 0.8061          
                                          
       'Positive' Class : No  ' 


#KNN
Accuracy was used to select the optimal model using the largest value.
The final value used for the model was k = 11.

k-Nearest Neighbors 

164960 samples
18 predictor
2 classes: 'No', 'Si' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 148464, 148464, 148465, 148465, 148464, 148464, ... 
Resampling results across tuning parameters:
  
  k   Accuracy   Kappa    
11  0.8062440  0.1749429

Accuracy was used to select the optimal model using the largest value.
The final value used for the model was k = 11.

#LDA1
Linear Discriminant Analysis 

164960 samples
12 predictor
2 classes: 'No', 'Si' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 148464, 148464, 148463, 148465, 148465, 148464, ... 
Resampling results:
  
  Accuracy   Kappa   
0.8268247  0.360719

#LDA2
Linear Discriminant Analysis 

164960 samples
13 predictor
2 classes: 'No', 'Si' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 148465, 148464, 148463, 148464, 148464, 148465, ... 
Resampling results:
  
  Accuracy   Kappa    
0.8505032  0.4823988

#QDA1
Quadratic Discriminant Analysis 

164960 samples
12 predictor
2 classes: 'No', 'Si' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 148464, 148464, 148464, 148464, 148464, 148464, ... 
Resampling results:
  
  Accuracy   Kappa    
0.7830504  0.3196741

#QDA2
Quadratic Discriminant Analysis 

164960 samples
13 predictor
2 classes: 'No', 'Si' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 148464, 148463, 148463, 148463, 148465, 148465, ... 
Resampling results:
  
  Accuracy   Kappa    
0.7868028  0.3940926
