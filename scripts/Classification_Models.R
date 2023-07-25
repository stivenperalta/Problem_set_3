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

test<-load("../stores/test_final.RData")
train<-load("../stores/train_final.RData")

#vemos variables
names(test_final)
names(train_final)

table(train_final$Pobre)
glimpse(train_final)

#Mutación de factores (tenemos que hacerlo por niveles/levels)
train_final$Pobre <- factor(train_final$Pobre, levels = c("0", "1"), labels = c("No", "Si"))

# LOGIT  -------------------------------------------------------------------

#Logit
ctrl<- trainControl(method = "cv", #controla el entrenamiento, la validacion cruzada.
                    number = 10, #mejor 10. no sirve para dato espaciales
                    classProbs = TRUE, #probabilidad de las clases en lugar de raw predicciones
                    verbose=FALSE,
                    savePredictions = T) #que guarde las predicciones


set.seed(2023)

#hacemos la grilla para los hiperparámetros
hyperparameter_grid <- expand.grid(alpha = seq(0, 1, 0.1), # iremos variando los valores
                                   lambda = seq(0, 100, 1)) # iremos variando los valores


colnames(hyperparameter_grid) <- c("alpha", "lambda")

logit1 <- train(Pobre~., #especifico mi formula, dejo los que pueden crear multicolinealidad
                data = train_final,
                metric="Accuracy",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid = hyperparameter_grid,
                family= "binomial"
)

logit2 <- train(pobre~Dominio+cuartos+habitaciones+estado+amortizacion+ #especifico mi formula, dejo los que pueden crear multicolinealidad
                  arriendo_aprox+arriendo_real+Nper+Lp,
                data = hogares,
                metric="F1 Score",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid = hyperparameter_grid,
                family= "binomial",
)


#Adaptamos hiperparámetros en base a esto
logit1$bestTune

logit1

#predictions (probar ambas)
predictTest_logit <- data.frame(
  obs = hogares$pobre,                    ## observed class labels
  predict(logit1, type = "prob"),         ## predicted class probabilities
  pred = predict(logit1, type = "raw")    ## predicted class labels (esto luego lo sacamos porque vamos a variar el corte)
)

predictTest_logit2<- predictTest_logit2 %>%
  mutate(class_ROC= predict(logit1, newdata=hogares,type="raw"),
         p_hat_ROC= predict(logit1, newdata=hogares, type="prob")$Si,
         Default_num=ifelse(Defaulta=="No",0,1)
         )

head(predictTest_logit)
head(predictTest_logit2)

confusionMatrix(data = predictTest_logit$hat_default, reference=predictTest_logit$Default)


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

set.seed(1410)
mylogit_knn <- train(Default~duration+amount+installment+age+
                       history.buena+history.mala+
                       purpose.auto_nuevo+purpose.auto_usado+purpose.bienes+purpose.educacion+
                       foreign.extranjero+
                       +rent.TRUE, 
                     data = train, 
                     method = "knn",
                     trControl = ctrl,
                     tuneGrid = expand.grid(k=c(3,5,7,9,11)))


mylogit_knn
