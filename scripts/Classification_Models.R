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

hogares<-read_csv("../stores/test_hogares.csv")
personas<-read_csv("../stores/test_personas.csv")

#vemos variables
names(hogares)
names(personas)

#para facilidad cambiamos nombres de variables
hogares <- hogares %>%
  rename(
    cuartos = P5000,
    habitaciones=P5010,
    estado=P5090,
    amortizacion=P5100,
    arriendo_aprox=P5130,
    arriendo_real=P5140,
    personas_gasto=Npersug
    )

personas<-personas %>%
  rename(
    sexo=P6020,
    edad=P6040,
    parentesco=P6050,
    afil_seg_social=P6090,
    tipo_seg_social=P6100,
    nivel_educ=P6210,
    grado=P6210s1,
    actividad=P6240,
    tiempo_empr=P6426,
    posicion=P6430,
    ing_horas_ext=P6510,
    ing_otros=P6545,
    bonificacion=P6580,
    sub_ali=P6585s1,
    sub_trans=P6585s2,
    sub_fam=P6585s3,
    sub_edu=P6585s4,
    ing_alim=P6590,
    ing_viv=P6600,
    transp=P6610,
    ing_otro=P6620,
    prima_ser=P6630s1, 
    prima_nav=P6630s2,
    prima_vac=P6630s3,  
    viaticos=P6630s4,
    bono_anual=P6630s6,
    horas_trab_sem=P6800,
    tamano_empr=P6870,
    fondo_pens=P6920,
    seg_traba=P7040,
    horas_seg_trab=P7045,    
    seg_posicion=P7050,    
    traba_mas=P7090,
    busc_trab_mas=P7110,    
    disp_trab_mas=P7120,    
    camb_trab=P7150,
    emp_mes=P7160,
    busc_trab=P7310,    
    trab_desocup=P7350,    
    ing_desocup=P7422,    
    ing_desocup2=P7472,    
    ing_arr_pens=P7495,    
    ing_pens=P7500s2,
    ing_pens_alim=P7500s3,
    ing_otros_total=P7505,   
    ing_resid=P7510s1,  
    ing_fpais=P7510s2,  
    ing_instit=P7510s3,
    ing_inter=P7510s5,
    ing_cesantia=P7510s6,
    ing_otra_fuente=P7510s7
  )

#Creo una variable de pobreza en hogares para poder avanzar 
hogares <- hogares %>%
  mutate(pobre = rbinom(n(), 1, 0.5))

table(hogares$pobre)

#Mutación de factores (tenemos que hacerlo por niveles/levels)
hogares$pobre <- factor(hogares$pobre, levels = c("0", "1"), labels = c("No", "Si"))
hogares$Dominio<-as.factor(hogares$Dominio)

#Saco Li y personas_gasto por alta correlación
hogares <- hogares %>%
  select(-Li, -personas_gasto)

#reemplazo NAs por 0s (solo para armar formulas)
hogares <- replace(hogares, is.na(hogares), 0)

# ESTIMAMOS PROBABILIDADES  -------------------------------------------------------------------

#Logit
ctrl<- trainControl(method = "cv", #controla el entrenamiento, la validacion cruzada.
                    number = 10, #mejor 10. no sirve para dato espaciales
                    classProbs = TRUE, #probabilidad de las clases en lugar de raw predicciones
                    verbose=FALSE,
                    savePredictions = T) #que guarde las predicciones


set.seed(2023)

#hacemos la grilla para los hiperparámetros
hyperparameter_grid <- expand.grid(alpha = seq(0, 1, 0.1), # iremos variando los valores
                                   lambda = seq(0, 1, 0.1)) # iremos variando los valores

hyperparameter_grid2 <- expand.grid(alpha = seq(0, 1, 0.1), # iremos variando los valores
                                   lambda = seq(0, 1, 0.1),
                                   splitrule="gini") # iremos variando los valores


colnames(hyperparameter_grid) <- c("alpha", "lambda")
colnames(hyperparameter_grid2) <- c("alpha","lambda","gini")

logit1 <- train(pobre~Dominio+cuartos+habitaciones+estado+amortizacion+ #especifico mi formula, dejo los que pueden crear multicolinealidad
                arriendo_aprox+arriendo_real+Nper+Lp,
                data = hogares,
                metric="Accuracy",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid = hyperparameter_grid,
                family= "binomial"
)

logit2 <- train(pobre~Dominio+cuartos+habitaciones+estado+amortizacion+ #especifico mi formula, dejo los que pueden crear multicolinealidad
                  arriendo_aprox+arriendo_real+Nper+Lp,
                data = hogares,
                metric="ROC",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid = hyperparameter_grid2,
                family= "binomial"
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

#ROC


# RECOBRAMOS PROBABILIDADES PREDICHAS -------------------------------------


# CLASIFICACION -----------------------------------------------------------



