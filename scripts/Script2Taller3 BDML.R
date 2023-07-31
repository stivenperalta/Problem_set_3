#### Ahora voy a hacer lo mismo pero agregando la variable de ingreso en la base de datos, para que luego sea predicho ####

rm(list = ls())
require("pacman")
p_load("tidyverse")
library(readr)
library(dplyr)

### IMPORTO LAS BASES DE DATOS DE TRAIN Y TEST TANTO DE HOGARES COMO DE PERSONAS ###


train_hogares<-read_csv("/Users/nicolas/Downloads/uniandes-bdml-202313-ps31/train_hogares.csv")
train_personas<-read_csv("/Users/nicolas/Downloads/uniandes-bdml-202313-ps31/train_personas.csv")
test_hogares<-read_csv("/Users/nicolas/Downloads/uniandes-bdml-202313-ps31/test_hogares.csv")
test_personas<-read_csv("/Users/nicolas/Downloads/uniandes-bdml-202313-ps31/test_personas.csv")


#### Renombrar las variables ##### Primero lo hago con la test personas

test_personas <- test_personas %>%
  rename(sexo = P6020, edad = P6040, jef_hog = P6050, cotizante= P6090, seg_soc= P6100, 
         max_educ= P6210, educ= P6210s1, actividad= P6240, tiempo_empresa =P6426, tipo_oficio= P6430,
         ing_hor_ext= P6510, prima= P6545, bonif= P6580, aux_alim= P6585s1, sub_trans= P6585s2,
         subsid_fam= P6585s3, subsid_educ= P6585s4, alim_trab = P6590, viv_pag_trab = P6600,
         ing_esp= P6620, prima_serv= P6630s1, prima_nav= P6630s2, prima_vac= P6630s3,
         viat_perm= P6630s4, bonif_anual= P6630s6, personas_empresa= P6870, fondo_pensiones= P6920,
         otro_trab= P7040, ocup_seg_activ= P7050, dilig_trab_mas_h= P7110, disp_trab_mas_h= P7120, dilig_camb_trab= P7150,
         disp_camb_trab= P7160, primer_trab= P7310, ocupacion=P7350, ingr_trab= P7422, ingr_trab_d= P7472,
         pagos_arr_pen= P7495, pag_pen_jub= P7500s2, pag_pen_alim= P7500s3, din_otr_per= P7505,
         din_otr_per_res= P7510s1, din_otr_per_no_res= P7510s2, ayud_inst= P7510s3, din_prest_CDT= P7510s5,
         din_cesant= P7510s6, din_otr_fuent= P7510s7, hor_trab_sem= P6800, hor_trab_seg_sem= P7045, deseo_hor= P7090,
         oficio= Oficio, tr_empr= P6610, pet= Pet, ocupado= Oc, desocupado= Des, inactivo= Ina, fex_c= Fex_c,
         depto= Depto, fex_dpto=Fex_dpto)

names(test_personas)

###   Para el primer ejercicio, solo nos quedamos con las observaciones del jefe de hogar ###
test_personas_jef_hog <- subset(test_personas, jef_hog == 1)

###Pegamos la base de datos de personas a hogares ###
test_hogares <- merge(test_hogares, test_personas_jef_hog, by = "id")

### Ahora renombramos las variables del hogar ###
test_hogares <- test_hogares %>%
  rename(cuartos_hog= P5000, cuartos_dorm= P5010, vivienda= P5090, c_amortiz= P5100, arr_hip= P5130,
         arriendo= P5140, nper=Nper, npersug= Npersug)

### Miramos qué variables hay en la test para quedarnos con las que nos sirven y quitar las que no ###

names(test_hogares)

### Selecciono las variables que quiero para la nueva test_hogares ###
test_hogares <- test_hogares %>%
  select(-Clase.y,-Dominio.y, -fex_c, -fex_dpto, -depto)

###Renombro las de clase y dominio que estaban repetidas ###
test_hogares <- test_hogares %>%
  rename(clase= Clase.x, dominio= Dominio.x)

### Por ahora dejo asi la base de test personas#####################################################

test_personas_def <- test_personas
rm(test_personas)

### Vuelvo a cargar la test_personas original #####

test_personas <-read_csv("/Users/nicolas/Documents/Problem_set_3/stores/test_personas.csv")


### Ahora miro cuáles están en la test_personas que tambien esten en la train_personas, para meter esa información a la train_hogares#######

var_comm_personas <- intersect(names(test_personas), names(train_personas))
print(var_comm_personas)

### Vemos que hay 63 variables en comun### Entonces esas variables son las que voy a usar ###

### Ahora cambio el nombre de las variables de la train_personas### Me quedo solamente con las que estén en la test y la train ###

train_personas <- train_personas %>%
  rename(sexo = P6020, edad = P6040, jef_hog = P6050, cotizante= P6090, seg_soc= P6100, 
         max_educ= P6210, educ= P6210s1, actividad= P6240, tiempo_empresa =P6426, tipo_oficio= P6430,
         ing_hor_ext= P6510, prima= P6545, bonif= P6580, aux_alim= P6585s1, sub_trans= P6585s2,
         subsid_fam= P6585s3, subsid_educ= P6585s4, alim_trab = P6590, viv_pag_trab = P6600,
         ing_esp= P6620, prima_serv= P6630s1, prima_nav= P6630s2, prima_vac= P6630s3,
         viat_perm= P6630s4, bonif_anual= P6630s6, personas_empresa= P6870, fondo_pensiones= P6920,
         otro_trab= P7040, ocup_seg_activ= P7050, dilig_trab_mas_h= P7110, disp_trab_mas_h= P7120, dilig_camb_trab= P7150,
         disp_camb_trab= P7160, primer_trab= P7310, ocupacion=P7350, ingr_trab= P7422, ingr_trab_d= P7472,
         pagos_arr_pen= P7495, pag_pen_jub= P7500s2, pag_pen_alim= P7500s3, din_otr_per= P7505,
         din_otr_per_res= P7510s1, din_otr_per_no_res= P7510s2, ayud_inst= P7510s3, din_prest_CDT= P7510s5,
         din_cesant= P7510s6, din_otr_fuent= P7510s7, hor_trab_sem= P6800, hor_trab_seg_sem= P7045, deseo_hor= P7090,
         oficio= Oficio, tr_empr= P6610, pet= Pet, ocupado= Oc, desocupado= Des, inactivo= Ina, fex_c= Fex_c,
         depto= Depto, fex_dpto=Fex_dpto)

### Ahora miro las que esten en train y no en test, y las elimino ###

var_diff_personas <- setdiff(names(train_personas), names(test_personas))
print(var_diff_personas)

### Elimino las variables de la train_personas que no tengo en test_personas  ####
train_personas <- train_personas %>%
  select(-Estrato1, -P6500, -P6510s1, -P6510s2, -P6545s1, -P6545s2, -P6580s1, -P6580s2, -P6585s1a1, -P6585s1a2,
         -P6585s2a1, -P6585s2a2, -P6585s3a1, -P6585s3a2, -P6585s4a1, -P6585s4a2, -P6590s1, -P6600s1, -P6610s1, -P6620s1,  
         -P6630s1a1, -P6630s2a1, -P6630s3a1, -P6630s4a1, -P6630s6a1, -P6750, -P6760, -P550, -P7070, -P7140s1, -P7140s2, -P7422s1, 
         -P7472s1, -P7500s1, -P7500s1a1, -P7500s2a1, -P7500s3a1, -P7510s1a1, -P7510s2a1, -P7510s3a1, 
         -P7510s5a1, -P7510s6a1, -P7510s7a1, -Impa, -Isa, -Ie, -Imdi, -Iof1, -Iof2, -Iof3h,
         -Iof3i, -Iof6, -Cclasnr2, -Cclasnr3, -Cclasnr4, -Cclasnr5, -Cclasnr6, -Cclasnr7, -Cclasnr8, -Cclasnr11,
         -Impaes, -Isaes, -Iees, -Imdies, -Iof1es, -Iof2es, -Iof3hes, -Iof3ies, -Iof6es, -Ingtotob, 
         -Ingtotes, -Ingtot)

pers_diff <- setdiff(names(train_personas), names(test_personas))
print(pers_diff)

### Asignamos lo del jefe de hogar a la base train personas ###
train_personas_jef_hog <- subset(train_personas, jef_hog == 1)

###Pegamos la base de datos de train_personas a train_hogares ###
train_hogares <- merge(train_hogares, train_personas_jef_hog, by = "id")

train_hogares <- train_hogares %>%
  rename(cuartos_hog= P5000, cuartos_dorm= P5010, vivienda= P5090, c_amortiz= P5100, arr_hip= P5130,
         arriendo= P5140, nper=Nper, npersug= Npersug)

#### Vemos qué variables hay en train que no hay en hogares y nos disponemos a quitarlas para que queden igual ###
var_diff_hogares <- setdiff(names(train_hogares), names(test_hogares))
print(var_diff_hogares)

train_hogares <- train_hogares %>%
  select(-Clase.y,-Dominio.y, -fex_c, -fex_dpto, -depto, -Npobres, -Nindigentes, -Ingtotug, -Ingtotugarr, -Indigente)

#####Agregamos la variable Ingpcug a test_hogares #####

test_hogares <- test_hogares %>%
  mutate(Ingpcug = "")

train_hogares <- train_hogares %>%
  select(-Pobre)

train_hogares <- train_hogares %>%
  rename(clase= Clase.x, dominio= Dominio.x)

names(train_hogares)


###########Uno de nuevo las train y test para verificar los missing values de toda la base de hogares ##################
####

hogares_db <- rbind(train_hogares, test_hogares)

#### Miro cuantos missing values hay en cada variable para ver cómo imputar y también decidir por cuál quedarme ####

miss_values <- colSums(is.na(hogares_db))
print(miss_values[miss_values > 0])

### Cambio los valores NA de arriendo y arr_hip por 0 para poder sumarlos ###

hogares_db <- hogares_db %>%
  mutate(arr_hip = ifelse(is.na(arr_hip), 0, arr_hip),
         arriendo = ifelse(is.na(arriendo), 0, arriendo))

### Creo una dummy de si el hogar paga arriendo o no ###

hogares_db <- hogares_db %>%
  mutate(d_arriendo = ifelse(arriendo > 0, "1", "0"))

hogares_db <- hogares_db %>%
  mutate(arriendo= arr_hip + arriendo)

### Ordeno un poco la base a gusto

hogares_db <- hogares_db %>%
  select(id, clase, dominio, ocupado, desocupado, inactivo, d_arriendo, arr_hip, arriendo, everything())

### Imputo el arriendo por la media por hogar, a los que tienen valor 98 y 99 ###

n_val_arr <- c(98,99)

media_arr <- mean(hogares_db$arriendo[!hogares_db$arriendo %in% n_val_arr], na.rm = TRUE)
media_arr <- round(media_arr, -3)

### Reemplazo los valores que son 98 y 99 en la variable arriendo ###

hogares_db$arriendo <- ifelse(hogares_db$arriendo %in% c("98", "99"), media_arr, hogares_db$arriendo)

###Elimino la variable de arr_hip ###

hogares_db <- hogares_db %>%
  select(-arr_hip)


### Le pongo 0 a los missing values de ocupado, desocupado e inactivo, ya que son variables exlcluyentes ###
hogares_db <- hogares_db %>%
  mutate(
    ocupado = ifelse(is.na(ocupado), "0", ocupado),
    desocupado = ifelse(is.na(desocupado), "0", desocupado),
    inactivo = ifelse(is.na(inactivo), "0", inactivo),
  )


###### Para las variables que tienen valores missing y el jefe de hogar es inactivo o desocupado, se imputa, agregando No como respuesta ####


hogares_db  <- hogares_db %>%
  mutate(tiempo_empresa = ifelse(is.na(tiempo_empresa) & ocupado == "0", "0", tiempo_empresa)) %>%
  mutate(ing_hor_ext = ifelse(is.na(ing_hor_ext) & ocupado == "0", "2", ing_hor_ext)) %>%
  mutate(prima = ifelse(is.na(prima) & ocupado == "0", "2", prima)) %>%
  mutate(bonif = ifelse(is.na(bonif) & ocupado == "0", "2", bonif)) %>%
  mutate(sub_trans = ifelse(is.na(sub_trans) & ocupado == "0", "2", sub_trans)) %>%
  mutate(subsid_fam = ifelse(is.na(subsid_fam) & ocupado == "0", "2", subsid_fam)) %>%
  mutate(subsid_educ = ifelse(is.na(subsid_educ) & ocupado == "0", "2", subsid_educ)) %>%
  mutate(alim_trab = ifelse(is.na(alim_trab) & ocupado == "0", "2", alim_trab)) %>%
  mutate(viv_pag_trab = ifelse(is.na(viv_pag_trab) & ocupado == "0", "2", viv_pag_trab)) %>%
  mutate(tr_empr = ifelse(is.na(tr_empr) & ocupado == "0", "2", tr_empr)) %>%
  mutate(ing_esp = ifelse(is.na(ing_esp) & ocupado == "0", "2", ing_esp)) %>%
  mutate(hor_trab_sem = ifelse(is.na(hor_trab_sem) & ocupado == "0", "0", hor_trab_sem)) %>%
  mutate(otro_trab = ifelse(is.na(otro_trab) & ocupado == "0", "2", otro_trab)) %>%
  mutate(hor_trab_seg_sem = ifelse(is.na(hor_trab_seg_sem) & ocupado == "0", "0", hor_trab_seg_sem)) %>%
  mutate(deseo_hor = ifelse(is.na(deseo_hor) & ocupado == "0", "2", deseo_hor)) %>%
  mutate(ingr_trab_d = ifelse(is.na(ingr_trab_d) & desocupado == "0" & inactivo == "0", "2", ingr_trab_d))

missing_values <- colSums(is.na(hogares_db))
print(missing_values[missing_values > 0])


### Me quedo con las variables que me sirven ####
hogares_db <- hogares_db %>%
  select(-c_amortiz, -oficio, -tipo_oficio, -aux_alim, -tr_empr, -prima_serv, -prima_nav, -prima_vac,
         -viat_perm, bonif_anual, -personas_empresa, -hor_trab_seg_sem, -ocup_seg_activ, -dilig_trab_mas_h,
         -disp_trab_mas_h, -dilig_camb_trab, -disp_camb_trab, -primer_trab, -ocupacion, -ingr_trab, -pag_pen_jub,
         -pag_pen_alim, -din_otr_per_res, -din_otr_per_no_res,-ayud_inst, -din_prest_CDT, -din_cesant, -din_otr_fuent,
         -bonif_anual)

names(hogares_db)

############### Estadísticas descriptivas Parte 1 ############################################

### Se usa la funcion describe para ver todas las estadisticas descriptivas de cada una de las variables para cada base de datos ###
library(psych)
data_summary_train <- describe(train_final)
print(data_summary)

data_summary_test <- describe(test_final)
print(data_summary)

##### Para ver las frecuencias de cada una de las variables se usa lapply #####

### Primero los hacemos con la base train_final ###
vars_to_analyze_train <- c("d_arriendo", "Jefe_mujer", "Regimen_salud", "sexo", "edad", "seg_soc", "ing_hor_ext","prima", "bonif", "sub_trans", "subsid_fam", "subsid_educ", "alim_trab", "viv_pag_trab", "ing_esp", "bonif_anual",
                     "fondo_pensiones", "otro_trab", "deseo_hor", "pagos_arr_pen", "din_otr_per", "ocupado",
                     "desocupado", "inactivo", "Pobre")

lapply(train_final[vars_to_analyze], table)

### Luego lo hacemos con la base test_final ###
vars_to_analyze_test <- c("d_arriendo", "Porcentaje_ocupados", "Jefe_mujer", "Regimen_salud", "sexo", "edad", "seg_soc", "ing_hor_ext","prima", "bonif", "sub_trans", "subsid_fam", "subsid_educ", "alim_trab", "viv_pag_trab", "ing_esp", "bonif_anual",
                          "fondo_pensiones", "otro_trab", "deseo_hor", "pagos_arr_pen", "din_otr_per", "ocupado",
                          "desocupado", "inactivo", "Pobre")
lapply(test_final[vars_to_analyze], table)






