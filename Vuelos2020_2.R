
install.packages("mlr", dependencies = TRUE)
install.packages("tidyverse")
install.packages("kernlab")
install.packages ("e1071")
install.packages("rpart.plot")
install.packages("caret")
install.packages("prodlim")

library(mlr)
library(tidyverse)
library(kernlab)
library(e1071)
library(rpart)
library(rpart.plot)
library(dplyr)
library(dslabs)
library(prodlim)
library(caret)
library(XML)


###################################################################
################### IMPORTAR BASE DE DATOS ########################
###################################################################

Vuelos2020 <- read.csv("Jan_2020_ontime.csv")
Vuelos2020

###################################################################
################### LIMPIEZA DE BASE DE DATOS #####################
###################################################################

#------------------- BORRAR VARIABLE x -------------------#
Vuelos2020$X <- NULL

#--------------------- ENCONTRAR NA ----------------------#
sapply(Vuelos2020, function(Vuelos2020)sum(is.na(Vuelos2020)))
## se encontraron NA en: DEP_TIME, DEP_DEL15, ARR_TIME, ARR_DEL15. 
## (OBS actuales: 607346)

#---------------------- SOLUCIONAR NA --------------------#
Vuelos2020 <- Vuelos2020[Vuelos2020$ARR_DEL15!="NA",]
sapply(Vuelos2020, function(Vuelos2020)sum(is.na(Vuelos2020)))
Vuelos2020 <- na.omit(Vuelos2020)
sapply(Vuelos2020, function(Vuelos2020)sum(is.na(Vuelos2020)))


#------------------ CREACION VARIABLE BLOQUE ---------------------#

Vuelos2020_1 <- ifelse(Vuelos2020$DEP_TIME_BLK=="0001-0559",0,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="0600-0659",1,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="0700-0759",2,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="0700-0759",3,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="0800-0859",4,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="0900-0959",5,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1000-1059",6,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1100-1159",7,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1200-1259",8,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1300-1359",9,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1400-1459",10,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1500-1559",11,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1600-1659",12,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1700-1759",13,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1800-1859",14,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="1900-1959",15,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="2000-2059",16,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="2100-2159",17,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="2200-2259",18,
                     ifelse(Vuelos2020$DEP_TIME_BLK=="2300-2359",19,20))))))))))))))))))))

## Se transforma la variable DEP_DEL15 a factor para poder realizar la tarea de
## de clasificacion.

DEP_DEL <- as.factor(Vuelos2020$DEP_DEL15)

Vuelos2020_mutate1 <- mutate(Vuelos2020, BLOCK = Vuelos2020_1, DEP_DEL_15 = DEP_DEL)



Vuelos2020_mutate2 <- mutate(transmute(Vuelos2020_mutate1, DAY_OF_MONTH,
                                       DAY_OF_WEEK, OP_CARRIER_AIRLINE_ID,
                                       OP_CARRIER_FL_NUM, ORIGIN_AIRPORT_ID,
                                       DEST_AIRPORT_ID,DEP_TIME, DEP_DEL_15,
                                       BLOCK, ARR_TIME, ARR_DEL15, CANCELLED,
                                       DIVERTED, DISTANCE))

class(Vuelos2020_mutate2)


Vuelos2020_mutate3 <- Vuelos2020_mutate2[1:5000,]

#--------------------- TRANSFORMAR DE DATA.FRAME A TIBBLE --------------------#

Vuelos2020_Tib <- as.tibble(Vuelos2020_mutate3)

#-------------------------- TRAREA DE CLASIFICACION --------------------------#

## Clasificar si es un vuelo atrasado o no.

VuelosTask <- makeClassifTask(data = Vuelos2020_Tib, target = 'DEP_DEL_15')
VuelosTask

## Sin atraso: 0
## Con  atraso: 1

#------------------------------- CLASIFICADOR ---------------------------------#

svm <- makeLearner("classif.svm")
svm

#------------------------------ HYPERPARAMETROS --------------------------------#

getParamSet("classif.svm")

kernels <- c("polynomial","radial","sigmoid")

svmParamSpace <- makeParamSet(
  makeDiscreteParam("kernel", values = kernels),
  makeIntegerParam("degree", lower = 1, upper = 3),
  makeNumericParam("cost", lower = 0.1, upper = 10),
  makeNumericParam("gamma", lower = 0.1, upper = 10)
)

svmParamSpace

## Se realiza (1) la busqueda de los parametro

randSearch <- makeTuneControlRandom(maxit = 10)
cvForTunnig <- makeResampleDesc("Holdout", split =2/3)


tunedSvmParams <- tuneParams("classif.svm", task = VuelosTask,
                             resampling = cvForTunnig, par.set = svmParamSpace,
                             control = randSearch)


## Se realiza la busqueda de los parametros

## [Tune] Result: kernel=polynomial; degree=3; cost=4.4; gamma=0.911 : 
## mmce.test.mean=0.1127774

tunedSvmParams

tunedSvm <- setHyperPars(makeLearner("classif.svm"),
                         par.vals = tunedSvmParams$x)

tunedSvm


tunedSvmModel <- train(tunedSvm, VuelosTask)

tunedSvmModel

pred <- predict(tunedSvmModel, newdata = Vuelos2020_Tib)

performance(pred, measures = acc)

### el modelo acierta en un 89,12% respecto al total


#------------------------------------------------------------------------------#

###################### ARBOLES DE DECISION ########################


Vuelos_entrenamiento <- sample_frac(Vuelos2020_mutate2, .7)
Vuelos_prueba <- setdiff(Vuelos2020_mutate2, Vuelos_entrenamiento)

arbol_1 <- rpart(formula = DEP_DEL_15 ~ ., data = Vuelos_entrenamiento)
arbol_1

rpart.plot(arbol_1)

prediccion_1 <- predict(arbol_1, newdata = Vuelos_prueba, type = "class")

confusionMatrix(prediccion_1, Vuelos_prueba[["DEP_DEL_15"]])





Vuelos_entrenamiento_2 <- sample_frac(Vuelos2020_mutate3, .7)

Vuelos_prueba_2 <- setdiff(Vuelos2020_mutate3, Vuelos_entrenamiento)

arbol_2 <- rpart(formula = DEP_DEL_15 ~ ., data = Vuelos_entrenamiento_2)

prediccion_2 <- predict(arbol_2, newdata = Vuelos_prueba_2, type = "class")

rpart.plot(arbol_2)

confusionMatrix(prediccion_2, Vuelos_prueba_2[["DEP_DEL_15"]])











