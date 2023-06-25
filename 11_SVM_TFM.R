# limpiar al inicio para ir probando el script.

rm(list = ls())

# Paralelizado

library(parallel)
library(doParallel)

GS_T0 <- Sys.time()
cluster <-makeCluster(detectCores()-1)
registerDoParallel(cluster)

# Librerías

library(randomForest)
library(caret)
library(ggplot2)
library(gam)
library(klaR)
library(MASS)
library(Boruta)
library(MXM)
library(readr)
library(tidyverse)
library(skimr)
library(dplyr)
library(tidyr)

library(plyr)
library(dummies)
library(ggplot2)
library(naniar)

source("funcion steprepetido.R")
source("cruzadas avnnet y lin.R")

SCM_FINAL_SVM <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_SELML2.csv", 
            sep = ",", header = TRUE, dec = ".")

#  SVM LINEAL: SOLO PARÁMETRO C

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,
                      savePredictions = "all") 

# Prueba con las variables seleccionadas con el mejor modelo de SEl_ML
SVM<- train(data=SCM_FINAL_SVM,Pob_EV_Total~Eco_Ind_RDBM_PC+Pob_Grupo.100001_500000_hab+ 
            Pob_Envejecimiento+DEGURBA.2+Eco_Afiliados_Tem_Porc_Resid+ 
            Soc_Mov_Auto_T.menor_de_3+Eco_Energ_Elect_PC+Eco_Indice_Gini+ 
            Eco_Establec_Hotel+Eco_Pre_Gastos_Liquidados+Eco_PBI_municipal_pc+ 
            Soc_Mov_Hosp_T.Mayor,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$RMSE)


#  SVM Polinomial: PARÁMETROS C, degree, scale, degree )

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
                     degree=c(2,3),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(data=SCM_FINAL_SVM,Pob_EV_Total~Eco_Ind_RDBM_PC+Pob_Grupo.100001_500000_hab+ 
              Pob_Envejecimiento+DEGURBA.2+Eco_Afiliados_Tem_Porc_Resid+ 
              Soc_Mov_Auto_T.menor_de_3+Eco_Energ_Elect_PC+Eco_Indice_Gini+ 
              Eco_Establec_Hotel+Eco_Pre_Gastos_Liquidados+Eco_PBI_municipal_pc+ 
              Soc_Mov_Hosp_T.Mayor,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

SVM$results
plot(SVM$results$C,SVM$results$RMSE)


plot(SVM$results$C,SVM$results$RMSE)
plot(SVM$results$degree,SVM$results$RMSE)
plot(SVM$results$scale,SVM$results$RMSE)

dat<-as.data.frame(SVM$results)
library(ggplot2)

# PLOT DE DOS VARIABLES CATEGÓRICAS, UNA CONTINUA
ggplot(dat, aes(x=factor(C), y=RMSE, 
                color=factor(degree),pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# AFINO MÁS
dat2<-dat[dat$degree==3&dat$scale<5&dat$scale>0.1,]  

ggplot(dat2, aes(x=factor(C), y=RMSE, 
                 colour=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

#  SVM RBF: PARÁMETROS C, sigma

SVMgrid<-expand.grid(C=c(0.1,0.2, 0.5, 1, 2, 1.5,2.5,5,4),
                     sigma=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(data=SCM_FINAL_SVM,Pob_EV_Total~Eco_Ind_RDBM_PC+Pob_Grupo.100001_500000_hab+ 
              Pob_Envejecimiento+DEGURBA.2+Eco_Afiliados_Tem_Porc_Resid+ 
              Soc_Mov_Auto_T.menor_de_3+Eco_Energ_Elect_PC+Eco_Indice_Gini+ 
              Eco_Establec_Hotel+Eco_Pre_Gastos_Liquidados+Eco_PBI_municipal_pc+ 
              Soc_Mov_Hosp_T.Mayor,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

dat<-as.data.frame(SVM$results)

ggplot(dat, aes(x=factor(C), y=RMSE, 
                color=factor(sigma)))+ 
  geom_point(position=position_dodge(width=0.5),size=3)


source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")
source("cruzada rf continua.R")
source("cruzada gbm continua.R")
source("cruzada xgboost continua.R")
source("cruzada SVM continua lineal.R")
source("cruzada SVM continua polinomial.R")
source("cruzada SVM continua RBF.R")


data<-SCM_FINAL_SVM
medias2<-cruzadalin(data=data,
                    vardep="Pob_EV_Total",listconti=
                      c("Eco_Ind_RDBM_PC", "Pob_Grupo.100001_500000_hab", 
                        "Pob_Envejecimiento", "Eco_Afiliados_Tem_Porc_Resid", "DEGURBA.2", "Eco_Indice_Gini", "Soc_Mov_Auto_T.menor_de_3", "Eco_Energ_Elect_PC","Eco_Establec_Hotel", "Eco_Pre_Gastos_Liquidados", "Eco_PBI_municipal_pc",
                        "Soc_Mov_Hosp_T.Mayor", "SAL.de_6_a_10", "MOV.13_o_más", "Pob_Grupo.251_500_hab"
                      ),
                    listclass=c(),
                    grupos=4,sinicio=1234,repe=25)


medias2$modelo="STEPBIC"

medias3<-cruzadalin(data=data,
                    vardep="Pob_EV_Total",listconti=
                      c("Eco_Ind_RDBM_PC", "Pob_Grupo.100001_500000_hab", "Pob_Envejecimiento",
                        "Eco_Indice_Gini", "Eco_Afiliados_Tem_Porc_Resid", "DEGURBA.2", 
                        "Soc_Mov_Auto_T.menor_de_3", "Eco_Energ_Elect_PC", 
                        "Eco_Pre_Gastos_Liquidados", "Eco_PBI_municipal_pc", "Eco_Establec_Hotel"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias3$modelo="STEPrep1"

medias4<-cruzadalin(data=data,
                    vardep="Pob_EV_Total",listconti=
                      c("Eco_Ind_RDBM_PC", "Pob_Grupo.100001_500000_hab", 
                        "Pob_Envejecimiento", "DEGURBA.2", "Eco_Afiliados_Tem_Porc_Resid", 
                        "Soc_Mov_Auto_T.menor_de_3", "Eco_Energ_Elect_PC", "Eco_Indice_Gini", 
                        "Eco_Establec_Hotel", "Eco_Pre_Gastos_Liquidados", 
                        "Eco_PBI_municipal_pc", "Soc_Mov_Hosp_T.Mayor"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias4$modelo="STEPrep2"
medias5<-cruzadalin(data=data,
                    vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC", "Eco_Paro_100",
                                                      "Eco_Afiliados_Tem_Porc_Resid",  "Eco_Pre_Ingresos_Liquidados",
                                                      "Pob_Empadronada",           "Eco_Pre_Gastos_Liquidados",
                                                      "Eco_PBI_municipal_pc" ,    "Eco_Afiliados_Total" ,         
                                                      "Eco_Energ_Elect_PC","Eco_Indice_Gini", "Superficie" ,
                                                      "Pob_Envejecimiento",   "Eco_Establec_Hotel" ,  "Eco_Pre_Deuda_Viva",             
                                                      "Eco_Contratos_Temp" ,   "Pob_Juventud"   ,               
                                                      "Pob_Edad_Media"     ,  "MOV.13_o_más" ,                  
                                                      "Soc_Salud_Def_Otras"                     ),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias5$modelo="Import-RF"
medias6<-cruzadalin(data=data,
                    vardep="Pob_EV_Total",listconti=
                      c("Eco_Ind_RDBM_PC", "Pob_Empadronada", "Eco_Pre_Ingresos_Liquidados", 
                        "Superficie", "Eco_Paro_100", "Eco_PBI_municipal_pc", "Eco_Establec_Hotel", 
                        "Eco_Afiliados_Total", "Eco_Energ_Elect_PC", "Pob_Envejecimiento", 
                        "Eco_Indice_Gini", "MOV.13_o_más", "Eco_Afiliados_Tem_Porc_Resid", 
                        "Pob_Grupo.100001_500000_hab", "Eco_Pre_Gastos_Liquidados", "Pob_Grupo.20001_50000_hab", 
                        "Pob_Juventud", "Eco_Pre_Deuda_Viva", "Eco_Contratos_Temp", "Pob_Edad_Media", 
                        "Soc_Salud_Def_SistResp", "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_Otras"
                      ),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias6$modelo="Import-gbm"
medias7<-cruzadaavnnet(data=data,
                       vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Grupo.100001_500000_hab","Pob_Envejecimiento",
                                                         "Eco_Afiliados_Tem_Porc_Resid","DEGURBA.2","Eco_Indice_Gini", 
                                                         "Soc_Mov_Auto_T.menor_de_3","Eco_Energ_Elect_PC","Eco_Establec_Hotel", 
                                                         "Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                         "SAL.de_6_a_10","MOV.13_o_más","Pob_Grupo.251_500_hab", 
                                                         "Superficie","Eco_Paro_100","SAL.11_o_más","EDU.25_o_más", 
                                                         "Pob_Grupo.20001_50000_hab","Pob_Empadronada", 
                                                         "MA_Sup_Forestal_Total.Mayor","Pob_Grupo.501_1000_hab", 
                                                         "Pob_Grupo.101_250_hab","Eco_Pre_Deuda_Viva", 
                                                         "Soc_Mov_Auto_T.de_4_a_7","MA_Sup_Protegida.Mayor")    ,
                       listclass=c(""),grupos=4,sinicio=1234,repe=25,repeticiones=5,itera=100,
                       size=c(5),decay=c(0.1))

medias7$modelo="Red1"

medias8<-cruzadaavnnet(data=data,
                       vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Grupo.100001_500000_hab", 
                                                         "Pob_Envejecimiento","DEGURBA.2","Eco_Afiliados_Tem_Porc_Resid", 
                                                         "Soc_Mov_Auto_T.menor_de_3","Eco_Energ_Elect_PC","Eco_Indice_Gini", 
                                                         "Eco_Establec_Hotel","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc", 
                                                         "Soc_Mov_Hosp_T.Mayor")    ,
                       listclass=c(""),grupos=4,sinicio=1234,repe=25,repeticiones=5,itera=100,
                       size=c(5),decay=c(0.001))

medias8$modelo="Red2"
medias10<-cruzadaxgbm(data=data,
                      vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                        "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                        "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                        "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                        "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                                        "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                        "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp"),
                      listclass=c(""),
                      grupos=7,sinicio=1254,repe=10,
                      min_child_weight=10,eta=0.02,nrounds=500,max_depth=5,
                      gamma=0,colsample_bytree=1,subsample=1)

medias10$modelo="xgbm2"
medias11<-cruzadaxgbm(data=data,
                      vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                        "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                        "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                        "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                        "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                                        "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                        "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=5,
                      min_child_weight=10,eta=0.02,nrounds=500,max_depth=8,
                      gamma=0,colsample_bytree=1,subsample=0.8, alpha=0.3)

medias11$modelo="xgbm3"

medias12<-cruzadaSVM(data=data,
                    vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                      "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                      "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                      "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                      "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                                      "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                      "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp"),
                    listclass=c(""),
                    grupos=4,sinicio=1234,repe=5,C=0.01)

medias12$modelo="SVM"

medias13<-cruzadaSVMpoly(data=data,
                        vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                          "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                          "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                          "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                          "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                                          "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                          "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,C=0.5,degree=3,scale=0.1)

medias13$modelo="SVMPoly"


medias14<-cruzadaSVMRBF(data=data,
                        vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                          "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                          "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                          "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                          "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                                          "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                          "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=5,C=5,sigma=0.2)

medias14$modelo="SVMRBF"

union1<-rbind(medias2,medias3,medias4,medias5,medias6,
              medias7,medias8,medias10, medias11, medias12, medias13, medias14)

par(cex.axis=0.8)
boxplot(data=union1,error~modelo,col="pink")


union2<-rbind(medias10, medias11, medias13, medias14)

par(cex.axis=0.8)
boxplot(data=union2,error~modelo, col="darkgreen")

union3<-rbind(medias7, medias8, medias10, medias11, medias13, medias14)

par(cex.axis=0.8)
boxplot(data=union3,error~modelo, col="purple")

# EVALUACIÓN

# Cargar las bibliotecas necesarias
library(e1071)
library(caret)
sample_size = floor(0.3*nrow(data))

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(data)),size = sample_size)

# Se crean los archivos train test
test =data[indices,]
train =data[-indices,]

# Definir las variables de entrenamiento
train_features <- train[, c("Eco_Ind_RDBM_PC", "Pob_Envejecimiento", "Eco_Afiliados_Tem_Porc_Resid",
                            "Eco_Indice_Gini", "Eco_Energ_Elect_PC", "Eco_Pre_Gastos_Liquidados",
                            "Eco_PBI_municipal_pc", "Soc_Mov_Hosp_T.Mayor", "MOV.13_o_más",
                            "Eco_Paro_100", "Pob_Empadronada", "Eco_Pre_Deuda_Viva", "Superficie",
                            "Eco_Afiliados_Total", "Soc_Salud_Def_Otras", "Soc_Salud_Def_Tumor",
                            "Soc_Salud_Def_SistCirc", "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp")]

# Definir la variable dependiente
train_labels <- train$Pob_EV_Total

# Definir la función de control de validación cruzada
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Ajustar el modelo SVM polinómico utilizando el conjunto de entrenamiento y validación cruzada
svm_model <- train(train_features, train_labels, method = "svmPoly",
                   trControl = control, preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(C = 0.5, degree = 3, scale = 0.1))

# Realizar las predicciones en el conjunto de prueba
test_features <- test[, c("Eco_Ind_RDBM_PC", "Pob_Envejecimiento", "Eco_Afiliados_Tem_Porc_Resid",
                          "Eco_Indice_Gini", "Eco_Energ_Elect_PC", "Eco_Pre_Gastos_Liquidados",
                          "Eco_PBI_municipal_pc", "Soc_Mov_Hosp_T.Mayor", "MOV.13_o_más",
                          "Eco_Paro_100", "Pob_Empadronada", "Eco_Pre_Deuda_Viva", "Superficie",
                          "Eco_Afiliados_Total", "Soc_Salud_Def_Otras", "Soc_Salud_Def_Tumor",
                          "Soc_Salud_Def_SistCirc", "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp")]
predictions <- predict(svm_model, newdata = test_features)

# Calcular el error cuadrático medio (MSE)
mse <- mean((test$Pob_EV_Total - predictions)^2)

# Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mse)
print(mse)
print(rmse)

# Crear el data frame con los valores reales y predichos
df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predictions)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")


# EVALUACIÓ SVM RADIAL
svm_model <- train(train_features, train_labels, method = "svmPoly",
                   trControl = control, preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(C = 5, degree = 3, scale = 0.2))

# Realizar las predicciones en el conjunto de prueba
test_features <- test[, c("Eco_Ind_RDBM_PC", "Pob_Envejecimiento", "Eco_Afiliados_Tem_Porc_Resid",
                          "Eco_Indice_Gini", "Eco_Energ_Elect_PC", "Eco_Pre_Gastos_Liquidados",
                          "Eco_PBI_municipal_pc", "Soc_Mov_Hosp_T.Mayor", "MOV.13_o_más",
                          "Eco_Paro_100", "Pob_Empadronada", "Eco_Pre_Deuda_Viva", "Superficie",
                          "Eco_Afiliados_Total", "Soc_Salud_Def_Otras", "Soc_Salud_Def_Tumor",
                          "Soc_Salud_Def_SistCirc", "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp")]
predictions <- predict(svm_model, newdata = test_features)

# Calcular el error cuadrático medio (MSE)
mse <- mean((test$Pob_EV_Total - predictions)^2)

# Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mse)
print(mse)
print(rmse)

# Crear el data frame con los valores reales y predichos
df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predictions)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")

