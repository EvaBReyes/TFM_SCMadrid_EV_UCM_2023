# limpiar al inicio para ir probando el script.

rm(list = ls())

#Cargamos preparamos la paralelización y la detección de cores del ordenador para agilizar el rendimiento.


library(parallel)
library(doParallel)

GS_T0 <- Sys.time()
cluster <-makeCluster(detectCores()-1)
registerDoParallel(cluster)

# cargamos las librerías necesarias.

library(randomForest)
library(caret)
library(ggplot2)
library(gam)
library(klaR)
#library(MASS)
library(Boruta)
library(MXM)
library(readr)
library(tidyverse)
library(skimr)
library(tidyr)

library(plyr)
library(dplyr)
library(dummies)
library(naniar)
library(corrr)

# SCM_FINAL2 y SCM_FINAL Tienen MadridCapital
# SCM_FINAL3 no tiene Madrid Capital
# SCM_FINAL_outliers no tiene Madrid Capital ni outliers ni ausentes

SCM_FINAL2_REV <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL2.csv", 
            sep = ",", header = TRUE, dec = ".")

SCM_FINAL_SELML <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_outliers.csv", 
            sep = ",", header = TRUE, dec = ".")

SCM_FINAL_SELML <- na.omit(SCM_FINAL_SELML)

dput(names(SCM_FINAL_SELML))
SCM_FINAL_SELML$DEGURBA<-as.character(SCM_FINAL_SELML$DEGURBA)

SCM_FINAL_SELML<-SCM_FINAL_SELML |> select(-c(Terr_Zonas_Estad_CM))

# Se preparan las listas de variables

listclass=c("MA_Pelig_Incend_AltoExt", 
            "MA_Sup_Forestal_Total", "MA_Sup_Protegida", "MA_Sup_quemada", 
             "Pob_Grupo","Soc_Mov_Auto_T", "Soc_Mov_Hosp_T", "Soc_Mov_Muni_5000_T", 
            "Soc_Salud_Discapacidad", 
            "EDU", "MOV", "SAL", "OCI","DEGURBA")
listcuanti= c("Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
              "Eco_Cap_Nec_Financ", "Eco_Contratos_Temp", "Eco_Energ_Elect_PC", 
              "Eco_Establec_Hotel", "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", 
              "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", "Superficie", "Eco_Pre_Gastos_Liquidados", 
              "Eco_Pre_Ingresos_Liquidados", "Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", 
              "Pob_Juventud","Soc_Salud_Def_Otras", 
              "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor")
vardep<-"Pob_EV_Total"

# Primero me quedo con las variables de interés
# (listcuanti, listclass y vardep)

SCM_FINAL_SELML2 <- SCM_FINAL_SELML [,c(listcuanti,listclass,vardep)]
SCM_FINAL_SELML2 <- as.data.frame(SCM_FINAL_SELML2)

# Estandarizar la variable Pob_EV_Total
SCM_FINAL_SELML2$Pob_EV_Total <- scale(SCM_FINAL_SELML2$Pob_EV_Total)

# Lista de Frecuencias de las categóricas, útil pues algunos niveles con pocas observaciones no deben ser tenidos en cuenta en modelos de machine learning para evitar sobreajuste

frecu<-ldply(SCM_FINAL_SELML2[,listclass],function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)

frecu

# No hay problema, hay muchas obs de cada categoría

# Pasar las categóricas a dummies

SCM_FINAL_SELML2<- dummy.data.frame(SCM_FINAL_SELML2, listclass, sep = ".")

write.csv(SCM_FINAL_SELML2, "SCM_FINAL_SELML2.csv", row.names = FALSE)

names(SCM_FINAL_SELML2)

archivo1<-SCM_FINAL_SELML2
glimpse(SCM_FINAL_SELML2)

library(tibble)

SCM_FINAL_SELML2 <- as.data.frame(SCM_FINAL_SELML2)

vardep <- "Pob_EV_Total"
nombres1<-  c( "Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", "Eco_Cap_Nec_Financ", 
"Eco_Contratos_Temp", "Eco_Energ_Elect_PC", "Eco_Establec_Hotel", 
"Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", "Eco_PBI_municipal_pc", 
"Eco_Pre_Deuda_Viva", "Eco_Pre_Gastos_Liquidados", "Eco_Pre_Ingresos_Liquidados", 
"Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", "Pob_Juventud", 
"Soc_Salud_Def_Otras", "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", 
"Soc_Salud_Def_Tumor", "DEGURBA.1", "DEGURBA.2", 
"DEGURBA.3", "MA_Pelig_Incend_AltoExt.Mayor", "MA_Pelig_Incend_AltoExt.Menor", 
"MA_Sup_Forestal_Total.Mayor", "MA_Sup_Forestal_Total.Menor", 
"MA_Sup_Protegida.Mayor", "MA_Sup_Protegida.Menor", "MA_Sup_quemada.Mayor", 
"MA_Sup_quemada.Menor", "Pob_Grupo.100001_500000_hab", "Pob_Grupo.1001_5000_hab", 
"Pob_Grupo.101_250_hab", "Pob_Grupo.20001_50000_hab", "Pob_Grupo.251_500_hab", 
"Pob_Grupo.50001_75000", "Pob_Grupo.5001_20000_hab", "Pob_Grupo.501_1000_hab", 
"Pob_Grupo.75001_100000_hab", "Pob_Grupo.menor_que_100_hab", 
"Soc_Mov_Auto_T.13_o_más", "Soc_Mov_Auto_T.de_4_a_7", "Soc_Mov_Auto_T.de_8_a_12", 
"Soc_Mov_Auto_T.menor_de_3", "Soc_Mov_Hosp_T.Mayor", "Soc_Mov_Hosp_T.Menor", 
"Soc_Mov_Muni_5000_T.Mayor", "Soc_Mov_Muni_5000_T.Menor", "Soc_Salud_Discapacidad.Mayor", 
"Soc_Salud_Discapacidad.Menor", "EDU.25_o_más", "EDU.de_4_a_8", 
"EDU.de_9_a_24", "EDU.menor_de_3", "MOV.13_o_más", "MOV.de 6_a_12", 
"MOV.de_4_a_5", "MOV.menor_de_3", "SAL.11_o_más", "SAL.de_4_a_5", 
"SAL.de_6_a_10", "SAL.menor_de_3", "OCI.16_o_más", "OCI.de_4_a_7", 
"OCI.de_8_a_15", "OCI.menor_de_3")
y<-archivo1[,vardep]
x<-archivo1[,nombres1]

# Se define el objeto filtro que se encarga de realizar la selección de variables utilizando la función sbf de la librería caret. 

# Los argumentos de sbf son x e y, que son las matrices predictoras y la variable dependiente, respectivamente. 
# Además, se define un sbfControl que indica que se utilizará la función rfSBF para realizar la selección de variables y que se hará una validación cruzada.

# Para hacer que los resultados sean reproducibles, se establece una semilla (seed) antes de ejecutar la función rfe

set.seed(1234)

filtro<-sbf(x,y,sbfControl = sbfControl(functions = rfSBF,
                                        method = "cv", verbose = FALSE))

# Se extraen las variables seleccionadas por filtro y se guardan en el objeto a utilizando la función dput que permite imprimir un objeto en formato legible por R.

a<-dput(filtro$optVariables)

# Se obtiene la longitud del objeto a, que corresponde al número de variables seleccionadas por filtro.
length(a)


#RFE

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x, y, sizes=c(1:8), rfeControl=control)

selecrfe<-results$optVariables
length(selecrfe)
dput(selecrfe)
length(selecrfe)

# STEPWISE, BACKWARD, FORWARD

library(MASS)

# CON AIC
# la función glm() que se utiliza para especificar la distribución y 
# la función de enlace utilizada en la regresión lineal generalizada. La sintaxis para glm() es la siguiente: glm(formula, data, family, ...)

full<-glm(Pob_EV_Total~.,data=archivo1,family = gaussian(link = "identity"))
null<-glm(Pob_EV_Total~1,data=archivo1,family = gaussian(link = "identity"))

# se ajusta un modelo con todas las variables (objeto "full") y un modelo con solo el intercepto (objeto "null"). Luego, se utiliza la función "stepAIC" para seleccionar las variables que minimizan el AIC, comenzando con el modelo con solo el intercepto y añadiendo o eliminando variables en función de si mejora o no el AIC.

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = gaussian(link = "identity"),trace=FALSE)

# se extraen los nombres de las variables seleccionadas (almacenados en el objeto "selec1") y se guardan en el objeto "vec".

vec<-(names(selec1[[1]]))

length(vec) 

dput(vec)


# CON BIC, ponemos k=log(n) en stepAIC, en este caso n=981 observaciones
log(1780) # 7.484369

#modelo más flexible

full<-glm(Pob_EV_Total~.,data=archivo1,family = gaussian(link = "identity"))
null<-glm(Pob_EV_Total~1,data=archivo1,family = gaussian(link = "identity"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = gaussian(link = "identity"),trace=FALSE, k=6)

vec<-(names(selec1[[1]]))

length(vec) 

dput(vec)

# BORUTA

# Ejecuto la función "Boruta" del paquete "Boruta" y almaceno el resultado en la variable "out.boruta". 

out.boruta <- Boruta(Pob_EV_Total~., data = archivo1)

# Imprime el resultado de la variable "out.boruta", que contiene información sobre las variables seleccionadas, su importancia relativa y su estado (confirmado, rechazado o pendiente) en el análisis Boruta.

print(out.boruta)

summary(out.boruta)

# Creo un data frame llamado "sal" que contiene la variable "out.boruta$finalDecision". Luego, se seleccionan solo las filas donde la variable "out.boruta.finalDecision" es "Confirmed" y se almacenan en un nuevo data frame llamado "sal2".
sal<-data.frame(out.boruta$finalDecision)
sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]

dput(row.names(sal2))

length(dput(row.names(sal2))) 

# Se ejecuta el algoritmo mmpc en el conjunto de datos archivo1.

# El argumento max_k es el número máximo de padres o hijos que se buscarán
# hash es un parámetro que optimiza la velocidad de ejecución de l afunción.
# con text le decimos el tipo de prueba estadística que se utilizará para medir la independencia entre las variables

mmpc1 <- MMPC(vardep, archivo1, max_k = 3, hash = TRUE,test = "testIndFisher")

# Se crea el vector con los nombres de las variables seleccionadas
mmpc1@selectedVars
# [1]  9 15 17 55 58 64

# Se muestran los nombres de las variables del vector y se imprime la longitud y los nombres de las variables seleccionadas:

a<-dput(names(archivo1[,c(mmpc1@selectedVars)]))
# c("Eco_Paro_100", "Pob_Empadronada", "Terr_Zonas_Estad_CM.ESTEMETROPOLITANO", 
# "Terr_Zonas_Estad_CM.OESTEMETROPOLITANO", "Terr_Zonas_Estad_CM.SURMETROPOLITANO"
length(a) # 6

# SES

SES1 <- SES(vardep, archivo1, max_k = 3, hash = TRUE,
            test = "testIndFisher")

SES1@selectedVars

dput(names(archivo1[,c(SES1@selectedVars)]))

a<-dput(names(archivo1[,c(SES1@selectedVars)]))

length(a) 

# IGUAL AL ANTERIOR USO SES

# STEPREPETIDO AIC

source("funcion steprepetido.R")


lista<-steprepetido(data=archivo1,vardep=c("Pob_EV_Total"),
                    listconti=c("Eco_Afiliados_Tem_Porc_Resid","Eco_Afiliados_Total","Eco_Cap_Nec_Financ",
                                "Eco_Contratos_Temp","Eco_Energ_Elect_PC","Eco_Establec_Hotel",
                                "Eco_Ind_RDBM_PC","Eco_Indice_Gini","Eco_Paro_100",
                                "Eco_PBI_municipal_pc","Eco_Pre_Deuda_Viva","Eco_Pre_Gastos_Liquidados",
                                "Eco_Pre_Ingresos_Liquidados","Pob_Edad_Media","Pob_Empadronada",
                                "Pob_Envejecimiento","Pob_Juventud",
                                "Soc_Salud_Def_Otras","Soc_Salud_Def_SistCirc","Soc_Salud_Def_SistResp",
                                "Soc_Salud_Def_Tumor","DEGURBA.1",
                                "DEGURBA.2","DEGURBA.3","MA_Pelig_Incend_AltoExt.Mayor",
                                "MA_Pelig_Incend_AltoExt.Menor","MA_Sup_Forestal_Total.Mayor","MA_Sup_Forestal_Total.Menor",
                                "MA_Sup_Protegida.Mayor","MA_Sup_Protegida.Menor","MA_Sup_quemada.Mayor",
                                "MA_Sup_quemada.Menor","Pob_Grupo.100001_500000_hab","Pob_Grupo.1001_5000_hab",
                                "Pob_Grupo.101_250_hab","Pob_Grupo.20001_50000_hab","Pob_Grupo.251_500_hab",
                                "Pob_Grupo.50001_75000","Pob_Grupo.5001_20000_hab","Pob_Grupo.501_1000_hab",
                                "Pob_Grupo.75001_100000_hab","Pob_Grupo.menor_que_100_hab","Soc_Mov_Auto_T.13_o_más",
                                "Soc_Mov_Auto_T.de_4_a_7","Soc_Mov_Auto_T.de_8_a_12","Soc_Mov_Auto_T.menor_de_3",
                                "Soc_Mov_Hosp_T.Mayor","Soc_Mov_Hosp_T.Menor","Soc_Mov_Muni_5000_T.Mayor",
                                "Soc_Mov_Muni_5000_T.Menor","Soc_Salud_Discapacidad.Mayor","Soc_Salud_Discapacidad.Menor",
                                "EDU.25_o_más","EDU.de_4_a_8","EDU.de_9_a_24","EDU.menor_de_3","MOV.13_o_más",
                                "MOV.de_4_a_5","MOV.de 6_a_12","MOV.menor_de_3",
                                "SAL.11_o_más","SAL.de_4_a_5","SAL.de_6_a_10",
                                "SAL.menor_de_3","OCI.16_o_más","OCI.de_4_a_7",
                                "OCI.de_8_a_15","OCI.menor_de_3"
                    ),
                    sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# 

lista<-steprepetido(data=archivo1,vardep=c("Pob_EV_Total"),
                    listconti=c("Eco_Afiliados_Tem_Porc_Resid","Eco_Afiliados_Total","Eco_Cap_Nec_Financ",
                                "Eco_Contratos_Temp","Eco_Energ_Elect_PC","Eco_Establec_Hotel",
                                "Eco_Ind_RDBM_PC","Eco_Indice_Gini","Eco_Paro_100",
                                "Eco_PBI_municipal_pc","Eco_Pre_Deuda_Viva","Eco_Pre_Gastos_Liquidados",
                                "Eco_Pre_Ingresos_Liquidados","Pob_Edad_Media","Pob_Empadronada",
                                "Pob_Envejecimiento","Pob_Juventud",
                                "Soc_Salud_Def_Otras","Soc_Salud_Def_SistCirc","Soc_Salud_Def_SistResp",
                                "Soc_Salud_Def_Tumor","DEGURBA.1",
                                "DEGURBA.2","DEGURBA.3","MA_Pelig_Incend_AltoExt.Mayor",
                                "MA_Pelig_Incend_AltoExt.Menor","MA_Sup_Forestal_Total.Mayor","MA_Sup_Forestal_Total.Menor",
                                "MA_Sup_Protegida.Mayor","MA_Sup_Protegida.Menor","MA_Sup_quemada.Mayor",
                                "MA_Sup_quemada.Menor","Pob_Grupo.100001_500000_hab","Pob_Grupo.1001_5000_hab",
                                "Pob_Grupo.101_250_hab","Pob_Grupo.20001_50000_hab","Pob_Grupo.251_500_hab",
                                "Pob_Grupo.50001_75000","Pob_Grupo.5001_20000_hab","Pob_Grupo.501_1000_hab",
                                "Pob_Grupo.75001_100000_hab","Pob_Grupo.menor_que_100_hab","Soc_Mov_Auto_T.13_o_más",
                                "Soc_Mov_Auto_T.de_4_a_7","Soc_Mov_Auto_T.de_8_a_12","Soc_Mov_Auto_T.menor_de_3",
                                "Soc_Mov_Hosp_T.Mayor","Soc_Mov_Hosp_T.Menor","Soc_Mov_Muni_5000_T.Mayor",
                                "Soc_Mov_Muni_5000_T.Menor","Soc_Salud_Discapacidad.Mayor","Soc_Salud_Discapacidad.Menor",
                                "EDU.25_o_más","EDU.de_4_a_8","EDU.de_9_a_24","EDU.menor_de_3","MOV.13_o_más",
                                "MOV.de_4_a_5","MOV.de 6_a_12","MOV.menor_de_3",
                                "SAL.11_o_más","SAL.de_4_a_5","SAL.de_6_a_10",
                                "SAL.menor_de_3","OCI.16_o_más","OCI.de_4_a_7",
                                "OCI.de_8_a_15","OCI.menor_de_3"
                    ),
                    sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# 
dput(lista[[2]][[2]])

# 

# COMPARACION VIA CV REPETIDA Y BOXPLOT

source("cruzadas avnnet y lin.R")

SCM_FINAL_SELML2 <- as.data.frame(SCM_FINAL_SELML2)

data<-SCM_FINAL_SELML2

medias1<-cruzadalin(data=data,
                    vardep="Pob_EV_Total",listconti=
                      c("Eco_Ind_RDBM_PC", "Pob_Grupo.100001_500000_hab", "Pob_Envejecimiento", 
                        "Eco_Afiliados_Tem_Porc_Resid", "DEGURBA.2", "Eco_Indice_Gini", 
                        "Soc_Mov_Auto_T.menor_de_3", "Eco_Energ_Elect_PC", "Eco_Establec_Hotel", 
                        "Eco_Pre_Gastos_Liquidados", "Eco_PBI_municipal_pc", "Soc_Mov_Hosp_T.Mayor", 
                        "SAL.de_6_a_10", "MOV.13_o_más", "Pob_Grupo.251_500_hab", 
                        "Superficie", "Eco_Paro_100", "SAL.11_o_más", "EDU.25_o_más", 
                        "Pob_Grupo.20001_50000_hab", "Pob_Empadronada", 
                        "MA_Sup_Forestal_Total.Mayor", "Pob_Grupo.501_1000_hab", 
                        "MOV.de 6_a_12", "Pob_Grupo.101_250_hab", "Eco_Pre_Deuda_Viva", 
                        "Soc_Mov_Auto_T.de_4_a_7", "MA_Sup_Protegida.Mayor"
                      ),
                    listclass=c(),
                    grupos=4,sinicio=1234,repe=25)

medias1$modelo="STEPAIC"

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
                    vardep="Pob_EV_Total",listconti=
                      c("Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", "Eco_Contratos_Temp", "Eco_Establec_Hotel", "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", "Eco_Pre_Gastos_Liquidados", 
                        "Eco_Pre_Ingresos_Liquidados", "Pob_Edad_Media", "Pob_Empadronada", 
                        "Pob_Envejecimiento", "Pob_Juventud", "DEGURBA.1", "MA_Sup_Protegida.Mayor", 
                        "MA_Sup_Protegida.Menor", "MA_Sup_quemada.Mayor", "MA_Sup_quemada.Menor", 
                        "Pob_Grupo.100001_500000_hab", "Pob_Grupo.20001_50000_hab", "Pob_Grupo.75001_100000_hab", 
                        "Soc_Mov_Auto_T.menor_de_3", "SAL.de_6_a_10"
                      ),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias5$modelo="SBF"

medias6<-cruzadalin(data=data,
                    vardep="Pob_EV_Total",listconti=
                      c("Eco_Afiliados_Tem_Porc_Resid", "Eco_Energ_Elect_PC", "Eco_Ind_RDBM_PC", 
                        "Eco_Indice_Gini", "Eco_Paro_100", 
                        "Pob_Grupo.100001_500000_hab", "Pob_Grupo.75001_100000_hab"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)


medias6$modelo="SES"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6)

par(cex.axis=0.8)
boxplot(data=union1,col="pink",error~modelo)

union1$error2<-sqrt(union1$error)

par(cex.axis=1.2)
boxplot(data=union1,col="pink",error2~modelo)

# elimino SES y SBF del modelo

union2<-rbind(medias1,medias2,medias3,medias4)

par(cex.axis=0.8)
boxplot(data=union2,col="pink",error~modelo)

union2$error2<-sqrt(union2$error)

par(cex.axis=1.2)
boxplot(data=union2,col="pink",error2~modelo)

# Represento cuántas variables tiene cada modelo

dput(names(table(union2$modelo)))

#Crea un vector con las variables de los modelos
nvar<-c(28,16,11,12)

nvar2 <- paste(nvar, "var.")

# posiciones de las etiquetas

max_error <- max(union2$error)
min_error <- min(union2$error)
num_modelos <- length(unique(union2$modelo))


# Crear el boxplot y agregar las etiquetas
par(cex.axis=0.6,las=2)
boxplot(data=union2,col="pink",error~modelo, ylim=c(min_error, max_error*1.03))
text(x = seq(1:num_modelos), y = rep(max_error, num_modelos), labels = nvar2, pos = 3,col="blue")
axis(2, at=pretty(range(union1$error),n=20))


# para colorear las cajas según el número de variables

# Generar una secuencia de colores arcoíris
colores <- rainbow(length(unique(nvar)))
# Asignar un color a cada caja según el valor de nvar
colores_cajas <- colores[match(nvar, unique(nvar))]

par(cex.axis=0.6,las=2)
boxplot(data=union2,col=colores_cajas, error~modelo, ylim=c(min_error, max_error*1.03))
text(x = seq(1:num_modelos), y=c(max_error,max_error,0.42,0.24,0.28,0.44,0.22,0.42), 
     labels = nvar2, 
     font=3,pos = 3,col="blue")
axis(2, at=pretty(range(union1$error),n=20))


library(ggplot2)

# Crear el gráfico base
ggplot(data = union2, aes(x = modelo, y = error, fill = modelo)) +
  geom_boxplot() +
  scale_fill_manual(values = colores_cajas) +
  ylim(min_error, max_error * 1.03) +
  theme_minimal() +
  labs(x = "Modelo", y = "Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none")


# Parece que con 12 variables STEPrep2 puede funcionar bien.Probamos RF importancia

rfgrid<-expand.grid(mtry=c(3,4,5,6,7,8,9,10,11))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

rf<- train(Pob_EV_Total~.,data=data,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = T,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf

# IMPORTANCIA DE VARIABLES RANDOM FOREST

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$IncNodePurity),]
tabla

dput(rownames(tabla))



# GRADIENT BOOSTING MACHINE

gbmgrid<-expand.grid(shrinkage=c(0.1, 0.2,0.4,1),
                     n.minobsinnode=c(10),
                     n.trees=c(50,100,300,500,800,1000,1200),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

gbm<- train(Pob_EV_Total~.,data=data,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

gbmgrid<-expand.grid(shrinkage=c(0.2),
                     n.minobsinnode=c(10),
                     n.trees=c(800,1000,1200,1250,1300,1400),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

gbm<- train(Pob_EV_Total~.,data=data,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

gbmgrid<-expand.grid(shrinkage=c(0.2),
                     n.minobsinnode=c(10),
                     n.trees=c(800,850,900,950,1000,1050,1100,1200,1400),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

gbm<- train(Pob_EV_Total~.,data=data,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

# IMPORTANCIA DE VARIABLES
par(cex=1.3)
summary(gbm)
tabla2<-summary(gbm)
dput(rownames(tabla2))

# Con ggplot
tabla2_df <- as.data.frame(tabla2)
tabla2_df$variable <- rownames(tabla2)

library(ggplot2)
ggplot(tabla2_df, aes(x = reorder(variable, tabla2_df$rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  coord_flip() + # esto coloca las etiquetas verticalmente
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # rotar las etiquetas del eje x
  labs(x = "Variable", y = "Relative Importance") +
  ggtitle("Importancia de las variables modelo GBM")


# Corto por 23 variables y 950 árboles, shrinkage=0.2


# AÑADIMOS AL BOXPLOTS ESTOS SETS TENTATIVOS

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

# Al ser las mismas variables cambio la semilla

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


union3<-rbind(medias1,medias2,medias3,medias4, medias5,medias6)

par(cex.axis=0.8)
boxplot(data=union3,col="pink",error~modelo)


union3$error2<-sqrt(union3$error)

par(cex.axis=0.8)
boxplot(data=union3,col="pink",error2~modelo)

## MODELOS DE RED

# Tuneo con Validación cruzada con avNNet

control<-trainControl(method = "cv",
                      number=7,savePredictions = "all") 

set.seed(1234)
nnetgrid <-  expand.grid(size=c(10),decay=c(0.01,0.1,0.001,0.0001),bag=F)

completo<-data.frame()
listaiter<-c(10,20,50,100,200,300,500,1000,2000,3000)

for (iter in listaiter)
{
  rednnet<- train(Pob_EV_Total~Eco_Ind_RDBM_PC+Pob_Grupo.100001_500000_hab+Pob_Envejecimiento+
                    Eco_Afiliados_Tem_Porc_Resid+DEGURBA.2+Eco_Indice_Gini+ 
                  Soc_Mov_Auto_T.menor_de_3+Eco_Energ_Elect_PC+Eco_Establec_Hotel+ 
                  Eco_Pre_Gastos_Liquidados+Eco_PBI_municipal_pc+Soc_Mov_Hosp_T.Mayor+ 
                  SAL.de_6_a_10+MOV.13_o_más+Pob_Grupo.251_500_hab+ 
                  Superficie+Eco_Paro_100+SAL.11_o_más+EDU.25_o_más+ 
                  Pob_Grupo.20001_50000_hab+Pob_Empadronada+ 
                  MA_Sup_Forestal_Total.Mayor+Pob_Grupo.501_1000_hab+ 
                  Pob_Grupo.101_250_hab+Eco_Pre_Deuda_Viva+ 
                  Soc_Mov_Auto_T.de_4_a_7+MA_Sup_Protegida.Mayor,
                  data=data,
                  method="avNNet",linout = TRUE,maxit=iter,
                  trControl=control,repeats=5,tuneGrid=nnetgrid,trace=F)
  # Añado la columna del parametro de iteraciones
  rednnet$results$itera<-iter
  # Voy incorporando los resultados a completo
  completo<-rbind(completo,rednnet$results)
  }

completo<-completo[order(completo$RMSE),]

ggplot(completo, aes(x=factor(itera), y=RMSE, 
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# Tuneo con Validación cruzada con avNNet. Cambio semilla y variables e iteraciones

control<-trainControl(method = "cv",
                      number=7,savePredictions = "all") 

set.seed(1254)
nnetgrid <-  expand.grid(size=c(10),decay=c(0.001,0.0001,0.01,0.02,0.05,0.1),bag=F)

completo<-data.frame()
listaiter<-c(10,15,20,25,30,35,40,50,75,100,200)

for (iter in listaiter)
{
  rednnet<- train(Pob_EV_Total~Eco_Ind_RDBM_PC+Pob_Grupo.100001_500000_hab+ 
  Pob_Envejecimiento+DEGURBA.2+Eco_Afiliados_Tem_Porc_Resid+ 
  Soc_Mov_Auto_T.menor_de_3+Eco_Energ_Elect_PC+Eco_Indice_Gini+ 
  Eco_Establec_Hotel+Eco_Pre_Gastos_Liquidados+Eco_PBI_municipal_pc+ 
  Soc_Mov_Hosp_T.Mayor,
                  data=data,
                  method="avNNet",linout = TRUE,maxit=iter,
                  trControl=control,repeats=5,tuneGrid=nnetgrid,trace=F)
  # Añado la columna del parametro de iteraciones
  rednnet$results$itera<-iter
  # Voy incorporando los resultados a completo
  completo<-rbind(completo,rednnet$results)
  
  
}

completo<-completo[order(completo$RMSE),]

ggplot(completo, aes(x=factor(itera), y=RMSE, 
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# Añadimos la caja de las 2 redes:



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

union4<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8)

par(cex.axis=0.8)
boxplot(data=union4,col="pink",error~modelo)


union4$error2<-sqrt(union4$error)

par(las=2, cex.axis=0.8)
boxplot(data=union4, col="pink", error2~modelo)


# XGBOOST

# Define the parameter grid for tuning

xgbmgrid <- expand.grid(
  min_child_weight = c(5, 10, 20),
  eta = c(0.1, 0.05, 0.03, 0.01, 0.001),
  nrounds = c(100, 500, 1000, 5000),
  max_depth = 6, gamma = 0, colsample_bytree = 1, subsample = 1
)

# Set up the cross-validation
control <- trainControl(method = "cv", number = 4, savePredictions = "all")

# Train the model
xgbm <- train(Pob_EV_Total ~ ., data = SCM_FINAL_SELML2,
              method = "xgbTree", trControl = control,
              tuneGrid = xgbmgrid, verbose = FALSE)


# Tuning parameter 'subsample' was held constant at a value of 1
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 5000, max_depth = 6,
# eta = 0.02, gamma = 0, colsample_bytree = 1, min_child_weight = 10
# and subsample = 1.

summary(xgbm)
plot(xgbm)


# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

xgbmgrid<-expand.grid(eta=c(0.001,0.005,0.01,0.02),
                      min_child_weight=c(10),
                      nrounds=c(500,1000,2000),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)

control<-trainControl(method = "cv",number=4,savePredictions = "all")

xgbm <- train(Pob_EV_Total ~ ., data = SCM_FINAL_SELML2,
              method = "xgbTree", trControl = control,
              tuneGrid = xgbmgrid, verbose = FALSE)

plot(xgbm)

# IMPORTANCIA DE VARIABLES

varImp(xgbm)
plot(varImp(xgbm))

source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")
source("cruzada rf continua.R")
source("cruzada gbm continua.R")
source("cruzada xgboost continua.R")

medias9<-cruzadaxgbm(data=SCM_FINAL_SELML2,
                      vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Grupo.100001_500000_hab","Pob_Envejecimiento",
                                                        "Eco_Afiliados_Tem_Porc_Resid","DEGURBA.2","Eco_Indice_Gini", 
                                                        "Soc_Mov_Auto_T.menor_de_3","Eco_Energ_Elect_PC","Eco_Establec_Hotel", 
                                                        "Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                        "SAL.de_6_a_10","MOV.13_o_más","Pob_Grupo.251_500_hab", 
                                                        "Superficie","Eco_Paro_100","SAL.11_o_más","EDU.25_o_más", 
                                                        "Pob_Grupo.20001_50000_hab","Pob_Empadronada", 
                                                        "MA_Sup_Forestal_Total.Mayor","Pob_Grupo.501_1000_hab", 
                                                        "Pob_Grupo.101_250_hab","Eco_Pre_Deuda_Viva", 
                                                        "Soc_Mov_Auto_T.de_4_a_7","MA_Sup_Protegida.Mayor"),
                      listclass=c(""),
                      grupos=7,sinicio=1254,repe=10,
                      min_child_weight=10,eta=0.02,nrounds=100,max_depth=5,
                      gamma=0,colsample_bytree=1,subsample=1)

medias9$modelo="xgbm1"

medias10<-cruzadaxgbm(data=SCM_FINAL_SELML2,
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


union6<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7, medias8,medias9, medias10)

par(cex.axis=0.5)

boxplot(data=union6,error~modelo)

union7<-rbind(medias7, medias8,medias9, medias10)

par(cex.axis=0.5)

boxplot(data=union7,error~modelo)

# PRUEBAS VARIANDO SEMILLA EL PARÁMETRO ALPHA DE REGULARIZACIÓN

medias11<-cruzadaxgbm(data=SCM_FINAL_SELML2,
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

medias12<-cruzadaxgbm(data=SCM_FINAL_SELML2,
                      vardep="Pob_EV_Total",listconti=c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                        "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                        "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                        "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                        "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                                        "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                        "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=5,
                      min_child_weight=10,eta=0.03,nrounds=500,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1,alpha=0.5)

medias12$modelo="xgbm4"


union8<-rbind(medias7, medias8,medias9, medias10, medias11, medias12)

par(cex.axis=0.5)

boxplot(data=union8,error~modelo)

# EVALUACIÓN EN RED

sample_size = floor(0.3*nrow(data))

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(data)),size = sample_size)

# Se crean los archivos train test
test =data[indices,]
train =data[-indices,]

# Se graban para exportar a SAS:

library(foreign)


write.dbf(train,"./train.dbf")
write.dbf(test,"./test.dbf")


# Crear objeto "control" para validación cruzada
control <- trainControl(method="repeatedcv", number=10, repeats=5)

## RED 1

# Definir el grid de búsqueda
nnetgrid <- expand.grid(size=c(5),decay=c(0.1),bag=F)

# Entrenar el modelo
rednnet <- train(Pob_EV_Total ~ Eco_Ind_RDBM_PC+Pob_Grupo.100001_500000_hab+Pob_Envejecimiento+
                 Eco_Afiliados_Tem_Porc_Resid+DEGURBA.2+Eco_Indice_Gini+ 
                 Soc_Mov_Auto_T.menor_de_3+Eco_Energ_Elect_PC+Eco_Establec_Hotel+ 
                 Eco_Pre_Gastos_Liquidados+Eco_PBI_municipal_pc+Soc_Mov_Hosp_T.Mayor+ 
                 SAL.de_6_a_10+MOV.13_o_más+Pob_Grupo.251_500_hab+ 
                 Superficie+Eco_Paro_100+SAL.11_o_más+EDU.25_o_más+ 
                 Pob_Grupo.20001_50000_hab+Pob_Empadronada+ 
                 MA_Sup_Forestal_Total.Mayor+Pob_Grupo.501_1000_hab+ 
                 Pob_Grupo.101_250_hab+Eco_Pre_Deuda_Viva+ 
                 Soc_Mov_Auto_T.de_4_a_7+MA_Sup_Protegida.Mayor
                 ,
                 data=train,
                 method="avNNet",
                 linout = TRUE,
                 trControl=control,
                 repeats=5,
                 tuneGrid=nnetgrid,
                 maxit=100,
                 trace=F)

# Se aplica la función predict:

predicciones<-predict(rednnet,test)

# Se añaden las predicciones al archivo test y se calcula el error:

comple<-cbind(test,predicciones)

comple$error<-(comple$Pob_EV_Total-comple$predicciones)^2

MSE<-mean(comple$error)
RMSE<-sqrt(MSE)
print(MSE) 
print(RMSE) 

df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predicciones)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = predicciones)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")

## RED 2

# Definir el grid de búsqueda
nnetgrid <- expand.grid(size=c(5),decay=c(0.001),bag=F)


# Entrenar el modelo
rednnet <- train(Pob_EV_Total ~ Eco_Ind_RDBM_PC
+Pob_Grupo.100001_500000_hab+Pob_Envejecimiento+
DEGURBA.2+Eco_Afiliados_Tem_Porc_Resid+
Soc_Mov_Auto_T.menor_de_3+Eco_Energ_Elect_PC+
Eco_Indice_Gini+Eco_Establec_Hotel+
Eco_Pre_Gastos_Liquidados+Eco_PBI_municipal_pc+Soc_Mov_Hosp_T.Mayor,
                 data=train,
                 method="avNNet",
                 linout = TRUE,
                 trControl=control,
                 repeats=5,
                 tuneGrid=nnetgrid,
                 maxit=100,
                 trace=F)

# Se aplica la función predict:

predicciones<-predict(rednnet,test)

# Se añaden las predicciones al archivo test y se calcula el error:

comple<-cbind(test,predicciones)

comple$error<-(comple$Pob_EV_Total-comple$predicciones)^2

MSE<-mean(comple$error)
RMSE<-sqrt(MSE)
print(MSE) 
print(RMSE) 

df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predicciones)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = predicciones)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")


# EVALUACIÓN DE XGBOOST

sample_size = floor(0.3*nrow(data))

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(data)),size = sample_size)

# Se crean los archivos train test
test =data[indices,]
train =data[-indices,]

# Configurar los parámetros del modelo XGBoost SEGÚN el xgbm3

params <- list(
  objective = "reg:linear",  
  booster = "gbtree",
  nrounds = 500,
  eta = 0.02,
  max_depth = 8,
  min_child_weight = 10,
  gamma = 0,
  colsample_bytree = 1,
  subsample = 0.8,
  alpha = 0.3
)
library(xgboost)

# Convertir los datos a una matriz DMatrix para usar en XGBoost
# Se definen las variables en los conjuntos train y test. 
# CUIDADO! CONTINE TODAS SALDRÁ PERFECTO

train_matrix <- xgb.DMatrix(as.matrix(train[, -1]), label = train$Pob_EV_Total)
test_matrix <- xgb.DMatrix(as.matrix(test[, -1]), label = test$Pob_EV_Total)

# Entrenar el modelo XGBoost
xgb_model <- xgboost(params, data = train_matrix, nrounds = params$nrounds)

# Realizar predicciones en el conjunto de datos de prueba
predictions <- predict(xgb_model, test_matrix)

# Calcular el error cuadrático medio (MSE)
mse <- mean((test$Pob_EV_Total - predictions)^2)
print(mse)
# 0.0003590823
# Calcular el RMSE a partir del MSE
rmse <- sqrt(mse)

# Imprimir el resultado
print(rmse)


# Representación:

library(ggplot2)

# Crear un dataframe con las predicciones y los valores reales
df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predictions)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")


# XGBOOST4 PRUEBO CON LAS VARIABLES DE XGBOOST3

library(xgboost)

# Convertir los datos a una matriz DMatrix para usar en XGBoost
train_matrix <- xgb.DMatrix(as.matrix(train[, c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                                "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp")]), label = train$Pob_EV_Total)
test_matrix <- xgb.DMatrix(as.matrix(test[, c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                              "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                              "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                              "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                              "Eco_Pre_Deuda_Viva","Superficie", "Eco_Afiliados_Total",
                                              "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                              "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp")]), label = test$Pob_EV_Total)


# Entrenar el modelo XGBoost
xgb_model <- xgboost(params, data = train_matrix, nrounds = params$nrounds)

# Realizar predicciones en el conjunto de datos de prueba
predictions <- predict(xgb_model, test_matrix)

# Calcular el error cuadrático medio (MSE)
mse <- mean((test$Pob_EV_Total - predictions)^2)

# Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mse)

# Imprimir el resultado del RMSE
print(rmse)
print(mse)

df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predictions)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "Predicción") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")

train_matrix <- xgb.DMatrix(as.matrix(train[, -which(names(train) == "Pob_EV_Total")]), label = train$Pob_EV_Total)
test_matrix <- xgb.DMatrix(as.matrix(test[, -which(names(test) == "Pob_EV_Total")]), label = test$Pob_EV_Total)


# XGBOOST 5
sample_size = floor(0.3*nrow(data))

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(data)),size = sample_size)

# Se crean los archivos train test
test =data[indices,]
train =data[-indices,]
# Configurar los parámetros del modelo XGBoost SEGÚN el xgbm3
params <- list(
  objective = "reg:linear",  
  booster = "gbtree",
  nrounds = 500,
  eta = 0.02,
  max_depth = 8,
  min_child_weight = 10,
  gamma = 0,
  colsample_bytree = 1,
  subsample = 0.8,
  alpha = 0.3
)
library(xgboost)

# Convertir los datos a una matriz DMatrix para usar en XGBoost
train_matrix <- xgb.DMatrix(as.matrix(train[, c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                                "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                                "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados",
                                                "Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                                "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                                "Eco_Pre_Deuda_Viva","Superficie","MA_Sup_Protegida.Mayor",
                                                "Eco_Afiliados_Total","Pob_Juventud","Soc_Mov_Muni_5000_T.Mayor",
                                                "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                                "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp","EDU.25_o_más", "EDU.de_4_a_8", 
                                                "EDU.de_9_a_24", "EDU.menor_de_3", "MOV.13_o_más", "MOV.de 6_a_12", 
                                                "MOV.de_4_a_5", "MOV.menor_de_3", "SAL.11_o_más", "SAL.de_4_a_5", 
                                                "SAL.de_6_a_10", "SAL.menor_de_3", "OCI.16_o_más", "OCI.de_4_a_7", 
                                                "OCI.de_8_a_15", "OCI.menor_de_3", "DEGURBA.1", "DEGURBA.2", 
                                                "DEGURBA.3")]), label = train$Pob_EV_Total)
test_matrix <- xgb.DMatrix(as.matrix(test[, c("Eco_Ind_RDBM_PC","Pob_Envejecimiento",
                                              "Eco_Afiliados_Tem_Porc_Resid","Eco_Indice_Gini", 
                                              "Eco_Energ_Elect_PC","Eco_Pre_Gastos_Liquidados",
                                              "Eco_PBI_municipal_pc","Soc_Mov_Hosp_T.Mayor", 
                                              "MOV.13_o_más","Eco_Paro_100","Pob_Empadronada", 
                                              "Eco_Pre_Deuda_Viva","Superficie","MA_Sup_Protegida.Mayor",
                                              "Eco_Afiliados_Total","Pob_Juventud","Soc_Mov_Muni_5000_T.Mayor",
                                              "Soc_Salud_Def_Otras","Soc_Salud_Def_Tumor","Soc_Salud_Def_SistCirc",
                                              "Eco_Contratos_Temp", "Soc_Salud_Def_SistResp","EDU.25_o_más", "EDU.de_4_a_8", 
                                              "EDU.de_9_a_24", "EDU.menor_de_3", "MOV.13_o_más", "MOV.de 6_a_12", 
                                              "MOV.de_4_a_5", "MOV.menor_de_3", "SAL.11_o_más", "SAL.de_4_a_5", 
                                              "SAL.de_6_a_10", "SAL.menor_de_3", "OCI.16_o_más", "OCI.de_4_a_7", 
                                              "OCI.de_8_a_15", "OCI.menor_de_3", "DEGURBA.1", "DEGURBA.2", 
                                              "DEGURBA.3")]), label = test$Pob_EV_Total)


# Entrenar el modelo XGBoost
xgb_model <- xgboost(params, data = train_matrix, nrounds = params$nrounds)

# Realizar predicciones en el conjunto de datos de prueba
predictions2 <- predict(xgb_model, test_matrix)

# Calcular el error cuadrático medio (MSE)
mse <- mean((test$Pob_EV_Total - predictions2)^2)

# Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mse)

# Imprimir el resultado del RMSE
print(rmse)
print(mse)

df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predictions2)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "predictions2") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")

# Para hacerlo con todas las variables.
# train_matrix <- xgb.DMatrix(as.matrix(train[, -which(names(train) == "Pob_EV_Total")]), label = train$Pob_EV_Total)
# test_matrix <- xgb.DMatrix(as.matrix(test[, -which(names(test) == "Pob_EV_Total")]), label = test$Pob_EV_Total)

# XGBoost 6
train_matrix <- xgb.DMatrix(as.matrix(train[, c("Eco_Ind_RDBM_PC","Pob_Grupo.100001_500000_hab", 
                                                           "Pob_Envejecimiento","DEGURBA.2","Eco_Afiliados_Tem_Porc_Resid", 
                                                           "Soc_Mov_Auto_T.menor_de_3","Eco_Energ_Elect_PC","Eco_Indice_Gini", 
                                                           "Eco_Establec_Hotel","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc", 
                                                           "Soc_Mov_Hosp_T.Mayor")]), label = train$Pob_EV_Total)
test_matrix <- xgb.DMatrix(as.matrix(test[, c("Eco_Ind_RDBM_PC","Pob_Grupo.100001_500000_hab", 
                                                           "Pob_Envejecimiento","DEGURBA.2","Eco_Afiliados_Tem_Porc_Resid", 
                                                           "Soc_Mov_Auto_T.menor_de_3","Eco_Energ_Elect_PC","Eco_Indice_Gini", 
                                                           "Eco_Establec_Hotel","Eco_Pre_Gastos_Liquidados","Eco_PBI_municipal_pc", 
                                                           "Soc_Mov_Hosp_T.Mayor")]), label = test$Pob_EV_Total)


# Entrenar el modelo XGBoost
xgb_model <- xgboost(params, data = train_matrix, nrounds = params$nrounds)

# Realizar predicciones en el conjunto de datos de prueba
predictions2 <- predict(xgb_model, test_matrix)

# Calcular el error cuadrático medio (MSE)
mse <- mean((test$Pob_EV_Total - predictions2)^2)

# Calcular el error cuadrático medio (RMSE)
rmse <- sqrt(mse)

# Imprimir el resultado del RMSE
print(rmse)
print(mse)

df <- data.frame(Actual = test$Pob_EV_Total, Predicted = predictions2)

# Crear el gráfico de dispersión
ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "predictions2") +
  ggtitle("Gráfico de Dispersión: Valor Real vs. Predicción")

