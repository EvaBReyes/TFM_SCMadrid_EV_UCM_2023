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

### INICIO CÓDIG FUNCIÓN STEPREPETIDO.R Y CRUZADAS AVNNET Y LIN.R ####

# source("funcion steprepetido.R")
# source("cruzadas avnnet y lin.R")

steprepetido<- function(data=data,vardep="vardep",
                        listconti="listconti",sinicio=12345,sfinal=12355,porcen=0.8,criterio="AIC")
{
  
  library(MASS)
  library(dplyr)
  
  resultados<-data.frame(c())
  data<-data[,c(listconti,vardep)]
  formu1<-formula(paste(vardep,"~.",sep=""))
  formu2<-formula(paste(vardep,"~1",sep=""))
  listamodelos<-list()
  
  for (semilla in sinicio:sfinal)
  {
    
    set.seed(semilla)
    sample <- sample.int(n = nrow(data),
                         size = floor(porcen*nrow(data)), replace = F)
    
    train <- data[sample, ]
    test  <- data[-sample, ]
    
    
    full<-lm(formu1,data=train)
    null<-lm(formu2,data=train)
    
    
    if  (criterio=='AIC')
    {
      selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)
    } 
    else   if  (criterio=='BIC')
    {
      k1=log(nrow(train))
      selec1<-stepAIC(null,scope=list(upper=full),direction="both",k=k1,trace=FALSE)
    }
    
    vec<-(names(selec1[[1]]))
    
    
    # CAMBIOS
    
    cosa<-as.data.frame(table(vec))
    cosa<-as.data.frame(t(cosa))
    colnames(cosa)<-vec
    
    # 1) creo un vector con todas las variables input y ceros
    # 2) voy añadiendo
    
    cosa<-cosa[2,]
    cosa<-cosa[,-c(1)]
    cosa<- data.frame(lapply(cosa, function(x) as.numeric(as.character(x))))
    cosa$id<-semilla
    
    vectormodelo<-list(names(cosa),semilla)
    listamodelos<-append(listamodelos,vectormodelo)  
    
    if (semilla==sinicio)
    {
      listamod<-cosa
    }
    
    else if (semilla!=sinicio)
    {
      listamod<-suppressMessages(full_join(cosa,listamod,by = NULL, copy =TRUE))
    }
    
  }
  
  listamod[is.na(listamod)] <- 0
  
  nom<-names(listamod)
  listamod$modelo<-""
  for (i in 1:nrow(listamod))
  {
    listamod[i,c("modelo")]<-""
    listamod[i,c("contador")]=0
    
    for (vari in nom)
    { 
      if (listamod[i,vari]==1)
      {
        listamod[i,c("modelo")]<-paste(listamod[i,c("modelo")],vari,collapse="",sep="+")
        listamod[i,c("contador")]=listamod[i,c("contador")]+1
      }
      
    }
    
  }
  
  listamod$modelo<-substring(listamod$modelo, 2)
  
  tablamod<-as.data.frame(table(listamod$modelo))
  names(tablamod)<-c("modelo","Freq")
  
  tablamod<-tablamod[order(-tablamod$Freq,tablamod$modelo),]
  
  nuevo<-listamod[,c("modelo","id","contador")]
  
  uni<-full_join(tablamod,nuevo,by ="modelo", copy =TRUE)
  
  uni= uni[!duplicated(uni$modelo),]
  uni$semilla<-semilla
  
  li1<-list()
  # str(listamodelos)
  for (i in 1:nrow(uni))
  {
    for (j in 1:length(listamodelos))
    {
      if (any(uni[i,c("id")]==listamodelos[j][[1]]))
      {
        k<-as.vector(listamodelos[j-1][[1]])
        length(k)<-length(k)-1
        li1<-c(li1,list(k))
        j=length(listamodelos)
      }
    } 
    
  }
  
  uni$semilla<-NULL
  uni$id<-NULL
  return(list(uni,li1))
  
}

cruzadaavnnet<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=FALSE)
    
  { 
    library(caret)
    
    
    
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=databis,
                   method="avNNet",linout = TRUE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid,trace=trace)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }

cruzadalin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5)
    
  { 
    
    library(caret)
    
    
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    lineal<- train(formu,data=databis,
                   method="lm",trControl=control)
    
    print(lineal$results)
    
    preditest<-lineal$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }




### FIN CÓIDGO FUNCIÓN STEPREPETIDO.R Y CRUZADAS AVNNET Y LIN.R ###


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

### INICIO CÓDIGO FUNCIÓN GBM, XGBOOST Y SVM ####

# source("cruzada gbm continua.R")
# source("cruzada xgboost continua.R")
# source("cruzada SVM continua lineal.R")
# source("cruzada SVM continua polinomial.R")
# source("cruzada SVM continua RBF.R")


library(dummies)
library(caret)




cruzadagbm<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2)
    
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    # n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2
    
    gbmgrid <-expand.grid(n.minobsinnode=n.minobsinnode,
                          shrinkage=shrinkage,n.trees=n.trees,
                          interaction.depth=interaction.depth)
    
    gbm<- train(formu,data=databis,
                method="gbm",trControl=control,
                tuneGrid=gbmgrid,distribution="gaussian",verbose=FALSE)
    
    print(gbm$results)
    
    preditest<-gbm$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    return(medias)
    
  }


library(dummies)
library(MASS)
library(reshape)
library(caret)




cruzadaxgbm<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           min_child_weight=20,eta=0.1,nrounds=100,max_depth=2,
           gamma=0,colsample_bytree=1,subsample=1,alpha=0,lambda=0)  
  { 
    library(caret)
    
    library(dummies)
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    xgbmgrid <-expand.grid( min_child_weight=min_child_weight,
                            eta=eta,nrounds=nrounds,max_depth=max_depth,
                            gamma=gamma,colsample_bytree=colsample_bytree,subsample=subsample)
    
    xgbm<- train(formu,data=databis,
                 method="xgbTree",trControl=control,
                 tuneGrid=xgbmgrid,verbose=FALSE,
                 alpha=alpha,lambda=lambda)
    
    print(xgbm$results)
    
    preditest<-xgbm$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }

library(dummies)
library(MASS)
library(reshape)
library(caret)



cruzadaSVM<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5, C=1)  
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C)
    
    SVM<- train(formu,data=databis,
                method="svmLinear",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }


cruzadaSVMpoly<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5, C=1,degree=2,scale=1)  
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,degree=degree,scale=scale)
    
    SVM<- train(formu,data=databis,
                method="svmPoly",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }



cruzadaSVMRBF<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5, C=1,sigma=1)  
  { 
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
    if (any(listclass==c(""))==FALSE)
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",
                          number=grupos,repeats=repe,
                          savePredictions = "all") 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,sigma=sigma)
    
    SVM<- train(formu,data=databis,
                method="svmRadial",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    preditest$error<-(preditest$pred-preditest$obs)^2
    
    
    
    
    tabla<-table(preditest$Rep)
    listarep<-c(names(tabla))
    medias<-data.frame()
    for (repi in listarep) {
      paso1<-preditest[which(preditest$Rep==repi),]
      error=mean(paso1$error)  
      medias<-rbind(medias,error)
    }
    names(medias)<-"error"
    
    
    
    
    return(medias)
    
  }



### FIN CÓDIGO FUNCIÓN GBM, XGBOOST Y SVM ####




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

