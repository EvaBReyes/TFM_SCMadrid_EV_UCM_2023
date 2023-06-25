# limpiar al inicio para ir probando el script.

rm(list = ls())

# cargamos las librerías necesarias.

library(readr)
library(tidyverse)
library(skimr)
library(tidyr)

library(plyr)
library(dplyr)
library(dummies)
library(naniar)
library(corrr)
library(foreign)
library(openxlsx)

# SCM_FINAL_SELML2 tiene ausentes y outliers tratados, dummies, está estandarizada y no tiene Madrid Capital

SCM_FINAL_CLASISAS <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_SELML2.csv", 
            sep = ",", header = TRUE, dec = ".")

mediana <- median(SCM_FINAL_CLASISAS$Pob_EV_Total)
mediana
SCM_FINAL_CLASISAS$Pob_EV_Binaria <- ifelse(SCM_FINAL_CLASISAS$Pob_EV_Total >= mediana, 1, 0)

write.csv(SCM_FINAL_CLASISAS, "SCM_FINAL_CLASISAS.csv", row.names = FALSE)
write.xlsx(SCM_FINAL_CLASISAS, "SCM_FINAL_CLASISAS.xlsx")
