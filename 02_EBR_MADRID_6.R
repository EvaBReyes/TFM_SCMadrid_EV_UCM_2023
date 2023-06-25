# CONSTRUCCIÓN DEL DATASET SCMADRID 
# A PARTIR DE LOS DATOS DE SIDAMUN PARA LA COMUNIDAD DE MADRID

rm(list = ls())

# Librerías necesarias

library(readr)
library(tidyverse)
library(skimr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(readxl)
library(openxlsx)

# Cargo el DF base

DATOS_TFM_4 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/DATOS_TFM_4.xlsx")


# Incorporación de datos. Banco de Datos Territoriales. ALMUDENA.
# Variables a utilizar desde el año 2000 o más antiguo.

# Eco_Afiliados_Tem_Porc_Resid

Eco_Afiliados_Tem_Porc_Resid <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1915822.csv", sep = ";", header = FALSE, skip = 1)
Eco_Afiliados_Tem_Porc_Resid<- Eco_Afiliados_Tem_Porc_Resid [, -c(1,2)]
colnames(Eco_Afiliados_Tem_Porc_Resid) <- Eco_Afiliados_Tem_Porc_Resid[1, ]
colnames(Eco_Afiliados_Tem_Porc_Resid)[1] <- "Entidad"
Eco_Afiliados_Tem_Porc_Resid <- Eco_Afiliados_Tem_Porc_Resid[which(Eco_Afiliados_Tem_Porc_Resid$Entidad == "Acebeda (La)"):(nrow(Eco_Afiliados_Tem_Porc_Resid)), ]
Eco_Afiliados_Tem_Porc_Resid <- Eco_Afiliados_Tem_Porc_Resid[, -ncol(Eco_Afiliados_Tem_Porc_Resid)]
Eco_Afiliados_Tem_Porc_Resid <- na.omit(Eco_Afiliados_Tem_Porc_Resid)
Eco_Afiliados_Tem_Porc_Resid <- Eco_Afiliados_Tem_Porc_Resid[, -c(2:which(colnames(Eco_Afiliados_Tem_Porc_Resid) == "2014"))]
Eco_Afiliados_Tem_Porc_Resid <- head(Eco_Afiliados_Tem_Porc_Resid, 179)
Eco_Afiliados_Tem_Porc_Resid <- pivot_longer(Eco_Afiliados_Tem_Porc_Resid, cols = -c(Entidad), names_to = "año", values_to = "Eco_Afiliados_Tem_Porc_Resid")
Eco_Afiliados_Tem_Porc_Resid <- Eco_Afiliados_Tem_Porc_Resid  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Afiliados_Tem_Porc_Resid = as.numeric(Eco_Afiliados_Tem_Porc_Resid))


# Eco_Afiliados_Total

Eco_Afiliados_Total <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1915782.csv", sep = ";", header = FALSE, skip = 1)
Eco_Afiliados_Total<- Eco_Afiliados_Total [, -c(1,2)]
colnames(Eco_Afiliados_Total) <- Eco_Afiliados_Total[1, ]
colnames(Eco_Afiliados_Total)[1] <- "Entidad"
Eco_Afiliados_Total <- Eco_Afiliados_Total[which(Eco_Afiliados_Total$Entidad == "Acebeda (La)"):(nrow(Eco_Afiliados_Total)), ]
Eco_Afiliados_Total <- Eco_Afiliados_Total[, -ncol(Eco_Afiliados_Total)]
Eco_Afiliados_Total <- na.omit(Eco_Afiliados_Total)
Eco_Afiliados_Total <- Eco_Afiliados_Total[, -c(2:which(colnames(Eco_Afiliados_Total) == "2014"))]
Eco_Afiliados_Total <- head(Eco_Afiliados_Total, 179)
Eco_Afiliados_Total <- pivot_longer(Eco_Afiliados_Total, cols = -c(Entidad), names_to = "año", values_to = "Eco_Afiliados_Total")

# Eco_Contratos_Indef

Eco_Contratos_Indef <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1900745.csv", sep = ";", header = FALSE, skip = 1)
Eco_Contratos_Indef<- Eco_Contratos_Indef [, -c(1,2)]
colnames(Eco_Contratos_Indef) <- Eco_Contratos_Indef[1, ]
colnames(Eco_Contratos_Indef)[1] <- "Entidad"
Eco_Contratos_Indef <- Eco_Contratos_Indef[which(Eco_Contratos_Indef$Entidad == "Acebeda (La)"):(nrow(Eco_Contratos_Indef)), ]
Eco_Contratos_Indef <- Eco_Contratos_Indef[, -ncol(Eco_Contratos_Indef)]
Eco_Contratos_Indef <- na.omit(Eco_Contratos_Indef)
Eco_Contratos_Indef <- Eco_Contratos_Indef[, -c(2:which(colnames(Eco_Contratos_Indef) == "2005"))]
Eco_Contratos_Indef <- head(Eco_Contratos_Indef, 179)
Eco_Contratos_Indef <- pivot_longer(Eco_Contratos_Indef, cols = -c(Entidad), names_to = "año", values_to = "Eco_Contratos_Indef")

skim(Eco_Contratos_Indef)

# Eco_Contratos_Temp

Eco_Contratos_Temp <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902955.csv", sep = ";", header = FALSE, skip = 1)
Eco_Contratos_Temp<- Eco_Contratos_Temp [, -c(1,2)]
colnames(Eco_Contratos_Temp) <- Eco_Contratos_Temp[1, ]
colnames(Eco_Contratos_Temp)[1] <- "Entidad"
Eco_Contratos_Temp <- Eco_Contratos_Temp[which(Eco_Contratos_Temp$Entidad == "Acebeda (La)"):(nrow(Eco_Contratos_Temp)), ]
Eco_Contratos_Temp <- Eco_Contratos_Temp[, -ncol(Eco_Contratos_Temp)]
Eco_Contratos_Temp <- na.omit(Eco_Contratos_Temp)
Eco_Contratos_Temp <- Eco_Contratos_Temp[, -c(2:which(colnames(Eco_Contratos_Temp) == "2005"))]
Eco_Contratos_Temp <- head(Eco_Contratos_Temp, 179)
Eco_Contratos_Temp <- pivot_longer(Eco_Contratos_Temp, cols = -c(Entidad), names_to = "año", values_to = "Eco_Contratos_Temp")
Eco_Contratos_Temp <- Eco_Contratos_Temp  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Contratos_Temp = as.numeric(Eco_Contratos_Temp))


# Eco_Contratos_Total

Eco_Contratos_Total <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1900742.csv", sep = ";", header = FALSE, skip = 1)
Eco_Contratos_Total<- Eco_Contratos_Total [, -c(1,2)]
colnames(Eco_Contratos_Total) <- Eco_Contratos_Total[1, ]
colnames(Eco_Contratos_Total)[1] <- "Entidad"
Eco_Contratos_Total <- Eco_Contratos_Total[which(Eco_Contratos_Total$Entidad == "Acebeda (La)"):(nrow(Eco_Contratos_Total)), ]
Eco_Contratos_Total <- Eco_Contratos_Total[, -ncol(Eco_Contratos_Total)]
Eco_Contratos_Total <- na.omit(Eco_Contratos_Total)
Eco_Contratos_Total <- Eco_Contratos_Total[, -c(2:which(colnames(Eco_Contratos_Total) == "2005"))]
Eco_Contratos_Total <- head(Eco_Contratos_Total, 179)
Eco_Contratos_Total <- pivot_longer(Eco_Contratos_Total, cols = -c(Entidad), names_to = "año", values_to = "Eco_Contratos_Total")

# Eco_Energ_Elect_PC

Eco_Energ_Elect_PC <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1203004.csv", sep = ";", header = FALSE, skip = 1)
Eco_Energ_Elect_PC<- Eco_Energ_Elect_PC [, -c(1,2)]
colnames(Eco_Energ_Elect_PC) <- Eco_Energ_Elect_PC[1, ]
colnames(Eco_Energ_Elect_PC)[1] <- "Entidad"
Eco_Energ_Elect_PC <- Eco_Energ_Elect_PC[which(Eco_Energ_Elect_PC$Entidad == "Acebeda (La)"):(nrow(Eco_Energ_Elect_PC)), ]
Eco_Energ_Elect_PC <- Eco_Energ_Elect_PC[, -ncol(Eco_Energ_Elect_PC)]
Eco_Energ_Elect_PC <- na.omit(Eco_Energ_Elect_PC)
Eco_Energ_Elect_PC <- Eco_Energ_Elect_PC[, -c(2:which(colnames(Eco_Energ_Elect_PC) == "2000"))]
Eco_Energ_Elect_PC <- head(Eco_Energ_Elect_PC, 179)
Eco_Energ_Elect_PC <- pivot_longer(Eco_Energ_Elect_PC, cols = -c(Entidad), names_to = "año", values_to = "Eco_Energ_Elect_PC")
Eco_Energ_Elect_PC <- Eco_Energ_Elect_PC  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Energ_Elect_PC = as.numeric(Eco_Energ_Elect_PC))


# Eco_Establec_Hotel

Eco_Establec_Hotel <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1302067.csv", sep = ";", header = FALSE, skip = 1)
Eco_Establec_Hotel<- Eco_Establec_Hotel [, -c(1,2)]
colnames(Eco_Establec_Hotel) <- Eco_Establec_Hotel[1, ]
colnames(Eco_Establec_Hotel)[1] <- "Entidad"
Eco_Establec_Hotel <- Eco_Establec_Hotel[which(Eco_Establec_Hotel$Entidad == "Acebeda (La)"):(nrow(Eco_Establec_Hotel)), ]
Eco_Establec_Hotel <- Eco_Establec_Hotel[, -ncol(Eco_Establec_Hotel)]
Eco_Establec_Hotel <- na.omit(Eco_Establec_Hotel)
Eco_Establec_Hotel <- Eco_Establec_Hotel[, -c(2:which(colnames(Eco_Establec_Hotel) == "2008"))]
Eco_Establec_Hotel <- head(Eco_Establec_Hotel, 179)
Eco_Establec_Hotel <- pivot_longer(Eco_Establec_Hotel, cols = -c(Entidad), names_to = "año", values_to = "Eco_Establec_Hotel")


# Eco_Ind_RDBM_PC

Eco_Ind_RDBM_PC <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1926581.csv", sep = ";", header = FALSE, skip = 1)
Eco_Ind_RDBM_PC<- Eco_Ind_RDBM_PC [, -c(1,2)]
colnames(Eco_Ind_RDBM_PC) <- Eco_Ind_RDBM_PC[1, ]
colnames(Eco_Ind_RDBM_PC)[1] <- "Entidad"
Eco_Ind_RDBM_PC <- Eco_Ind_RDBM_PC[which(Eco_Ind_RDBM_PC$Entidad == "Acebeda (La)"):(nrow(Eco_Ind_RDBM_PC)), ]
Eco_Ind_RDBM_PC <- Eco_Ind_RDBM_PC[, -ncol(Eco_Ind_RDBM_PC)]
Eco_Ind_RDBM_PC <- na.omit(Eco_Ind_RDBM_PC)
Eco_Ind_RDBM_PC <- Eco_Ind_RDBM_PC[, -c(2:which(colnames(Eco_Ind_RDBM_PC) == "2000"))]
colnames(Eco_Ind_RDBM_PC)[colnames(Eco_Ind_RDBM_PC) == "2020(A)"] <- "2020"
Eco_Ind_RDBM_PC <- head(Eco_Ind_RDBM_PC, 179)
Eco_Ind_RDBM_PC <- pivot_longer(Eco_Ind_RDBM_PC, cols = -c(Entidad), names_to = "año", values_to = "Eco_Ind_RDBM_PC")
Eco_Ind_RDBM_PC <- Eco_Ind_RDBM_PC  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Ind_RDBM_PC = as.numeric(Eco_Ind_RDBM_PC))


# Eco_Paro_100

Eco_Paro_100 <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902280.csv", sep = ";", header = FALSE, skip = 1)
Eco_Paro_100<- Eco_Paro_100 [, -c(1,2)]
colnames(Eco_Paro_100) <- Eco_Paro_100[1, ]
colnames(Eco_Paro_100)[1] <- "Entidad"
Eco_Paro_100 <- Eco_Paro_100[which(Eco_Paro_100$Entidad == "Acebeda (La)"):(nrow(Eco_Paro_100)), ]
Eco_Paro_100 <- Eco_Paro_100[, -ncol(Eco_Paro_100)]
Eco_Paro_100 <- na.omit(Eco_Paro_100)
Eco_Paro_100 <- Eco_Paro_100[, -c(2:which(colnames(Eco_Paro_100) == "2006"))]
Eco_Paro_100 <- head(Eco_Paro_100, 179)
Eco_Paro_100 <- pivot_longer(Eco_Paro_100, cols = -c(Entidad), names_to = "año", values_to = "Eco_Paro_100")
Eco_Paro_100 <- Eco_Paro_100  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Paro_100 = as.numeric(Eco_Paro_100))

# Eco_Paro_Porc_Evol

Eco_Paro_Porc_Evol <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1903260.csv", sep = ";", header = FALSE, skip = 1)
Eco_Paro_Porc_Evol<- Eco_Paro_Porc_Evol [, -c(1,2)]
colnames(Eco_Paro_Porc_Evol) <- Eco_Paro_Porc_Evol[1, ]
colnames(Eco_Paro_Porc_Evol)[1] <- "Entidad"
Eco_Paro_Porc_Evol <- Eco_Paro_Porc_Evol[which(Eco_Paro_Porc_Evol$Entidad == "Acebeda (La)"):(nrow(Eco_Paro_Porc_Evol)), ]
Eco_Paro_Porc_Evol <- Eco_Paro_Porc_Evol[, -ncol(Eco_Paro_Porc_Evol)]
Eco_Paro_Porc_Evol <- na.omit(Eco_Paro_Porc_Evol)
Eco_Paro_Porc_Evol <- Eco_Paro_Porc_Evol[, -c(2:which(colnames(Eco_Paro_Porc_Evol) == "2007"))]
Eco_Paro_Porc_Evol <- head(Eco_Paro_Porc_Evol, 179)
Eco_Paro_Porc_Evol <- pivot_longer(Eco_Paro_Porc_Evol, cols = -c(Entidad), names_to = "año", values_to = "Eco_Paro_Porc_Evol")
Eco_Paro_Porc_Evol <- Eco_Paro_Porc_Evol  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Paro_Porc_Evol = as.numeric(Eco_Paro_Porc_Evol))

# Eco_PBI_municipal_pc

Eco_PBI_municipal_pc <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1927480.csv", sep = ";", header = FALSE, skip = 1)
Eco_PBI_municipal_pc <- Eco_PBI_municipal_pc[, -c(1, 2)]
colnames(Eco_PBI_municipal_pc) <- as.character(Eco_PBI_municipal_pc[1, ])
colnames(Eco_PBI_municipal_pc)[1] <- "Entidad"
Eco_PBI_municipal_pc <- Eco_PBI_municipal_pc[which(Eco_PBI_municipal_pc$Entidad == "Acebeda (La)"):(nrow(Eco_PBI_municipal_pc)), ]
Eco_PBI_municipal_pc <- Eco_PBI_municipal_pc[, -ncol(Eco_PBI_municipal_pc)]
Eco_PBI_municipal_pc <- na.omit(Eco_PBI_municipal_pc)
Eco_PBI_municipal_pc <- head(Eco_PBI_municipal_pc, 179)
# Las columnas de los años son avances y provisionales, hay que renombrar
colnames(Eco_PBI_municipal_pc) <- as.character(c("Entidad", "2015", "2016", "2017", "2018", "2019"))
# Los datos de los años 2018 y 2019 son character y deben ser numéricos para el pivot_longer
Eco_PBI_municipal_pc[, 2:6] <- apply(Eco_PBI_municipal_pc[, 2:6], 2, as.numeric)
Eco_PBI_municipal_pc <- pivot_longer(Eco_PBI_municipal_pc, cols = -Entidad, names_to = "año", values_to = "Eco_PBI_municipal_pc")


# Eco_Uds_Productivas

Eco_Uds_Productivas <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1919242.csv", sep = ";", header = FALSE, skip = 1)
Eco_Uds_Productivas<- Eco_Uds_Productivas [, -c(1,2)]
colnames(Eco_Uds_Productivas) <- Eco_Uds_Productivas[1, ]
colnames(Eco_Uds_Productivas)[1] <- "Entidad"
Eco_Uds_Productivas <- Eco_Uds_Productivas[which(Eco_Uds_Productivas$Entidad == "Acebeda (La)"):(nrow(Eco_Uds_Productivas)), ]
Eco_Uds_Productivas <- Eco_Uds_Productivas[, -ncol(Eco_Uds_Productivas)]
Eco_Uds_Productivas <- na.omit(Eco_Uds_Productivas)
Eco_Uds_Productivas <- Eco_Uds_Productivas[, -c(2:which(colnames(Eco_Uds_Productivas) == "2015"))]
Eco_Uds_Productivas <- head(Eco_Uds_Productivas, 179)
colnames(Eco_Uds_Productivas)[colnames(Eco_Uds_Productivas) == "2022(A)"] <- "2022"
Eco_Uds_Productivas[, 2:8] <- apply(Eco_Uds_Productivas[, 2:8], 2, as.numeric)
Eco_Uds_Productivas <- pivot_longer(Eco_Uds_Productivas, cols = -c(Entidad), names_to = "año", values_to = "Eco_Uds_Productivas")


# Eco_Distrib_Renta_P80_20
# Eco_Indice_Gini

IG_2015_2020 <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/CALIDAD DE VIDA/IG_2015_2020.xlsx")
P80_20_2015_2020 <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/CALIDAD DE VIDA/P80_20_2015_2020.xlsx")
Eco_Renta_Neta_Media_PP <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/CALIDAD DE VIDA/Eco_Renta_Neta_Media_PP.xlsx")


Eco_Indice_Gini <- IG_2015_2020 |> 
  pivot_longer(cols = c("2020","2019","2018","2017","2016","2015"), 
               names_to = "año", values_to = "Eco_Indice_Gini") |> 
  mutate(CMUNXL=substr(CMUNXL, 1, 5)) 
  
Eco_Indice_Gini <- join(Eco_Indice_Gini, DATOS_TFM_4 %>% 
                          select(CMUNXL, Entidad), by = "CMUNXL")


Eco_Distrib_Renta_P80_20 <- P80_20_2015_2020 |> 
  pivot_longer(cols = c("2020","2019","2018","2017","2016","2015"), 
               names_to = "año", values_to = "Eco_Distrib_Renta_P80_20") |> 
  mutate(CMUNXL=substr(CMUNXL, 1, 5))

Eco_Distrib_Renta_P80_20 <- join(Eco_Distrib_Renta_P80_20, DATOS_TFM_4 %>% 
                          select(CMUNXL, Entidad), by = "CMUNXL")


Eco_Renta_Neta_Media_PP$`2020` <- as.character(Eco_Renta_Neta_Media_PP$`2020`)

Eco_Renta_Neta_Media_PP <- Eco_Renta_Neta_Media_PP |> 
  pivot_longer(cols = c("2020","2019","2018","2017","2016","2015"), 
               names_to = "año", values_to = "Eco_Renta_Neta_Media_PP") |> 
  mutate(CMUNXL=substr(CMUNXL, 1, 5)) 

Eco_Renta_Neta_Media_PP <- join(Eco_Renta_Neta_Media_PP, DATOS_TFM_4 %>% 
                          select(CMUNXL, Entidad), by = "CMUNXL")

Eco_Indice_Gini <- Eco_Indice_Gini  |> 
  mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Indice_Gini = as.numeric(Eco_Indice_Gini))
Eco_Distrib_Renta_P80_20 <- Eco_Distrib_Renta_P80_20  |> 
  mutate(across(where(is.numeric),as.numeric))  |> mutate(Eco_Distrib_Renta_P80_20 = as.numeric(Eco_Distrib_Renta_P80_20))
Eco_Renta_Neta_Media_PP <- Eco_Renta_Neta_Media_PP  |> mutate(across(where(is.numeric),as.numeric))  |> 
  mutate(Eco_Renta_Neta_Media_PP = as.numeric(Eco_Renta_Neta_Media_PP))

# Pob_Crecim_Relativo

Pob_Crecim_Relativo <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201161.csv", sep = ";", header = FALSE, skip = 1)
Pob_Crecim_Relativo<- Pob_Crecim_Relativo [, -c(1,2)]
colnames(Pob_Crecim_Relativo) <- Pob_Crecim_Relativo[1, ]
colnames(Pob_Crecim_Relativo)[1] <- "Entidad"
Pob_Crecim_Relativo <- Pob_Crecim_Relativo[which(Pob_Crecim_Relativo$Entidad == "Acebeda (La)"):(nrow(Pob_Crecim_Relativo)), ]
Pob_Crecim_Relativo <- Pob_Crecim_Relativo[, -ncol(Pob_Crecim_Relativo)]
Pob_Crecim_Relativo <- na.omit(Pob_Crecim_Relativo)
Pob_Crecim_Relativo <- Pob_Crecim_Relativo[, -c(2:which(colnames(Pob_Crecim_Relativo) == "2003"))]
Pob_Crecim_Relativo <- head(Pob_Crecim_Relativo, 179)
Pob_Crecim_Relativo <- pivot_longer(Pob_Crecim_Relativo, cols = -c(Entidad), names_to = "año", values_to = "Pob_Crecim_Relativo")
Pob_Crecim_Relativo <- Pob_Crecim_Relativo  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Pob_Crecim_Relativo = as.numeric(Pob_Crecim_Relativo))

# Pob_Dependencia

Pob_Dependencia <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201161.csv", sep = ";", header = FALSE, skip = 1)
Pob_Dependencia<- Pob_Dependencia [, -c(1,2)]
colnames(Pob_Dependencia) <- Pob_Dependencia[1, ]
colnames(Pob_Dependencia)[1] <- "Entidad"
Pob_Dependencia <- Pob_Dependencia[which(Pob_Dependencia$Entidad == "Acebeda (La)"):(nrow(Pob_Dependencia)), ]
Pob_Dependencia <- Pob_Dependencia[, -ncol(Pob_Dependencia)]
Pob_Dependencia <- na.omit(Pob_Dependencia)
Pob_Dependencia <- Pob_Dependencia[, -c(2:which(colnames(Pob_Dependencia) == "2003"))]
Pob_Dependencia <- head(Pob_Dependencia, 179)
Pob_Dependencia <- pivot_longer(Pob_Dependencia, cols = -c(Entidad), names_to = "año", values_to = "Pob_Dependencia")
Pob_Dependencia <- Pob_Dependencia  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Pob_Dependencia = as.numeric(Pob_Dependencia))


# Pob_Edad_Media

Pob_Edad_Media <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201276.csv", sep = ";", header = FALSE, skip = 1)
Pob_Edad_Media<- Pob_Edad_Media [, -c(1,2)]
colnames(Pob_Edad_Media) <- Pob_Edad_Media[1, ]
colnames(Pob_Edad_Media)[1] <- "Entidad"
Pob_Edad_Media <- Pob_Edad_Media[which(Pob_Edad_Media$Entidad == "Acebeda (La)"):(nrow(Pob_Edad_Media)), ]
Pob_Edad_Media <- Pob_Edad_Media[, -ncol(Pob_Edad_Media)]
Pob_Edad_Media <- na.omit(Pob_Edad_Media)
Pob_Edad_Media <- Pob_Edad_Media[, -c(2:which(colnames(Pob_Edad_Media) == "2000"))]
Pob_Edad_Media <- head(Pob_Edad_Media, 179)
Pob_Edad_Media <- pivot_longer(Pob_Edad_Media, cols = -c(Entidad), names_to = "año", values_to = "Pob_Edad_Media")
Pob_Edad_Media <- Pob_Edad_Media  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Pob_Edad_Media = as.numeric(Pob_Edad_Media))


# Pob_Empadronada

Pob_Empadronada <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201007.csv", sep = ";", header = FALSE, skip = 1)
Pob_Empadronada<- Pob_Empadronada [, -c(1,2)]
colnames(Pob_Empadronada) <- Pob_Empadronada[1, ]
colnames(Pob_Empadronada)[1] <- "Entidad"
Pob_Empadronada <- Pob_Empadronada[which(Pob_Empadronada$Entidad == "Acebeda (La)"):(nrow(Pob_Empadronada)), ]
Pob_Empadronada <- Pob_Empadronada[, -ncol(Pob_Empadronada)]
Pob_Empadronada <- na.omit(Pob_Empadronada)
Pob_Empadronada <- Pob_Empadronada[, -c(2:which(colnames(Pob_Empadronada) == "2000"))]
Pob_Empadronada <- head(Pob_Empadronada, 179)
Pob_Empadronada <- pivot_longer(Pob_Empadronada, cols = -c(Entidad), names_to = "año", values_to = "Pob_Empadronada")


# Pob_Empadronada_H

Pob_Empadronada_H <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201158.csv", sep = ";", header = FALSE, skip = 1)
Pob_Empadronada_H<- Pob_Empadronada_H [, -c(1,2)]
colnames(Pob_Empadronada_H) <- Pob_Empadronada_H[1, ]
colnames(Pob_Empadronada_H)[1] <- "Entidad"
Pob_Empadronada_H <- Pob_Empadronada_H[which(Pob_Empadronada_H$Entidad == "Acebeda (La)"):(nrow(Pob_Empadronada_H)), ]
Pob_Empadronada_H <- Pob_Empadronada_H[, -ncol(Pob_Empadronada_H)]
Pob_Empadronada_H <- na.omit(Pob_Empadronada_H)
Pob_Empadronada_H <- Pob_Empadronada_H[, -c(2:which(colnames(Pob_Empadronada_H) == "2000"))]
Pob_Empadronada_H <- head(Pob_Empadronada_H, 179)
Pob_Empadronada_H <- pivot_longer(Pob_Empadronada_H, cols = -c(Entidad), names_to = "año", values_to = "Pob_Empadronada_H")


# Pob_Empadronada_M

Pob_Empadronada_M <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201005.csv", sep = ";", header = FALSE, skip = 1)
Pob_Empadronada_M<- Pob_Empadronada_M [, -c(1,2)]
colnames(Pob_Empadronada_M) <- Pob_Empadronada_M[1, ]
colnames(Pob_Empadronada_M)[1] <- "Entidad"
Pob_Empadronada_M <- Pob_Empadronada_M[which(Pob_Empadronada_M$Entidad == "Acebeda (La)"):(nrow(Pob_Empadronada_M)), ]
Pob_Empadronada_M <- Pob_Empadronada_M[, -ncol(Pob_Empadronada_M)]
Pob_Empadronada_M <- na.omit(Pob_Empadronada_M)
Pob_Empadronada_M <- Pob_Empadronada_M[, -c(2:which(colnames(Pob_Empadronada_M) == "2000"))]
Pob_Empadronada_M <- head(Pob_Empadronada_M, 179)
Pob_Empadronada_M <- pivot_longer(Pob_Empadronada_M, cols = -c(Entidad), names_to = "año", values_to = "Pob_Empadronada_M")


# Pob_Envejecimiento

Pob_Envejecimiento <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201273.csv", sep = ";", header = FALSE, skip = 1)
Pob_Envejecimiento<- Pob_Envejecimiento [, -c(1,2)]
colnames(Pob_Envejecimiento) <- Pob_Envejecimiento[1, ]
colnames(Pob_Envejecimiento)[1] <- "Entidad"
Pob_Envejecimiento <- Pob_Envejecimiento[which(Pob_Envejecimiento$Entidad == "Acebeda (La)"):(nrow(Pob_Envejecimiento)), ]
Pob_Envejecimiento <- Pob_Envejecimiento[, -ncol(Pob_Envejecimiento)]
Pob_Envejecimiento <- na.omit(Pob_Envejecimiento)
Pob_Envejecimiento <- Pob_Envejecimiento[, -c(2:which(colnames(Pob_Envejecimiento) == "2000"))]
Pob_Envejecimiento <- head(Pob_Envejecimiento, 179)
Pob_Envejecimiento <- pivot_longer(Pob_Envejecimiento, cols = -c(Entidad), names_to = "año", values_to = "Pob_Envejecimiento")
Pob_Envejecimiento <- Pob_Envejecimiento  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Pob_Envejecimiento = as.numeric(Pob_Envejecimiento))


# Pob_Juventud

Pob_Juventud <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/201090.csv", sep = ";", header = FALSE, skip = 1)
Pob_Juventud<- Pob_Juventud [, -c(1,2)]
colnames(Pob_Juventud) <- Pob_Juventud[1, ]
colnames(Pob_Juventud)[1] <- "Entidad"
Pob_Juventud <- Pob_Juventud[which(Pob_Juventud$Entidad == "Acebeda (La)"):(nrow(Pob_Juventud)), ]
Pob_Juventud <- Pob_Juventud[, -ncol(Pob_Juventud)]
Pob_Juventud <- na.omit(Pob_Juventud)
Pob_Juventud <- Pob_Juventud[, -c(2:which(colnames(Pob_Juventud) == "2000"))]
Pob_Juventud <- head(Pob_Juventud, 179)
Pob_Juventud <- pivot_longer(Pob_Juventud, cols = -c(Entidad), names_to = "año", values_to = "Pob_Juventud")
Pob_Juventud <- Pob_Juventud  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Pob_Juventud = as.numeric(Pob_Juventud))



# Pob_Saldo_Migratorio

Pob_Saldo_Migratorio <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/204008.csv", sep = ";", header = FALSE, skip = 1)
Pob_Saldo_Migratorio<- Pob_Saldo_Migratorio [, -c(1,2)]
colnames(Pob_Saldo_Migratorio) <- Pob_Saldo_Migratorio[1, ]
colnames(Pob_Saldo_Migratorio)[1] <- "Entidad"
Pob_Saldo_Migratorio <- Pob_Saldo_Migratorio[which(Pob_Saldo_Migratorio$Entidad == "Acebeda (La)"):(nrow(Pob_Saldo_Migratorio)), ]
Pob_Saldo_Migratorio <- Pob_Saldo_Migratorio[, -ncol(Pob_Saldo_Migratorio)]
Pob_Saldo_Migratorio <- na.omit(Pob_Saldo_Migratorio)
Pob_Saldo_Migratorio <- Pob_Saldo_Migratorio[, -c(2:which(colnames(Pob_Saldo_Migratorio) == "2000"))]
Pob_Saldo_Migratorio <- head(Pob_Saldo_Migratorio, 179)
Pob_Saldo_Migratorio <- pivot_longer(Pob_Saldo_Migratorio, cols = -c(Entidad), names_to = "año", values_to = "Pob_Saldo_Migratorio")

# Pob_Def_Total

Pob_Def_Total <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/202005.csv", sep = ";", header = FALSE, skip = 1)
Pob_Def_Total<- Pob_Def_Total [, -c(1,2)]
colnames(Pob_Def_Total) <- Pob_Def_Total[1, ]
colnames(Pob_Def_Total)[1] <- "Entidad"
Pob_Def_Total <- Pob_Def_Total[which(Pob_Def_Total$Entidad == "Acebeda (La)"):(nrow(Pob_Def_Total)), ]
Pob_Def_Total <- Pob_Def_Total[, -ncol(Pob_Def_Total)]
Pob_Def_Total <- na.omit(Pob_Def_Total)
Pob_Def_Total <- Pob_Def_Total[, -c(2:which(colnames(Pob_Def_Total) == "2001"))]
Pob_Def_Total <- head(Pob_Def_Total, 179)
Pob_Def_Total <- pivot_longer(Pob_Def_Total, cols = -c(Entidad), names_to = "año", values_to = "Pob_Def_Total")

# Pob_EV_65_H
# Pob_EV_65_M
# Pob_EV_65_Total
# Pob_EV_H
# Pob_EV_M
# Pob_EV_Total

EV_MUNI_INE_2014_2020 <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/DEMOGRAFÍA Y POBLACIÓN/EV_MUNI_INE_2014_2020.xlsx")
EV_65_MUNI_INE_2014_2020 <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/DEMOGRAFÍA Y POBLACIÓN/EV_65_MUNI_INE_2014_2020_EXCEL.xlsx")

EV_MUNI_INE_2014_2020 <- EV_MUNI_INE_2014_2020 |> 
  mutate(CMUNXL = as.numeric(CMUNXL)) |> 
  filter(grepl("^28", CMUNXL)) |> 
  pivot_wider(names_from = Sexo, values_from = Total) 

colnames(EV_MUNI_INE_2014_2020) <- c("CMUNXL", "Entidad", "año",  "Pob_EV_Total", "Pob_EV_H", "Pob_EV_M")


EV_65_MUNI_INE_2014_2020 <- EV_65_MUNI_INE_2014_2020 %>%
  mutate(CMUNXL = as.numeric(CMUNXL)) %>%
  filter(grepl("^28", CMUNXL)) %>%
  pivot_wider(names_from = Sexo, values_from = Total)

colnames(EV_65_MUNI_INE_2014_2020) <- c("CMUNXL", "Entidad", "año", "Pob_EV_65_Total", "Pob_EV_65_H", "Pob_EV_65_M")

Pob_EV <- merge(EV_MUNI_INE_2014_2020, EV_65_MUNI_INE_2014_2020, by = c("año", "CMUNXL", "Entidad"))
Pob_EV[c("año", "CMUNXL", "Entidad")] <- lapply(Pob_EV[c("año", "CMUNXL", "Entidad")], as.character)

skim(Pob_EV)


# Soc_Bibliot_públicas

Soc_Bibliot_públicas <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/701042.csv", sep = ";", header = FALSE, skip = 1)
Soc_Bibliot_públicas<- Soc_Bibliot_públicas [, -c(1,2)]
colnames(Soc_Bibliot_públicas) <- Soc_Bibliot_públicas[1, ]
colnames(Soc_Bibliot_públicas)[1] <- "Entidad"
Soc_Bibliot_públicas <- Soc_Bibliot_públicas[which(Soc_Bibliot_públicas$Entidad == "Acebeda (La)"):(nrow(Soc_Bibliot_públicas)), ]
Soc_Bibliot_públicas <- Soc_Bibliot_públicas[, -ncol(Soc_Bibliot_públicas)]
Soc_Bibliot_públicas <- na.omit(Soc_Bibliot_públicas)
Soc_Bibliot_públicas <- Soc_Bibliot_públicas[, -c(2:which(colnames(Soc_Bibliot_públicas) == "2000"))]
Soc_Bibliot_públicas <- head(Soc_Bibliot_públicas, 179)
Soc_Bibliot_públicas <- pivot_longer(Soc_Bibliot_públicas, cols = -c(Entidad), names_to = "año", values_to = "Soc_Bibliot_públicas")

# Soc_Ed_Cent_Priv_NoUni

Soc_Ed_Cent_Priv_NoUni <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902485.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_Cent_Priv_NoUni<- Soc_Ed_Cent_Priv_NoUni [, -c(1,2)]
colnames(Soc_Ed_Cent_Priv_NoUni) <- Soc_Ed_Cent_Priv_NoUni[1, ]
colnames(Soc_Ed_Cent_Priv_NoUni)[1] <- "Entidad"
Soc_Ed_Cent_Priv_NoUni <- Soc_Ed_Cent_Priv_NoUni[which(Soc_Ed_Cent_Priv_NoUni$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_Cent_Priv_NoUni)), ]
Soc_Ed_Cent_Priv_NoUni <- Soc_Ed_Cent_Priv_NoUni[, -ncol(Soc_Ed_Cent_Priv_NoUni)]
Soc_Ed_Cent_Priv_NoUni <- na.omit(Soc_Ed_Cent_Priv_NoUni)
Soc_Ed_Cent_Priv_NoUni <- Soc_Ed_Cent_Priv_NoUni[, -c(2:which(colnames(Soc_Ed_Cent_Priv_NoUni) == "2005"))]
Soc_Ed_Cent_Priv_NoUni <- head(Soc_Ed_Cent_Priv_NoUni, 179)
colnames(Soc_Ed_Cent_Priv_NoUni)[colnames(Soc_Ed_Cent_Priv_NoUni) == "2021(P)"] <- "2021"
Soc_Ed_Cent_Priv_NoUni[, 2:ncol(Soc_Ed_Cent_Priv_NoUni)] <- apply(Soc_Ed_Cent_Priv_NoUni[, 2:ncol(Soc_Ed_Cent_Priv_NoUni)], 2, as.numeric)
Soc_Ed_Cent_Priv_NoUni <- pivot_longer(Soc_Ed_Cent_Priv_NoUni, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_Cent_Priv_NoUni")



# Soc_Ed_Cent_Pub_NoUni

Soc_Ed_Cent_Pub_NoUni <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902484.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_Cent_Pub_NoUni<- Soc_Ed_Cent_Pub_NoUni [, -c(1,2)]
colnames(Soc_Ed_Cent_Pub_NoUni) <- Soc_Ed_Cent_Pub_NoUni[1, ]
colnames(Soc_Ed_Cent_Pub_NoUni)[1] <- "Entidad"
Soc_Ed_Cent_Pub_NoUni <- Soc_Ed_Cent_Pub_NoUni[which(Soc_Ed_Cent_Pub_NoUni$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_Cent_Pub_NoUni)), ]
Soc_Ed_Cent_Pub_NoUni <- Soc_Ed_Cent_Pub_NoUni[, -ncol(Soc_Ed_Cent_Pub_NoUni)]
Soc_Ed_Cent_Pub_NoUni <- na.omit(Soc_Ed_Cent_Pub_NoUni)
Soc_Ed_Cent_Pub_NoUni <- Soc_Ed_Cent_Pub_NoUni[, -c(2:which(colnames(Soc_Ed_Cent_Pub_NoUni) == "2005"))]
Soc_Ed_Cent_Pub_NoUni <- head(Soc_Ed_Cent_Pub_NoUni, 179)
colnames(Soc_Ed_Cent_Pub_NoUni)[colnames(Soc_Ed_Cent_Pub_NoUni) == "2021(P)"] <- "2021"
Soc_Ed_Cent_Pub_NoUni[, 2:ncol(Soc_Ed_Cent_Pub_NoUni)] <- apply(Soc_Ed_Cent_Pub_NoUni[, 2:ncol(Soc_Ed_Cent_Pub_NoUni)], 2, as.numeric)
Soc_Ed_Cent_Pub_NoUni <- pivot_longer(Soc_Ed_Cent_Pub_NoUni, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_Cent_Pub_NoUni")


# Soc_Ed_Infantil

Soc_Ed_Infantil <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902409.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_Infantil<- Soc_Ed_Infantil [, -c(1,2)]
colnames(Soc_Ed_Infantil) <- Soc_Ed_Infantil[1, ]
colnames(Soc_Ed_Infantil)[1] <- "Entidad"
Soc_Ed_Infantil <- Soc_Ed_Infantil[which(Soc_Ed_Infantil$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_Infantil)), ]
Soc_Ed_Infantil <- Soc_Ed_Infantil[, -ncol(Soc_Ed_Infantil)]
Soc_Ed_Infantil <- na.omit(Soc_Ed_Infantil)
Soc_Ed_Infantil <- Soc_Ed_Infantil[, -c(2:which(colnames(Soc_Ed_Infantil) == "2005"))]
Soc_Ed_Infantil <- head(Soc_Ed_Infantil, 179)
colnames(Soc_Ed_Infantil)[colnames(Soc_Ed_Infantil) == "2022(P)"] <- "2022"
Soc_Ed_Infantil[, 2:ncol(Soc_Ed_Infantil)] <- apply(Soc_Ed_Infantil[, 2:ncol(Soc_Ed_Infantil)], 2, as.numeric)
Soc_Ed_Infantil <- pivot_longer(Soc_Ed_Infantil, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_Infantil")


# Soc_Ed_Primaria

Soc_Ed_Primaria <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902408.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_Primaria<- Soc_Ed_Primaria [, -c(1,2)]
colnames(Soc_Ed_Primaria) <- Soc_Ed_Primaria[1, ]
colnames(Soc_Ed_Primaria)[1] <- "Entidad"
Soc_Ed_Primaria <- Soc_Ed_Primaria[which(Soc_Ed_Primaria$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_Primaria)), ]
Soc_Ed_Primaria <- Soc_Ed_Primaria[, -ncol(Soc_Ed_Primaria)]
Soc_Ed_Primaria <- na.omit(Soc_Ed_Primaria)
Soc_Ed_Primaria <- Soc_Ed_Primaria[, -c(2:which(colnames(Soc_Ed_Primaria) == "2005"))]
Soc_Ed_Primaria <- head(Soc_Ed_Primaria, 179)
colnames(Soc_Ed_Primaria)[colnames(Soc_Ed_Primaria) == "2022(P)"] <- "2022"
Soc_Ed_Primaria[, 2:ncol(Soc_Ed_Primaria)] <- apply(Soc_Ed_Primaria[, 2:ncol(Soc_Ed_Primaria)], 2, as.numeric)
Soc_Ed_Primaria <- pivot_longer(Soc_Ed_Primaria, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_Primaria")



# Soc_Ed_ESO

Soc_Ed_ESO <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/501058.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_ESO<- Soc_Ed_ESO [, -c(1,2)]
colnames(Soc_Ed_ESO) <- Soc_Ed_ESO[1, ]
colnames(Soc_Ed_ESO)[1] <- "Entidad"
Soc_Ed_ESO <- Soc_Ed_ESO[which(Soc_Ed_ESO$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_ESO)), ]
Soc_Ed_ESO <- Soc_Ed_ESO[, -ncol(Soc_Ed_ESO)]
Soc_Ed_ESO <- na.omit(Soc_Ed_ESO)
Soc_Ed_ESO <- Soc_Ed_ESO[, -c(2:which(colnames(Soc_Ed_ESO) == "2000"))]
Soc_Ed_ESO <- head(Soc_Ed_ESO, 179)
colnames(Soc_Ed_ESO)[colnames(Soc_Ed_ESO) == "2022(P)"] <- "2022"
Soc_Ed_ESO[, 2:ncol(Soc_Ed_ESO)] <- apply(Soc_Ed_ESO[, 2:ncol(Soc_Ed_ESO)], 2, as.numeric)
Soc_Ed_ESO <- pivot_longer(Soc_Ed_ESO, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_ESO")


# Soc_Ed_Bachiller

Soc_Ed_Bachiller <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/501060.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_Bachiller<- Soc_Ed_Bachiller [, -c(1,2)]
colnames(Soc_Ed_Bachiller) <- Soc_Ed_Bachiller[1, ]
colnames(Soc_Ed_Bachiller)[1] <- "Entidad"
Soc_Ed_Bachiller <- Soc_Ed_Bachiller[which(Soc_Ed_Bachiller$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_Bachiller)), ]
Soc_Ed_Bachiller <- Soc_Ed_Bachiller[, -ncol(Soc_Ed_Bachiller)]
Soc_Ed_Bachiller <- na.omit(Soc_Ed_Bachiller)
Soc_Ed_Bachiller <- Soc_Ed_Bachiller[, -c(2:which(colnames(Soc_Ed_Bachiller) == "2000"))]
Soc_Ed_Bachiller <- head(Soc_Ed_Bachiller, 179)
colnames(Soc_Ed_Bachiller)[colnames(Soc_Ed_Bachiller) == "2022(P)"] <- "2022"
Soc_Ed_Bachiller[, 2:ncol(Soc_Ed_Bachiller)] <- apply(Soc_Ed_Bachiller[, 2:ncol(Soc_Ed_Bachiller)], 2, as.numeric)
Soc_Ed_Bachiller <- pivot_longer(Soc_Ed_Bachiller, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_Bachiller")

# Soc_Ed_CiclosFormat

Soc_Ed_CiclosFormat <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/501062.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_CiclosFormat<- Soc_Ed_CiclosFormat [, -c(1,2)]
colnames(Soc_Ed_CiclosFormat) <- Soc_Ed_CiclosFormat[1, ]
colnames(Soc_Ed_CiclosFormat)[1] <- "Entidad"
Soc_Ed_CiclosFormat <- Soc_Ed_CiclosFormat[which(Soc_Ed_CiclosFormat$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_CiclosFormat)), ]
Soc_Ed_CiclosFormat <- Soc_Ed_CiclosFormat[, -ncol(Soc_Ed_CiclosFormat)]
Soc_Ed_CiclosFormat <- na.omit(Soc_Ed_CiclosFormat)
Soc_Ed_CiclosFormat <- Soc_Ed_CiclosFormat[, -c(2:which(colnames(Soc_Ed_CiclosFormat) == "2000"))]
Soc_Ed_CiclosFormat <- head(Soc_Ed_CiclosFormat, 179)
colnames(Soc_Ed_CiclosFormat)[colnames(Soc_Ed_CiclosFormat) == "2022(P)"] <- "2022"
Soc_Ed_CiclosFormat[, 2:ncol(Soc_Ed_CiclosFormat)] <- apply(Soc_Ed_CiclosFormat[, 2:ncol(Soc_Ed_CiclosFormat)], 2, as.numeric)
Soc_Ed_CiclosFormat <- pivot_longer(Soc_Ed_CiclosFormat, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_CiclosFormat")


# Soc_Ed_Especial

Soc_Ed_Especial <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/501063.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ed_Especial<- Soc_Ed_Especial [, -c(1,2)]
colnames(Soc_Ed_Especial) <- Soc_Ed_Especial[1, ]
colnames(Soc_Ed_Especial)[1] <- "Entidad"
Soc_Ed_Especial <- Soc_Ed_Especial[which(Soc_Ed_Especial$Entidad == "Acebeda (La)"):(nrow(Soc_Ed_Especial)), ]
Soc_Ed_Especial <- Soc_Ed_Especial[, -ncol(Soc_Ed_Especial)]
Soc_Ed_Especial <- na.omit(Soc_Ed_Especial)
Soc_Ed_Especial <- Soc_Ed_Especial[, -c(2:which(colnames(Soc_Ed_Especial) == "2000"))]
Soc_Ed_Especial <- head(Soc_Ed_Especial, 179)
colnames(Soc_Ed_Especial)[colnames(Soc_Ed_Especial) == "2022(P)"] <- "2022"
Soc_Ed_Especial[, 2:ncol(Soc_Ed_Especial)] <- apply(Soc_Ed_Especial[, 2:ncol(Soc_Ed_Especial)], 2, as.numeric)
Soc_Ed_Especial <- pivot_longer(Soc_Ed_Especial, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ed_Especial")


# Soc_Mov_Est_Cercan

Soc_Mov_Est_Cercan <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/902006.csv", sep = ";", header = FALSE, skip = 1)
Soc_Mov_Est_Cercan<- Soc_Mov_Est_Cercan [, -c(1,2)]
colnames(Soc_Mov_Est_Cercan) <- Soc_Mov_Est_Cercan[1, ]
colnames(Soc_Mov_Est_Cercan)[1] <- "Entidad"
Soc_Mov_Est_Cercan <- Soc_Mov_Est_Cercan[which(Soc_Mov_Est_Cercan$Entidad == "Acebeda (La)"):(nrow(Soc_Mov_Est_Cercan)), ]
Soc_Mov_Est_Cercan <- Soc_Mov_Est_Cercan[, -ncol(Soc_Mov_Est_Cercan)]
Soc_Mov_Est_Cercan <- na.omit(Soc_Mov_Est_Cercan)
Soc_Mov_Est_Cercan <- Soc_Mov_Est_Cercan[, -c(2:which(colnames(Soc_Mov_Est_Cercan) == "2000"))]
Soc_Mov_Est_Cercan <- head(Soc_Mov_Est_Cercan, 179)
Soc_Mov_Est_Cercan[, 2:ncol(Soc_Mov_Est_Cercan)] <- apply(Soc_Mov_Est_Cercan[, 2:ncol(Soc_Mov_Est_Cercan)], 2, as.numeric)
Soc_Mov_Est_Cercan <- pivot_longer(Soc_Mov_Est_Cercan, cols = -c(Entidad), names_to = "año", values_to = "Soc_Mov_Est_Cercan")

# Soc_Mov_Gasolin

Soc_Mov_Gasolin <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1920300.csv", sep = ";", header = FALSE, skip = 1)
Soc_Mov_Gasolin<- Soc_Mov_Gasolin [, -c(1,2)]
colnames(Soc_Mov_Gasolin) <- Soc_Mov_Gasolin[1, ]
colnames(Soc_Mov_Gasolin)[1] <- "Entidad"
Soc_Mov_Gasolin <- Soc_Mov_Gasolin[which(Soc_Mov_Gasolin$Entidad == "Acebeda (La)"):(nrow(Soc_Mov_Gasolin)), ]
Soc_Mov_Gasolin <- Soc_Mov_Gasolin[, -ncol(Soc_Mov_Gasolin)]
Soc_Mov_Gasolin <- na.omit(Soc_Mov_Gasolin)
Soc_Mov_Gasolin <- Soc_Mov_Gasolin[, -c(2:which(colnames(Soc_Mov_Gasolin) == "2015"))]
Soc_Mov_Gasolin <- head(Soc_Mov_Gasolin, 179)
Soc_Mov_Gasolin[, 2:ncol(Soc_Mov_Gasolin)] <- apply(Soc_Mov_Gasolin[, 2:ncol(Soc_Mov_Gasolin)], 2, as.numeric)
Soc_Mov_Gasolin <- pivot_longer(Soc_Mov_Gasolin, cols = -c(Entidad), names_to = "año", values_to = "Soc_Mov_Gasolin")


# Soc_Mov_Lin_Autob

Soc_Mov_Lin_Autob <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/902007.csv", sep = ";", header = FALSE, skip = 1)
Soc_Mov_Lin_Autob<- Soc_Mov_Lin_Autob [, -c(1,2)]
colnames(Soc_Mov_Lin_Autob) <- Soc_Mov_Lin_Autob[1, ]
colnames(Soc_Mov_Lin_Autob)[1] <- "Entidad"
Soc_Mov_Lin_Autob <- Soc_Mov_Lin_Autob[which(Soc_Mov_Lin_Autob$Entidad == "Acebeda (La)"):(nrow(Soc_Mov_Lin_Autob)), ]
Soc_Mov_Lin_Autob <- Soc_Mov_Lin_Autob[, -ncol(Soc_Mov_Lin_Autob)]
Soc_Mov_Lin_Autob <- na.omit(Soc_Mov_Lin_Autob)
Soc_Mov_Lin_Autob <- Soc_Mov_Lin_Autob[, -c(2:which(colnames(Soc_Mov_Lin_Autob) == "2000"))]
Soc_Mov_Lin_Autob <- head(Soc_Mov_Lin_Autob, 179)
Soc_Mov_Lin_Autob[, 2:ncol(Soc_Mov_Lin_Autob)] <- apply(Soc_Mov_Lin_Autob[, 2:ncol(Soc_Mov_Lin_Autob)], 2, as.numeric)
Soc_Mov_Lin_Autob <- pivot_longer(Soc_Mov_Lin_Autob, cols = -c(Entidad), names_to = "año", values_to = "Soc_Mov_Lin_Autob")


# Soc_Mov_Turismos_Pub

Soc_Mov_Turismos_Pub <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1909960.csv", sep = ";", header = FALSE, skip = 1)
Soc_Mov_Turismos_Pub<- Soc_Mov_Turismos_Pub [, -c(1,2)]
colnames(Soc_Mov_Turismos_Pub) <- Soc_Mov_Turismos_Pub[1, ]
colnames(Soc_Mov_Turismos_Pub)[1] <- "Entidad"
Soc_Mov_Turismos_Pub <- Soc_Mov_Turismos_Pub[which(Soc_Mov_Turismos_Pub$Entidad == "Acebeda (La)"):(nrow(Soc_Mov_Turismos_Pub)), ]
Soc_Mov_Turismos_Pub <- Soc_Mov_Turismos_Pub[, -ncol(Soc_Mov_Turismos_Pub)]
Soc_Mov_Turismos_Pub <- na.omit(Soc_Mov_Turismos_Pub)
Soc_Mov_Turismos_Pub <- Soc_Mov_Turismos_Pub[, -c(2:which(colnames(Soc_Mov_Turismos_Pub) == "2000"))]
Soc_Mov_Turismos_Pub <- head(Soc_Mov_Turismos_Pub, 179)
Soc_Mov_Turismos_Pub[, 2:ncol(Soc_Mov_Turismos_Pub)] <- apply(Soc_Mov_Turismos_Pub[, 2:ncol(Soc_Mov_Turismos_Pub)], 2, as.numeric)
Soc_Mov_Turismos_Pub <- pivot_longer(Soc_Mov_Turismos_Pub, cols = -c(Entidad), names_to = "año", values_to = "Soc_Mov_Turismos_Pub")



# Soc_Salud_Ambul_Pub

Soc_Salud_Ambul_Pub <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1909961.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Ambul_Pub<- Soc_Salud_Ambul_Pub [, -c(1,2)]
colnames(Soc_Salud_Ambul_Pub) <- Soc_Salud_Ambul_Pub[1, ]
colnames(Soc_Salud_Ambul_Pub)[1] <- "Entidad"
Soc_Salud_Ambul_Pub <- Soc_Salud_Ambul_Pub[which(Soc_Salud_Ambul_Pub$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Ambul_Pub)), ]
Soc_Salud_Ambul_Pub <- Soc_Salud_Ambul_Pub[, -ncol(Soc_Salud_Ambul_Pub)]
Soc_Salud_Ambul_Pub <- na.omit(Soc_Salud_Ambul_Pub)
Soc_Salud_Ambul_Pub <- Soc_Salud_Ambul_Pub[, -c(2:which(colnames(Soc_Salud_Ambul_Pub) == "2000"))]
Soc_Salud_Ambul_Pub <- head(Soc_Salud_Ambul_Pub, 179)
Soc_Salud_Ambul_Pub[, 2:ncol(Soc_Salud_Ambul_Pub)] <- apply(Soc_Salud_Ambul_Pub[, 2:ncol(Soc_Salud_Ambul_Pub)], 2, as.numeric)
Soc_Salud_Ambul_Pub <- pivot_longer(Soc_Salud_Ambul_Pub, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Ambul_Pub")

# Soc_Salud_Ambul_Priv

Soc_Salud_Ambul_Priv <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1909968.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Ambul_Priv<- Soc_Salud_Ambul_Priv [, -c(1,2)]
colnames(Soc_Salud_Ambul_Priv) <- Soc_Salud_Ambul_Priv[1, ]
colnames(Soc_Salud_Ambul_Priv)[1] <- "Entidad"
Soc_Salud_Ambul_Priv <- Soc_Salud_Ambul_Priv[which(Soc_Salud_Ambul_Priv$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Ambul_Priv)), ]
Soc_Salud_Ambul_Priv <- Soc_Salud_Ambul_Priv[, -ncol(Soc_Salud_Ambul_Priv)]
Soc_Salud_Ambul_Priv <- na.omit(Soc_Salud_Ambul_Priv)
Soc_Salud_Ambul_Priv <- Soc_Salud_Ambul_Priv[, -c(2:which(colnames(Soc_Salud_Ambul_Priv) == "2000"))]
Soc_Salud_Ambul_Priv <- head(Soc_Salud_Ambul_Priv, 179)
Soc_Salud_Ambul_Priv[, 2:ncol(Soc_Salud_Ambul_Priv)] <- apply(Soc_Salud_Ambul_Priv[, 2:ncol(Soc_Salud_Ambul_Priv)], 2, as.numeric)
Soc_Salud_Ambul_Priv <- pivot_longer(Soc_Salud_Ambul_Priv, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Ambul_Priv")

# Soc_Salud_Ambul
# Suma de Soc_Salud_Ambul_Pub y Soc_Salud_Ambul_Priv

Soc_Salud_Ambul <- merge(Soc_Salud_Ambul_Pub, Soc_Salud_Ambul_Priv, by = c("Entidad", "año"))
Soc_Salud_Ambul$Soc_Salud_Ambul <- Soc_Salud_Ambul$Soc_Salud_Ambul_Pub + Soc_Salud_Ambul$Soc_Salud_Ambul_Priv
Soc_Salud_Ambul <- Soc_Salud_Ambul |> select(-c(Soc_Salud_Ambul_Pub,Soc_Salud_Ambul_Priv))


# Soc_Salud_CentrosS

Soc_Salud_CentrosS <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1905415.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_CentrosS<- Soc_Salud_CentrosS [, -c(1,2)]
colnames(Soc_Salud_CentrosS) <- Soc_Salud_CentrosS[1, ]
colnames(Soc_Salud_CentrosS)[1] <- "Entidad"
Soc_Salud_CentrosS <- Soc_Salud_CentrosS[which(Soc_Salud_CentrosS$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_CentrosS)), ]
Soc_Salud_CentrosS <- Soc_Salud_CentrosS[, -ncol(Soc_Salud_CentrosS)]
Soc_Salud_CentrosS <- na.omit(Soc_Salud_CentrosS)
Soc_Salud_CentrosS <- Soc_Salud_CentrosS[, -c(2:which(colnames(Soc_Salud_CentrosS) == "2009"))]
Soc_Salud_CentrosS <- head(Soc_Salud_CentrosS, 179)
Soc_Salud_CentrosS[, 2:ncol(Soc_Salud_CentrosS)] <- apply(Soc_Salud_CentrosS[, 2:ncol(Soc_Salud_CentrosS)], 2, as.numeric)
Soc_Salud_CentrosS <- pivot_longer(Soc_Salud_CentrosS, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_CentrosS")


# Soc_Salud_Consult

Soc_Salud_Consult <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1905414.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Consult<- Soc_Salud_Consult [, -c(1,2)]
colnames(Soc_Salud_Consult) <- Soc_Salud_Consult[1, ]
colnames(Soc_Salud_Consult)[1] <- "Entidad"
Soc_Salud_Consult <- Soc_Salud_Consult[which(Soc_Salud_Consult$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Consult)), ]
Soc_Salud_Consult <- Soc_Salud_Consult[, -ncol(Soc_Salud_Consult)]
Soc_Salud_Consult <- na.omit(Soc_Salud_Consult)
Soc_Salud_Consult <- Soc_Salud_Consult[, -c(2:which(colnames(Soc_Salud_Consult) == "2009"))]
Soc_Salud_Consult <- head(Soc_Salud_Consult, 179)
Soc_Salud_Consult[, 2:ncol(Soc_Salud_Consult)] <- apply(Soc_Salud_Consult[, 2:ncol(Soc_Salud_Consult)], 2, as.numeric)
Soc_Salud_Consult <- pivot_longer(Soc_Salud_Consult, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Consult")


# Soc_Salud_Farmacia

Soc_Salud_Farmacia <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1904760.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Farmacia<- Soc_Salud_Farmacia [, -c(1,2)]
colnames(Soc_Salud_Farmacia) <- Soc_Salud_Farmacia[1, ]
colnames(Soc_Salud_Farmacia)[1] <- "Entidad"
Soc_Salud_Farmacia <- Soc_Salud_Farmacia[which(Soc_Salud_Farmacia$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Farmacia)), ]
Soc_Salud_Farmacia <- Soc_Salud_Farmacia[, -ncol(Soc_Salud_Farmacia)]
Soc_Salud_Farmacia <- na.omit(Soc_Salud_Farmacia)
Soc_Salud_Farmacia <- Soc_Salud_Farmacia[, -c(2:which(colnames(Soc_Salud_Farmacia) == "2009"))]
Soc_Salud_Farmacia <- head(Soc_Salud_Farmacia, 179)
Soc_Salud_Farmacia[, 2:ncol(Soc_Salud_Farmacia)] <- apply(Soc_Salud_Farmacia[, 2:ncol(Soc_Salud_Farmacia)], 2, as.numeric)
Soc_Salud_Farmacia <- pivot_longer(Soc_Salud_Farmacia, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Farmacia")


# Soc_Salud_Def_Tumor

Soc_Salud_Def_Tumor <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902956.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Def_Tumor<- Soc_Salud_Def_Tumor [, -c(1,2)]
colnames(Soc_Salud_Def_Tumor) <- Soc_Salud_Def_Tumor[1, ]
colnames(Soc_Salud_Def_Tumor)[1] <- "Entidad"
Soc_Salud_Def_Tumor <- Soc_Salud_Def_Tumor[which(Soc_Salud_Def_Tumor$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Def_Tumor)), ]
Soc_Salud_Def_Tumor <- Soc_Salud_Def_Tumor[, -ncol(Soc_Salud_Def_Tumor)]
Soc_Salud_Def_Tumor <- na.omit(Soc_Salud_Def_Tumor)
Soc_Salud_Def_Tumor <- Soc_Salud_Def_Tumor[, -c(2:which(colnames(Soc_Salud_Def_Tumor) == "2000"))]
Soc_Salud_Def_Tumor <- head(Soc_Salud_Def_Tumor, 179)
Soc_Salud_Def_Tumor <- pivot_longer(Soc_Salud_Def_Tumor, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Def_Tumor")
Soc_Salud_Def_Tumor <- Soc_Salud_Def_Tumor  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Soc_Salud_Def_Tumor = as.numeric(Soc_Salud_Def_Tumor))


# Soc_Salud_Def_SistCirc

Soc_Salud_Def_SistCirc <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902957.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Def_SistCirc<- Soc_Salud_Def_SistCirc [, -c(1,2)]
colnames(Soc_Salud_Def_SistCirc) <- Soc_Salud_Def_SistCirc[1, ]
colnames(Soc_Salud_Def_SistCirc)[1] <- "Entidad"
Soc_Salud_Def_SistCirc <- Soc_Salud_Def_SistCirc[which(Soc_Salud_Def_SistCirc$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Def_SistCirc)), ]
Soc_Salud_Def_SistCirc <- Soc_Salud_Def_SistCirc[, -ncol(Soc_Salud_Def_SistCirc)]
Soc_Salud_Def_SistCirc <- na.omit(Soc_Salud_Def_SistCirc)
Soc_Salud_Def_SistCirc <- Soc_Salud_Def_SistCirc[, -c(2:which(colnames(Soc_Salud_Def_SistCirc) == "2000"))]
Soc_Salud_Def_SistCirc <- head(Soc_Salud_Def_SistCirc, 179)

Soc_Salud_Def_SistCirc <- pivot_longer(Soc_Salud_Def_SistCirc, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Def_SistCirc")
Soc_Salud_Def_SistCirc <- Soc_Salud_Def_SistCirc  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Soc_Salud_Def_SistCirc = as.numeric(Soc_Salud_Def_SistCirc))


# Soc_Salud_Def_SistResp

Soc_Salud_Def_SistResp <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1914000.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Def_SistResp<- Soc_Salud_Def_SistResp [, -c(1,2)]
colnames(Soc_Salud_Def_SistResp) <- Soc_Salud_Def_SistResp[1, ]
colnames(Soc_Salud_Def_SistResp)[1] <- "Entidad"
Soc_Salud_Def_SistResp <- Soc_Salud_Def_SistResp[which(Soc_Salud_Def_SistResp$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Def_SistResp)), ]
Soc_Salud_Def_SistResp <- Soc_Salud_Def_SistResp[, -ncol(Soc_Salud_Def_SistResp)]
Soc_Salud_Def_SistResp <- na.omit(Soc_Salud_Def_SistResp)
Soc_Salud_Def_SistResp <- Soc_Salud_Def_SistResp[, -c(2:which(colnames(Soc_Salud_Def_SistResp) == "2000"))]
Soc_Salud_Def_SistResp <- head(Soc_Salud_Def_SistResp, 179)

Soc_Salud_Def_SistResp <- pivot_longer(Soc_Salud_Def_SistResp, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Def_SistResp")
Soc_Salud_Def_SistResp <- Soc_Salud_Def_SistResp  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Soc_Salud_Def_SistResp = as.numeric(Soc_Salud_Def_SistResp))


# Soc_Salud_Def_Otras

Soc_Salud_Def_Otras <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1902958.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Def_Otras<- Soc_Salud_Def_Otras [, -c(1,2)]
colnames(Soc_Salud_Def_Otras) <- Soc_Salud_Def_Otras[1, ]
colnames(Soc_Salud_Def_Otras)[1] <- "Entidad"
Soc_Salud_Def_Otras <- Soc_Salud_Def_Otras[which(Soc_Salud_Def_Otras$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Def_Otras)), ]
Soc_Salud_Def_Otras <- Soc_Salud_Def_Otras[, -ncol(Soc_Salud_Def_Otras)]
Soc_Salud_Def_Otras <- na.omit(Soc_Salud_Def_Otras)
Soc_Salud_Def_Otras <- Soc_Salud_Def_Otras[, -c(2:which(colnames(Soc_Salud_Def_Otras) == "2009"))]
Soc_Salud_Def_Otras <- head(Soc_Salud_Def_Otras, 179)

Soc_Salud_Def_Otras <- pivot_longer(Soc_Salud_Def_Otras, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Def_Otras")
Soc_Salud_Def_Otras <- Soc_Salud_Def_Otras  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Soc_Salud_Def_Otras = as.numeric(Soc_Salud_Def_Otras))


# Soc_Salud_Hospitales

# Soc_Salud_Hospitales <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/CALIDAD DE VIDA/Soc_Salud_Hospitales.xlsx")
# names(Soc_Salud_Hospitales) 
# Soc_Salud_Hospitales<-Soc_Salud_Hospitales |> 
#   rename(Entidad = Municipio) |> 
#   mutate(año = 2022)|> 
#   filter(`Clase de Centro` == "Hospitales Generales")
# 
# # 
# Soc_Salud_Hosp_Gen <- Soc_Salud_Hospitales |>
#   select(-c("Nombre Centro", "Cód. Municipio", "Dependencia Funcional", "Clase de Centro")) |>
#   group_by(Entidad, año) |>
#   mutate(Soc_Camas_Hop_General = sum(CAMAS)) |>
#   ungroup() |>
#   distinct(Entidad, Camas_Hop_General)
# 
# print(Soc_Salud_Hosp_Gen)


# Soc_Salud_Discapacidad

Soc_Salud_Discapacidad <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1917375.csv", sep = ";", header = FALSE, skip = 1)
Soc_Salud_Discapacidad<- Soc_Salud_Discapacidad [, -c(1,2)]
colnames(Soc_Salud_Discapacidad) <- Soc_Salud_Discapacidad[1, ]
colnames(Soc_Salud_Discapacidad)[1] <- "Entidad"
Soc_Salud_Discapacidad <- Soc_Salud_Discapacidad[which(Soc_Salud_Discapacidad$Entidad == "Acebeda (La)"):(nrow(Soc_Salud_Discapacidad)), ]
Soc_Salud_Discapacidad <- Soc_Salud_Discapacidad[, -ncol(Soc_Salud_Discapacidad)]
Soc_Salud_Discapacidad <- na.omit(Soc_Salud_Discapacidad)
Soc_Salud_Discapacidad <- Soc_Salud_Discapacidad[, -c(2:which(colnames(Soc_Salud_Discapacidad) == "2011"))]
Soc_Salud_Discapacidad <- head(Soc_Salud_Discapacidad, 179)
Soc_Salud_Discapacidad[, 2:ncol(Soc_Salud_Discapacidad)] <- apply(Soc_Salud_Discapacidad[, 2:ncol(Soc_Salud_Discapacidad)], 2, as.numeric)
Soc_Salud_Discapacidad <- pivot_longer(Soc_Salud_Discapacidad, cols = -c(Entidad), names_to = "año", values_to = "Soc_Salud_Discapacidad")


# Soc_Serv_Social_Centros

Soc_Serv_Social_Centros <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1917701.csv", sep = ";", header = FALSE, skip = 1)
Soc_Serv_Social_Centros<- Soc_Serv_Social_Centros [, -c(1,2)]
colnames(Soc_Serv_Social_Centros) <- Soc_Serv_Social_Centros[1, ]
colnames(Soc_Serv_Social_Centros)[1] <- "Entidad"
Soc_Serv_Social_Centros <- Soc_Serv_Social_Centros[which(Soc_Serv_Social_Centros$Entidad == "Acebeda (La)"):(nrow(Soc_Serv_Social_Centros)), ]
Soc_Serv_Social_Centros <- Soc_Serv_Social_Centros[, -ncol(Soc_Serv_Social_Centros)]
Soc_Serv_Social_Centros <- na.omit(Soc_Serv_Social_Centros)
Soc_Serv_Social_Centros <- Soc_Serv_Social_Centros[, -c(2:which(colnames(Soc_Serv_Social_Centros) == "2014"))]
Soc_Serv_Social_Centros <- head(Soc_Serv_Social_Centros, 179)
Soc_Serv_Social_Centros[, 2:ncol(Soc_Serv_Social_Centros)] <- apply(Soc_Serv_Social_Centros[, 2:ncol(Soc_Serv_Social_Centros)], 2, as.numeric)
Soc_Serv_Social_Centros <- pivot_longer(Soc_Serv_Social_Centros, cols = -c(Entidad), names_to = "año", values_to = "Soc_Serv_Social_Centros")


# Soc_Vivienda_Libre

Soc_Vivienda_Libre <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1004050.csv", sep = ";", header = FALSE, skip = 1)
Soc_Vivienda_Libre<- Soc_Vivienda_Libre [, -c(1,2)]
colnames(Soc_Vivienda_Libre) <- Soc_Vivienda_Libre[1, ]
colnames(Soc_Vivienda_Libre)[1] <- "Entidad"
Soc_Vivienda_Libre <- Soc_Vivienda_Libre[which(Soc_Vivienda_Libre$Entidad == "Acebeda (La)"):(nrow(Soc_Vivienda_Libre)), ]
Soc_Vivienda_Libre <- Soc_Vivienda_Libre[, -ncol(Soc_Vivienda_Libre)]
Soc_Vivienda_Libre <- na.omit(Soc_Vivienda_Libre)
Soc_Vivienda_Libre <- Soc_Vivienda_Libre[, -c(2:which(colnames(Soc_Vivienda_Libre) == "2005"))]
Soc_Vivienda_Libre <- head(Soc_Vivienda_Libre, 179)

Soc_Vivienda_Libre <- pivot_longer(Soc_Vivienda_Libre, cols = -c(Entidad), names_to = "año", values_to = "Soc_Vivienda_Libre")
Soc_Vivienda_Libre <- Soc_Vivienda_Libre  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Soc_Vivienda_Libre = as.numeric(Soc_Vivienda_Libre))


# DATOS DE OCIO (CINE,TEATRO, BIENES CULTURALES) Y SERVICIOS QUE FALTAN. SUP AGRARIA Y GANADERA

# Soc_Ent_Credito

Soc_Ent_Credito <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1915709.csv", sep = ";", header = FALSE, skip = 1)
Soc_Ent_Credito<- Soc_Ent_Credito [, -c(1,2)]
colnames(Soc_Ent_Credito) <- Soc_Ent_Credito[1, ]
colnames(Soc_Ent_Credito)[1] <- "Entidad"
Soc_Ent_Credito <- Soc_Ent_Credito[which(Soc_Ent_Credito$Entidad == "Acebeda (La)"):(nrow(Soc_Ent_Credito)), ]
Soc_Ent_Credito <- Soc_Ent_Credito[, -ncol(Soc_Ent_Credito)]
Soc_Ent_Credito <- na.omit(Soc_Ent_Credito)
Soc_Ent_Credito <- Soc_Ent_Credito[, -c(2:which(colnames(Soc_Ent_Credito) == "2014"))]
Soc_Ent_Credito <- head(Soc_Ent_Credito, 179)

Soc_Ent_Credito <- pivot_longer(Soc_Ent_Credito, cols = -c(Entidad), names_to = "año", values_to = "Soc_Ent_Credito")

# Soc_Cines_Proy

Soc_Cines_Proy <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/702006.csv", sep = ";", header = FALSE, skip = 1)
Soc_Cines_Proy<- Soc_Cines_Proy [, -c(1,2)]
colnames(Soc_Cines_Proy) <- Soc_Cines_Proy[1, ]
colnames(Soc_Cines_Proy)[1] <- "Entidad"
Soc_Cines_Proy <- Soc_Cines_Proy[which(Soc_Cines_Proy$Entidad == "Acebeda (La)"):(nrow(Soc_Cines_Proy)), ]
Soc_Cines_Proy <- Soc_Cines_Proy[, -ncol(Soc_Cines_Proy)]
Soc_Cines_Proy <- na.omit(Soc_Cines_Proy)
Soc_Cines_Proy <- Soc_Cines_Proy[, -c(2:which(colnames(Soc_Cines_Proy) == "2014"))]
Soc_Cines_Proy <- head(Soc_Cines_Proy, 179)

Soc_Cines_Proy <- pivot_longer(Soc_Cines_Proy, cols = -c(Entidad), names_to = "año", values_to = "Soc_Cines_Proy")
Soc_Cines_Proy <- Soc_Cines_Proy  |> mutate(across(where(is.numeric),as.numeric))  |> mutate(Soc_Cines_Proy = as.numeric(Soc_Cines_Proy))


# Soc_Teatros

Soc_Teatros <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1923962.csv", sep = ";", header = FALSE, skip = 1)
Soc_Teatros<- Soc_Teatros [, -c(1,2)]
colnames(Soc_Teatros) <- Soc_Teatros[1, ]
colnames(Soc_Teatros)[1] <- "Entidad"
Soc_Teatros <- Soc_Teatros[which(Soc_Teatros$Entidad == "Acebeda (La)"):(nrow(Soc_Teatros)), ]
Soc_Teatros <- Soc_Teatros[, -ncol(Soc_Teatros)]
Soc_Teatros <- na.omit(Soc_Teatros)
Soc_Teatros <- Soc_Teatros[, -c(2:which(colnames(Soc_Teatros) == "2017"))]
Soc_Teatros <- head(Soc_Teatros, 179)

Soc_Teatros <- pivot_longer(Soc_Teatros, cols = -c(Entidad), names_to = "año", values_to = "Soc_Teatros")

# Soc_Bienes_Cult

Soc_Bienes_Cult <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/702022.csv", sep = ";", header = FALSE, skip = 1)
Soc_Bienes_Cult<- Soc_Bienes_Cult [, -c(1,2)]
colnames(Soc_Bienes_Cult) <- Soc_Bienes_Cult[1, ]
colnames(Soc_Bienes_Cult)[1] <- "Entidad"
Soc_Bienes_Cult <- Soc_Bienes_Cult[which(Soc_Bienes_Cult$Entidad == "Acebeda (La)"):(nrow(Soc_Bienes_Cult)), ]
Soc_Bienes_Cult <- Soc_Bienes_Cult[, -ncol(Soc_Bienes_Cult)]
Soc_Bienes_Cult <- na.omit(Soc_Bienes_Cult)
Soc_Bienes_Cult <- Soc_Bienes_Cult[, -c(2:which(colnames(Soc_Bienes_Cult) == "2006"))]
Soc_Bienes_Cult <- head(Soc_Bienes_Cult, 179)

Soc_Bienes_Cult <- pivot_longer(Soc_Bienes_Cult, cols = -c(Entidad), names_to = "año", values_to = "Soc_Bienes_Cult")


# DATOS PRESUPUESTARIOS

# ALMUDENA

# Eco_Pre_Gastos_Liquidados


Eco_Pre_Gastos_Liquidados <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1502108.csv", sep = ";", header = FALSE, skip = 1)
Eco_Pre_Gastos_Liquidados<- Eco_Pre_Gastos_Liquidados [, -c(1,2)]
colnames(Eco_Pre_Gastos_Liquidados) <- Eco_Pre_Gastos_Liquidados[1, ]
colnames(Eco_Pre_Gastos_Liquidados)[1] <- "Entidad"
Eco_Pre_Gastos_Liquidados <- Eco_Pre_Gastos_Liquidados[which(Eco_Pre_Gastos_Liquidados$Entidad == "Acebeda (La)"):(nrow(Eco_Pre_Gastos_Liquidados)), ]
Eco_Pre_Gastos_Liquidados <- Eco_Pre_Gastos_Liquidados[, -ncol(Eco_Pre_Gastos_Liquidados)]
Eco_Pre_Gastos_Liquidados <- na.omit(Eco_Pre_Gastos_Liquidados)
Eco_Pre_Gastos_Liquidados <- Eco_Pre_Gastos_Liquidados[, -c(2:which(colnames(Eco_Pre_Gastos_Liquidados) == "2001"))]
Eco_Pre_Gastos_Liquidados <- head(Eco_Pre_Gastos_Liquidados, 179)

Eco_Pre_Gastos_Liquidados <- pivot_longer(Eco_Pre_Gastos_Liquidados, cols = -c(Entidad), names_to = "año", values_to = "Eco_Pre_Gastos_Liquidados")

# Eco_Pre_Ingresos_Liquidados

Eco_Pre_Ingresos_Liquidados <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1502099.csv", sep = ";", header = FALSE, skip = 1)
Eco_Pre_Ingresos_Liquidados<- Eco_Pre_Ingresos_Liquidados [, -c(1,2)]
colnames(Eco_Pre_Ingresos_Liquidados) <- Eco_Pre_Ingresos_Liquidados[1, ]
colnames(Eco_Pre_Ingresos_Liquidados)[1] <- "Entidad"
Eco_Pre_Ingresos_Liquidados <- Eco_Pre_Ingresos_Liquidados[which(Eco_Pre_Ingresos_Liquidados$Entidad == "Acebeda (La)"):(nrow(Eco_Pre_Ingresos_Liquidados)), ]
Eco_Pre_Ingresos_Liquidados <- Eco_Pre_Ingresos_Liquidados[, -ncol(Eco_Pre_Ingresos_Liquidados)]
Eco_Pre_Ingresos_Liquidados <- na.omit(Eco_Pre_Ingresos_Liquidados)
Eco_Pre_Ingresos_Liquidados <- Eco_Pre_Ingresos_Liquidados[, -c(2:which(colnames(Eco_Pre_Ingresos_Liquidados) == "2001"))]
Eco_Pre_Ingresos_Liquidados <- head(Eco_Pre_Ingresos_Liquidados, 179)

Eco_Pre_Ingresos_Liquidados <- pivot_longer(Eco_Pre_Ingresos_Liquidados, cols = -c(Entidad), names_to = "año", values_to = "Eco_Pre_Ingresos_Liquidados")

# Eco_Pre_Deuda_Viva

Eco_Pre_Deuda_Viva <- read.csv2("C:/Users/evaba/Documents/TFM/DATOS/ALMUDENA/1919700.csv", sep = ";", header = FALSE, skip = 1)
Eco_Pre_Deuda_Viva<- Eco_Pre_Deuda_Viva [, -c(1,2)]
colnames(Eco_Pre_Deuda_Viva) <- Eco_Pre_Deuda_Viva[1, ]
colnames(Eco_Pre_Deuda_Viva)[1] <- "Entidad"
Eco_Pre_Deuda_Viva <- Eco_Pre_Deuda_Viva[which(Eco_Pre_Deuda_Viva$Entidad == "Acebeda (La)"):(nrow(Eco_Pre_Deuda_Viva)), ]
Eco_Pre_Deuda_Viva <- Eco_Pre_Deuda_Viva[, -ncol(Eco_Pre_Deuda_Viva)]
Eco_Pre_Deuda_Viva <- na.omit(Eco_Pre_Deuda_Viva)
Eco_Pre_Deuda_Viva <- Eco_Pre_Deuda_Viva[, -c(2:which(colnames(Eco_Pre_Deuda_Viva) == "2008"))]
Eco_Pre_Deuda_Viva <- head(Eco_Pre_Deuda_Viva, 179)

Eco_Pre_Deuda_Viva <- pivot_longer(Eco_Pre_Deuda_Viva, cols = -c(Entidad), names_to = "año", values_to = "Eco_Pre_Deuda_Viva")


# Eco_Cap_Nec_Financ
# Eco_Imp_Saldo_Nfinanc

# 2021

Estab_Pres_21 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2021.xlsx")

# Eliminar las 14 primeras filas
Estab_Pres_21 <- Estab_Pres_21[-c(1:14), ]
names(Estab_Pres_21)
#Renombrar columnas para filtrar
colnames(Estab_Pres_21) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")

# Filtrar por Código de la entidad local igual a 28
Estab_Pres_21 <- Estab_Pres_21[Estab_Pres_21$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2021)

# 2020
Estab_Pres_20 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2020.xlsx")
Estab_Pres_20 <- Estab_Pres_20[-c(1:14), ]
colnames(Estab_Pres_20) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_20 <- Estab_Pres_20[Estab_Pres_20$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2020)

# 2019
Estab_Pres_19 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2019.xlsx")
Estab_Pres_19 <- Estab_Pres_19[-c(1:14), ]
colnames(Estab_Pres_19) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_19 <- Estab_Pres_19[Estab_Pres_19$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2019)

# 2018
Estab_Pres_18 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2018.xlsx")
Estab_Pres_18 <- Estab_Pres_18[-c(1:14), ]
colnames(Estab_Pres_18) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_18 <- Estab_Pres_18[Estab_Pres_18$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2018)

# 2017
Estab_Pres_17 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2017.xlsx")
Estab_Pres_17 <- Estab_Pres_17[-c(1:14), ]
colnames(Estab_Pres_17) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_17 <- Estab_Pres_17[Estab_Pres_17$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2017)

# 2016
Estab_Pres_16 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2016.xlsx")
Estab_Pres_16 <- Estab_Pres_16[-c(1:14), ]
colnames(Estab_Pres_16) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_16 <- Estab_Pres_16[Estab_Pres_16$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2016)

# 2015
Estab_Pres_15 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2015.xlsx")
Estab_Pres_15 <- Estab_Pres_15[-c(1:14), ]
colnames(Estab_Pres_15) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_15 <- Estab_Pres_15[Estab_Pres_15$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2015)

# 2014
Estab_Pres_14 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2014.xlsx")
Estab_Pres_14 <- Estab_Pres_14[-c(1:14), ]
colnames(Estab_Pres_14) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_14 <- Estab_Pres_14[Estab_Pres_14$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2014)

# 2013
Estab_Pres_13 <- read_xlsx("C:/Users/evaba/Documents/TFM/DATOS/PRESUPUESTOS/EP_EJERC_2013.xlsx")
Estab_Pres_13 <- Estab_Pres_13[-c(1:14), ]
colnames(Estab_Pres_13) <- c("PROV","MUN","1","2","Entidad","Provincia",
                             "Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc",
                             "remis","fecha","pob")
Estab_Pres_13 <- Estab_Pres_13[Estab_Pres_13$PROV == 28, ] |> 
  select (c("Entidad","Eco_Cap_Nec_Financ","Eco_Imp_Saldo_Nfinanc")) |> 
  mutate(año=2013)

Estab_Pres <- bind_rows(Estab_Pres_21, Estab_Pres_20, Estab_Pres_19, Estab_Pres_18, Estab_Pres_17, Estab_Pres_16, Estab_Pres_15, Estab_Pres_14, Estab_Pres_13)


# Unir los DF

# Definir una lista de dataframes a combinar
DF_ECO <- list(Eco_Afiliados_Total, Eco_Contratos_Indef, 
                   Eco_Afiliados_Tem_Porc_Resid, Eco_Contratos_Temp, 
                   Eco_Contratos_Total, Eco_Energ_Elect_PC,
               Eco_Paro_100, Eco_Paro_Porc_Evol, Eco_PBI_municipal_pc,
               Eco_Uds_Productivas, Eco_Ind_RDBM_PC, Eco_Establec_Hotel,
               Eco_Distrib_Renta_P80_20, Eco_Indice_Gini, Eco_Renta_Neta_Media_PP
                   )

DF_POB <- list(Pob_Empadronada, Pob_Empadronada_H,Pob_Empadronada_M,
               Pob_Dependencia,Pob_Edad_Media,
               Pob_Envejecimiento, Pob_Juventud,
               Pob_Crecim_Relativo,Pob_Saldo_Migratorio, Pob_EV)

DF_EDU <- list(Soc_Bibliot_públicas, Soc_Ed_Cent_Priv_NoUni, 
               Soc_Ed_Cent_Pub_NoUni, Soc_Ed_Infantil, Soc_Ed_Primaria,
               Soc_Ed_ESO, Soc_Ed_Bachiller, Soc_Ed_CiclosFormat,
               Soc_Ed_Especial)

DF_MOV <- list(Soc_Mov_Est_Cercan, Soc_Mov_Gasolin, 
               Soc_Mov_Lin_Autob, Soc_Mov_Turismos_Pub)

DF_SALUD <- list(Soc_Salud_Ambul, Soc_Salud_CentrosS, Soc_Salud_Consult,
                 Soc_Salud_Def_Otras, Soc_Salud_Def_SistCirc, Soc_Salud_Def_SistResp, 
                 Soc_Salud_Def_Tumor,
                 Soc_Salud_Discapacidad, Soc_Salud_Farmacia, Soc_Serv_Social_Centros)

DF_SERV <- list(Soc_Vivienda_Libre, Soc_Ent_Credito, Soc_Cines_Proy,
                Soc_Teatros, Soc_Bienes_Cult) 

Estab_Pres1 <- list(Eco_Pre_Gastos_Liquidados, Eco_Pre_Ingresos_Liquidados, 
                    Eco_Pre_Deuda_Viva,Estab_Pres)

by_cols <- c("Entidad", "año")

Estab_Pres1 <- map(Estab_Pres1, ~mutate(.x, año = as.character(año)))



# Especificar las columnas de unión
by_cols <- c("Entidad", "año")


# Combinar los dataframes utilizando Reduce y merge. REVISAR ESTÁ ELIMINANDO LOS AÑOS CON AUSENTES

DATOS_TFM_ECO <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DF_ECO)

skim(DATOS_TFM_ECO)


# Convertir variables de caracteres a numéricas excepto Entidad

DATOS_TFM_ECO <- DATOS_TFM_ECO %>%
  mutate(across(where(is.character),as.factor)) 

# Verificar el resumen de los datos actualizados
summary(DATOS_TFM_ECO)



# DATOS_TFM_POB <- reduce(DF_POB, 
#                         function(x, y) left_join(x, y, by = c("Entidad", "año"))  |>  
#                           mutate(across(.vars = -c(Entidad, año), .fns = ~coalesce(., get(paste0("x.", cur_column()))))))

DATOS_TFM_POB <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DF_POB)

# Agregar columna categórica de Grupo de población
# Pob_Grupo

DATOS_TFM_POB$Pob_Grupo <- 
  cut(DATOS_TFM_POB$Pob_Empadronada,
      breaks = c(0, 100, 250, 500, 1000, 5000, 20000, 50000, 75000, 100000, 500000, Inf),
      labels = c("≤100 hab.", "101-250 hab.", "251-500 hab.", "501-1.000 hab.", 
                 "1.001-5.000 hab.", "5.001-20.000 hab.", "20.001-50.000 hab.", "50.001-75.000", 
                 "75.001-100.000 hab.", "100.001-500.000 hab.", ">500.000 hab."))

# DATOS_TFM_EDU <- reduce(DF_EDU, 
#                        function(x, y) left_join(x, y, by = c("Entidad", "año"))  |>  
#                          mutate(across(.vars = -c(Entidad, año), .fns = ~coalesce(., get(paste0("x.", cur_column())))))) 
# 
# DATOS_TFM_MOV <- reduce(DF_MOV, 
#                         function(x, y) left_join(x, y, by = c("Entidad", "año"))  |>  
#                           mutate(across(.vars = -c(Entidad, año), .fns = ~coalesce(., get(paste0("x.", cur_column()))))))
# 
# DATOS_TFM_SALUD <- reduce(DF_SALUD, 
#                         function(x, y) left_join(x, y, by = c("Entidad", "año"))  |>  
#                           mutate(across(.vars = -c(Entidad, año), .fns = ~coalesce(., get(paste0("x.", cur_column()))))))
# 
# DATOS_TFM_SERV <- reduce(DF_SERV, 
#                           function(x, y) left_join(x, y, by = c("Entidad", "año"))  |>  
#                             mutate(across(.vars = -c(Entidad, año), .fns = ~coalesce(., get(paste0("x.", cur_column()))))))
# DATOS_TFM_PRES <- reduce(Estab_Pres1, 
#                          function(x, y) left_join(x, y, by = c("Entidad", "año"))  |>  
#                            mutate(across(.vars = -c(Entidad, año), .fns = ~coalesce(., get(paste0("x.", cur_column()))))))

DATOS_TFM_EDU <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DF_EDU)
DATOS_TFM_MOV <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DF_MOV)
DATOS_TFM_SALUD <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DF_SALUD)
DATOS_TFM_SERV <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DF_SERV)
DATOS_TFM_PRES <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), Estab_Pres1)

names(DATOS_TFM_ECO)
DATOS_TFM_ECO<- DATOS_TFM_ECO |> select (-c(CMUNXL.y,CMUNXL.x ))


DATOS_TFM_PRES <- DATOS_TFM_PRES %>%
  mutate(Eco_Pre_Gastos_Liquidados = as.numeric(Eco_Pre_Gastos_Liquidados),
         Eco_Pre_Ingresos_Liquidados = as.numeric(Eco_Pre_Ingresos_Liquidados),
         Eco_Pre_Deuda_Viva = as.numeric(Eco_Pre_Deuda_Viva),
         Eco_Imp_Saldo_Nfinanc = as.numeric(Eco_Imp_Saldo_Nfinanc))
names(DATOS_TFM_PRES)

# Unir los dataframes por las columnas "Entidad" y "año"

DATOS_TFM1 <- list(DATOS_TFM_POB, DATOS_TFM_ECO, 
                  DATOS_TFM_EDU, DATOS_TFM_MOV, 
                  DATOS_TFM_SALUD, DATOS_TFM_SERV, 
                  DATOS_TFM_PRES
)

by_cols <- c("Entidad", "año")


DATOS_TFM_TOTAL1 <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DATOS_TFM1)

write.csv(DATOS_TFM_TOTAL1, "DATOS_TFM_TOTAL1.csv", row.names = FALSE)



# Añado las columnas identificativas y de medioambiente 
#Selecciono las variables que irán al DF

names(DATOS_TFM_4)


DATOS_TFM_ID <- DATOS_TFM_4 %>%
  select(año = Año_Referencia, CMUNXL, CMUN, NATCODE, DEGURBA, Entidad, CIF,
         Municipio, Terr_Zonas_Estad_CM, Superficie, Soc_Mov_Muni_5000_T,
         Soc_Mov_Hosp_T, Soc_Mov_Auto_T, MA_Incendios, MA_Sup_quemada, MA_Pelig_Incend_AltoExt,
         MA_Sup_Forestal_Total, MA_Sup_Forestal_Arbol, MA_Sup_Forestal_Desarb,
         MA_Sup_Protegida) 
DATOS_TFM_ID <- DATOS_TFM_ID %>%
  mutate(across(
    where(is.numeric),
    as.numeric
  )) %>%
  mutate(MA_Incendios = as.numeric(MA_Incendios), 
         MA_Sup_quemada = as.numeric(MA_Sup_quemada),
         MA_Sup_Forestal_Total = as.numeric(MA_Sup_Forestal_Total),
         MA_Sup_Forestal_Arbol = as.numeric(MA_Sup_Forestal_Arbol),
         MA_Sup_Forestal_Desarb = as.numeric(MA_Sup_Forestal_Desarb),
         MA_Sup_Protegida = as.numeric(MA_Sup_Protegida),
         Soc_Mov_Auto_T = as.numeric(Soc_Mov_Auto_T),
         Soc_Mov_Hosp_T = as.numeric(Soc_Mov_Hosp_T),
         Soc_Mov_Muni_5000_T = as.numeric(Soc_Mov_Muni_5000_T),
         Superficie = as.numeric(Superficie))

DATOS_TFM2 <- list(DATOS_TFM_TOTAL1, DATOS_TFM_ID)

by_cols <- c("Entidad", "año")

DATOS_TFM_TOTAL2 <- Reduce(function(x, y) merge(x, y, by = c("Entidad", "año"), all = TRUE), DATOS_TFM2)


names (DATOS_TFM_TOTAL2)

DATOS_TFM_TOTAL2<- DATOS_TFM_TOTAL2 |> select (-c(CMUNXL.y, CMUNXL.x))

write.csv(DATOS_TFM_TOTAL2, "DATOS_TFM_TOTAL2.csv", row.names = FALSE)



DATOS_TFM_TOTAL3 <- DATOS_TFM_TOTAL2 %>%
  group_by(Entidad) %>%
  fill(c(Municipio, Terr_Zonas_Estad_CM, Superficie, Soc_Mov_Muni_5000_T,
         Soc_Mov_Auto_T,MA_Sup_Protegida, CMUNXL),
       .direction = "up") |> 
  ungroup()

names(DATOS_TFM_TOTAL3)


# Crear un vector con los valores únicos de Entidad y sus respectivos valores de CIF
entidad_cif <- unique(DATOS_TFM_TOTAL3[!is.na(DATOS_TFM_TOTAL3$CIF), c("Entidad", "CIF")])
# Completar los valores NA de CIF en función de Entidad
DATOS_TFM_TOTAL3$CIF <- ifelse(is.na(DATOS_TFM_TOTAL3$CIF), entidad_cif$CIF[match(DATOS_TFM_TOTAL3$Entidad, entidad_cif$Entidad)], DATOS_TFM_TOTAL3$CIF)

entidad_CMUN <- unique(DATOS_TFM_TOTAL3[!is.na(DATOS_TFM_TOTAL3$CMUN), c("Entidad", "CMUN")])
DATOS_TFM_TOTAL3$CMUN <- ifelse(is.na(DATOS_TFM_TOTAL3$CMUN), entidad_CMUN$CMUN[match(DATOS_TFM_TOTAL3$Entidad, entidad_CMUN$Entidad)], DATOS_TFM_TOTAL3$CMUN)

entidad_CMUNXL <- unique(DATOS_TFM_TOTAL3[!is.na(DATOS_TFM_TOTAL3$CMUNXL), c("Entidad", "CMUNXL")])
DATOS_TFM_TOTAL3$CMUNXL <- ifelse(is.na(DATOS_TFM_TOTAL3$CMUNXL), entidad_CMUNXL$CMUNXL[match(DATOS_TFM_TOTAL3$Entidad, entidad_CMUNXL$Entidad)], DATOS_TFM_TOTAL3$CMUNXL)

entidad_DEGURBA <- unique(DATOS_TFM_TOTAL3[!is.na(DATOS_TFM_TOTAL3$DEGURBA), c("Entidad", "DEGURBA")])
DATOS_TFM_TOTAL3$DEGURBA <- ifelse(is.na(DATOS_TFM_TOTAL3$DEGURBA), entidad_DEGURBA$DEGURBA[match(DATOS_TFM_TOTAL3$Entidad, entidad_DEGURBA$Entidad)], DATOS_TFM_TOTAL3$DEGURBA)

entidad_NATCODE <- unique(DATOS_TFM_TOTAL3[!is.na(DATOS_TFM_TOTAL3$NATCODE), c("Entidad", "NATCODE")])
DATOS_TFM_TOTAL3$NATCODE <- ifelse(is.na(DATOS_TFM_TOTAL3$NATCODE), entidad_NATCODE$NATCODE[match(DATOS_TFM_TOTAL3$Entidad, entidad_NATCODE$Entidad)], DATOS_TFM_TOTAL3$NATCODE)


DATOS_TFM_TOTAL3 <-select(DATOS_TFM_TOTAL3, sort(names(DATOS_TFM_TOTAL3)))
summary(DATOS_TFM_TOTAL3)

DATOS_TFM_TOTAL3 <- DATOS_TFM_TOTAL3  |> 
  mutate(CMUNXL = paste0("28", CMUN))


glimpse(DATOS_TFM_TOTAL3)


write.csv(DATOS_TFM_TOTAL3, "DATOS_TFM_TOTAL3.csv", row.names = FALSE)


descriptivos <- summary(DATOS_TFM_TOTAL3)
servicalidMadrid <- createWorkbook()
addWorksheet(servicalidMadrid, "Descriptivos")
writeData(servicalidMadrid, "Descriptivos", descriptivos)
saveWorkbook(servicalidMadrid, "descriptivos.xlsx", overwrite = TRUE)


datos_resumen <- as.data.frame(skim(DATOS_TFM_TOTAL3), stringsAsFactors = FALSE)
write.csv(datos_resumen, "datos_resumen.csv", row.names = FALSE)


