# limpiar al inicio para ir probando el script.
rm(list = ls())

# cargamos las librerías necesarias.


library(plyr) # Manipulación y transformación
library(dplyr) # Manipulación y transformación
library(tidyverse) # Conjunto paquetes manipulación datos, visualización y análisis
library(tidyr) # Manipulación y transformación
library(corrr) # biblioteca cálculo y visualización de correlación
library(skimr) # fundiones de resumen de datos

library(readr) # importar datos en formato plano (por ejemplo, CSV)
library(readxl) # importación de datos desde archivos de Excel (.xls y .xlsx) 
library(openxlsx) # funciones para la lectura, escritura y manipulación de archivos de Excel

library(ggplot2) # Visualización
library(corrplot) # Visualización matrices correlación
library(ggthemes)#temas para graficos
library(plotly)#Graficos interactivos
library(scales)#scala adecuadamente graficos
library(kableExtra)#tablas
library(viridis) # Visualización
library(fastmap) # Actualización para el renderizado
library(bslib) # Actualización para el renderizado
library(reactable) # Creación de tablas


# Cargo el DF base

DATOS_TFM_TOTAL3 <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/DATOS_TFM_TOTAL3.csv", 
            sep = ",", header = TRUE, dec = ".")

# TRATAMIENTO DE AUSENTES

# Filtro desde el año con valores en las variables de esperanza de vida y presupuestos (2012) y elimino algunas variables que sé no voy a necesitar


servicalidMadrid <- DATOS_TFM_TOTAL3 %>%
  filter(año > 2012) %>%
  dplyr::select(-c(CMUN, NATCODE))

# Calcular el número de ausentes por variables
num_ausentes <- colSums(is.na(servicalidMadrid))

tabla <- tibble(Variables = names(num_ausentes), Ausentes = num_ausentes) 
tabla

# Recodificación

unique(DATOS_TFM_TOTAL3$Eco_Cap_Nec_Financ)
servicalidMadrid <- servicalidMadrid %>%
  mutate(Eco_Cap_Nec_Financ = case_when(
    Eco_Cap_Nec_Financ %in% c("cumple", "Cumple", "Capacidad de financiación") ~ "Capacidad",
    Eco_Cap_Nec_Financ %in% c("NO CUMPLE", "no cumple", "Necesidad de financiación") ~ "Necesidad",
    TRUE ~ NA_character_
  ))
unique(servicalidMadrid$Eco_Cap_Nec_Financ)

# Primero ver dónde se encuentran y en qué proporción

servicalidMadrid2 <- servicalidMadrid |> #Se calcula el porcentaje de Missing en las variables
  summarize_all(funs(sum(is.na(.))/length(.)*100)) |>  
  select_if(where(~any(.x != 0)))

servicalidMadrid <-
  servicalidMadrid |> 
  mutate (across(where(is.character), as.factor)) 

servicalidMadrid <- servicalidMadrid %>%
  filter(año != 2023)

write.csv(servicalidMadrid, "servicalidMadrid.csv", row.names = FALSE)

# Segundo: imputar valores cuando proceda y sino se eliminarán.


servicalidMadrid2 <- servicalidMadrid %>%
  group_by(Pob_Grupo) %>%
  mutate(across(c( "Eco_Afiliados_Tem_Porc_Resid", 
                   "Eco_Afiliados_Total",  "Eco_Contratos_Indef", 
                   "Eco_Contratos_Temp", "Eco_Contratos_Total", "Eco_Distrib_Renta_P80_20", 
                   "Eco_Energ_Elect_PC",
                   "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", "Eco_Paro_Porc_Evol", 
                   "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", "Eco_Pre_Gastos_Liquidados", 
                   "Eco_Pre_Ingresos_Liquidados", "Eco_Renta_Neta_Media_PP", 
                   "MA_Incendios", "MA_Pelig_Incend_AltoExt", "MA_Sup_Forestal_Arbol", 
                   "MA_Sup_Forestal_Desarb", "MA_Sup_Forestal_Total", "MA_Sup_Protegida", 
                   "MA_Sup_quemada",   "Pob_Edad_Media","Pob_Saldo_Migratorio",
                   "Soc_Mov_Auto_T",  "Soc_Mov_Hosp_T",  "Soc_Vivienda_Libre"),
                ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup() |>  
  group_by(Terr_Zonas_Estad_CM) %>%
  mutate(across(c("Eco_Establec_Hotel", "Eco_Uds_Productivas", 
                  "Soc_Bibliot_públicas", 
                  "Soc_Bienes_Cult", "Soc_Cines_Proy", "Soc_Ed_Bachiller", "Soc_Ed_Cent_Priv_NoUni", 
                  "Soc_Ed_Cent_Pub_NoUni", "Soc_Ed_CiclosFormat", "Soc_Ed_ESO", 
                  "Soc_Ed_Especial", "Soc_Ed_Infantil", "Soc_Ed_Primaria", "Soc_Ent_Credito",
                  "Soc_Mov_Est_Cercan", "Soc_Mov_Gasolin","Soc_Mov_Lin_Autob",
                  "Soc_Mov_Turismos_Pub","Soc_Salud_Ambul", "Soc_Salud_CentrosS", "Soc_Mov_Hosp_T","Soc_Salud_Consult", 
                  "Soc_Salud_Farmacia","Soc_Serv_Social_Centros", "Soc_Teatros","Pob_EV_Total"),
                ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

servicalidMadrid2 <- servicalidMadrid2 %>%
  mutate(across(c("Pob_EV_65_H", "Pob_EV_65_M", 
                  "Pob_EV_65_Total", "Pob_EV_H", "Pob_EV_M", "Pob_EV_Total", "Soc_Salud_Def_Otras",
                  "Soc_Mov_Muni_5000_T",  "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", 
                  "Soc_Salud_Def_Tumor","Soc_Salud_Discapacidad"),
                ~ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
  ungroup() 

unique(servicalidMadrid2$Pob_EV_Total)

# Filtrar las observaciones con valores ausentes en "DEGURBA"
observaciones_ausentes <- subset(servicalidMadrid2, is.na(DEGURBA))

# Obtener las entidades correspondientes a los ausentes
entidades_ausentes <- observaciones_ausentes$Entidad

# Mostrar las entidades resultantes
unique(entidades_ausentes)

# Imputar el mismo valor de "DEGURBA" para cada entidad en los ausentes
servicalidMadrid2 <- servicalidMadrid2 %>%
  group_by(CMUNXL) %>%
  mutate(DEGURBA = ifelse(is.na(DEGURBA), unique(DEGURBA[!is.na(DEGURBA)]), DEGURBA)) |> 
  ungroup()

# Imputo por la moda para Eco_Cap_Nec_Financ
Mode <- function(x) {ux <- unique(x)
ux[which.max(tabulate(match(x, ux)))]}

servicalidMadrid2 <- servicalidMadrid2 %>%
  group_by(Entidad) %>%
  mutate(across(c("Eco_Cap_Nec_Financ"), ~ifelse(is.na(.), Mode(.), .))) %>%
  ungroup()

servicalidMadrid2 <- servicalidMadrid2 %>%
  group_by(CMUNXL) %>%
  mutate(Eco_Cap_Nec_Financ = 
           ifelse(is.na(Eco_Cap_Nec_Financ), 
                  unique(Eco_Cap_Nec_Financ[!is.na(Eco_Cap_Nec_Financ)]), Eco_Cap_Nec_Financ)) |> 
  ungroup()

#Elimino ausentes restantes
servicalidMadrid2 <- servicalidMadrid2 %>%
  filter(!is.na(DEGURBA))
servicalidMadrid2 <- servicalidMadrid2 %>%
  filter(!is.na(Pob_Grupo))



#Elimino las variables de identificación que ya no van a hacer falta

servicalidCM2 <- servicalidMadrid2  |> 
  dplyr::select(-CMUNXL, -Eco_Imp_Saldo_Nfinanc,-Soc_Vivienda_Libre)

skim(servicalidMadrid2)



# Agrupo variables

servicalidCM3 <- servicalidCM2 %>%
  mutate(
    EDU = rowSums(dplyr::select(., c("Soc_Bibliot_públicas", "Soc_Ed_Bachiller", "Soc_Ed_Cent_Priv_NoUni",
                              "Soc_Ed_Cent_Pub_NoUni", "Soc_Ed_CiclosFormat", "Soc_Ed_ESO",
                              "Soc_Ed_Especial", "Soc_Ed_Infantil", "Soc_Ed_Primaria"))),
    MOV = rowSums(dplyr::select(., c("Soc_Mov_Est_Cercan", "Soc_Mov_Gasolin", "Soc_Mov_Lin_Autob", "Soc_Mov_Turismos_Pub"))),
    SAL = rowSums(dplyr::select(., c("Soc_Salud_Ambul", "Soc_Salud_CentrosS","Soc_Salud_Consult", "Soc_Salud_Farmacia",
                              "Soc_Serv_Social_Centros"))),
    OCI = rowSums(dplyr::select(., c("Soc_Bienes_Cult", "Soc_Cines_Proy", "Soc_Teatros", "Soc_Ent_Credito","Eco_Establec_Hotel")))
  ) %>%
  dplyr::select(-c("Soc_Bibliot_públicas", "Soc_Ed_Bachiller", "Soc_Ed_Cent_Priv_NoUni",
            "Soc_Ed_Cent_Pub_NoUni", "Soc_Ed_CiclosFormat", "Soc_Ed_ESO",
            "Soc_Ed_Especial", "Soc_Ed_Infantil","Soc_Ed_Primaria",
            "Soc_Mov_Est_Cercan", "Soc_Mov_Gasolin", "Soc_Mov_Lin_Autob", "Soc_Mov_Turismos_Pub",
            "Soc_Salud_Ambul", "Soc_Salud_CentrosS", "Soc_Salud_Consult", "Soc_Salud_Farmacia",
            "Soc_Serv_Social_Centros",
            "Soc_Bienes_Cult", "Soc_Cines_Proy", "Soc_Teatros", "Soc_Ent_Credito"))


# Correlación Económicas

prueba <- servicalidCM3 |> dplyr::select("Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
                                  "Eco_Contratos_Indef", "Eco_Contratos_Temp", 
                                  "Eco_Contratos_Total", "Eco_Distrib_Renta_P80_20", "Eco_Energ_Elect_PC", 
                                  "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", 
                                  "Eco_Paro_100", "Eco_Paro_Porc_Evol", 
                                  "Eco_Renta_Neta_Media_PP", "Eco_Uds_Productivas",
                                  "Eco_PBI_municipal_pc","Eco_Pre_Deuda_Viva", 
                                  "Eco_Pre_Gastos_Liquidados", "Eco_Pre_Ingresos_Liquidados")
correlate(prueba  |>  dplyr::select(where(is.numeric)))

cor_matrix_prueba <-
  cor(prueba %>% dplyr::select(where(is.numeric)))
corrplot(cor_matrix_prueba, tl.cex = 0.7)

# Calcular la matriz de correlación
cor_matrix <- cor(prueba)

# Filtrar las variables con correlación mayor a 0.5
cor_threshold <- 0.5
cor_high <- which(cor_matrix > cor_threshold & cor_matrix < 1, arr.ind = TRUE)

# Obtener los nombres de las variables con correlación mayor a 0.5
variables_high <- colnames(prueba)[unique(cor_high[, 2])]

# Mostrar los nombres de las variables con correlación mayor a 0.5
print(variables_high)

# [1] "Eco_Afiliados_Total"         "Eco_Contratos_Indef"         "Eco_Contratos_Total"         "Eco_Distrib_Renta_P80_20"   
# [5] "Eco_Ind_RDBM_PC"             "Eco_Indice_Gini"             "Eco_Renta_Neta_Media_PP"     "Eco_Uds_Productivas"        
# [9] "Eco_PBI_municipal_pc"        "Eco_Pre_Gastos_Liquidados"   "Eco_Pre_Ingresos_Liquidados"

# Se elimina del modelo una de las variables de las parejas más correladas.

servicalidCM4<-servicalidCM3 |> dplyr::select(-c(Eco_Contratos_Indef,Eco_Contratos_Total,
Eco_Distrib_Renta_P80_20,Eco_Renta_Neta_Media_PP,Eco_Uds_Productivas))

# CORRELACIÓN MA

prueba2 <- servicalidCM4 |> dplyr::select("MA_Incendios", "MA_Pelig_Incend_AltoExt", "MA_Sup_Forestal_Arbol", 
                                  "MA_Sup_Forestal_Desarb", "MA_Sup_Forestal_Total", "MA_Sup_Protegida", 
                                  "MA_Sup_quemada")
correlate(prueba2  |>  dplyr::select(where(is.numeric)))

cor_matrix_prueba2 <-
  cor(prueba2 %>% dplyr::select(where(is.numeric)))
corrplot(cor_matrix_prueba2, tl.cex = 0.7)

# Calcular la matriz de correlación
cor_matrix <- cor(prueba2)

# Filtrar las variables con correlación mayor a 0.5
cor_threshold <- 0.5
cor_high <- which(cor_matrix > cor_threshold & cor_matrix < 1, arr.ind = TRUE)

# Obtener los nombres de las variables con correlación mayor a 0.5
variables_high <- colnames(prueba2)[unique(cor_high[, 2])]

# Mostrar los nombres de las variables con correlación mayor a 0.5
print(variables_high)
# [1] "MA_Sup_Forestal_Arbol"  "MA_Sup_Forestal_Desarb" "MA_Sup_Forestal_Total" 

# Se elimina del modelo una de las variables de las parejas más correladas.

servicalidCM4<-servicalidCM4 |> dplyr::select(-c(MA_Incendios,MA_Sup_Forestal_Arbol,
                                          MA_Sup_Forestal_Desarb))


servicalidCM4$MA_Pelig_Incend_AltoExt <- ifelse(servicalidCM4$MA_Pelig_Incend_AltoExt > 28.6, "Mayor", "Menor")
servicalidCM4$MA_Sup_Forestal_Total <- ifelse(servicalidCM4$MA_Sup_Forestal_Total > 45, "Mayor", "Menor")
servicalidCM4$MA_Sup_Protegida <- ifelse(servicalidCM4$MA_Sup_Protegida > 32.6, "Mayor", "Menor")
servicalidCM4$MA_Sup_quemada <- ifelse(servicalidCM4$MA_Sup_quemada > 0.275, "Mayor", "Menor")


# Correlación con población

prueba3 <- servicalidCM4 |> dplyr::select("Pob_Crecim_Relativo", "Pob_Dependencia", "Pob_Edad_Media", 
                                   "Pob_Empadronada", "Pob_Empadronada_H", "Pob_Empadronada_M", 
                                   "Pob_Envejecimiento", "Pob_EV_65_H", "Pob_EV_65_M", "Pob_EV_65_Total", 
                                   "Pob_EV_H", "Pob_EV_M", "Pob_EV_Total", "Pob_Juventud", 
                                   "Pob_Saldo_Migratorio")

correlate(prueba3  |>  dplyr::select(where(is.numeric)))

cor_matrix_prueba3 <-
  cor(prueba3 %>% dplyr::select(where(is.numeric)))
corrplot(cor_matrix_prueba3, tl.cex = 0.7)

# Calcular la matriz de correlación
cor_matrix <- cor(prueba3)

# Filtrar las variables con correlación mayor a 0.5
cor_threshold <- 0.5
cor_high <- which(cor_matrix > cor_threshold & cor_matrix < 1, arr.ind = TRUE)

# Obtener los nombres de las variables con correlación mayor a 0.5
variables_high <- colnames(prueba3)[unique(cor_high[, 2])]

# Mostrar los nombres de las variables con correlación mayor a 0.5
print(variables_high)

# [1] "Pob_Edad_Media"     "Pob_Empadronada"    "Pob_Empadronada_H"  "Pob_Empadronada_M"  "Pob_Envejecimiento" "Pob_EV_65_H"       
# [7] "Pob_EV_65_M"        "Pob_EV_65_Total"    "Pob_EV_H"           "Pob_EV_M"           "Pob_EV_Total"  

# Se elimina del modelo una de las variables de las parejas más correladas.

servicalidCM5<-servicalidCM4 |> dplyr::select(-c(Pob_Empadronada_H,Pob_Empadronada_M,
                                          Pob_EV_65_H,Pob_EV_65_M,
                                          Pob_EV_H,Pob_EV_M))

# Correlación con servicios

prueba4 <- servicalidCM5 |> dplyr::select("Soc_Mov_Auto_T", "Soc_Mov_Hosp_T", "Soc_Mov_Muni_5000_T", "Soc_Salud_Def_Otras", 
                                   "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor", 
                                   "Soc_Salud_Discapacidad", "Superficie", 
                                   "EDU", "MOV", "SAL", "OCI")
correlate(prueba4  |>  dplyr::select(where(is.numeric)))

cor_matrix_prueba4 <-
  cor(prueba4 %>% dplyr::select(where(is.numeric)))
corrplot(cor_matrix_prueba4, tl.cex = 0.7)

# Calcular la matriz de correlación
cor_matrix <- cor(prueba4)

# Filtrar las variables con correlación mayor a 0.5
cor_threshold <- 0.5
cor_high <- which(cor_matrix > cor_threshold & cor_matrix < 1, arr.ind = TRUE)

# Obtener los nombres de las variables con correlación mayor a 0.5
variables_high <- colnames(prueba4)[unique(cor_high[, 2])]

# Mostrar los nombres de las variables con correlación mayor a 0.5
print(variables_high)

# [1] "Soc_Mov_Hosp_T"         "Soc_Mov_Muni_5000_T"    "Soc_Salud_Discapacidad"
# [4] "EDU"                    "MOV"                    "SAL"                   
# [7] "OCI"    

# Aunque están correladas explican situaciones diferentes, por lo que se mantienen en el modelo para ver cómo se comportan.

# Elimino variables que repiten información

servicalidCM6<-servicalidCM5 |> dplyr::select (-c(Eco_Paro_Porc_Evol,
                                          Pob_Crecim_Relativo,Pob_Dependencia,Pob_Saldo_Migratorio))
skim(servicalidCM6)
dput(names(servicalidCM6))

servicalidCM6$Eco_Cap_Nec_Financ <- ifelse(is.na(servicalidCM6$Eco_Cap_Nec_Financ), "2", servicalidCM6$Eco_Cap_Nec_Financ)

# se observan los datos de las zonas territoriales.

unique(servicalidCM6$Terr_Zonas_Estad_CM)
servicalidCM6$Terr_Zonas_Estad_CM <- gsub(" ", "", servicalidCM6$Terr_Zonas_Estad_CM)

unique(servicalidCM6$Terr_Zonas_Estad_CM)

unique(servicalidCM6$Pob_Grupo)

servicalidCM6$Pob_Grupo <- gsub("[- ]", "_", servicalidCM6$Pob_Grupo)
servicalidCM6$Pob_Grupo <- gsub("≤", "menor_que_", servicalidCM6$Pob_Grupo)
servicalidCM6$Pob_Grupo <- gsub("\\.", "", servicalidCM6$Pob_Grupo)
servicalidCM6$Pob_Grupo <- gsub(">", "mayor_que_", servicalidCM6$Pob_Grupo)
unique(servicalidCM6$Pob_Grupo)

# Elimino caracteres especiales de los nombres de las variables

nombres_originales <- names(servicalidCM6)

# Nombres modificados sin caracteres especiales
nombres_modificados <- gsub("[^[:alnum:]_]", "", nombres_originales)

# Asignar los nuevos nombres a las variables
names(servicalidCM6) <- nombres_modificados

SCM_FINAL<-servicalidCM6

skim(SCM_FINAL)

# Primera creación de listas

listconti<-c(c("Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
"Eco_Contratos_Temp", "Eco_Energ_Elect_PC", 
"Eco_Ind_RDBM_PC", "Eco_Indice_Gini", 
"Eco_Paro_100", "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", 
"Eco_Pre_Gastos_Liquidados", "Eco_Pre_Ingresos_Liquidados",  
"MA_Pelig_Incend_AltoExt", "MA_Sup_Forestal_Total", "MA_Sup_Protegida", 
"MA_Sup_quemada", "Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", 
"Pob_EV_65_Total", "Pob_Juventud", 
"Soc_Mov_Auto_T", "Soc_Mov_Hosp_T", "Soc_Mov_Muni_5000_T", "Soc_Salud_Def_Otras", 
"Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor", 
"Soc_Salud_Discapacidad", "Superficie", 
"EDU", "MOV", "SAL", "OCI"))
             
listclass<-c("Terr_Zonas_Estad_CM", "Pob_Grupo", "Eco_Cap_Nec_Financ", "DEGURBA")

vardep<-c("Pob_EV_Total")





# Correlación

# Calcular la matriz de correlación completa

cor_matrix <- cor(SCM_FINAL |> select(where(is.numeric)))
corrplot(cor_matrix, method = "number", tl.cex = 0.5, number.cex = 0.5, type = "lower")


# Identificar las correlaciones altas
high_cor <- which(abs(cor_matrix) > 0.5 & upper.tri(cor_matrix), arr.ind = TRUE)

high_cor_vars <- unique(colnames(cor_matrix)[high_cor[,2]])
dput(high_cor_vars)

# c("Eco_Afiliados_Total", "Eco_Ind_RDBM_PC", "Eco_PBI_municipal_pc", 
#   "Eco_Pre_Gastos_Liquidados", "Eco_Pre_Ingresos_Liquidados", "Pob_Empadronada", 
#   "Pob_Envejecimiento", "Pob_Juventud", "Soc_Mov_Hosp_T", "Soc_Mov_Muni_5000_T", 
#   "Soc_Salud_Discapacidad", "EDU", "MOV", "SAL", "OCI")

cor_matrix <- SCM_FINAL  |>  dplyr::select(where(is.numeric))  |>  cor()  |>  round(2)
cor_matrix

cor_matrix  |> 
  corrplot(method = "number", tl.cex = 0.55, number.cex = 0.5, type = "lower")

# Crear la submatriz de correlación con variables altamente correlacionadas
sub_cor_matrix <- cor_matrix[high_cor_vars, high_cor_vars]

# Visualizar la submatriz de correlación
corrplot(sub_cor_matrix, method = "number", tl.cex = 0.55, number.cex = 0.5, type = "lower")


# Convertir la variable "DEGURBA" a caracteres
SCM_FINAL$DEGURBA <- as.character(SCM_FINAL$DEGURBA)

# Convertir la variable "DEGURBA" en un factor
SCM_FINAL$DEGURBA <- as.factor(SCM_FINAL$DEGURBA)



# Seleccionar las variables de tipo entero
int_variables <- SCM_FINAL %>%
  select_if(is.integer)

# Imprimir los nombres de las variables seleccionadas
names(int_variables)
skim(int_variables)

SCM_FINAL$Soc_Salud_Discapacidad<- ifelse(SCM_FINAL$Soc_Salud_Discapacidad> 183, "Mayor", "Menor")
SCM_FINAL$Soc_Mov_Muni_5000_T<- ifelse(SCM_FINAL$Soc_Mov_Muni_5000_T> 5.12, "Mayor", "Menor")
SCM_FINAL$Soc_Mov_Hosp_T<- ifelse(SCM_FINAL$Soc_Mov_Hosp_T> 18.7, "Mayor", "Menor")

#Recategorizo por percentiles las variables con mayor diferencia entre la media y la mediana 
skim(SCM_FINAL)
SCM_FINAL <- SCM_FINAL |>  
  mutate(EDU = cut(EDU, breaks = c(-Inf, 3, 8, 24, Inf),
                   labels = c("menor_de_3", "de_4_a_8", "de_9_a_24", "25_o_más")))

SCM_FINAL <- SCM_FINAL |>  
  mutate(OCI = cut(OCI, breaks = c(-Inf, 3, 7, 15, Inf),
                   labels = c("menor_de_3", "de_4_a_7", "de_8_a_15", "16_o_más")))

SCM_FINAL <- SCM_FINAL |>  
  mutate(SAL = cut(SAL, breaks = c(-Inf, 3, 5, 10, Inf),
                   labels = c("menor_de_3", "de_4_a_5", "de_6_a_10", "11_o_más")))

SCM_FINAL <- SCM_FINAL |>  
  mutate(MOV = cut(MOV, breaks = c(-Inf, 3, 5, 12, Inf),
                   labels = c("menor_de_3", "de_4_a_5", "de 6_a_12", "13_o_más")))

SCM_FINAL <- SCM_FINAL |>  
  mutate(Soc_Mov_Auto_T = cut(Soc_Mov_Auto_T, breaks = c(-Inf, 3, 7, 12, Inf),
                   labels = c("menor_de_3", "de_4_a_7", "de_8_a_12", "13_o_más")))

# Guardamos el dataset

write.csv(SCM_FINAL, "SCM_FINAL.csv", row.names = FALSE) # sin estandarizar
save(SCM_FINAL, file = "SCM_FINAL.rda")
write.xlsx(SCM_FINAL, "SCM_FINAL.xlsx")

SCM_FINAL2 <- SCM_FINAL %>%
  mutate(across(where(is.character), as.factor))

#SCM_FINAL2 y SCM_FINAL Tienen MadridCapital

# Quitamos el municipio de Madrid del dataset. Outlier en la receta
SCM_FINAL3 <-  SCM_FINAL2[SCM_FINAL2$Entidad != "Madrid", ]
# Filtrar las observaciones que no tienen el valor "MADRIDCAPITAL"
SCM_FINAL3 <- SCM_FINAL2[SCM_FINAL2$Terr_Zonas_Estad_CM != "MADRIDCAPITAL", ]

# SCM_FINAL3 no tiene Madrid Capital
glimpse(SCM_FINAL3)
skim(SCM_FINAL3)
# Pob_empadronada, Entidad, salen del modelo. Se mantienen en el dataset final antes de modelar.
# Guardamos los dataset

write.csv(SCM_FINAL2, "SCM_FINAL2.csv", row.names = FALSE) # sin estandarizar
save(SCM_FINAL2, file = "SCM_FINAL2.rda")
write.xlsx(SCM_FINAL2, "SCM_FINAL2.xlsx")

write.csv(SCM_FINAL3, "SCM_FINAL3.csv", row.names = FALSE) # sin estandarizar
save(SCM_FINAL3, file = "SCM_FINAL3.rda")
write.xlsx(SCM_FINAL3, "SCM_FINAL3.xlsx")


# TRATAMIENTO DE OUTLIERS SIN MADRID CAPITAL. MÁS RESTRICTIVO

# Calcular los z-scores

SCM_FINAL_na_outliers <- SCM_FINAL3 %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(scale(.)) > 3.5 & !is.na(.), NA, .)))

# Filtrar y mostrar observaciones con NA
na_data <- SCM_FINAL_na_outliers %>%
  filter(if_any(everything(), is.na))

# Mostrar las observaciones con NA
print(na_data)
num_na <- nrow(na_data)
print(num_na)

# Identificar y mostrar los outliers en cada columna numérica
outliers <- SCM_FINAL3 %>%
  select(where(is.numeric)) %>%
  filter(if_any(where(is.numeric), ~ abs(scale(.)) > 3.5)) %>%
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "outlier_value")

unique(outliers$column_name)

# Mostrar el número de outliers por variable
num_outliers <- outliers %>%
  group_by(column_name) %>%
  summarize(num_outliers = n_distinct(outlier_value))

print(num_outliers)

# Imputar ausentes por la mediana agrupando por zona estadística, todas las variable excepto la superficie.

SCM_FINAL_outliers <- SCM_FINAL_na_outliers  |> 
  group_by(Terr_Zonas_Estad_CM)  |> 
  mutate(across(c("Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", "Eco_Contratos_Temp", "Eco_Energ_Elect_PC",
                  "Eco_Establec_Hotel", "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", "Eco_PBI_municipal_pc",
                  "Eco_Pre_Deuda_Viva", "Eco_Pre_Gastos_Liquidados", "Eco_Pre_Ingresos_Liquidados", "Pob_Edad_Media",
                  "Pob_Empadronada", "Pob_Envejecimiento", "Pob_EV_65_Total", "Pob_EV_Total", "Pob_Juventud",
                  "Soc_Salud_Def_Otras", "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor"),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |> 
  ungroup()


# Guardamos el dataset

write.csv(SCM_FINAL_outliers, "SCM_FINAL_outliers.csv", row.names = FALSE) # sin estandarizar
save(SCM_FINAL_outliers, file = "SCM_FINAL_outliers.rda")
write.xlsx(SCM_FINAL_outliers, "SCM_FINAL_outliers.xlsx")



