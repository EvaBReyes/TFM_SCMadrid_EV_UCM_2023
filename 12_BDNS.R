# Incorporo los datos de la BDNS obtenidos por WebScrapping al dataset

# Cargar los paquetes readxl y dplyr
library(readxl)
library(dplyr)
library(readxl)

# Leer el dataset SCM_TOTAL3 (sin Madrid)
SCM_FINAL_AUTO <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL3.csv", 
            sep = ",", header = TRUE, dec = ".")

# Leer el archivo de importes en formato Excel
BDNS <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/BDNS/BDNS.xlsx")

Conc_EDU <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/BDNS/BDNS.xlsx", sheet = 1)
Conc_SAL <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/BDNS/BDNS.xlsx", sheet = 2)
Conc_SS <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/BDNS/BDNS.xlsx", sheet = 3)
Conc_AP <- read_excel("C:/Users/evaba/Documents/TFM/DATOS/BDNS/BDNS.xlsx", sheet = 4)

# Extraer los 9 primeros dígitos de la columna "Beneficiario" 
Conc_EDU$Beneficiario <- substr(Conc_EDU$Beneficiario, 1, 9)
Conc_SAL$Beneficiario <- substr(Conc_SAL$Beneficiario, 1, 9)
Conc_SS$Beneficiario <- substr(Conc_SS$Beneficiario, 1, 9)
Conc_AP$Beneficiario <- substr(Conc_AP$Beneficiario, 1, 9)

# Quedarse con los 4 últimos dígitos de la columna "fecha_concesión" en Conc_EDU
Conc_EDU$fecha_concesion <- substr(Conc_EDU$fecha_concesion, nchar(Conc_EDU$fecha_concesion) - 3, nchar(Conc_EDU$fecha_concesion))
Conc_SAL$fecha_concesion <- substr(Conc_SAL$fecha_concesion, nchar(Conc_SAL$fecha_concesion) - 3, nchar(Conc_SAL$fecha_concesion))
Conc_SS$fecha_concesion <- substr(Conc_SS$fecha_concesion, nchar(Conc_SS$fecha_concesion) - 3, nchar(Conc_SS$fecha_concesion))
Conc_AP$fecha_concesion <- substr(Conc_AP$fecha_concesion, nchar(Conc_AP$fecha_concesion) - 3, nchar(Conc_AP$fecha_concesion))

# Seleccionar las columnas deseadas en Conc_EDU
# Seleccionar las columnas deseadas en Conc_EDU
Conc_EDU <- Conc_EDU[, c("Beneficiario", "fecha_concesion", "Importe_EDU")]
Conc_SAL <- Conc_SAL[, c("Beneficiario", "fecha_concesion", "Importe_SAL")]
Conc_SS <- Conc_SS[, c("Beneficiario", "fecha_concesion", "Importe_SS")]
Conc_AP <- Conc_AP[, c("Beneficiario", "fecha_concesion", "Importe_AP")]


# Unir las hojas por Beneficiario y fecha_concesion
merged_data <- merge(Conc_AP, Conc_EDU, by = c("Beneficiario", "fecha_concesion"), all = TRUE)
merged_data <- merge(merged_data, Conc_SAL, by = c("Beneficiario", "fecha_concesion"), all = TRUE)
merged_data <- merge(merged_data, Conc_SS, by = c("Beneficiario", "fecha_concesion"), all = TRUE)

# Ordenar el dataset por Beneficiario y fecha_concesion
merged_data <- merged_data[order(merged_data$Beneficiario, merged_data$fecha_concesion), ]

# Verificar y ajustar los nombres de las columnas de importe en caso de que haya duplicados
colnames(merged_data) <- make.unique(colnames(merged_data))

# Mostrar el nuevo dataset resultante
merged_data
colnames(merged_data)[colnames(merged_data) == "Beneficiario"] <- "CIF"

# Cambiar el nombre de la columna "fecha_concesion" por "año"
colnames(merged_data)[colnames(merged_data) == "fecha_concesion"] <- "año"

# Mostrar el nuevo dataset resultante
merged_data

# Filtrar las observaciones cuyo CIF empieza por "P"
filtered_dataset <- subset(merged_data, grepl("^P", CIF))

# Mostrar el nuevo dataset resultante
filtered_dataset
filtered_dataset <- subset(filtered_dataset, select = -c(Importe_SAL))

# La actividad sanitaria no tiene subvenciones dirigidas a los municipios, los CIF corresponden a los centros sanitarios y otros.

# Realizar el join por CIF y año
filtered_dataset$año <- as.integer(filtered_dataset$año)
library(dplyr)

library(dplyr)

# Convertir las columnas de importe a tipo numérico
filtered_dataset <- filtered_dataset %>%
  mutate(Importe_AP = as.numeric(Importe_AP),
         Importe_EDU = as.numeric(Importe_EDU),
         Importe_SS = as.numeric(Importe_SS))

# Calcular las sumas de las columnas Importe_AP, Importe_EDU y Importe_SS por año y CIF
summed_dataset <- filtered_dataset %>%
  group_by(CIF, año) %>%
  summarize(Importe_AP_Sumado = sum(Importe_AP),
            Importe_EDU_Sumado = sum(Importe_EDU),
            Importe_SS_Sumado = sum(Importe_SS)) |> 
  ungroup()
summed_dataset

summed_dataset2 <- summed_dataset |>  select(-Importe_EDU_Sumado, -Importe_SS_Sumado)

# Unir el dataset sumado a SCM_FINAL_AUTOML por CIF y año
SCM_FINAL_AUTOML <- left_join(SCM_FINAL_AUTO, summed_dataset2, by = c("CIF", "año"))

# Guardar el dataset actualizado
write.csv(SCM_FINAL_AUTOML, "SCM_FINAL_AUTOML.csv", row.names = FALSE)
