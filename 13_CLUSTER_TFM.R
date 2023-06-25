
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


library(plyr)
library(dplyr)

library(naniar)
library(corrr)

SCM_FINAL_CLUSTER <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_AUTOML.csv", 
            sep = ",", header = TRUE, dec = ".")
names(SCM_FINAL_CLUSTER)

library(cluster)
# Filtro los últimos años
library(dplyr)

# Filtro desde que tengo subvenciones
SCM_FINAL_CLUSTER1 <- SCM_FINAL_CLUSTER %>%
  filter(año >= 2019)

# Ausentes
SCM_FINAL_CLUSTER2 <- SCM_FINAL_CLUSTER1 %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

library(dummies)
library(tidyr)

listclass <- c("MA_Pelig_Incend_AltoExt","MA_Sup_Forestal_Total","MA_Sup_Protegida", "MA_Sup_quemada",
               "Soc_Mov_Auto_T","Soc_Mov_Hosp_T","Soc_Mov_Muni_5000_T","Soc_Salud_Discapacidad", "EDU", "MOV",
               "SAL", "OCI")

SCM_FINAL_CLUSTER2<- dummy.data.frame(SCM_FINAL_CLUSTER2, listclass, sep = ".")
names(SCM_FINAL_CLUSTER2)
SCM_FINAL_CLUSTER2<-as.data.frame(SCM_FINAL_CLUSTER2)

data_cluster <- SCM_FINAL_CLUSTER |> 
  dplyr::select("DEGURBA",                     
                "Eco_Afiliados_Total",                  
                "Eco_Cap_Nec_Financ", 
                "Eco_Pre_Deuda_Viva",                         
                "Pob_EV_Total",
                "Soc_Salud_Def_SistResp",            
                "Importe_AP_Sumado",   
                "Importe_EDU_Sumado",
                "Importe_SS_Sumado")

# Sin valores NA en tus datos
data_cluster[is.na(data_cluster)] <- 0

data_cluster <- data_cluster %>%
  mutate(across(everything(), ~ replace_na(., mean(., na.rm = TRUE))))

k <- 6
resultados <- kmeans(data_cluster, centers = k)

cluster_labels <- resultados$cluster
cluster_centers <- resultados$centers

SCM_FINAL_CLUSTER$cluster <- as.factor(resultados$cluster)

# Imprimir los municipios y sus asignaciones de cluster
municipios <- SCM_FINAL_CLUSTER$Municipio
data_cluster_with_labels <- cbind(data_cluster, cluster = cluster_labels, municipio = municipios)
print(data_cluster_with_labels)


# Suma de cuadrados dentro del cluster
ss_within_cluster <- resultados$tot.withinss

# Visualización de los resultados
plot(data_cluster, col = cluster_labels, pch = 16)
points(cluster_centers, col = 1:k, pch = 19, cex = 2)

# Obtener las variables con más peso dentro de cada cluster
for (i in 1:k) {  # Iterar sobre cada cluster
  centroid_values <- cluster_centers[i, ]  # Obtener los valores del centroide del cluster
  max_weight_variables <- names(centroid_values)[order(centroid_values, decreasing = TRUE)[1:5]]
  # Obtener las tres variables con los valores más altos en el centroide del cluster
  print(paste("Cluster", i))
  print(max_weight_variables)
}


# VISUALIZACIÓN EN MAPA. CARGAR CON MAPSPAIN DESDE EL CÓDIGO DE MAPAS  

MAPAS_TFM_CLUSTER <- merge(MAPAS_TFM, SCM_FINAL_CLUSTER, by = "Municipio")

library(ggplot2)
mapCLUSTER <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM_CLUSTER,
    aes(fill = factor(cluster)),
    color = "blue", alpha = 0.8, lwd = 0.2
  ) +
  scale_fill_manual(values = c("lightblue", "darkblue", "green", "red", "orange", "purple")) +
  theme_minimal() +
  labs(title = "Municipios en Madrid")

mapCLUSTER

# CLUSTER 2

data_cluster <- SCM_FINAL_CLUSTER |> 
  dplyr::select("DEGURBA", "Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
                "Eco_Cap_Nec_Financ", "Eco_Contratos_Temp", "Eco_Energ_Elect_PC", 
                "Eco_Establec_Hotel", "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", 
                "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", "Eco_Pre_Gastos_Liquidados", 
                "Eco_Pre_Ingresos_Liquidados", "Entidad", "MA_Pelig_Incend_AltoExt", 
                "MA_Sup_Forestal_Total", "MA_Sup_Protegida", "MA_Sup_quemada", 
                "Municipio", "Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", 
                "Pob_EV_65_Total", "Pob_EV_Total", "Pob_Grupo", "Pob_Juventud", 
                "Soc_Mov_Auto_T", "Soc_Mov_Hosp_T", "Soc_Mov_Muni_5000_T", "Soc_Salud_Def_Otras", 
                "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor", 
                "Soc_Salud_Discapacidad", "Superficie", "Terr_Zonas_Estad_CM", 
                "EDU", "MOV", "SAL", "OCI", "Importe_AP_Sumado", "Importe_EDU_Sumado", 
                "Importe_SS_Sumado")

# Sin valores NA en tus datos
data_cluster[is.na(data_cluster)] <- 0

data_cluster <- data_cluster %>%
  mutate(across(everything(), ~ replace_na(., mean(., na.rm = TRUE))))

k <- 6
resultados <- kmeans(data_cluster, centers = k)

cluster_labels <- resultados$cluster
cluster_centers <- resultados$centers

SCM_FINAL_CLUSTER$cluster <- as.factor(resultados$cluster)

# Imprimir los municipios y sus asignaciones de cluster
municipios <- SCM_FINAL_CLUSTER$Municipio
data_cluster_with_labels <- cbind(data_cluster, cluster = cluster_labels, municipio = municipios)
print(data_cluster_with_labels)


# Suma de cuadrados dentro del cluster
ss_within_cluster <- resultados$tot.withinss

# # Visualización de los resultados
# plot(data_cluster, col = cluster_labels, pch = 16)
# points(cluster_centers, col = 1:k, pch = 19, cex = 2)


library(reactable)

# Crear una lista para almacenar los resultados
resultados_variables_peso <- list()

# Obtener las variables con más peso dentro de cada cluster
for (i in 1:k) {  # Iterar sobre cada cluster
  centroid_values <- cluster_centers[i, ]  # Obtener los valores del centroide del cluster
  max_weight_variables <- names(centroid_values)[order(centroid_values, decreasing = TRUE)[1:7]]
  # Obtener las cinco variables con los valores más altos en el centroide del cluster
  resultados_variables_peso[[paste0("Cluster", i)]] <- max_weight_variables
}

# Convertir la lista en un data frame
df_variables_peso <- as.data.frame(resultados_variables_peso)

# Crear la tabla con reactable
tabla_variables_peso <- reactable(df_variables_peso)

# Mostrar la tabla
tabla_variables_peso

# VISUALIZACIÓN EN MAPA. CARGAR CON MAPSPAIN DESDE EL CÓDIGO DE MAPAS  

MAPAS_TFM_CLUSTER2 <- merge(MAPAS_TFM, SCM_FINAL_CLUSTER, by = "Municipio")

library(ggplot2)
mapCLUSTER2 <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM_CLUSTER2,
    aes(fill = factor(cluster)),
    color = "blue", alpha = 0.8, lwd = 0.2
  ) +
  scale_fill_manual(values = c("lightblue", "darkblue", "green", "red", "orange", "purple")) +
  theme_minimal() +
  labs(title = "Municipios en Madrid")

mapCLUSTER2

mapCLUSTER2 <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM_CLUSTER2,
    aes(fill = factor(cluster)),
    color = "blue", alpha = 0.8, lwd = 0.2
  ) +
  scale_fill_manual(values = c("lightblue", "darkblue", "green", "red", "orange", "purple")) +
  theme_minimal() +
  labs(title = "Municipios en Madrid")

mapCLUSTER2 <- mapCLUSTER2 +
  geom_text(data = subset(MAPAS_TFM_CLUSTER2, cluster == 5),
            aes(x = lon, y = lat, label = Municipio), size=3) +
  theme_minimal() +
  theme(text = element_text(size = 8))

mapCLUSTER2

##### CLUSTER JERÁRQUICO #######

SCM_FINAL_CLUSTERJ <- SCM_FINAL_CLUSTER |> select(c("DEGURBA",                     
                                                    "Eco_Afiliados_Total",                  
                                                    "Eco_Cap_Nec_Financ", 
                                                    "Eco_Pre_Deuda_Viva",                         
                                                    "Pob_EV_Total",
                                                    "Soc_Salud_Def_SistResp",            
                                                    "Importe_AP_Sumado",   
                                                    "Importe_EDU_Sumado",
                                                    "Importe_SS_Sumado"))
# Calcular la matriz de distancias
distancia <- dist(SCM_FINAL_CLUSTERJ, method = "euclidean")

# Realizar el agrupamiento jerárquico
resultados_hclust <- hclust(distancia, method = "complete")

# Obtener los grupos/clusters a partir del resultado del agrupamiento jerárquico
k <- 6  # Número deseado de grupos/clusters
grupos <- cutree(resultados_hclust, k = k)

# Crear un data frame con los resultados del agrupamiento jerárquico y las etiquetas
resultados <- data.frame(nodo = rownames(SCM_FINAL_CLUSTERJ), grupo = grupos)

# Crear un dendrograma del modelo jerárquico
dendrograma <- as.dendrogram(resultados_hclust)

# Crear el gráfico del modelo jerárquico
modelo_jerarquico <- plot(dendrograma)

# Agregar etiquetas a los nodos terminales del gráfico del modelo jerárquico
modelo_jerarquico_con_etiquetas <- modelo_jerarquico +
  geom_text(data = resultados, aes(label = nodo), hjust = -0.1, vjust = 0.5)

# Mostrar el gráfico con las etiquetas
modelo_jerarquico_con_etiquetas

# Crear el gráfico del modelo jerárquico con opciones mejoradas de legibilidad
modelo_jerarquico <- plot(dendrograma, main = "Dendrograma", ylab = "Distancia", sub = NULL, ylim = NULL,
                          xlim = NULL, xlab = NULL, hang = -1, axes = TRUE, frame.plot = FALSE,
                          ann = TRUE, type = "rectangle", horiz = FALSE, 
                          lwd = 1, edgePar = list(lty = "solid", col = "black"),
                          horiz.dendrogram = FALSE, nodePar = NULL, 
                          leaflab = "perpendicular", cex = 0.8)

# Ajustar tamaño de la figura del gráfico
par(mar = c(2, 5, 2, 2))  # Margen izquierdo extendido

# Mostrar el gráfico del modelo jerárquico
modelo_jerarquico

# Calcular la matriz de distancias entre variables
distancia_variables <- dist(t(SCM_FINAL_CLUSTERJ))

# Realizar el agrupamiento jerárquico de variables
resultado_hclust <- hclust(distancia_variables, method = "complete")

# Obtener los grupos/clusters de variables a partir del resultado del agrupamiento jerárquico
k <- 6  # Número deseado de grupos/clusters
grupos_variables <- cutree(resultado_hclust, k = k)

# Crear un data frame con los resultados del agrupamiento jerárquico de variables y las etiquetas
resultados_variables <- data.frame(variable = colnames(SCM_FINAL_CLUSTERJ), grupo = grupos_variables)

# Mostrar las variables más importantes para cada cluster
for (i in 1:k) {
  cat("Cluster", i, ":\n")
  variables_importantes <- resultados_variables$variable[resultados_variables$grupo == i]
  print(variables_importantes)
}

# Convertir la lista en un data frame
df_variables_peso <- as.data.frame(resultados_variables_peso)

# Crear la tabla con reactable
tabla_variables_peso <- reactable(df_variables_peso)

# Mostrar la tabla
tabla_variables_peso

## CLUSTER JERÁRQUICO 2 #### 

data_clusterj <- SCM_FINAL_CLUSTER |> 
  dplyr::select("DEGURBA", "Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
                "Eco_Cap_Nec_Financ", "Eco_Contratos_Temp", "Eco_Energ_Elect_PC", 
                "Eco_Establec_Hotel", "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", 
                "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", "Eco_Pre_Gastos_Liquidados", 
                "Eco_Pre_Ingresos_Liquidados", "Entidad", "MA_Pelig_Incend_AltoExt", 
                "MA_Sup_Forestal_Total", "MA_Sup_Protegida", "MA_Sup_quemada", 
                "Municipio", "Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", 
                "Pob_EV_65_Total", "Pob_EV_Total", "Pob_Grupo", "Pob_Juventud", 
                "Soc_Mov_Auto_T", "Soc_Mov_Hosp_T", "Soc_Mov_Muni_5000_T", "Soc_Salud_Def_Otras", 
                "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor", 
                "Soc_Salud_Discapacidad", "Superficie", "Terr_Zonas_Estad_CM", 
                "EDU", "MOV", "SAL", "OCI", "Importe_AP_Sumado", "Importe_EDU_Sumado", 
                "Importe_SS_Sumado")

# Sin valores NA en tus datos
data_clusterj[is.na(data_clusterj)] <- 0

data_clusterj <- data_clusterj %>%
  mutate(across(everything(), ~ replace_na(., mean(., na.rm = TRUE))))

# Calcular la matriz de distancias
distancia <- dist(data_clusterj, method = "euclidean")

# Realizar el agrupamiento jerárquico
resultados_hclust <- hclust(distancia, method = "complete")

# Obtener los grupos/clusters a partir del resultado del agrupamiento jerárquico
k <- 6  # Número deseado de grupos/clusters
grupos <- cutree(resultados_hclust, k = k)

# Crear un data frame con los resultados del agrupamiento jerárquico y las etiquetas
resultados <- data.frame(nodo = rownames(SCM_FINAL_CLUSTER), grupo = grupos)

# Crear un dendrograma del modelo jerárquico
dendrograma <- as.dendrogram(resultados_hclust)

# Crear el gráfico del modelo jerárquico
modelo_jerarquico <- plot(dendrograma)

# Agregar etiquetas a los nodos terminales del gráfico del modelo jerárquico
modelo_jerarquico_con_etiquetas <- modelo_jerarquico +
  geom_text(data = resultados, aes(label = nodo), hjust = -0.1, vjust = 0.5)

# Mostrar el gráfico con las etiquetas
modelo_jerarquico_con_etiquetas

# Crear el gráfico del modelo jerárquico con opciones mejoradas de legibilidad
modelo_jerarquico <- plot(dendrograma, main = "Dendrograma", ylab = "Distancia", sub = NULL, ylim = NULL,
                          xlim = NULL, xlab = NULL, hang = -1, axes = TRUE, frame.plot = FALSE,
                          ann = TRUE, type = "rectangle", horiz = FALSE, 
                          lwd = 1, edgePar = list(lty = "solid", col = "black"),
                          horiz.dendrogram = FALSE, nodePar = NULL, 
                          leaflab = "perpendicular", cex = 0.8)

# Ajustar tamaño de la figura del gráfico
par(mar = c(2, 5, 2, 2))  # Margen izquierdo extendido

# Mostrar el gráfico del modelo jerárquico
modelo_jerarquico

SCM_FINAL_CLUSTER <-SCM_FINAL_CLUSTER |> select(c("DEGURBA", "Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
                                                    "Eco_Cap_Nec_Financ", "Eco_Contratos_Temp", "Eco_Energ_Elect_PC", 
                                                    "Eco_Establec_Hotel", "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", "Eco_Paro_100", 
                                                    "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", "Eco_Pre_Gastos_Liquidados", 
                                                    "Eco_Pre_Ingresos_Liquidados", "Entidad", "MA_Pelig_Incend_AltoExt", 
                                                    "MA_Sup_Forestal_Total", "MA_Sup_Protegida", "MA_Sup_quemada", 
                                                    "Municipio", "Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", 
                                                    "Pob_EV_65_Total", "Pob_EV_Total", "Pob_Grupo", "Pob_Juventud", 
                                                    "Soc_Mov_Auto_T", "Soc_Mov_Hosp_T", "Soc_Mov_Muni_5000_T", "Soc_Salud_Def_Otras", 
                                                    "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor", 
                                                    "Soc_Salud_Discapacidad", "Superficie", "Terr_Zonas_Estad_CM", 
                                                    "EDU", "MOV", "SAL", "OCI", "Importe_AP_Sumado", "Importe_EDU_Sumado", 
                                                    "Importe_SS_Sumado"))

# Calcular la matriz de distancias entre variables
distancia_variables <- dist(t(SCM_FINAL_CLUSTER))

# Realizar el agrupamiento jerárquico de variables
resultado_hclust <- hclust(distancia_variables, method = "complete")

# Obtener los grupos/clusters de variables a partir del resultado del agrupamiento jerárquico
k <- 6  # Número deseado de grupos/clusters
grupos_variables <- cutree(resultado_hclust, k = k)

# Crear un data frame con los resultados del agrupamiento jerárquico de variables y las etiquetas
resultados_variables <- data.frame(variable = colnames(SCM_FINAL_CLUSTER), grupo = grupos_variables)

# Mostrar las variables más importantes para cada cluster
for (i in 1:k) {
  cat("Cluster", i, ":\n")
  variables_importantes <- resultados_variables$variable[resultados_variables$grupo == i]
  print(variables_importantes)
}

# Convertir la lista en un data frame
df_variables_peso <- as.data.frame(resultados_variables_peso)

# Crear la tabla con reactable
tabla_variables_peso <- reactable(df_variables_peso)

# Mostrar la tabla
tabla_variables_peso

