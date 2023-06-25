library(ggplot2)
library(forcats)
library(dplyr)
library(ggthemes)

# Cargo los dataset

SCM_FINAL_GR <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL2.csv", 
            sep = ",", header = TRUE, dec = ".")

SCM_FINAL_outliers <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_outliers.csv", 
            sep = ",", header = TRUE, dec = ".")

library(ggplot2)

# Gráficos Box-Plot entre cualis y Variable objetivo (Esperanza de Vida al nacer)

# Según el Grupo de población

ggplot(SCM_FINAL_GR, aes(x = Pob_Grupo, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Grupo de Población", y = "Esperanza de Vida") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()


# gráfico mejorado
unique(SCM_FINAL_GR$Pob_Grupo)
ggplot(SCM_FINAL_GR, aes(x = Pob_Grupo, y = Pob_EV_Total, fill = Pob_Grupo)) +
  geom_boxplot() +
  labs(x = "Grupo de Población", y = "Esperanza de Vida") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_manual(values = c("100001_500000_hab" = "red", "20001_50000_hab" = "blue",
                               "50001_75000" = "green", "75001_100000_hab"="pink", 
                               "mayor_que_500000_hab" = "lightblue"))


# Según las zonas estadísticas
unique(SCM_FINAL_GR$Terr_Zonas_Estad_CM)
ggplot(SCM_FINAL_GR, aes(x = Terr_Zonas_Estad_CM, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Zonas estadísticas NUT4", y = "Esperanza de Vida al nacer") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_manual(values = c("SURMETROPOLITANO" = "red", "OESTEMETROPOLITANO" = "blue",
                               "MADRIDCAPITAL" = "lightblue"))

# Sin outliers
unique(SCM_FINAL_outliers$Terr_Zonas_Estad_CM)
ggplot(SCM_FINAL_outliers, aes(x = Terr_Zonas_Estad_CM, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Zonas estadísticas NUT4", y = "Esperanza de Vida al nacer") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_manual(values = c("SURMETROPOLITANO" = "red", "OESTEMETROPOLITANO" = "blue",
                               "SIERRANORTE" = "green", "SIERRACENTRAL"="pink", 
                               "SIERRASUR" = "lightblue"))

# Según los equipamientos de salud
unique(SCM_FINAL_GR$SAL)
ggplot(SCM_FINAL_GR, aes(x = SAL, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Equipamientos de salud", y = "Esperanza de Vida al nacer") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_manual(values = c("menor_de_3" = "red", "11_o_más" = "blue",
                               "de_4_a_5" = "green", "de_6_a_10"="pink" 
                        ))

# Según los equipamientos de ocio
unique(SCM_FINAL_GR$OCI)
ggplot(SCM_FINAL_GR, aes(x = OCI, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Equipamientos de ocio", y = "Esperanza de Vida al nacer") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_manual(values = c("menor_de_3" = "red", "16_o_más" = "blue",
                               "de_4_a_7" = "green", "de_8_a_15"="pink" 
  ))

# Según el tiempo a la autovía
unique(SCM_FINAL_GR$Soc_Mov_Auto_T)
ggplot(SCM_FINAL_GR, aes(x = Soc_Mov_Auto_T, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Distancia en tiempo a la autovía", y = "Esperanza de Vida al nacer") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()

# Según el tiempo al hospital

ggplot(SCM_FINAL_GR, aes(x = Soc_Mov_Hosp_T, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Distancia al hospital", y = "Esperanza de Vida al nacer") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()

# Según equipamientos de educación

ggplot(SCM_FINAL_GR, aes(x = EDU, y = Pob_EV_Total)) +
  geom_boxplot() +
  labs(x = "Equipamientos de educación", y = "Esperanza de Vida al nacer") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()

# Variables cuantitativas

# HEATMAP

# Seleccionar las variables de interés
variables <- c("Pob_EV_Total", "Eco_Afiliados_Total", "Eco_Ind_RDBM_PC", "Eco_Pre_Gastos_Liquidados", 
               "Pob_Envejecimiento", "Eco_Paro_100")

install.packages("gplots")
library(gplots)
data <- SCM_FINAL_GR[, variables]
cor_matrix <- cor(data, use = "pairwise.complete.obs")

heatmap.2(cor_matrix,
          col = colorRampPalette(c("blue", "white", "red"))(100),
          main = "Heatmap de correlación",
          xlab = "",
          ylab = "",
          cexRow = 0.8,  # Ajusta el tamaño de las etiquetas del eje y
          cexCol = 0.6)  # Ajusta el tamaño de las etiquetas del eje x
par(cex.axis = 0.6)

variables2 <- c("Pob_EV_Total", "Eco_Establec_Hotel", "Eco_Indice_Gini", "Eco_PBI_municipal_pc", 
               "Eco_Pre_Deuda_Viva", "Pob_Empadronada", "Pob_Juventud","Soc_Salud_Def_Otras",
               "Soc_Salud_Def_SistCirc","Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor")

data <- SCM_FINAL_GR[, variables2]
cor_matrix <- cor(data, use = "pairwise.complete.obs")

heatmap.2(cor_matrix,
          col = colorRampPalette(c("blue", "white", "red"))(100),
          main = "Heatmap de correlación",
          xlab = "",
          ylab = "",
          cexRow = 0.6,  # Ajusta el tamaño de las etiquetas del eje y
          cexCol = 0.6)  # Ajusta el tamaño de las etiquetas del eje x
par(cex.axis = 0.6)

# Histograma

ggplot(SCM_FINAL_GR) +
  aes(x = Pob_EV_Total) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Pob_EV_Total",
       x = "Pob_EV_Total",
       y = "Frecuencia") +
  theme_minimal()

library(plotly)

plot_ly(data = SCM_FINAL_GR, x = ~Pob_EV_Total, type = "histogram") %>%
  layout(title = "Histograma de Pob_EV_Total",
         xaxis = list(title = "Pob_EV_Total"),
         yaxis = list(title = "Frecuencia"),
         showlegend = FALSE)

plot_ly(data = SCM_FINAL_outliers, x = ~Pob_EV_Total, type = "histogram") %>%
  layout(title = "Histograma de Pob_EV_Total",
         xaxis = list(title = "Pob_EV_Total"),
         yaxis = list(title = "Frecuencia"),
         showlegend = FALSE)

plot_ly(data = SCM_FINAL_outliers %>% filter(!(Pob_EV_Total >= 84.18 & Pob_EV_Total <= 84.22)),
        x = ~Pob_EV_Total,
        type = "histogram") %>%
  layout(title = "Histograma de Pob_EV_Total",
         xaxis = list(title = "Pob_EV_Total"),
         yaxis = list(title = "Frecuencia"),
         showlegend = FALSE)


plot_ly(data = SCM_FINAL_outliers, x = ~Eco_Afiliados_Total, type = "histogram") %>%
  layout(title = "Histograma de Eco_Afiliados_Total",
         xaxis = list(title = "Eco_Afiliados_Total"),
         yaxis = list(title = "Frecuencia"),
         showlegend = FALSE)

ggplot(SCM_FINAL_outliers) +
  aes(x = Eco_Afiliados_Total) +
  geom_histogram(binwidth = 1000, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Eco_Afiliados_Total",
       x = "Eco_Afiliados_Total",
       y = "Frecuencia") +
  theme_minimal()

# Gráficos de barras entre cualis y Variable objetivo (Esperanza de Vida al nacer)
# Crear el gráfico de barras

data_EV <- SCM_FINAL_GR %>% 
  mutate(Pob_EV_Total = cut(Pob_EV_Total, breaks = c(0, 82, 83, 84, 85, 86, Inf),
                            labels = c("De 0 a 82", "De 82 a 83", "De 83 a 84", "De 84 a 85", "De 85 a 86", "más de 86")))
# Filtrar los datos y eliminar el tramo de 84 a 85
data_EV2 <- data_EV[data_EV$Pob_EV_Total != "De 84 a 85", ]

ggplot(data_EV) +
  aes(x = MA_Sup_Forestal_Total, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer y superficie forestal",
       x = "MA_Sup_Forestal_Total",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))
names(data_EV)

ggplot(data_EV) +
  aes(x = Soc_Salud_Discapacidad, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer por nº personas con discapacidad reconocida",
       x = "Soc_Salud_Discapacidad",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))


ggplot(data_EV) +
  aes(x = Eco_Cap_Nec_Financ, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer por capacidad o necesidad de financiación municipal",
       x = "Eco_Cap_Nec_Financ",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))

ggplot(data_EV) +
  aes(x = MA_Sup_quemada, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer por superficie quemada",
       x = "MA_Sup_quemada",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))

ggplot(data_EV) +
  aes(x = MA_Sup_Protegida, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer por superficie protegida",
       x = "MA_Sup_Protegida",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))



ggplot(data_EV) +
  aes(x = Soc_Mov_Hosp_T, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer por tiempo al hospital",
       x = "Soc_Mov_Hosp_T",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))


# Generar el gráfico de barras actualizado
ggplot(data_EV2) +
  aes(x = Pob_Grupo, fill = Pob_EV_Total) +
  geom_bar() +
  labs(title = "Esperanza de vida al nacer por grupo de población",
       x = "Pob_Grupo",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()

ggplot(data_EV) +
  aes(x = Pob_Grupo, fill = Pob_EV_Total) +
  geom_bar() +
  labs(title = "Esperanza de vida al nacer por grupo de población",
       x = "Pob_Grupo",
       y = "Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  scale_color_economist()

ggplot(data_EV) +
  aes(x = Pob_Grupo, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer por grupo de población",
       x = "",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid = element_blank())+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))


ggplot(data_EV) +
  aes(x = Terr_Zonas_Estad_CM, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "Esperanza de vida al nacer por zonas NUTS4",
       x = "",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid = element_blank())+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))

data_EV_filtered <- data_EV[data_EV$Terr_Zonas_Estad_CM == "OESTEMETROPOLITANO", ]

ggplot(data_EV_filtered) +
  aes(x = Municipio, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "EV en Municipios del OESTEMETROPOLITANO",
       x = "Municipio",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid = element_blank())+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink",
                               "más de 86" = "orange"))


data_EV2_filtered2 <- data_EV[data_EV$Terr_Zonas_Estad_CM == "SURMETROPOLITANO", ]

ggplot(data_EV2_filtered2) +
  aes(x = Municipio, fill = Pob_EV_Total) +
  geom_bar(position = "fill") +
  labs(title = "EV en Municipios del SURMETROPOLITANO",
       x = "Municipio",
       y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink"))


ggplot(SCM_FINAL_GR) +
  aes(x = MOV, fill = MOV) +
  geom_bar() +
  labs(title = "Distribución equipamientos MOV",
       x = "MOV",
       y = "Count") +
  theme_minimal()

ggplot(SCM_FINAL_GR) +
  aes(x = SAL, fill = SAL) +
  geom_bar() +
  labs(title = "Distribución equipamientos SAL",
       x = "SAL",
       y = "Count") +
  theme_minimal()

ggplot(SCM_FINAL_GR) +
  aes(x = EDU, fill = EDU) +
  geom_bar() +
  labs(title = "Distribución equipamientos EDU",
       x = "EDU",
       y = "Count") +
  theme_minimal()

ggplot(SCM_FINAL_GR) +
  aes(x = OCI, fill = OCI) +
  geom_bar() +
  labs(title = "Distribución equipamientos OCI",
       x = "OCI",
       y = "Count") +
  theme_minimal()

# Agrupación de la variable objetivo en categorías para visualización
data_EV <- SCM_FINAL_GR %>% 
  mutate(Pob_EV_Total = cut(Pob_EV_Total, breaks = c(0, 82, 83, 84, 85, 86, Inf),
                            labels = c("De 0 a 82", "De 82 a 83", "De 83 a 84", "De 84 a 85", "De 85 a 86", "más de 86")))
# Filtrar los datos y eliminar el tramo de 84 a 85
data_EV2 <- data_EV[data_EV$Pob_EV_Total != "De 84 a 85", ]


ggplot(data_EV) +
  aes(x = Pob_EV_Total, fill = fct_infreq(Pob_EV_Total)) +
  geom_bar(color = "white", alpha = 0.5) +
  scale_fill_brewer(palette = "Accent") +
  labs(title = "Distribución Pob_EV_Total",
       subtitle = " ",
       x = "Rangos de Pob_EV_Total",
       y = "Conteo") +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("De 0 a 82" = "red",
                               "De 82 a 83" = "blue",
                               "De 83 a 84" = "lightgreen",
                               "De 84 a 85" = "lightblue",
                               "De 85 a 86" = "pink"))


SCM_FINAL_GR_85 <- SCM_FINAL_GR %>% filter(Pob_EV_Total >85)
SCM_FINAL_GR_84 <- SCM_FINAL_GR %>% filter(Pob_EV_Total <84)

ggplot(SCM_FINAL_GR_85) +
  aes(x = Pob_EV_Total) +
  geom_histogram(color = "white", fill = "skyblue", bins = 30) +
  labs(title = "Histograma de Pob_EV_Total",
       x = "Pob_EV_Total",
       y = "Frecuencia") +
  theme_minimal()

ggplot(SCM_FINAL_GR_85) +
  aes(x = Pob_Grupo, y = Pob_EV_Total) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Valores de Pob_EV_Total por Pob_Grupo",
       x = "Pob_Grupo",
       y = "Pob_EV_Total") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de dispersión

ggplot(SCM_FINAL_GR) +
  aes(x = Pob_Grupo, y = Pob_EV_Total) +
  geom_point(color = "red") +
  labs(title = "Valores de Pob_EV_Total por Pob_Grupo",
       x = "Pob_Grupo",
       y = "Pob_EV_Total") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(SCM_FINAL_GR) +
  aes(x = Entidad, y = Pob_EV_Total) +
  geom_point(color = "red") +
  labs(title = "Valores de Pob_EV_Total por Pob_Grupo",
       x = "Municipio",
       y = "Pob_EV_Total") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1, size=3))


SCM_FINAL_GR$año <- as.integer(SCM_FINAL_GR$año)
SCM_FINAL_GR_2014_2020 <- SCM_FINAL_GR |> filter(año > 2013 & año < 2021)


# Calcula el mínimo, máximo y valor intermedio de 'Pob_EV_Total'
y_min <- min(SCM_FINAL_GR_2014_2020$Pob_EV_Total, na.rm = TRUE)
y_max <- max(SCM_FINAL_GR_2014_2020$Pob_EV_Total, na.rm = TRUE)
y_mid <- (y_max + y_min) / 2  # Valor medio

ggplot(SCM_FINAL_GR_2014_2020) +
  aes(x = año, y = Pob_EV_Total) +
  geom_point(color = "red") +
  labs(title = "Esperanza de vida al nacer por año en la Comunidad de Madrid",
       x = " ",
       y = " ") +
  scale_x_continuous(breaks = unique(SCM_FINAL_GR_2014_2020$año)) +
  scale_y_continuous(labels = function(y) sprintf("%.3f", y), 
                     breaks = c(y_min, y_mid, y_max),
                     limits = c(y_min, y_max)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))





ggplot(SCM_FINAL_GR) +
  aes(x = DEGURBA, y = Pob_EV_Total) +
  geom_point(color = "red") +
  labs(title = "Valores de Pob_EV_Total por DEBURBA",
       x = "DEGURBA",
       y = "Pob_EV_Total") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Columnas acumuladas

ggplot(data_EV %>% 
         arrange(Terr_Zonas_Estad_CM),
       aes(x = Pob_Grupo, fill = Pob_EV_Total)) +
  geom_bar(color = "white", alpha = 0.5) +
  labs(title = "Distribución de la Esperanza de Vida por zonas estadísticas",
       subtitle = " ",
       x = "Zonas estadísticas", y = "nº de Municipios * 10") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FFC0CB", "#FFA500", "#FF0000", "#800080", "#FF00FF", "#FFC0CB"))

ggplot(data_EV %>% 
         arrange(Pob_Grupo),
       aes(x = Pob_Grupo, fill = Pob_EV_Total)) +
  geom_bar(color = "white", alpha = 0.5) +
  labs(title = "Distribución de la Esperanza de Vida por grupo de población",
       subtitle = " ",
       x = "Tipo de vivienda", y = "nº de Municipios * 10") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FFC0CB", "#FFA500", "#FF0000", "#800080", "#FF00FF", "#FFC0CB"))


ggplot(data_EV %>% 
         arrange(Terr_Zonas_Estad_CM),
       aes(x = Terr_Zonas_Estad_CM, fill = Pob_EV_Total)) +
  geom_bar(color = "white", alpha = 0.5) +
  labs(title = "Distribución de la Esperanza de Vida por zonas estadísticas",
       subtitle = " ",
       x = "Zonas estadísticas", y = "Nº de municipios x 10") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FFC0CB", "#FFA500", "#FF0000", "#800080", "#FF00FF", "#FFC0CB"))




