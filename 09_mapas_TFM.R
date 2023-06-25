
# Se instalan los paquetes si son necesarios
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")
if(!require("mapSpain")) installed.packages("mapSpain", dependencies = TRUE)
if(!require("giscoR")) installed.packages("giscoR")
if(!require("rlang")) install.packages("rlang")

install.packages("mapSpain")
install.packages('ggthemes', dependencies = TRUE)

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(ggplot2)
library(ggthemes)
library (mapSpain)


# Enable this universe
options(repos = c(
  ropenspain = "https://ropenspain.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))

library(remotes)

install_github("rOpenSpain/mapSpain", dependencies = TRUE)

library(tidyverse)
#Con MapSpain información en un formato sf, una tabla en la que cada fila hay una geometría
madrid <- esp_get_munic_siane(region = "Madrid") |> 
  mutate(Provincia =esp_dict_translate(ine.prov.name, "es")
  )

SCM_FINAL_GR <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL2.csv", 
            sep = ",", header = TRUE, dec = ".")

dataviz <- 
  read.csv2("C:/Users/evaba/Documents/TFM/MAPSPAIN/MAPSPAIN/dataviz.csv")

SCM_FINAL_GR_2020 <- SCM_FINAL_GR %>%
  filter(año == 2020) %>%
  select(everything())
SCM_FINAL_GR_2019 <- SCM_FINAL_GR %>%
  filter(año == 2019) %>%
  select(everything())


map1<-ggplot(madrid) +
  geom_sf(aes(fill = Provincia),
          color ="grey70"
  ) +
  labs(title ="Municipios de Madrid") +
  scale_fill_discrete(
    type = 
      hcl.colors(4, "Blues")
  ) +
  theme_bw()
map1


#Conexión con MapSpain para los datos 
esp_set_cache_dir("C:/Users/evaba/AppData/Local/R/mapslib/mapSpain", install = TRUE, verbose =TRUE, overwrite = TRUE )

madrid_munis2 <- esp_get_munic_siane(region = "Madrid")
base_pnoa <- esp_getTiles(madrid_munis2, "PNOA", bbox_expand = 0.1, zoommin = 1)

munic <- esp_get_munic_siane(verbose = TRUE)

madrid_munis2 <- madrid_munis2 %>%
  rename(Municipio = name)
library(tidyterra)

map2 <- ggplot() +
  # la función layer_spatraster da error y se cambia por geom_spatraster_rgb
  
  geom_spatraster_rgb(data=base_pnoa) +
  geom_sf(
    data = madrid_munis2, color ="blue", fill ="orange",
    alpha =0.25, lwd = 0.5
  ) +
  theme_minimal() +
  labs(title ="Municipios en Madrid")
map2

# Definir una lista de dataframes a combinar
madrid_munis2<-na.omit(madrid_munis2)

MAPAS_2020 <- list(madrid_munis2,SCM_FINAL_GR_2020)
by_cols <- c("Municipio")

# Combinar los dataframes utilizando Reduce y merge.
MAPAS_TFM <- Reduce(function(x, y) merge(x, y, by = c("Municipio"), all = TRUE), MAPAS_2020)
MAPAS_TFM <- distinct(MAPAS_TFM, .keep_all = TRUE)
MAPAS_TFM <- na.omit(MAPAS_TFM)

MAPAS_TFM$Pob_EV_Total <- as.numeric(as.character(MAPAS_TFM$Pob_EV_Total))

# Se observan 169 municipios en lugar de 179 se buscan las diferencias y se corrige.

madrid_munis2$Municipio <- as.character(madrid_munis2$Municipio)
SCM_FINAL_GR_2020$Municipio <- as.character(SCM_FINAL_GR_2020$Municipio)
comparacion <- merge(madrid_munis2, SCM_FINAL_GR_2020, by = "Municipio", all = TRUE)

en_madrid_no_scm <- setdiff(madrid_munis2$Municipio, SCM_FINAL_GR_2020$Municipio)
en_scm_no_madrid <- setdiff(SCM_FINAL_GR_2020$Municipio, madrid_munis2$Municipio)

print(en_madrid_no_scm)
print(en_scm_no_madrid)

# Cambia los nombres en el conjunto de datos madrid_munis2
madrid_munis2$Municipio[madrid_munis2$Municipio == "Batres"] <- "Batrés"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Braojos"] <- "Braojos de la Sierra"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Fresnedillas de la Oliva"] <- "Fresnedillas"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Gargantilla del Lozoya y Pinilla de Buitrago"] <- "Gargantilla del Lozoya"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Horcajo de la Sierra-Aoslos"] <- "Horcajo de la Sierra"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Navarredonda y San Mamés"] <- "Navarredonda"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Orusco de Tajuña"] <- "Orusco"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Piñuécar-Gandullas"] <- "Piñuécar"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Valdeolmos-Alalpardo"] <- "Valdeolmos"
madrid_munis2$Municipio[madrid_munis2$Municipio == "Lozoyuela-Navas-Sieteiglesias"] <- "Lozoyuela"

# Se vuelve a hacer el merge
MAPAS_2020 <- list(madrid_munis2,SCM_FINAL_GR_2020)
by_cols <- c("Municipio")

# Combinar los dataframes utilizando Reduce y merge.
MAPAS_TFM <- Reduce(function(x, y) merge(x, y, by = c("Municipio"), all = TRUE), MAPAS_2020)
MAPAS_TFM <- distinct(MAPAS_TFM, .keep_all = TRUE)
MAPAS_TFM <- na.omit(MAPAS_TFM)


library(sf)
library(ggplot2)

# Cálculo de los centroides

MAPAS_TFM$centroid <- st_centroid(MAPAS_TFM$geometry)

# Extracción de las coordenadas x e y
MAPAS_TFM$lon <- st_coordinates(MAPAS_TFM$centroid)[, 1]
MAPAS_TFM$lat <- st_coordinates(MAPAS_TFM$centroid)[, 2]

# Creación del gráfico
ggplot() +
  geom_sf(data = MAPAS_TFM) +
  geom_text(data = MAPAS_TFM, aes(x = lon, y = lat, label = Municipio)) +
  theme_minimal()+
  theme(text = element_text(size = 7))+ 
  geom_sf_label(aes(label = Municipio),
                fill = "white", alpha = 0.5,size = 3,
                label.size = 0  ) 


# MAPAS con variables Continuas

mapEV <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM, aes(fill = Pob_EV_Total),
          color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  labs(title = "Esperanza del Vida al Nacer.2020. Zonas y Municipios en la Comunidad de Madrid")

mapEV

mapPOB <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM[MAPAS_TFM$Municipio != "Madrid", ],
    aes(fill = Pob_Empadronada),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "yellow", high = "black") +
  theme_minimal() +
  labs(title = "Población empadronada 2020 en la Comunidad de Madrid")

mapPOB

mapEco <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM[MAPAS_TFM$Municipio != "Madrid", ], 
    aes(fill = Eco_Afiliados_Total),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Total afiliados 2020 a la Seguridad Social")

mapEco


mapPRE <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM[MAPAS_TFM$Municipio != "Madrid", ], 
    aes(fill = Eco_Pre_Gastos_Liquidados),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Gastos liquidados 2020")

mapPRE

mapCAP <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM[MAPAS_TFM$Municipio != "Madrid", ], 
    aes(fill = Eco_Cap_Nec_Financ),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "pink", high = "lightblue") +
  theme_minimal() +
  labs(title = "Capacidad o necesidad de financiación municipal")

mapCAP

mapENV <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM, 
    aes(fill = Pob_Envejecimiento),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "yellow", high = "black") +
  theme_minimal() +
  labs(title = "Grado de envejecimiento")

mapENV

mapParo <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM, 
    aes(fill = Eco_Paro_100),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "yellow", high = "black") +
  theme_minimal() +
  labs(title = "Paro por cada 100 habitantes")

mapParo

mapJuv <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM, 
    aes(fill = Pob_Juventud),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "yellow", high = "black") +
  theme_minimal() +
  labs(title = "Grado de Juventud")

mapJuv

# Binaria

mapMA <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM, 
    aes(fill = MA_Sup_Protegida),
    color = "blue", alpha = 0.4, lwd = 0.2
  ) +
  scale_fill_manual(values = c("darkgreen", "lightyellow")) +
  theme_minimal() +
  labs(title = "Superficie Protegida mayor o menor que la media")

mapMA

mapHosp <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM, 
    aes(fill = Soc_Mov_Hosp_T),
    color = "blue", alpha = 0.4, lwd = 0.2
  ) +
  scale_fill_manual(values = c("darkred", "lightyellow")) +
  theme_minimal() +
  labs(title = "Tiempo al hospital. Mayor o menor a la media")

mapHosp

# Discreta

mapDEGURBA <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM,
    aes(fill = DEGURBA),
    color = "black", alpha = 0.8, lwd = 0.2
  ) +
  scale_fill_gradient(low = "lightyellow", high = "darkorange") +
  theme_minimal() +
  labs(title = "DEGURBA. Urbano, intermedio y rural")

mapDEGURBA

mapGRUPO <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM,
    aes(fill = factor(Pob_Grupo)),
    color = "blue", alpha = 0.8, lwd = 0.2
  ) +
  scale_fill_manual(values = c("purple","lightblue", "green", "red", "orange","white" ,"brown", "cyan", "pink","lightyellow","yellow")) +
  theme_minimal() +
  labs(title = "Municipios por grupos de población")
mapGRUPO


SCM_FINAL_CLUSTER

# CLUSTER

mapCLUSTER <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = SCM_FINAL_CLUSTER,
    aes(fill = factor(cluster)),
    color = "blue", alpha = 0.8, lwd = 0.2
  ) +
  scale_fill_manual(values = c("lightblue", "darkblue", "green", "red", "orange", "purple", "yellow", "cyan", "pink", "brown")) +
  theme_minimal() +
  labs(title = "Municipios en Madrid")

mapCLUSTER


# Probando paletas de colores continuos

# library(paletteer)
# 
# 
# mapGRUPO2 <- ggplot() +
#   geom_spatraster_rgb(data = base_pnoa) +
#   geom_sf(
#     data = MAPAS_TFM[MAPAS_TFM$Municipio != "Madrid", ],
#     aes(fill = factor(Pob_Grupo)),
#     color = "blue", alpha = 0.8, lwd = 0.2
#   ) +
#   scale_fill_manual(values = paletteer::paletteer_d("Blue-Green Sequential", 30)) +
#   theme_economist() +
#   labs(title = "Municipios en Madrid") +
#   guides(fill = guide_legend(override.aes = list(color = NA))) +
#   theme(panel.background = element_rect(fill = "white"))
# 
# mapGRUPO2





# Intentando poner etiquetas en el mapa

# centroides <- st_centroid(MAPAS_TFM)
# 
# mapSal <- ggplot() +
#   geom_spatraster_rgb(data = base_pnoa) +
#   geom_sf(
#     data = MAPAS_TFM[MAPAS_TFM$Municipio != "Madrid", ], aes(fill =Pob_EV_Total),
#     color = "blue", alpha = 0.8, lwd = 0.2) +
#   scale_fill_gradient(low = "lightblue", high = "darkblue") +
#   geom_text(
#     data = MAPAS_TFM[MAPAS_TFM$Municipio != "Madrid", ], aes(label = Municipio),
#     color = "black", size = 3,
#     x = centroides$geometry$x,
#     y = centroides$geometry$y,
#     nudge_y = 0.05) +
#   theme_minimal() +
#   labs(title = "Municipios en Madrid")
# 
# mapSal


# 2019

# Definir una lista de dataframes a combinar
madrid_munis2<-na.omit(madrid_munis2)
MAPAS_TFM <- distinct(MAPAS_TFM, .keep_all = TRUE)
MAPAS_TFM <- na.omit(MAPAS_TFM)

MAPAS_2019 <- list(madrid_munis2,SCM_FINAL_GR_2019)
by_cols <- c("Municipio")

# Combinar los dataframes utilizando Reduce y merge.
MAPAS_TFM_2019 <- Reduce(function(x, y) merge(x, y, by = c("Municipio"), all = TRUE), MAPAS_2019)

media <- mean(MAPAS_TFM_2019$Pob_EV_Total, na.rm = TRUE)
MAPAS_TFM_2019$Pob_EV_Total[is.na(MAPAS_TFM_2019$Pob_EV_Total)] <- media

MAPAS_TFM_2019$Pob_EV_Total <- as.numeric(as.character(MAPAS_TFM_2019$Pob_EV_Total))

map3 <- ggplot() +
  geom_spatraster_rgb(data = base_pnoa) +
  geom_sf(
    data = MAPAS_TFM_2019, aes(fill = Pob_EV_Total),
    color = "blue", alpha = 0.8, lwd = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Esperanza del Vida al Nacer.2019. Zonas y Municipios en la Comunidad de Madrid")

map3


CM_MAP<-
  ggplot(dataviz) +
  geom_sf(aes(fill = currentUse), color = NA) +
  scale_fill_manual(values = hcl.colors(15, "Spectral")) +
  theme_void() +
  labs(title = "COMUNIDAD DE MADRID", fill = "") +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.justification = .5,
    legend.text = element_text(
      colour = "white",
      size = 12
    ),
    plot.title = element_text(
      colour = "white", hjust = .5,
      margin = margin(t = 20),
      size = 20
    ),
    plot.caption = element_text(
      colour = "white",
      margin = margin(b = 10), hjust = .5
    ),
    plot.margin = margin(r = 20, l = 20)
  )
CM_MAP
ggsave("CM_MAP_USE.png")



write.csv(dataviz, "dataviz.csv", row.names = FALSE) # sin estandarizar
save(dataviz, file = "dataviz.rda")
# write.xlsx(dataviz, "dataviz.xlsx")

# Catastro. Usos del suelo

install.packages("CatastRo")
library(CatastRo)
# Use mapSpain for getting the coords

mad<- esp_get_prov("Madrid")

#mad <- esp_get_capimun(munic = "^Madrid$")

mad_catr_code <- catr_get_code_from_coords(mad)

mad_catr_code
#> # A tibble: 1 × 12
#>   munic    catr_to catr_mu…¹ catrc…² cpro  cmun  inecode nm    cd    cmc   cp   
#>   <chr>    <chr>   <chr>     <chr>   <chr> <chr> <chr>   <chr> <chr> <chr> <chr>
#> 1 VALENCIA 46      900       46900   46    250   46250   VALE… 46    900   46   
#> # … with 1 more variable: cm <chr>, and abbreviated variable names ¹​catr_munic,
#> #   ²​catrcode

madrid_bu <- catr_atom_get_buildings(mad_catr_code$catrcode)

install.packages("sf")
library(sf)
buff <- mad %>%
  # Adjust CRS to 25830: (Buildings)
  st_transform(st_crs(madrid_bu)) %>%
  # Buffer
  st_buffer(2500)


# Cut buildings

dataviz <- st_intersection(madrid_bu, buff)

ggplot(dataviz) +
  geom_sf()

# Extract 4 initial positions
year <- substr(dataviz$beginning, 1, 4)

# Replace all that doesn't look as a number with 0000
year[!(year %in% 0:2500)] <- "0000"

# To numeric
year <- as.integer(year)

# New column
dataviz <- dataviz %>%
  mutate(year = year)

dataviz <- dataviz %>%
  mutate(year_cat = ggplot2::cut_number(year,
                                        n = 7
  ))

CM_MAP<-
  ggplot(dataviz) +
  geom_sf(aes(fill = year_cat), color = NA) +
  scale_fill_manual(values = hcl.colors(15, "Spectral")) +
  theme_void() +
  labs(title = "COMUNIDAD DE MADRID", fill = "") +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.justification = .5,
    legend.text = element_text(
      colour = "white",
      size = 12
    ),
    plot.title = element_text(
      colour = "white", hjust = .5,
      margin = margin(t = 20),
      size = 20
    ),
    plot.caption = element_text(
      colour = "white",
      margin = margin(b = 10), hjust = .5
    ),
    plot.margin = margin(r = 20, l = 20)
  )
library(ggplot2)
ggsave("CM_MAP.png")
