
library(dplyr)
library(kableExtra)
library(corrplot)
library(dummies)

SCM_FINAL_COR <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL3.csv", 
            sep = ",", header = TRUE, dec = ".")


SCM_FINAL_COR_filtered <- SCM_FINAL_COR |> select(-c(Pob_EV_65_Total))
listclass <- c("MA_Pelig_Incend_AltoExt","MA_Sup_Forestal_Total","MA_Sup_Protegida", "MA_Sup_quemada",
               "Soc_Mov_Auto_T","Soc_Mov_Hosp_T","Soc_Mov_Muni_5000_T","Soc_Salud_Discapacidad", "EDU", "MOV",
               "SAL", "OCI")

SCM_FINAL_COR_filtered<- dummy.data.frame(SCM_FINAL_COR_filtered, listclass, sep = ".")

#Matriz de Correlaciones----
corr_simple <- 
  function(tbl = tibble(),tbl_obj = NULL,sig = 0.5){

  tbl <- tbl |> select(where(is.numeric)) #solo numéricas
  
  name_c <- deparse(substitute(tbl_obj))
  
  corr <- cor(tbl) #haz la correlacion
  corr[lower.tri(corr,diag=TRUE)] <- NA # elimina la diag
  corr[corr == 1] <- NA  #elimina correlaciones perfectas
  corr <- as.data.frame(as.table(corr)) #tabla de 3 columnas
  corr <- na.omit(corr) #Quita NA generados 
  corr <- subset(corr, abs(Freq) > sig) #selecciona con significacion
  # se filtran las filas del objeto corr en las que el valor absoluto de la columna 
  # Freq es mayor que el valor de la variable sig. 
  # El resultado final será un objeto corr con solo aquellas correlaciones
  # cuyo valor absoluto es mayor que sig
  
   if(tbl |> has_name(name_c)){#existe variable objetivo?

              corr <- corr |>  filter(Var2 == name_c)
              corr <- corr[order(abs(corr$Freq)),]  #Ordenala mayor correlacion
              if(nrow(corr) < 2){
                
                tbl_out <- corr |> rename(Corr = Freq) |> select(-Var2)|>  
                        kable(caption = paste('Correlacion frente a',name_c)) |>
                        kable_styling (full_width = F, html_font = "Cambria") |> 
                        kable_classic(c("striped")) 
                
                return(tbl_out)
              }else{
               #Transforma corr a matrix para realizar el plot 
                mtx_corr <- reshape2::acast(corr, Var2~Var1, value.var="Freq")
                corrplot(mtx_corr,method = 'number', is.corr=FALSE, tl.col="black", 
                      na.label=" ",cl.ratio=0.2,cl.length=2)
              }
              
   }else{
       corr <- corr[order(abs(corr$Freq)),]  #Ordenala mayor correlacion
       mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
       corrplot(mtx_corr,method = 'number', is.corr=FALSE, tl.col="black", 
                      na.label=" ",cl.ratio=0.2,cl.length=2)
    }          

  }

corr_simple(SCM_FINAL_COR_filtered,Pob_EV_Total,sig = 0.1)

# 0.3 Eco_Indice_Gini y Eco_Ind_RDBM_PC. El índice de Gini se construye con la Renta...
# 0.2 Eco Afiliados_Tem_Porc_Resid - Eco_Paro_100
# 0.1 Eco_Afiliados_Tem_Porc_Resid - Eco_Afiliados_Total - Eco_Establec_Hotel
# Eco_Ind_RDBM_PC - Eco_Indice_Gini - Eco_Paro_100 - Eco_Pre_Deuda_Viva - MA_Sup_quemada y Pob_Empadronada

# ESTADÍSTICOS - CONTRASTE DE INDEPENDENCIA

chisq_results <- tibble(
  variable = SCM_FINAL %>% select(where(is.character)) %>% names(),
  p_value = SCM_FINAL %>% select(where(is.character)) %>%
    map_dbl(.f = function(x) { chisq.test(x, SCM_FINAL$Pob_EV_Total)$p.value })
)

show <- chisq_results |>  arrange(desc(p_value)) |>  filter(p_value > 0.05)

#Tabla genérica----
tabla_g <-
  function(tblg_in = tibble(),titulo ='',scroll = FALSE){
    
    
    if(scroll == TRUE){
      
      tbl_out <- tblg_in |> kable(caption = titulo) |>
        kable_styling (full_width = F, html_font = "Cambria") |> 
        scroll_box(width = "100%", height = "400px") |> 
        kable_classic(c("striped")) 
    }else{
      tbl_out <- tblg_in |> kable(caption = titulo) |>
        kable_styling (full_width = F, html_font = "Cambria") |> 
        kable_classic(c("striped")) 
    }  
    return(tbl_out)
  }

tabla_g(show,titulo = 'Contraste Chi-cuadrado')

# Ejemplo de tabla de contingencia

SCM_FINAL_CON2 <- with(SCM_FINAL_COR, table(MA_Pelig_Incend_AltoExt,
                                           MA_Sup_Forestal_Total,
                                           MA_Sup_Protegida, MA_Sup_quemada))
SCM_FINAL_CON2

SCM_FINAL_CON3 <- with(SCM_FINAL_COR, table(Eco_Cap_Nec_Financ, 
                                           Soc_Mov_Auto_T,
                                           Soc_Mov_Hosp_T, Soc_Mov_Muni_5000_T, Soc_Salud_Discapacidad))
SCM_FINAL_CON3

SCM_FINAL_CON4 <- with(SCM_FINAL_COR, table(EDU, MOV, SAL, OCI))
SCM_FINAL_CON4



# ANOVA

modelo_anova <- aov(SCM_FINAL_COR$Pob_EV_Total ~ EDU, SCM_FINAL_COR)

# Obtiene los resultados del ANOVA
resultados_anova <- summary(modelo_anova)

# Imprime los resultados
print(resultados_anova)

# Según los resultados, no se encontró una relación significativa entre estas variables, 
# ya que el valor p (Pr(>F)) es 0.972, lo cual indica que no hay suficiente evidencia para rechazar 
# la hipótesis nula de que no hay diferencia significativa en los grupos definidos por la variable "EDU_cuali". 

modelo_anova <- aov(SCM_FINAL_COR$Pob_EV_Total ~ Entidad, SCM_FINAL_COR)
resultados_anova <- summary(modelo_anova)
print(resultados_anova)

# la variable Entidad tiene un efecto significativo sobre Pob_EV_Total, 
# lo que sugiere que hay diferencias significativas en Pob_EV_Total 
# entre las distintas entidades en el conjunto de datos analizado.

# ANOVA de regresión o ANOVA de 2 factores
modelo_anova <- aov(SCM_FINAL_COR$Pob_EV_Total ~ as.factor(Eco_Afiliados_Total), SCM_FINAL_COR)
resultados_anova <- summary(modelo_anova)
print(resultados_anova)

# Obtener los residuos del modelo ANOVA
residuos <- residuals(modelo_anova)

# Gráfico de cuantiles normales
qqnorm(residuos)
qqline(residuos)

# Histograma de los residuos
hist(residuos, breaks = "FD", freq = FALSE)
curve(dnorm(x, mean = mean(residuos), sd = sd(residuos)), add = TRUE)

# Obtener los valores ajustados del modelo ANOVA
valores_ajustados <- fitted(modelo_anova)

# Gráfico de dispersión de los residuos en función de los valores ajustados
plot(valores_ajustados, residuos, pch = 16, xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, col = "red")

# Gráfico de dispersión de los residuos en función de la variable predictora
plot(SCM_FINAL_COR$Eco_Afiliados_Total, residuos, pch = 16, xlab = "Variable predictora", ylab = "Residuos")

