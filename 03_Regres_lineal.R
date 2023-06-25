# Regresión lineal

# SCM_FINAL2 y SCM_FINAL Tienen Madrid Capital
# SCM_FINAL3 no tiene Madrid Capital
# SCM_FINAL_outliers no tiene Madrid Capital ni outliers ni ausentes

# En primer lugar regresión lineal con toda la Comunidad de Madrid

SCM_FINAL_RL <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL2.csv", 
            sep = ",", header = TRUE, dec = ".")


library(tidymodels)
# Partición 80-20% de train y test (solo instrucciones)
SCM_FINAL_RL_split <- initial_split(SCM_FINAL_RL, strata = Pob_EV_Total, prop = 0.8)

SCM_FINAL_RL_train <- training(SCM_FINAL_RL_split)
SCM_FINAL_RL_test <- testing(SCM_FINAL_RL_split)

#Comprobación
SCM_FINAL_RL_train %>% count(Pob_EV_Total) %>% mutate(porc = n / sum(n))
SCM_FINAL_RL_test %>% count(Pob_EV_Total) %>% mutate(porc = n / sum(n))

# Validación
SCM_FINAL_RL_cv_folds <- vfold_cv(data = SCM_FINAL_RL_train, v = 4, repeats = 8, strata = Pob_EV_Total)


# Receta
SCM_FINAL_RL_rec <-
  recipe(data = SCM_FINAL_RL_train, Pob_EV_Total ~ .) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.3)%>% 
  # Filtro de cero varianza
  step_zv(all_predictors())

bake(SCM_FINAL_RL_rec  |>  prep(), new_data = NULL) 

# Modelo
Lin_model <- linear_reg() |> set_mode('regression') |> set_engine('lm')

# Flujo
SCM_FINAL_RL_wflow <-
  workflow() |> 
  add_recipe(SCM_FINAL_RL_rec) %>%
  add_model(Lin_model)

# Ajuste
reg_fit_SCM_FINAL_RL <-
  SCM_FINAL_RL_wflow |> fit(data = SCM_FINAL_RL_train)
show <- tidy(reg_fit_SCM_FINAL_RL)
tidy(reg_fit_SCM_FINAL_RL)

# Ajuste
confint(reg_fit_SCM_FINAL_RL |> extract_fit_engine(),level = 0.95)

# Diagnosis
library(performance)
check_model(reg_fit_SCM_FINAL_RL |>  extract_fit_engine())

glance(reg_fit_SCM_FINAL_RL)

summary_table <- broom::glance(reg_fit_SCM_FINAL_RL)

# Ver la tabla
print(summary_table)

# A tibble: 1 × 12
# r.squared adj.r.squared sigma statistic p.value    df logLik    AIC    BIC deviance df.residual  nobs
# <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
#   1     0.942         0.932 0.100      92.7       0   214  1375. -2318. -1180.     12.3        1216  1431

#Predicciones EN TEST

final_fit_lineal <- SCM_FINAL_RL_wflow |> fit(data = SCM_FINAL_RL_train)

# Predecir el conjunto test: devuelve la clase
predictions_categories_lineal <- predict(final_fit_lineal, new_data = SCM_FINAL_RL_test)

# Incluir predicciones en tabla
prob_test_lineal <- augment(final_fit_lineal, SCM_FINAL_RL_test)

# Calculo las métricas del modelo

library(Metrics)
mae(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)

# [1] 0.03606326

rmse(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)

# [1] 0.1080443

library(ggplot2)
ggplot() +
  geom_point(aes(x = SCM_FINAL_RL_test$Pob_EV_Total, y = predictions_categories_lineal$.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  xlab("Real values") +
  ylab("Predicted values")

# Revisar las variables que emplea para la predicción


## REGRESIÓN LINEAL CON GRÁFICOS

nombres_vars_independientes <- c("Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
                                 "Eco_Contratos_Temp", "Eco_Energ_Elect_PC", 
                                 "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", 
                                 "Eco_Paro_100", "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", 
                                 "Eco_Pre_Gastos_Liquidados", "Eco_Pre_Ingresos_Liquidados",  
                                 "Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", 
                                 "Pob_Juventud","Soc_Salud_Def_Otras",
                                 "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor", 
                                 "Superficie"
                                 
)

modelo <- lm(Pob_EV_Total ~ . - 1, data = SCM_FINAL_RL_train[, c("Pob_EV_Total", nombres_vars_independientes)])

summary (modelo)

# Crear un data frame con los resultados de la regresión
results <- data.frame(variables = names(coef(modelo)),
                      coeficientes = coef(modelo))
 
library(reactable)

reactable(results [1:10, ], filterable = TRUE, minRows = 21)

# Crear el gráfico de barras
ggplot(results, aes(x = variables, y = coeficientes)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  ggtitle("Coeficientes de la regresión lineal") +
  xlab("Variables") +
  ylab("Coeficientes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convertir variables independientes en una sola columna
datos <- SCM_FINAL_RL[, c("Pob_EV_Total", nombres_vars_independientes)]
datos_long <- gather(datos, variable, value, -Pob_EV_Total)

# Crear gráfico con líneas de regresión para cada variable independiente
ggplot(datos_long, aes(x = value, y = Pob_EV_Total)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~variable, scales = "free_x") +
  xlab("Valor de la variable independiente") +
  ylab("Esperanza de Vida") +
  ggtitle("Líneas de regresión de las variables independientes")+
  theme(axis.text.x = element_text(size = 8))  # Ajusta el tamaño de las etiquetas del eje x a 8


# Regresión lineal con gráficos sin outliers

SCM_FINAL_RL_outl <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_outliers.csv", 
            sep = ",", header = TRUE, dec = ".")


nombres_vars_independientes2 <- c("Eco_Afiliados_Tem_Porc_Resid", "Eco_Afiliados_Total", 
                                 "Eco_Contratos_Temp", "Eco_Energ_Elect_PC", 
                                 "Eco_Ind_RDBM_PC", "Eco_Indice_Gini", 
                                 "Eco_Paro_100", "Eco_PBI_municipal_pc", "Eco_Pre_Deuda_Viva", 
                                 "Eco_Pre_Gastos_Liquidados", "Eco_Pre_Ingresos_Liquidados",  
                                 "Pob_Edad_Media", "Pob_Empadronada", "Pob_Envejecimiento", 
                                 "Pob_Juventud","Soc_Salud_Def_Otras",
                                 "Soc_Salud_Def_SistCirc", "Soc_Salud_Def_SistResp", "Soc_Salud_Def_Tumor", 
                                 "Superficie"
                                 
)

modelo2 <- lm(Pob_EV_Total ~ . - 1, data = SCM_FINAL_RL_outl[, c("Pob_EV_Total", nombres_vars_independientes2)])

summary (modelo2)

# Crear un data frame con los resultados de la regresión
results2 <- data.frame(variables = names(coef(modelo2)),
                      coeficientes = coef(modelo2))
reactable(results2 [1:10, ], filterable = TRUE, minRows = 21)

# Crear el gráfico de barras
ggplot(results2, aes(x = variables, y = coeficientes)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  ggtitle("Coeficientes de la regresión lineal") +
  xlab("Variables") +
  ylab("Coeficientes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convertir variables independientes en una sola columna
datos2 <- SCM_FINAL_RL_outl[, c("Pob_EV_Total", nombres_vars_independientes2)]
datos_long2 <- gather(datos2, variable, value, -Pob_EV_Total)

# Crear gráfico con líneas de regresión para cada variable independiente
ggplot(datos_long2, aes(x = value, y = Pob_EV_Total)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~variable, scales = "free_x") +
  xlab("Valor de la variable independiente") +
  ylab("Esperanza de Vida") +
  ggtitle("Líneas de regresión de las variables independientes")+
  theme(axis.text.x = element_text(size = 8))  # Ajusta el tamaño de las etiquetas del eje x a 8

# RECETA CON DATASET SIN OUTLIERS, DUMMIES EN CATEGÓRICAS SIN CIF Y EVALUACIÓN DEL MODELO

listclass <-SCM_FINAL_RL_outl |>  c("MA_Pelig_Incend_AltoExt","MA_Sup_Forestal_Total","MA_Sup_Protegida",
                                    "MA_Sup_quemada","Pob_Grupo",
               "Soc_Mov_Auto_T","Soc_Mov_Hosp_T","Soc_Mov_Muni_5000_T", "Soc_Salud_Discapacidad", "EDU",
               "MOV", "SAL", "OCI")

library(dummies)
SCM_FINAL_RL_outDUM <- SCM_FINAL_RL_outl<- dummy.data.frame(SCM_FINAL_RL_outl, listclass, sep = ".")
names(SCM_FINAL_RL_outDUM)

SCM_FINAL_RL_outDUM <-SCM_FINAL_RL_outDUM |> select(-c(CIF, Entidad, Municipio, Terr_Zonas_Estad_CM))

# Partición 80-20% de train y test (solo instrucciones)
SCM_FINAL_RL_outl_split <- initial_split(SCM_FINAL_RL_outDUM, strata = Pob_EV_Total, prop = 0.8)

SCM_FINAL_RL_outl_train <- training(SCM_FINAL_RL_outl_split)
SCM_FINAL_RL_outl_test <- testing(SCM_FINAL_RL_outl_split)

#Comprobación
SCM_FINAL_RL_outl_train %>% count(Pob_EV_Total) %>% mutate(porc = n / sum(n))

# Validación
SCM_FINAL_RL_outl_cv_folds <- vfold_cv(data = SCM_FINAL_RL_outl_train, v = 4, repeats = 8, strata = Pob_EV_Total)

# Receta
SCM_FINAL_RL_outl_rec <-
  recipe(data = SCM_FINAL_RL_outl_train, Pob_EV_Total ~ .) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5)%>% 
  # Filtro de cero varianza
  step_zv(all_predictors())

bake(SCM_FINAL_RL_outl_rec  |>  prep(), new_data = NULL) 

# Modelo
Lin_model <- linear_reg() |> set_mode('regression') |> set_engine('lm')

# Flujo
SCM_FINAL_RL_outl_wflow <-
  workflow() |> 
  add_recipe(SCM_FINAL_RL_outl_rec) %>%
  add_model(Lin_model)

# Ajuste
reg_fit_SCM_FINAL_RL_outl <-
  SCM_FINAL_RL_outl_wflow |> fit(data = SCM_FINAL_RL_outl_train)
show <- tidy(reg_fit_SCM_FINAL_RL_outl)
tidy(reg_fit_SCM_FINAL_RL_outl)

# Ajuste
confint(reg_fit_SCM_FINAL_RL_outl |> extract_fit_engine(),level = 0.95)

# Diagnosis
library(performance)
check_model(reg_fit_SCM_FINAL_RL_outl |>  extract_fit_engine())

# Si VIFs may not be sensible: Problema de multicolinealidad: .
# técnicas como la regularización (por ejemplo, Ridge o Lasso) 
# pueden ayudar a manejar la multicolinealidad. 
# Estas técnicas añaden un término de penalización al modelo de regresión 
# que puede ayudar a reducir los coeficientes de las variables predictoras correlacionadas..

glance(reg_fit_SCM_FINAL_RL_outl)
summary_table <- broom::glance(reg_fit_SCM_FINAL_RL_outl)
print(summary_table)

# # A tibble: 1 × 12
# r.squared adj.r.squared sigma statistic  p.value    df logLik    AIC   BIC deviance df.residual  nobs
# <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>       <int> <int>
#   1     0.105        0.0965 0.169      11.9 2.16e-26    14   521. -1010. -926.     40.1        1409  1424

#Predicciones EN TEST

final_fit_lineal <- SCM_FINAL_RL_outl_wflow |> fit(data = SCM_FINAL_RL_outl_train)

# Predecir el conjunto test: devuelve la clase
predictions_categories_lineal <- predict(final_fit_lineal, new_data = SCM_FINAL_RL_outl_test)

# Incluir predicciones en tabla
prob_test_lineal <- augment(final_fit_lineal, SCM_FINAL_RL_outl_test)

# Calculo las métricas del modelo

mae(predictions_categories_lineal$.pred, SCM_FINAL_RL_outl_test$Pob_EV_Total)

# [1] 0.03480331 Antes y Ahora [1] 0.08473891

rmse(predictions_categories_lineal$.pred, SCM_FINAL_RL_outl_test$Pob_EV_Total)

# [1] 0.103023 Antes y Ahora [1] 0.1681456

library(ggplot2)
ggplot() +
  geom_point(aes(x = SCM_FINAL_RL_outl_test$Pob_EV_Total, y = predictions_categories_lineal$.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  xlab("Real values") +
  ylab("Predicted values")

