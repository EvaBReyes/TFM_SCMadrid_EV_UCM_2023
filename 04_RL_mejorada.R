# Regresión lineal

# SCM_FINAL2 y SCM_FINAL Tienen MadridCapital
# SCM_FINAL3 no tiene Madrid Capital
# SCM_FINAL_outliers no tiene Madrid Capital ni outliers ni ausentes

SCM_FINAL_RLF <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_outliers.csv", 
            sep = ",", header = TRUE, dec = ".")

library(tidymodels)
# Partición 80-20% de train y test (solo instrucciones)
SCM_FINAL_RL_split <- initial_split(SCM_FINAL_RLF, strata = Pob_EV_Total, prop = 0.8)

SCM_FINAL_RL_train <- training(SCM_FINAL_RL_split)
SCM_FINAL_RL_test <- testing(SCM_FINAL_RL_split)

#Comprobación
SCM_FINAL_RL_train %>% count(Pob_EV_Total) %>% mutate(porc = n / sum(n))

# Validación
SCM_FINAL_RL_cv_folds <- vfold_cv(data = SCM_FINAL_RL_train, v = 4, repeats = 8, strata = Pob_EV_Total)

# Receta
SCM_FINAL_RL_rec <-
  recipe(data = SCM_FINAL_RL_train, Pob_EV_Total ~ .) |> 
  # Elimino variables
  step_rm(año, CIF, Entidad, Municipio) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5) |>  
  # Convertir variables categóricas en dummies
  step_dummy(all_nominal()) |> 
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

# A tibble: 1 × 12
# r.squared adj.r.squared sigma statistic p.value    df logLik    AIC    BIC deviance df.residual  nobs
# <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
#   1     0.668         0.654 0.103      48.2       0    57  1246. -2374. -2064.     14.5        1366  1424
#Predicciones EN TEST

library(knitr)
library(reactable)
library(htmlwidgets)
model_summary <- glance(reg_fit_SCM_FINAL_RL)
knitr::kable(model_summary)
reactable_table <- reactable(model_summary)
# Exporta la tabla a HTML
saveWidget(reactable_table, file = "my_table.html", selfcontained = FALSE)


final_fit_lineal <- SCM_FINAL_RL_wflow |> fit(data = SCM_FINAL_RL_train)

# Predecir el conjunto test: devuelve la clase
predictions_categories_lineal <- predict(final_fit_lineal, new_data = SCM_FINAL_RL_test)

# Incluir predicciones en tabla
prob_test_lineal <- augment(final_fit_lineal, SCM_FINAL_RL_test)

# Calculo las métricas del modelo

library(Metrics)
mae(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)

# [1] 0.03480331 Antes y Ahora [1] 0.05582478

rmse(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)

# [1] 0.103023 Antes y Ahora [1] 0.1275441

library(ggplot2)
ggplot() +
  geom_point(aes(x = SCM_FINAL_RL_test$Pob_EV_Total, y = predictions_categories_lineal$.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  xlab("Real values") +
  ylab("Predicted values")

# Regresión lineal
# 
# SCM_FINAL3 no tiene Madrid Capital y sí outliers


SCM_FINAL_RLF3 <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL3.csv", 
            sep = ",", header = TRUE, dec = ".")


library(tidymodels)
# Partición 80-20% de train y test (solo instrucciones)
SCM_FINAL_RL_split <- initial_split(SCM_FINAL_RLF3, strata = Pob_EV_Total, prop = 0.8)

SCM_FINAL_RL_train <- training(SCM_FINAL_RL_split)
SCM_FINAL_RL_test <- testing(SCM_FINAL_RL_split)

#Comprobación
SCM_FINAL_RL_train %>% count(Pob_EV_Total) %>% mutate(porc = n / sum(n))

# Validación
SCM_FINAL_RL_cv_folds <- vfold_cv(data = SCM_FINAL_RL_train, v = 4, repeats = 8, strata = Pob_EV_Total)

# Receta
SCM_FINAL_RL_rec <-
  recipe(data = SCM_FINAL_RL_train, Pob_EV_Total ~ .) |> 
  # Elimino variables
  step_rm(año, CIF, Entidad, Municipio) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5) |>  
  # Convertir variables categóricas en dummies
  step_dummy(all_nominal()) |> 
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

# A tibble: 1 × 12
# r.squared adj.r.squared sigma statistic p.value    df logLik    AIC    BIC deviance df.residual  nobs
# <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
#   1     0.890         0.886 0.132      194.       0    57   895. -1671. -1361.     23.7        1365  1423
# > 
  
#Predicciones EN TEST

final_fit_lineal <- SCM_FINAL_RL_wflow |> fit(data = SCM_FINAL_RL_train)

# Predecir el conjunto test: devuelve la clase
predictions_categories_lineal <- predict(final_fit_lineal, new_data = SCM_FINAL_RL_test)

# Incluir predicciones en tabla
prob_test_lineal <- augment(final_fit_lineal, SCM_FINAL_RL_test)

# Calculo las métricas del modelo

library(Metrics)
mae(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)

# [1] 0.03480331 Antes y Ahora [1] 0.05582478 y [1] 0.06142108

rmse(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)

# [1] 0.103023 Antes y Ahora [1] 0.1275441 y [1] 0.1263559

library(ggplot2)
ggplot() +
  geom_point(aes(x = SCM_FINAL_RL_test$Pob_EV_Total, y = predictions_categories_lineal$.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  xlab("Real values") +
  ylab("Predicted values")

tidy_result <- tidy(reg_fit_SCM_FINAL_RL)
significant_vars <- tidy_result[tidy_result$p.value < 0.05, ]
print(significant_vars, n = Inf)

install.packages("writexl")
library(writexl)
resultados <- as_tibble(significant_vars)
write_xlsx(resultados, "resultados.xlsx")


######### REGRESIÓN LINEAL AJUSTADA ##############

# Receta
SCM_FINAL_RL_rec <-
  recipe(data = SCM_FINAL_RL_train, Pob_EV_Total ~ .) |> 
  # Elimino variables
  step_rm(año, CIF, Entidad, Municipio, Pob_EV_65_Total, Pob_Grupo, Terr_Zonas_Estad_CM) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5) |>  
  # Convertir variables categóricas en dummies
  step_dummy(all_nominal()) |> 
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

# A tibble: 1 × 12
# r.squared adj.r.squared sigma statistic p.value    df logLik    AIC    BIC deviance df.residual  nobs
# <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
#   1     0.890         0.886 0.132      194.       0    57   895. -1671. -1361.     23.7        1365  1423
# > 

#Predicciones EN TEST

final_fit_lineal <- SCM_FINAL_RL_wflow |> fit(data = SCM_FINAL_RL_train)

# Predecir el conjunto test: devuelve la clase
predictions_categories_lineal <- predict(final_fit_lineal, new_data = SCM_FINAL_RL_test)

# Incluir predicciones en tabla
prob_test_lineal <- augment(final_fit_lineal, SCM_FINAL_RL_test)

# Calculo las métricas del modelo

library(Metrics)
mae(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)

rmse(predictions_categories_lineal$.pred, SCM_FINAL_RL_test$Pob_EV_Total)


library(ggplot2)
ggplot() +
  geom_point(aes(x = SCM_FINAL_RL_test$Pob_EV_Total, y = predictions_categories_lineal$.pred)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  xlab("Real values") +
  ylab("Predicted values")

library(tidyverse)
tidy_result <- tidy(reg_fit_SCM_FINAL_RL)
significant_vars <- tidy_result[tidy_result$p.value < 0.05, ]
print(significant_vars, n = Inf)

library(writexl)
resultados2 <- as_tibble(significant_vars)
write_xlsx(resultados2, "resultados2.xlsx")
