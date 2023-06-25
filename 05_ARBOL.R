## ÁRBOLES Y RANDOM FOREST

# SCM_FINAL2 y SCM_FINAL Tienen MadridCapital
# SCM_FINAL3 no tiene Madrid Capital
# SCM_FINAL_outliers no tiene Madrid Capital ni outliers ni ausentes

library(tidymodels)
library(parallel)
library(doParallel)
GS_T0 <- Sys.time()
cluster <-makeCluster(detectCores()-1)
registerDoParallel(cluster)


set.seed(123)  # Establecer semilla para reproducibilidad

SCM_FINAL_RF <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL2.csv", 
            sep = ",", header = TRUE, dec = ".")

SCM_FINAL_RF_split <- initial_split(SCM_FINAL_RF, strata = Pob_EV_Total, prop = 0.8)

SCM_FINAL_RF_train <- training(SCM_FINAL_RF_split)
SCM_FINAL_RF_test <- testing(SCM_FINAL_RF_split)

# Conjunto de validación
val_set <- validation_split(SCM_FINAL_RF_train, strata = Pob_EV_Total,
                            prop = 0.70)

# RECETA
SCM_FINAL_RF_rec <-
  recipe(Pob_EV_Total ~ ., data = SCM_FINAL_RF_train) |> 
  # Elimino variables
  step_rm(año, CIF, Entidad, Municipio, Pob_EV_65_Total, Pob_Grupo, Terr_Zonas_Estad_CM) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5) |>  
  # En árboles no hace falta cambiar las categóricas a dummies
  # Filtro de cero varianza
  step_zv(all_predictors())

bake(SCM_FINAL_RF_rec  |>  prep(), new_data = NULL) 

# Modelo
Tree_model <- decision_tree(mode = "regression", tree_depth = 20,
                            min_n = 20 * nrow(SCM_FINAL_RF_test))

# Flujo
SCM_FINAL_RF_wflow <-
  workflow() |> 
  add_recipe(SCM_FINAL_RF_rec) %>%
  add_model(Tree_model)

# Ajuste
tree_fit <- SCM_FINAL_RF_wflow %>%
  fit(data = SCM_FINAL_RF_train)

# Obtener las predicciones

predictions <- predict(tree_fit, new_data = SCM_FINAL_RF_test)$`.pred`

# Calcular métricas de evaluación
mse <- mean((predictions - SCM_FINAL_RF_test$Pob_EV_Total)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - SCM_FINAL_RF_test$Pob_EV_Total))
r_squared <- 1 - sum((SCM_FINAL_RF_test$Pob_EV_Total - predictions)^2) / sum((SCM_FINAL_RF_test$Pob_EV_Total - mean(SCM_FINAL_RF_test$Pob_EV_Total))^2)

# Imprimir las métricas
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r_squared, "\n")


library(tibble)
library(gt)
# Crear la tabla de métricas
metrics_table <- tibble(
  Metric = c("MSE", "RMSE", "MAE", "R-squared"),
  Value = c(mse, rmse, mae, r_squared)
)

# Mostrar la tabla
gt(metrics_table)

library(rpart.plot)

# Obtener el modelo de árbol de decisión entrenado
tree_model <- tree_fit$fit$fit$fit

# Visualizar el árbol de decisión
rpart.plot(tree_model, extra = 100, cex = 0.8)
prp(tree_model, extra = 100, cex = 0.8, box.col = "white", fallen.leaves = TRUE)

summary(tree_model)
printcp(tree_model)

# ARBOL SIN OUTLIERS #####################################################################

SCM_FINAL_RF1 <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_outliers.csv", 
            sep = ",", header = TRUE, dec = ".")

SCM_FINAL_RF_split <- initial_split(SCM_FINAL_RF1, strata = Pob_EV_Total, prop = 0.8)

SCM_FINAL_RF_train <- training(SCM_FINAL_RF_split)
SCM_FINAL_RF_test <- testing(SCM_FINAL_RF_split)

# Conjunto de validación
val_set <- validation_split(SCM_FINAL_RF_train, strata = Pob_EV_Total,
                            prop = 0.70)

# RECETA
SCM_FINAL_RF_rec <-
  recipe(Pob_EV_Total ~ ., data = SCM_FINAL_RF_train) |> 
  # Elimino variables
  step_rm(año, CIF, Entidad, Municipio, Pob_EV_65_Total, Pob_Grupo, Terr_Zonas_Estad_CM) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5) |>  
  # En árboles no hace falta cambiar las categóricas a dummies
  # Filtro de cero varianza
  step_zv(all_predictors())

bake(SCM_FINAL_RF_rec  |>  prep(), new_data = NULL) 

# Modelo
Tree_model <- decision_tree(mode = "regression", tree_depth = 20,
                            min_n = 20 * nrow(SCM_FINAL_RF_test))

# Flujo
SCM_FINAL_RF_wflow <-
  workflow() |> 
  add_recipe(SCM_FINAL_RF_rec) %>%
  add_model(Tree_model)

# Ajuste
tree_fit <- SCM_FINAL_RF_wflow %>%
  fit(data = SCM_FINAL_RF_train)

# Obtener las predicciones

predictions <- predict(tree_fit, new_data = SCM_FINAL_RF_test)$`.pred`

# Calcular métricas de evaluación
mse <- mean((predictions - SCM_FINAL_RF_test$Pob_EV_Total)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - SCM_FINAL_RF_test$Pob_EV_Total))
r_squared <- 1 - sum((SCM_FINAL_RF_test$Pob_EV_Total - predictions)^2) / sum((SCM_FINAL_RF_test$Pob_EV_Total - mean(SCM_FINAL_RF_test$Pob_EV_Total))^2)

# Imprimir las métricas
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r_squared, "\n")

library(tibble)
library(gt)

# Crear la tabla de métricas
metrics_table <- tibble(
  Metric = c("MSE", "RMSE", "MAE", "R-squared"),
  Value = c(mse, rmse, mae, r_squared)
)

# Mostrar la tabla
gt(metrics_table)

library(rpart.plot)

# Obtener el modelo de árbol de decisión entrenado
tree_model <- tree_fit$fit$fit$fit

# Visualizar el árbol de decisión
rpart.plot(tree_model, extra = 100, cex = 0.8)
prp(tree_model, extra = 100, cex = 0.8, box.col = "white", fallen.leaves = TRUE)

summary(tree_model)
printcp(tree_model)

# ARBOL CON CUALITATIVA #####################################################################

SCM_FINAL_RF2 <- 
  read.csv2("C:/Users/evaba/Documents/TFM/CODIGO/SCM_FINAL_outliers.csv", 
            sep = ",", header = TRUE, dec = ".")

SCM_FINAL_RF_split <- initial_split(SCM_FINAL_RF2, strata = -	Eco_Cap_Nec_Financ, prop = 0.8)

SCM_FINAL_RF_train <- training(SCM_FINAL_RF_split)
SCM_FINAL_RF_test <- testing(SCM_FINAL_RF_split)

# Conjunto de validación
val_set <- validation_split(SCM_FINAL_RF_train, strata = Eco_Cap_Nec_Financ,
                            prop = 0.70)

# RECETA
SCM_FINAL_RF_rec <-
  recipe(Eco_Cap_Nec_Financ ~ ., data = SCM_FINAL_RF_train) |> 
  # Elimino variables
  step_rm(año, CIF, Entidad, Municipio, Pob_Grupo, Terr_Zonas_Estad_CM) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5) |>  
  # En árboles no hace falta cambiar las categóricas a dummies
  # Filtro de cero varianza
  step_zv(all_predictors())

bake(SCM_FINAL_RF_rec  |>  prep(), new_data = NULL) 

# Modelo
Tree_model <- decision_tree(mode = "classification", tree_depth = 20,
                            min_n = 20 * nrow(SCM_FINAL_RF_test))
 

# Flujo
SCM_FINAL_RF_wflow <-
  workflow() |> 
  add_recipe(SCM_FINAL_RF_rec) %>%
  add_model(Tree_model)

# Ajuste
tree_fit <- SCM_FINAL_RF_wflow %>%
  fit(data = SCM_FINAL_RF_train)

# Obtener las predicciones

predictions <- predict(tree_fit, new_data = SCM_FINAL_RF_test)$`.pred`

# Calcular métricas de evaluación
mse <- mean((predictions - SCM_FINAL_RF_test$Pob_EV_Total)^2)
rmse <- sqrt(mse)
mae <- mean(abs(predictions - SCM_FINAL_RF_test$Pob_EV_Total))
r_squared <- 1 - sum((SCM_FINAL_RF_test$Pob_EV_Total - predictions)^2) / sum((SCM_FINAL_RF_test$Pob_EV_Total - mean(SCM_FINAL_RF_test$Pob_EV_Total))^2)

# Imprimir las métricas
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r_squared, "\n")

library(tibble)
library(gt)

# Crear la tabla de métricas
metrics_table <- tibble(
  Metric = c("MSE", "RMSE", "MAE", "R-squared"),
  Value = c(mse, rmse, mae, r_squared)
)

# Mostrar la tabla
gt(metrics_table)

library(rpart.plot)

# Obtener el modelo de árbol de decisión entrenado
tree_model <- tree_fit$fit$fit$fit

# Visualizar el árbol de decisión
rpart.plot(tree_model, extra = 100, cex = 0.8)
prp(tree_model, extra = 100, cex = 0.8, box.col = "white", fallen.leaves = TRUE)

summary(tree_model)
printcp(tree_model)


