## ÁRBOLES Y RANDOM FOREST

# SCM_FINAL2 y SCM_FINAL Tienen MadridCapital
# SCM_FINAL3 no tiene Madrid Capital
# SCM_FINAL_outliers no tiene Madrid Capital ni outliers ni ausentes
rm(list = ls())
library(tidymodels)
library(parallel)
library(doParallel)
# Iniciamos la paralelización
clusters <- detectCores() - 1

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
  step_rm(año, CIF, Entidad, Municipio) |> 
  # Filtro de correlación
  step_corr(all_numeric_predictors(), threshold = 0.5) |>  
  # En árboles no hace falta cambiar las categóricas a dummies
  # Filtro de cero varianza
  step_zv(all_predictors())

bake(SCM_FINAL_RF_rec  |>  prep(), new_data = NULL) 

# Modelo
Tree_model <- decision_tree(mode = "regression", tree_depth = 10,
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

# CV con Random Forest.###########################################################

library(caret)

# Definir el esquema de validación cruzada con repetición
folds <- createMultiFolds(SCM_FINAL_RF_train$Pob_EV_Total, k = 10, times = 5)

# Paso 1: Definir la receta
SCM_FINAL_RF_rec <- as.formula("Pob_EV_Total ~ .")

# Paso 2: Definir el control del entrenamiento con validación cruzada
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  index = folds
)

# Paso 3: Ajustar el modelo de Random Forest
rf_model <- train(
  SCM_FINAL_RF_rec,
  data = SCM_FINAL_RF_train,
  method = "rf",
  trControl = ctrl,
  ntree = 500,
  importance = TRUE
)

# Obtener las predicciones
predictions <- predict(rf_model, newdata = SCM_FINAL_RF_test)

# Calcular las métricas de evaluación
metrics <- data.frame(
  RMSE = sqrt(mean((SCM_FINAL_RF_test$Pob_EV_Total - predictions)^2)),
  MAE = mean(abs(SCM_FINAL_RF_test$Pob_EV_Total - predictions))
)

# Imprimir las métricas
metrics
# finalizamos clusters (por si acaso)
stopCluster(make_cluster)
registerDoSEQ()

library(ggplot2)

# Crear un data frame con las predicciones y los errores
results <- data.frame(Actual = SCM_FINAL_RF_test$Pob_EV_Total, 
                      Prediction = predictions, 
                      Error = SCM_FINAL_RF_test$Pob_EV_Total - predictions)

# Crear el gráfico de dispersión
ggplot(results, aes(x = Actual, y = Error)) +
  geom_point() +
  labs(x = "Valor real", y = "Error") +
  ggtitle("Diagrama de dispersión de errores en Random Forest")

# Obtener la importancia de las variables
importance <- rf_model$finalModel$importance

importance_df <- data.frame(Variable = row.names(importance), 
                            Importance = importance[,1])

# Ordenar las variables por su importancia
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]


# Crear un gráfico de barras de las variables importantes
barplot(importance_df$Importance, horiz = TRUE, names.arg = importance_df$Variable, main = "Importancia de las variables en Random Forest")

top_15_importance <- head(importance_df, 15)
top_15_importance
