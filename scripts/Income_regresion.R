##Income regression model

colnames(train)
data_trained <- train
features <- c("edad","edad_2","mujer","busca_trabajo","exp_trabajo_actual","horas_trab_semana","superior", "estudiante", "primaria","secundaria")
data_trained <- data_trained %>% drop_na(any_of(features))

#Regresion lineal
linear_model <- lm(Ingtotug ~ edad + edad_2 + busca_trabajo + exp_trabajo_actual + horas_trab_semana + superior + estudiante + primaria + secundaria, data=data_trained)
summary(linear_model)

#Residuals
data_trained <- data_trained %>% mutate(residuals = linear_model$residuals)
linear_model2 <- lm(Ingtotug ~ edad + edad_2 + busca_trabajo + horas_trab_semana + estudiante, data=data_trained) 

#Predecir fuera de muestra
#Conjunto de Validación
set.seed(10101)

inTrain <- createDataPartition(
  y = data_trained$Ingtotug, p= .80, list=FALSE
)
training <- data_trained[inTrain,]
testing <- data_trained[-inTrain,]

#Entrenar modelos

#Modelo 1
form_1 <- Ingtotug ~ edad + edad_2 + busca_trabajo + exp_trabajo_actual + horas_trab_semana + superior + estudiante + primaria + secundaria
modelo1a <- lm(form_1, data = trining)

#Rendimiento fuera de muestra
predictions<- predict(modelo1a, testing)
score1a <- RMSE(predictions,testing$Ingtotug)
score1a

#Modelo 2
form_2 <- Ingtotug ~ edad + edad_2 + busca_trabajo + exp_trabajo_actual + horas_trab_semana + superior + estudiante
modelo2a <- lm(form_2, data = training)
predictions <- predict(modelo2a, testing)
score2a <- RMSE(predictions, testing$Ingtotug)
score2a

#Modelo 3
form_3 <- Ingtotug ~ edad + edad_2 + busca_trabajo + exp_trabajo_actual + horas_trab_semana 
modelo3a <- lm(form_3, data = training)
predictions <- predict(modelo3a, testing)
score3a <- RMSE(predictions, testing$Ingtotug)
score3a

##Validación Cruzada K-Fold

#Modelo 1: Regresión Lineal 

ctrl <- trainControl(number = 5, method = "cv")
modelo1b <- train(form_1, data = data_trained, method = "lm", trControl = ctrl)
modelo1b

modelo1b$resample
score1b<- mean(modelo1b$resample$RMSE)

# Metricas modelo 1
p_load(MLmetrics)

y_hat_outsample1 = predict(modelo1b, newdata = test)

MAE(y_pred = y_hat_outsample1, y_true = test$Ingtotug)
MedianAE(y_pred = y_hat_outsample1, y_true = test$Ingtotug)
MedianAPE(y_pred = y_hat_outsample1, y_true = test$Ingtotug)
R2_Score(y_pred = y_hat_outsample1, y_true = test$Ingtotug)
RMSE(y_pred = y_hat_outsample1, y_true = test$Ingtotug)

#Modelo 2: Regresión lineal

modelo2b <- train(form_2,
                  data = data_trained,
                  method = 'lm', 
                  trControl= ctrl)
modelo2b
score2b<- mean(modelo2b$resample$RMSE)

#Modelo 3
modelo3b <- train(form_3,
                  data = data_trained,
                  method = 'lm', 
                  trControl= ctrl)
modelo3b
score3b<- mean(modelo3b$resample$RMSE)
