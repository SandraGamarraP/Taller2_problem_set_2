##Income regression model

colnames(train)
data_trained <- train
features <- c("edad","edad_2","mujer","busca_trabajo","exp_trabajo_actual","horas_trab_semana","superior")
data_trained <- data_trained %>% drop_na(any_of(features))

#Regresion lineal
linear_model <- lm(Ingtotug ~ edad + edad_2 + busca_trabajo + exp_trabajo_actual + horas_trab_semana + superior, data=data_trained)
summary(linear_model)

#Residuals
data_trained <- data_trained %>% mutate(residuals = linear_model$residuals)
linear_model2 <- lm(Ingtotug ~ edad + edad_2 + busca_trabajo + horas_trab_semana, data=data_trained) 
stargazer(linear_model,linear_model2, type = "text",
          covariate.labels=c("Mean Ingtotug","mujer","edad","busca_trabajo","horas_trab_semana"))


#Predecir fuera de muestra
#Conjunto de Validación
set.seed(10101)

inTrain <- createDataPartition(
  y = data_trained$Ingtotug, p= .80, list=FALSE
)
trining <- data_trained[inTrain,]
testing <- data_trained[-inTrain,]

#Entrenar modelos

#Modelo 1
form_1 <- Ingtotug ~ edad + edad_2 + busca_trabajo + exp_trabajo_actual + horas_trab_semana
modelo1a <- lm(form_1, data = trining)

#Rendimiento fuera de muestra
predictions<- predict(modelo1a, testing)
score1a <- RMSE(predictions,testing$Ingtotug)
score1a

#Modelo 2
form_2 <- Ingtotug ~ edad + edad_2 + busca_trabajo + exp_trabajo_actual
modelo2a <- lm(form_2, data = trining)
predictions <- predict(modelo2a, testing)
score2a <- RMSE(predictions, testing$Ingtot)
score2a

#Modelo 3
form_3 <- Ingtotug ~ edad + edad_2 + busca_trabajo
modelo3a <- lm(form_3, data = trining)
predictions <- predict(modelo3a, testing)
score3a <- RMSE(predictions, testing$Ingtotug)
score3a
