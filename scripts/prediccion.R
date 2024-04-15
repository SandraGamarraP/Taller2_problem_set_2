rm(list = ls())
set.seed(12345) # important set seed.

# 0. Importar librerias
require("pacman")
p_load(tidyverse, # tidy-data
       rpart, # Recursive Partition and Regression Trees (To run Trees)
       caret ,  # for model training and tunning
       rpart.plot, ## for trees graphs
       Metrics, ## Evaluation Metrics for ML
       ipred,  # For Bagging 
       ranger, #For random Forest
       adabag,
       biglm)  
# Elegir si se prueba con particiones o sobre los datos completos
opt <- 0 # 1 se hacen particiones para medir F1 de antemano, 0 se deja completo los datos

#Cargar datos

train<-(read.csv("stores/train_procesado_m.csv"))
test<-(read.csv("stores/test_procesado_m.csv"))
train$Ingreso <- train$Ingtotug
train <- train[, -which(names(train) == "Ingtotug")]

#  Se reemplazan los NA por 0
train[is.na(train)] <- 0
test[is.na(test)] <- 0

# Se divide la BD para medir error generalizado si opt=1
if (opt==1){
  #subdivisión de la base de datos para ir revisando el desempeño del modelo
  inTrain <- createDataPartition(
    y = train$Pobre,## La variable dependiente u objetivo 
    p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
    list = FALSE)
  train <- train[ inTrain,]
  test <- train[-inTrain,]
}

train <- train %>% mutate(
  Pobre = factor(Pobre,levels=c(1,0))) %>% select(-id)
test <- test %>% mutate(
  Pobre = factor(Pobre,levels=c(1,0), ))


# 1. Entrenamiento modelos de predicción -------------------------------------

####################### Regresion lineal ##########################

ctrl<- trainControl(method = "cv",
                     number = 100,
                     savePredictions = F)

modelo_lm <- train(Ingreso~nper+clase+nmujeres+nsalud+neducsup+
                         sub_transporte+otros_ing+ayu_inst+ndes+tot_cuartos+
                         tipo_vivienda+tiempo_con,
                       data=train,
                       method = "lm",
                       trControl = ctrl
                       )

predict_lm <- test   %>% 
  mutate(ingreso_lab = predict(modelo_lm, newdata = test, type = "raw")   
  )  %>% select(ingreso_lab)

percentil_5 <- quantile(train$Ingreso[train$Pobre == 1], probs = 0.05)
percentil_95 <- quantile(train$Ingreso[train$Pobre == 1], probs = 0.95)

datos_sin_colas <- train$Ingreso[train$Pobre == 1][train$Ingreso[train$Pobre == 1] > percentil_5 & train$Ingreso[train$Pobre == 1] < percentil_95]
umbral <- mean(datos_sin_colas)

predictSample_lm <- test %>%
  mutate(Pobre = ifelse(predict_lm > umbral, 0, 1)) %>% select(id, Pobre)


write.csv(predictSample_lm,"stores/Prediction_lm.csv", row.names = FALSE)

confusionMatrix(data = as.factor(predictSample_lm$Pobre),
                reference = test$Pobre, mode = "prec_recall")
############################# Elastic net ############################

ctrl <- trainControl(method = "cv", number = 10)

# Entrenar el modelo Elastic Net


modelo_elasticnet1 <- train(Ingreso~nper+clase+nmujeres+nsalud+neducsup+
                             sub_transporte+otros_ing+ayu_inst+ndes+tot_cuartos+
                             tipo_vivienda+cuota_amort,
                           data = train,
                           method = "glmnet",
                           trControl = ctrl,
                           tuneLength = 10)  

predict_elasticnet1 <- test   %>% 
  mutate(ingreso_lab = predict(modelo_elasticnet1, newdata = test, type = "raw")   
  )  %>% select(ingreso_lab)

predictSample_net1 <- test %>%
  mutate(pobre_lab = ifelse(predict_elasticnet1 > umbral, 0, 1))  %>% select(id, Pobre)

write.csv(predictSample_net1,"stores/Prediction_elasticnet1.csv", row.names = FALSE)


confusionMatrix(data = as.factor(predictSample_net1$pobre_lab),
                reference = test$Pobre, mode = "prec_recall")



modelo_elasticnet2 <- train(Ingreso~clase+tot_cuartos+nper+npet+ndes+
                              npension+jefe_mujer+jefe_educ_sup,
                            data = train,
                            method = "glmnet",
                            trControl = ctrl,
                            tuneGrid=expand.grid(
                              alpha = seq(0,1,by=.2),
                              lambda =10^seq(2, -2, length = 5)))  

predict_elasticnet2 <- test   %>% 
  mutate(ingreso_lab = predict(modelo_elasticnet2, newdata = test, type = "raw")   
  )  %>% select(ingreso_lab)

predictSample_net2 <- test %>%
  mutate(pobre_lab = ifelse(predict_elasticnet2 > umbral, 0, 1))


write.csv(predictSample_net2,"stores/Prediction_elasticnet.csv", row.names = FALSE)



confusionMatrix(data = as.factor(predictSample_net2$pobre_lab),
                reference = test$Pobre, mode = "prec_recall")

############################ Boosting ############################### 

tuneGrid <- expand.grid(n.trees = c(50, 200, 300),          # Número de árboles
                        interaction.depth = c(1, 3, 5),     # Profundidad de interacción
                        shrinkage = c(0.05, 0.1, 0.3),       # Tasa de aprendizaje (shrinkage)
                        n.minobsinnode = c(10, 20, 30))     # Mínimo de observaciones en un nodo terminal


modelo_boosting1 <- train(Ingreso~nper+clase+nmujeres+nsalud+neducsup+
                           sub_transporte+otros_ing+ayu_inst+ndes+tot_cuartos+
                           tipo_vivienda+tiempo_con,
                         data = train,
                         method = "gbm",
                         trControl = ctrl,
                         tuneGrid = tuneGrid, 
                         verbose = FALSE)

modelo_boosting2 <- train(Ingreso~nper+clase+nmujeres+nsalud+neducsup+
                            sub_transporte+otros_ing+ayu_inst+ndes+tot_cuartos+
                            tipo_vivienda,
                          data = train,
                          method = "AdaBoost.M1",
                          trControl = ctrl,
                          tuneGrid = tuneGrid, 
                          verbose = FALSE)

predict_booting1 <- test   %>% 
  mutate(ingreso_lab = predict(modelo_boosting1, newdata = test, type = "raw")   
  )  %>% select(ingreso_lab)

test <- test %>%
  mutate(Pobre = ifelse(predict_booting1 > umbral, 0, 1))

################ Random forest ###############################

#correr modelo con combinaciones que resultó mejor
RF<- ranger(Ingreso~nper+clase+nmujeres+nsalud+neducsup+
              sub_transporte+otros_ing+ayu_inst+ndes+tot_cuartos+
              tipo_vivienda+tiempo_con, 
            data = train,
            num.trees= 500, ## Numero de bootstrap samples y arboles a estimar 
            mtry= 5,   # N. var aleatoriamente seleccionadas en cada partición.
            min.node.size  = 100, ## Numero minimo de observaciones en un nodo para intentar 
            importance="impurity")

RF #visualizar arbol

RF_pred <- predict(RF,
                   data = test#, 
                   #predict.all = TRUE # para obtener la clasificación de cada arbol. 
)

predictSample_rf <- test   %>% 
  mutate(Pobre = RF_pred$predictions   
  )  %>% select(id,Pobre)


predictSample_rf <- predictSample_rf %>%
  mutate(Pobre = ifelse(predictSample_rf$Pobre > umbral, 0, 1)) 

if (opt==1){
  #Obtener F1 con datos de submuestra
  #dependiendo de qué resulta de bagged_pred o de pred, esa es la variable que va en data=
  confusionMatrix(data = as.factor(predictSample_rf$Pobre), 
                  reference = test$Pobre, mode = "prec_recall")
}

predictSample_rf <- predictSample_rf %>% select(id,Pobre)
write.csv(predictSample_rf,"stores/Prediction_randomforest.csv", row.names = FALSE)
