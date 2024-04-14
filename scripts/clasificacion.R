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
       adabag)   

# 1. Elegir si se prueba con particiones o sobre los datos completos
opt <- 0 # 1 se hacen particiones para medir F1 de antemano, 0 se deja completo los datos

# 2. Cargar datos
# setwd("C:/Users/Paula Osorio/Documents/Git Hub Repositorios/problem_set_2")
train<-read.csv("stores/train_procesado.csv")
test<-read.csv("stores/test_procesado.csv")

# 3. Se reemplazan los NA por 0
train[is.na(train)] <- 0
test[is.na(test)] <- 0

# 4. Se divide la BD para medir error generalizado si opt=1
if (opt==1){
  #subdivisión de la base de datos para ir revisando el desempeño del modelo
  inTrain <- createDataPartition(
    y = train$Pobre,## La variable dependiente u objetivo 
    p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
    list = FALSE)
  train <- train[ inTrain,]
  test <- train[-inTrain,]
}

# 5. Se convierte en factor la variable objetivo
train <- train %>% mutate(
  Pobre = factor(Pobre,levels=c(1,0), labels=c('Yes','No'))) %>% select(-id)
test <- test %>% mutate(
  Pobre = factor(Pobre,levels=c(1,0), labels=c('Yes','No')))

# 6. Entrenamiento de Modelos

###########################################################
############## 6.1 Modelo Eslastic Net
###########################################################

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

Elastic_Net_1 <- train(Pobre~.,#clase+tipo_vivienda+lp+tot_cuartos+porc_mujer+porc_salud+porc_educ_sup+porc_pension+porc_pet+porc_des,
                       data=train,
                       metric = "Accuracy",
                       method = "glmnet",
                       trControl = ctrl,
                       tuneGrid=expand.grid(
                         alpha = seq(0,0.5,by=.2),
                         lambda =10^seq(2, -2, length = 5)
                       )
)

#ver resultados en datos de test
predictSample <- test   %>% 
  mutate(pobre_lab = predict(Elastic_Net_1, newdata = test, type = "raw")   
  )  %>% select(id,pobre_lab)

# 6.1.1 Revisar resultados previo a subir a Kaggle
if (opt==1){
  #Obtener F1 con datos de submuestra
  confusionMatrix(data = predictSample$pobre_lab, reference = test$Pobre, mode = "prec_recall")
}

# Subir resultado a kaggle
predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
write.csv(predictSample,"stores/classification_elasticnet.csv", row.names = FALSE)




##################################################################################
##########################6.2 Random Forest
################################################################################

#Revisar mejores hiperparámetros de variables y complejidad del arbol(nod_size)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

mtry_grid<-expand.grid(mtry =c(2, 4,6,8), 
                       min.node.size= c(10, 50, 100, 500, 1000), #controla la complejidad del arbol
                       splitrule= 'gini') 

cv_RForest <- train(Pobre~clase+tot_cuartos+nper+npet+ndes+npension+jefe_mujer+jefe_educ_sup, 
                    data = train, 
                    method = "ranger",
                    trControl = ctrl,
                    metric="ROC",
                    tuneGrid = mtry_grid,
                    ntree=500)
cv_RForest

#correr modelo con combinaciones que resultó mejor
RF<- ranger(Pobre~clase+tot_cuartos+nper+npet+ndes+npension+jefe_mujer+jefe_educ_sup, 
            data = train,
            num.trees= 500, ## Numero de bootstrap samples y arboles a estimar 
            mtry= 4,   # N. var aleatoriamente seleccionadas en cada partición.
            min.node.size  = 100, ## Numero minimo de observaciones en un nodo para intentar 
            importance="impurity")

RF #visualizar arbol

RF_pred <- predict(RF,
                   data = test#, 
                   #predict.all = TRUE # para obtener la clasificación de cada arbol. 
)

predictSample <- test   %>% 
  mutate(pobre_lab = RF_pred$predictions   
  )  %>% select(id,pobre_lab)


if (opt==1){
  #Obtener F1 con datos de submuestra
  #dependiendo de qué resulta de bagged_pred o de pred, esa es la variable que va en data=
  confusionMatrix(data = predictSample$pobre_lab, reference = test$Pobre, mode = "prec_recall")
}

# Subir resultado a kaggle
predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
write.csv(predictSample,"stores/classification_random_forest.csv", row.names = FALSE)

# Se revisa la relevancia de cada variable
imp<-importance(RF)
imp2<- data.frame(variables= names(imp),
                  importance= imp)

ggplot(imp2, aes(x = reorder(variables, importance) , y =importance )) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Variable ", x = "Importance", y="Variable") +
  theme_minimal() +
  coord_flip()


###################################################################
#########Boosting
#################################################################

adagrid<-  expand.grid(
  mfinal = c( 50, 300 ,500),
  maxdepth = c(1,2,5),
  coeflearn = c('Breiman','Freund'))

adaboost_tree <- train(Default~duration+amount+installment+age+
                         history+purpose+foreign+rent,
                       data = train, 
                       method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
                       trControl = ctrl,
                       metric = "ROC",
                       tuneGrid=adagrid
)

adaboost_tree

Pobre<- ifelse(test$Pobre=="Yes",1,0)#devolver la configuración de pobre a 1s y 0s

boost_pred <- predict(adaboost_tree,
                      newdata = test, 
                      type = "prob")  

pred <- as.data.frame( boost_pred$predictions )

if (opt==1){
  #Obtener F1 con datos de submuestra
  #dependiendo de qué resulta de bagged_pred o de pred, esa es la variable que va en data=
  confusionMatrix(data = test$prediccion_pobre, reference = test$real_pobre, mode = "prec_recall")
}

# Subir resultado a kaggle
predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
write.csv(predictSample,"stores/classification_adaboost.csv", row.names = FALSE)