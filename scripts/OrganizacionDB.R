############################################
########## Problem set 2            ########
###### Big Data y Maching Learning #########
###### Paula Osorio:201327186   ############
###### Sandra Gamarra: 202225782 ###########
###### Erika M. Macías:  Educacion continua#  
###### Ingrith Sierra: 201720654    ########
############################################


rm(list = ls())

# Especificar Directorio

# directorio_destino <- "C:/Users/sandr/Documents/GitHub/BIG DATA/"

# Cargando librerias y paquetes necesarios 

require("pacman")
p_load( tidyverse, # tidy-data
        glmnet, # To implement regularization algorithms. 
        caret, # creating predictive models
        rvest, 
        tidyverse, 
        ggplot2, 
        robotstxt, 
        psych, 
        stargazer, 
        boot, 
        openxlsx,
        skimr
)

# Importando los datos 

train_personas <- read.csv("data/train_personas.csv")
train_hogares <- read.csv("data/train_hogares.csv")
test_personas <- read.csv(("data/test_personas.csv"))
test_hogares <- read.csv("data/test_hogares.csv")
sample_submission <- read.csv("data/sample_submission.csv")

# Revisando las variables y la información que contiene la base de datos
## Hogares
colnames(train_hogares)

train_hogares  %>% 
  select(id)  %>%
  head()

# Según la base del DANE un hogar es clasificado pobre si el “Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios” es menor a la Linea de pobreza que le corresponde al hogar.
table(train_hogares$Pobre)

train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand_2)

##Personas
colnames(train_personas)

train_personas  %>% 
  select(id, Orden)  %>%
  head()

# Calculamos el procentaje de inactivos
new_hogar_variable <- train_personas  %>% 
  group_by(id) %>%
  summarize(h_Inactivos=sum(Ina,na.rm=TRUE),
            h_Pet= sum(Pet, na.rm = TRUE)   ) %>%
  mutate(h_Inactivosp=h_Inactivos/h_Pet ) %>%
  ungroup()

new_hogar_variable %>%  
  head()

# Cálculos de pobreza
# Comparando variables en la base train y test
colnames(train_hogares)
colnames(test_hogares)

colnames(train_personas)
colnames(test_personas)

# Ver variables con datos missing en la base train_hogares
datos_miss <- skim(train_hogares) %>% select( skim_variable, n_missing)

Nobs= nrow(train_hogares) 
Nobs

datos_miss<- datos_miss %>% mutate(p_missing= n_missing/Nobs)
head(datos_miss)

# Ordenar variables con mayor número de missing
datos_miss <- datos_miss %>% arrange(-n_missing)
datos_miss<- datos_miss %>% filter(n_missing!= 0)

#Visualizando la estructura de los missing
ggplot(datos_miss, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5)) 

# Ver variables con datos missing en la base train_persona
datos_miss_per <- skim(train_personas) %>% select( skim_variable, n_missing)

Nobs= nrow(train_personas) 
Nobs

datos_miss_per <- datos_miss_per %>% mutate(p_missing= n_missing/Nobs)
head(datos_miss_per)

# Ordenar variables con mayor número de missing
datos_miss_per <- datos_miss_per %>% arrange(-n_missing)
datos_miss_per<- datos_miss_per %>% filter(n_missing!= 0)

#Visualizando la estructura de los missing
ggplot(datos_miss_per, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5)) 


#############################################

## En la base train_personas organizo las variables de interés

# en la base train_personas

# En Edad solo seleccionamos los mayores de 18 años

train_personas <- rename(train_personas, c("edad" = "P6040"))
train_personas$edad_2 <- train_personas$edad^2

# Ocupado 
train_personas$ocupado <- ifelse(train_personas$Oc == 1, 1, 0)
train_personas$ocupado[train_personas$Oc != 1] <- 0
train_personas$ocupado[is.na(train_personas$ocupado)] <- 0


# Género

train_personas$mujer <- ifelse(train_personas$P6020 == 2, 1, 0)
train_personas$mujer[train_personas$P6020 == 1] <- 0

# Estudiante

train_personas$estudiante <- ifelse(train_personas$P6240 == 3, 1, 0)
train_personas$estudiante[train_personas$P6240 != 3] <- 0
train_personas$estudiante[train_personas$P6240 == "."] <- 0
train_personas$estudiante[is.na(train_personas$estudiante)] <- 0

# Busca trabajo

train_personas$busca_trabajo <- ifelse(train_personas$P6240 == 2, 1, 0)
train_personas$busca_trabajo[train_personas$P6240 != 2] <- 0
train_personas$busca_trabajo[train_personas$P6240 == "."] <- 0
train_personas$busca_trabajo[is.na(train_personas$busca_trabajo)] <- 0

# Ama de casa

train_personas$ama_casa <- ifelse(train_personas$P6240 == 4, 1, 0)
train_personas$ama_casa[train_personas$P6240 != 4] <- 0
train_personas$ama_casa[train_personas$P6240 == "."] <- 0
train_personas$ama_casa[is.na(train_personas$ama_casa)] <- 0

# Jefe del hogar
train_personas$jefe_hogar <- ifelse(train_personas$P6050 == 1, 1, 0)
train_personas$jefe_hogar[train_personas$P6050 != 1] <- 0
train_personas$jefe_hogar[train_personas$P6050 == "."] <- 0
train_personas$jefe_hogar[is.na(train_personas$jefe_hogar)] <- 0

# Hijos en el hogar

train_personas$hijos_hogar <- ifelse(train_personas$P6050 == 3, 1, 0)
train_personas$hijos_hogar[train_personas$P6050 != 3] <- 0
train_personas$hijos_hogar[train_personas$P6050 == "."] <- 0
train_personas$hijos_hogar[is.na(train_personas$hijos_hogar)] <- 0

# Nivel Primaria

train_personas$primaria <- ifelse(train_personas$P6210 == 1, 1, 0)
train_personas$primaria[train_personas$P6210 == "."] <- 0
train_personas$primaria[is.na(train_personas$primaria)] <- 0

# Nivel Secundaria

train_personas$secundaria <- ifelse(train_personas$P6210 == 4, 1, 0)
train_personas$secundaria[train_personas$P6210 == "."] <- 0
train_personas$secundaria[is.na(train_personas$secundaria)] <- 0

# Nivel Media

train_personas$media <- ifelse(train_personas$P6210 == 5, 1, 0)
train_personas$media[train_personas$P6210 == "."] <- 0
train_personas$media[is.na(train_personas$media)] <- 0

# Nivel Superior

train_personas$superior <- ifelse(train_personas$P6210 == 6, 1, 0)
train_personas$superior[train_personas$P6210 == "."] <- 0
train_personas$superior[is.na(train_personas$superior)] <- 0

# Experiencia trabajo actual

train_personas <- rename(train_personas, c("exp_trab_actual" = "P6426"))

# Horas de trabajo a la semana

train_personas <- rename(train_personas, c("horas_trab_semana" = "P6800"))


# Ciudad

train_personas <- rename(train_personas, c("ciudad" = "Dominio"))



# Imputación de experiencia

train_personas$exp_trab_actual <- ifelse(train_personas$edad < 18 & 
                                           is.na(train_personas$exp_trab_actual), 0, 
                                         train_personas$exp_trab_actual)

train_personas <- train_personas %>% 
  group_by(id) %>% 
  mutate(mean_exp = mean(exp_trab_actual, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(exp_trab_actual = if_else(is.na(exp_trab_actual) & train_personas$edad >= 18, 
                                   mean_exp, train_personas$exp_trab_actual))

train_personas <- train_personas %>% 
  group_by(id) %>% 
  mutate(variable = ifelse(all(is.na(exp_trab_actual)), 0, 
                           exp_trab_actual)) %>% 
  ungroup() %>% 
  mutate(exp_trab_actual = if_else(is.na(exp_trab_actual), 
                                   variable, train_personas$exp_trab_actual))

# Imputación de Horas trabajadas a la semana

train_personas$horas_trab_semana <- ifelse(train_personas$edad < 18 & 
                                             is.na(train_personas$horas_trab_semana), 0, 
                                           train_personas$horas_trab_semana)

train_personas <- train_personas %>% 
  group_by(id) %>% 
  mutate(mean_h = mean(horas_trab_semana, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(horas_trab_semana = if_else(is.na(horas_trab_semana) & train_personas$edad >= 18, 
                                     mean_h, train_personas$horas_trab_semana))

train_personas <- train_personas %>% 
  group_by(id) %>% 
  mutate(variable = ifelse(all(is.na(horas_trab_semana)), 0, 
                           horas_trab_semana)) %>% 
  ungroup() %>% 
  mutate(horas_trab_semana = if_else(is.na(horas_trab_semana), 
                                     variable, train_personas$horas_trab_semana))

#Nos quedamos con las variables de interés

train_personas <- subset(train_personas, select = c("id", "Orden", "Clase",
                                                    "ciudad", "edad", "edad_2","ocupado", "mujer", 
                                                    "estudiante", "busca_trabajo","jefe_hogar", 
                                                    "ama_casa", "hijos_hogar", "primaria", 
                                                    "secundaria", "media", "superior", "Ingtot",
                                                    "exp_trab_actual",
                                                    "horas_trab_semana"))

train_personas$num_menores <- as.numeric(train_personas$edad < 18)

# Pasar la base únicamente a hogares

train_personas_hogar <- train_personas %>% group_by(id) %>%
  summarize(edad = mean(edad),
            edad_2 = mean(edad_2),
            ocupado = mean(ocupado), #mean
            mujer = mean(mujer), #mean
            estudiante = mean(estudiante),
            busca_trabajo = mean(busca_trabajo),
            jefe_hogar = mean(jefe_hogar),
            ama_casa =mean(ama_casa),
            hijos_hogar = mean(hijos_hogar),
            primaria = mean(primaria),
            secundaria = mean(secundaria),
            media = mean(media),
            superior = mean(superior),
            Ingtot = sum(Ingtot), #Se suma porque es por individuo
            exp_trab_actual = mean(exp_trab_actual),
            horas_trab_semana = mean(horas_trab_semana),
            num_menores = sum(num_menores),
            ciudad = first(ciudad))

############
## En la base train_hogares organizo las variables de interés

# en la base train_hogares

# Limpiando la base de datos

# Arrienda
train_hogares$arrienda <- ifelse(train_hogares$P5090 == 3, 1, 0)
train_hogares$arrienda[train_hogares$P5090 != 3] <- 0
train_hogares$arrienda[is.na(train_hogares$arrienda)] <- 0

# Ciudad

train_hogares <- rename(train_hogares, c("ciudad" = "Dominio"))

# Número de Cuartos

train_hogares <- rename(train_hogares, c("numero_cuartos" = "P5010"))

# Número de personas

train_hogares <- rename(train_hogares, c("numero_personas" = "Nper"))

# Nos quedamos con las variables de interés
train_hogares_final <- subset(train_hogares, select = c("id", "Clase",
                                                        "ciudad", "Ingtotug",  "Pobre", 
                                                        "numero_personas", "arrienda"))


# Haciendo join sobre la base train
train <- left_join(train_personas_hogar,train_hogares_final)
###########################

# Ahora organizamos para la base test_personas y test_hogares
# Para test personas
# En Edad solo seleccionamos los mayores de 18 años

test_personas <- rename(test_personas, c("edad" = "P6040"))
test_personas$edad_2 <- test_personas$edad^2

# Ocupado 
test_personas$ocupado <- ifelse(test_personas$Oc == 1, 1, 0)
test_personas$ocupado[test_personas$Oc != 1] <- 0
test_personas$ocupado[is.na(test_personas$ocupado)] <- 0


# Género

test_personas$mujer <- ifelse(test_personas$P6020 == 2, 1, 0)
test_personas$mujer[test_personas$P6020 == 1] <- 0

# Estudiante

test_personas$estudiante <- ifelse(test_personas$P6240 == 3, 1, 0)
test_personas$estudiante[test_personas$P6240 != 3] <- 0
test_personas$estudiante[test_personas$P6240 == "."] <- 0
test_personas$estudiante[is.na(test_personas$estudiante)] <- 0

# Busca trabajo

test_personas$busca_trabajo <- ifelse(test_personas$P6240 == 2, 1, 0)
test_personas$busca_trabajo[test_personas$P6240 != 2] <- 0
test_personas$busca_trabajo[test_personas$P6240 == "."] <- 0
test_personas$busca_trabajo[is.na(test_personas$busca_trabajo)] <- 0

# Ama de casa

test_personas$ama_casa <- ifelse(test_personas$P6240 == 4, 1, 0)
test_personas$ama_casa[test_personas$P6240 != 4] <- 0
test_personas$ama_casa[test_personas$P6240 == "."] <- 0
test_personas$ama_casa[is.na(test_personas$ama_casa)] <- 0

# Jefe del hogar
test_personas$jefe_hogar <- ifelse(test_personas$P6050 == 1, 1, 0)
test_personas$jefe_hogar[test_personas$P6050 != 1] <- 0
test_personas$jefe_hogar[test_personas$P6050 == "."] <- 0
test_personas$jefe_hogar[is.na(test_personas$jefe_hogar)] <- 0

# Hijos en el hogar

test_personas$hijos_hogar <- ifelse(test_personas$P6050 == 3, 1, 0)
test_personas$hijos_hogar[test_personas$P6050 != 3] <- 0
test_personas$hijos_hogar[test_personas$P6050 == "."] <- 0
test_personas$hijos_hogar[is.na(test_personas$hijos_hogar)] <- 0

# Nivel Primaria

test_personas$primaria <- ifelse(test_personas$P6210 == 1, 1, 0)
test_personas$primaria[test_personas$P6210 == "."] <- 0
test_personas$primaria[is.na(test_personas$primaria)] <- 0

# Nivel Secundaria

test_personas$secundaria <- ifelse(test_personas$P6210 == 4, 1, 0)
test_personas$secundaria[test_personas$P6210 == "."] <- 0
test_personas$secundaria[is.na(test_personas$secundaria)] <- 0

# Nivel Media

test_personas$media <- ifelse(test_personas$P6210 == 5, 1, 0)
test_personas$media[test_personas$P6210 == "."] <- 0
test_personas$media[is.na(test_personas$media)] <- 0

# Nivel Superior

test_personas$superior <- ifelse(test_personas$P6210 == 6, 1, 0)
test_personas$superior[test_personas$P6210 == "."] <- 0
test_personas$superior[is.na(test_personas$superior)] <- 0

# Experiencia trabajo actual

test_personas <- rename(test_personas, c("exp_trab_actual" = "P6426"))

# Horas de trabajo a la semana

test_personas <- rename(test_personas, c("horas_trab_semana" = "P6800"))


# Ciudad

test_personas <- rename(test_personas, c("ciudad" = "Dominio"))


# Imputación de experiencia

test_personas$exp_trab_actual <- ifelse(test_personas$edad < 18 & 
                                          is.na(test_personas$exp_trab_actual), 0, 
                                        test_personas$exp_trab_actual)

test_personas <- test_personas %>% 
  group_by(id) %>% 
  mutate(mean_exp = mean(exp_trab_actual, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(exp_trab_actual = if_else(is.na(exp_trab_actual) & test_personas$edad >= 18, 
                                   mean_exp, test_personas$exp_trab_actual))

test_personas <- test_personas %>% 
  group_by(id) %>% 
  mutate(variable = ifelse(all(is.na(exp_trab_actual)), 0, 
                           exp_trab_actual)) %>% 
  ungroup() %>% 
  mutate(exp_trab_actual = if_else(is.na(exp_trab_actual), 
                                   variable, test_personas$exp_trab_actual))

# Imputación de Horas trabajadas a la semana

test_personas$horas_trab_semana <- ifelse(test_personas$edad < 18 & 
                                            is.na(test_personas$horas_trab_semana), 0, 
                                          test_personas$horas_trab_semana)

test_personas <- test_personas %>% 
  group_by(id) %>% 
  mutate(mean_h = mean(horas_trab_semana, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(horas_trab_semana = if_else(is.na(horas_trab_semana) & test_personas$edad >= 18, 
                                     mean_h, test_personas$horas_trab_semana))

test_personas <- test_personas %>% 
  group_by(id) %>% 
  mutate(variable = ifelse(all(is.na(horas_trab_semana)), 0, 
                           horas_trab_semana)) %>% 
  ungroup() %>% 
  mutate(horas_trab_semana = if_else(is.na(horas_trab_semana), 
                                     variable, test_personas$horas_trab_semana))

test_personas$Ingtot <- NA

#Nos quedamos ocn las variables de interés

test_personas <- subset(test_personas, select = c("id", "Orden", "Clase",
                                                  "ciudad", "edad", "edad_2","ocupado", "mujer", 
                                                  "estudiante", "busca_trabajo","jefe_hogar", 
                                                  "ama_casa", "hijos_hogar", "primaria", 
                                                  "secundaria", "media", "superior", "Ingtot",
                                                  "exp_trab_actual",
                                                  "horas_trab_semana"))

test_personas$num_menores <- as.numeric(test_personas$edad < 18)

# Pasar la base únicamente a hogares

test_personas_hogar <- test_personas %>% group_by(id) %>%
  summarize(edad = mean(edad),
            edad_2 = mean(edad_2),
            ocupado = mean(ocupado), #mean
            mujer = mean(mujer), #mean
            estudiante = mean(estudiante),
            busca_trabajo = mean(busca_trabajo),
            jefe_hogar = mean(jefe_hogar),
            ama_casa =mean(ama_casa),
            hijos_hogar = mean(hijos_hogar),
            primaria = mean(primaria),
            secundaria = mean(secundaria),
            media = mean(media),
            superior = mean(superior),
            Ingtot = sum(Ingtot), #Se suma porque es por individuo
            exp_trab_actual = mean(exp_trab_actual),
            horas_trab_semana = mean(horas_trab_semana),
            num_menores = sum(num_menores),
            ciudad = first(ciudad))

############
## En la base test_hogares organizo las variables de interés

# en la base train_hogares

# Limpiando la base de datos

# Arrienda
test_hogares$arrienda <- ifelse(test_hogares$P5090 == 3, 1, 0)
test_hogares$arrienda[test_hogares$P5090 != 3] <- 0
test_hogares$arrienda[is.na(test_hogares$arrienda)] <- 0

# Ciudad

test_hogares <- rename(test_hogares, c("ciudad" = "Dominio"))

# Número de Cuartos

test_hogares <- rename(test_hogares, c("numero_cuartos" = "P5010"))

# Número de personas

test_hogares <- rename(test_hogares, c("numero_personas" = "Nper"))

# Creao las variables faltantes
test_hogares$Pobre <- NA
test_hogares$Ingtotug <- NA

# Nos quedamos con las variables de interés
test_hogares_final <- subset(test_hogares, select = c("id", "Clase",
                                                      "ciudad", "Ingtotug",  "Pobre", 
                                                      "numero_personas", "arrienda"))


# Haciendo join sobre la base train
test <- left_join(test_personas_hogar,test_hogares_final)

############


# Compruebo que la imputación y base haya quedado sin missing
# Ver variables con datos missing en la base completa train
datos_miss_train <- skim(train) %>% select( skim_variable, n_missing)

Nobs= nrow(train) 
Nobs

datos_miss_train<- datos_miss_train %>% mutate(p_missing= n_missing/Nobs)
head(datos_miss_train)

# Ordenar variables con mayor número de missing
datos_miss_train <- datos_miss_train %>% arrange(-n_missing)
datos_miss_train <- datos_miss_train %>% filter(n_missing!= 0)

#Visualizando la estructura de los missing
ggplot(datos_miss_train, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5)) 

# Ver variables con datos missing en la base completa test
datos_miss_test <- skim(test) %>% select( skim_variable, n_missing)

Nobs= nrow(test) 
Nobs

datos_miss_test <- datos_miss_test %>% mutate(p_missing= n_missing/Nobs)
head(datos_miss_test)

# Ordenar variables con mayor número de missing
datos_miss_test <- datos_miss_test %>% arrange(-n_missing)
datos_miss_test<- datos_miss_test %>% filter(n_missing!= 0)

#Visualizando la estructura de los missing
ggplot(datos_miss_test, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5)) 

############
