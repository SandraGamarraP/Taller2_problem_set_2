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


# Unión de las bases 

train <- left_join(train_personas,train_hogares)
test <- left_join(test_personas,test_hogares)

# mirar si se realizó bien la unión
train %>% 
  group_by(id) %>% 
  summarise(count = n()) %>% 
  summary()

test %>% 
  group_by(id) %>% 
  summarise(count = n()) %>% 
  summary()

# Creando las variables faltantes 

test$Pobre <- NA
test$Ingtot <- NA
test$Ingtotug <- NA

# Limpiando la base de datos

objetos <- c("train", "test")

for (obj in objetos) {
  
  database <- get(obj)
  
  
  # En Edad solo seleccionamos los mayores de 18 años
  
  database <- rename(database, c("edad" = "P6040"))
  database$edad_2 <- database$edad^2
  
  # Ocupado 
  database$ocupado <- ifelse(database$Oc == 1, 1, 0)
  database$ocupado[database$Oc != 1] <- 0
  database$ocupado[is.na(database$ocupado)] <- 0
  
  # Género
  
  database$mujer <- ifelse(database$P6020 == 2, 1, 0)
  database$mujer[database$P6020 == 1] <- 0
  
  # Estudiante
  
  database$estudiante <- ifelse(database$P6240 == 3, 1, 0)
  database$estudiante[database$P6240 != 3] <- 0
  database$estudiante[database$P6240 == "."] <- 0
  database$estudiante[is.na(database$estudiante)] <- 0
  
  # Busca trabajo
  
  database$busca_trabajo <- ifelse(database$P6240 == 2, 1, 0)
  database$busca_trabajo[database$P6240 != 2] <- 0
  database$busca_trabajo[database$P6240 == "."] <- 0
  database$busca_trabajo[is.na(database$busca_trabajo)] <- 0
  
  # Ama de casa
  
  database$ama_casa <- ifelse(database$P6240 == 4, 1, 0)
  database$ama_casa[database$P6240 != 4] <- 0
  database$ama_casa[database$P6240 == "."] <- 0
  database$ama_casa[is.na(database$ama_casa)] <- 0
  
  # Jefe del hogar
  database$jefe_hogar <- ifelse(database$P6050 == 1, 1, 0)
  database$jefe_hogar[database$P6050 != 1] <- 0
  database$jefe_hogar[database$P6050 == "."] <- 0
  database$jefe_hogar[is.na(database$jefe_hogar)] <- 0
  
  # Hijos en el hogar
  
  database$hijos_hogar <- ifelse(database$P6050 == 3, 1, 0)
  database$hijos_hogar[database$P6050 != 3] <- 0
  database$hijos_hogar[database$P6050 == "."] <- 0
  database$hijos_hogar[is.na(database$hijos_hogar)] <- 0
  
  # Nivel Primaria
  
  database$primaria <- ifelse(database$P6210 == 1, 1, 0)
  database$primaria[database$P6210 == "."] <- 0
  database$primaria[is.na(database$primaria)] <- 0
  
  # Nivel Secundaria
  
  database$secundaria <- ifelse(database$P6210 == 4, 1, 0)
  database$secundaria[database$P6210 == "."] <- 0
  database$secundaria[is.na(database$secundaria)] <- 0
  
  # Nivel Media
  
  database$media <- ifelse(database$P6210 == 5, 1, 0)
  database$media[database$P6210 == "."] <- 0
  database$media[is.na(database$media)] <- 0
  
  # Nivel Superior
  
  database$superior <- ifelse(database$P6210 == 6, 1, 0)
  database$superior[database$P6210 == "."] <- 0
  database$superior[is.na(database$superior)] <- 0
  
  # Arrienda
  database$arrienda <- ifelse(database$P5090 == 3, 1, 0)
  database$arrienda[database$P5090 != 3] <- 0
  database$arrienda[is.na(database$arrienda)] <- 0
  
  # Experiencia trabajo actual
  
  database <- rename(database, c("exp_trab_actual" = "P6426"))
  
  # Horas de trabajo a la semana
  
  database <- rename(database, c("horas_trab_semana" = "P6800"))
  
  
  # Ciudad
  
  database <- rename(database, c("ciudad" = "Dominio"))
  
  # Número de Cuartos
  
  database <- rename(database, c("numero_cuartos" = "P5010"))
  
  # Número de personas
  
  database <- rename(database, c("numero_personas" = "Nper"))
  
  # Imputación de experiencia
  
  database$exp_trab_actual <- ifelse(database$edad < 18 & 
                                       is.na(database$exp_trab_actual), 0, 
                                     database$exp_trab_actual)
  
  database <- database %>% 
    group_by(id) %>% 
    mutate(mean_exp = mean(exp_trab_actual, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(exp_trab_actual = if_else(is.na(exp_trab_actual) & database$edad >= 18, 
                                     mean_exp, database$exp_trab_actual))
  
  database <- database %>% 
    group_by(id) %>% 
    mutate(variable = ifelse(all(is.na(exp_trab_actual)), 0, 
                             exp_trab_actual)) %>% 
    ungroup() %>% 
    mutate(exp_trab_actual = if_else(is.na(exp_trab_actual), 
                                     variable, database$exp_trab_actual))
  
  # Imputación de Horas trabajadas a la semana
  
  database$horas_trab_semana <- ifelse(database$edad < 18 & 
                                         is.na(database$horas_trab_semana), 0, 
                                       database$horas_trab_semana)
  
  database <- database %>% 
    group_by(id) %>% 
    mutate(mean_h = mean(horas_trab_semana, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(horas_trab_semana = if_else(is.na(horas_trab_semana) & database$edad >= 18, 
                                       mean_h, database$horas_trab_semana))
  
  database <- database %>% 
    group_by(id) %>% 
    mutate(variable = ifelse(all(is.na(horas_trab_semana)), 0, 
                             horas_trab_semana)) %>% 
    ungroup() %>% 
    mutate(horas_trab_semana = if_else(is.na(horas_trab_semana), 
                                       variable, database$horas_trab_semana))
  
  
  
  database <- subset(database, select = c("id", "Orden", "Clase",
                                          "ciudad", "edad", "edad_2","ocupado", "mujer", 
                                          "estudiante", "busca_trabajo","jefe_hogar", 
                                          "ama_casa", "hijos_hogar", "primaria", 
                                          "secundaria", "media", "superior", "Ingtot",
                                          "Ingtotug", "exp_trab_actual",
                                          "horas_trab_semana", "Pobre", 
                                          "numero_personas", "arrienda"))
  
  database$num_menores <- as.numeric(database$edad < 18)
  
  # Pasar la base únicamente a hogares
  
  database <- database %>% group_by(id) %>%
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
              Ingtotug = mean(Ingtotug), # se promedia porque es por hogar
              exp_trab_actual = mean(exp_trab_actual),
              horas_trab_semana = mean(horas_trab_semana),
              Pobre = mean(Pobre),
              numero_personas = sum(numero_personas),
              num_menores = sum(num_menores),
              arrienda = mean(arrienda),
              ciudad = first(ciudad))
  
  assign(obj, database)
  rm(database)
  
}

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
