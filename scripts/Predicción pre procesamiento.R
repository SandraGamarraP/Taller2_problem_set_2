# Predicción de salarios para la prediccón de la pobreza

# 1. Pre-procesamineto de las bases

# 1.1 Importar librerias
require("pacman")
p_load(tidyverse, # tidy-data
       rpart, # Recursive Partition and Regression Trees (To run Trees)
       caret ,  # for model training and tunning
       rpart.plot, ## for trees graphs
       Metrics, ## Evaluation Metrics for ML
       ipred,  # For Bagging 
       ranger, #For random Forest
       adabag)  

# 1.2 Cargar bases de datos 

train_hogares_org<-read.csv("stores/train_hogares.csv")
train_personas_org<-read.csv("stores/train_personas.csv")
test_hogares_org<-read.csv("stores/test_hogares.csv")
test_personas_org<-read.csv("stores/test_personas.csv")

# 1.2 Agrega columnas de resultado a df de test
test_hogares_org$Pobre <- NA
test_hogares_org$Ingtotug <- NA

# 1.3 Procesamiento de todas las variables disponibles en conjuntos test
# Nota: No se revisan variables adicionales disponibles en datos de entrenamiento, pues no se podrán utilizar para el test.
train_hogares <- train_hogares_org %>% select(colnames(test_hogares_org))
train_personas <- train_personas_org %>% select(colnames(test_personas_org))

# 1.3.1 Se crea función para preprocesamiento hogares
pre_process_hogares<-  function(data, ...) {
  
  data <- data %>% mutate(
    tot_cuartos = P5000, # total cuartos
    tot_cuartos_dorm = P5010, # En cuantos de esos cuartos duermen
    tipo_vivienda = factor(P5090,levels=c(1:6), labels=c('Propia pagada','Propia pagando', 'Arriendo','Usufructo', 'Posesión','Otra')), # Tipo vivienda
    cuota_amort = ifelse(P5100<=99,0,P5100), # Cuota amortizacion, 98 no sabe, 99 no informa
    estim_arriendo = ifelse(P5130<=99,0,P5130), # Estimacion de si pagara arriendo, 99 no informa
    cuota_arriendo = P5140, # Cuota real del arriendo
    nper = Nper, # numero personas en el hogar
    npersug = Npersug, # numero personas en unidad de gasto
    li = Li, # Linea de indigencia
    lp = Lp, # Linea de pobreza
    clase = Clase, # 1 cabecera, 2 resto
  ) %>% 
    select(id, clase,tot_cuartos,tot_cuartos_dorm,tipo_vivienda,cuota_amort,cuota_arriendo,nper,li,lp,Pobre,Ingtotug)
}

# 1.3.2 Se crea función para preprocesamiento personas
pre_process_personas<-  function(data, ...) {
  
  data <- data %>% mutate(
    #clase = Clase, # 1 cabecera, 2 resto
    orden = Orden, # identificador de la persona dentro del hogar 
    mujer = ifelse(P6020==2,1,0), # Sexo, 2 mujer, 1 hombre 
    jefe = ifelse(P6050==1,1,0), # es jefe de hogar o no
    # edad = P6040, # años cumplidos
    # parentesco = factor(P6050,levels=c(1:9),labels=c('Jefe','Pareja','Hijo','Nieto','Otro pariente','Empleado','Pensionista','Trabajador','Otro no pariente')), # Parentesco con jefe de hogar
    salud = ifelse(P6090==1,1,0), # Afiliado a salud: 1 tiene, 2 no tiene, 9 no sabe
    regsalud = ifelse(P6100>2,0,1), # Regimen de salud: 1-contributivo, 2-especial, 3-subsidiado, 9-no sabe
    nivel_educ = ifelse(P6210==5 | P6210 == 6, 1,0), # Nivel educativo (se codifica: 1-media/superior, 0-no)
    # P6210s1 - grado hasta el que llego
    # P6240 - actividad en que ocupó el itmepo la semana pasada
    # Oficio
    # P6426 - tiempo trabajando continuo
    # P6430 - tipo de trabajo (obrero o trabajador particualr, de gobierno, domestico, independiente, patron, etc)
    # P6510 - horas extra?
    # P6545 - primas?
    # P6580 - bonificaciones?
    # P6585s1 - auxilio alimentación?
    sub_trans = ifelse(P6585s2 == 1, 1,0), #P6585s2 auxilio transporte, 1 - tiene, 2 - no tiene, 3 - no sabe
    # P6585s3 - subsidio familiar?
    # P6585s4 - subsidio educativo?
    # P6590 - recibio alimentos como parte de salaraio?
    # P6600 - recibio vivienda como parte de salaraio?
    # P6610 - usa transporte de empresa?
    # P6620 - recibio ingresos en especie como parte de salaraio?
    # P6630s1 - prima servicios?
    # P6630s2 - prima navidad?
    # P6630s3 - prima vacaciones?
    # P6630s4 - viaticos permanente / bonificaciones anuales
    # P6630s6 - bonificaciones anuales?
    # P6800 - horas a la semana que trabaja
    # P6870 - numero de personas de la empresa donde trabaja
    pension = ifelse(P6920 == 1 | P6920 == 3, 1, 0),# P6920 - cotizando en pensiones
    # P7040 - segundo trabajo o negocio?
    # P7045 - horas trabajadas en segundo negocio
    # P7050 - ocupacion segunda actividad (tipo de oficio)
    # P7090 - quiere trabajar más horas?
    # P7110 - diligencias para trabajar más horas?
    # P7120 - disponible par atrbaajar mas horas?
    # P7150 - hizo diligencias para cambiar de trabajo
    # P7160 - puede empezar antes de 1 mes
    # P7310 - 
    # P7350 - si es desocupado, que era el ultimo trabajo
    # P7422 - recibio ingresos por trabajo el mes pasado?
    # P7472 - ""
    # P7495 - recibio arriendos o pensiones?
    # P7500s2 - 
    # P7500s3
    # P7505 - recibio otros dineroes?
    # P7510s1
    ayu_inst = ifelse(P7510s3==1,1,0), #Iof3h - recibió ayuda por instituciones ultimos 12 meses, 1- si 2 - no , 3 no sabe 
    otros_ing = ifelse(P7510s5 == 1, 1, 0), #Ult. 12 meses recibio  dinero por inversiones
    #ing_arri = ifelse(P7500s1 == 1, 1, 0), #recibió pago por arriendo 1- si, 2 - no, 3 - no sabe
    # P7510s2
    # P7510s3
    # P7510s5
    # P7510s6
    # P7510s7
    pet = ifelse(Pet==1,1,0),# Pet - poblacion en edad de trabajar
    # Oc - ocupado si o no
    desocupado = ifelse(Des==1,1,0)# Des - desocupado si o no
    # Ina - inactivo
    # Fex_c
    # Depto
    # Fex_dpto
  ) %>% 
    select(id, orden, mujer,jefe,salud,regsalud,nivel_educ,
           pension,pet,desocupado, sub_trans, otros_ing, ayu_inst)
  
  data <- data %>%
    mutate(
      jefe_mujer = ifelse(mujer & jefe,1,0),
      jefe_salud_cont = ifelse(regsalud & jefe,1,0),
      jefe_educ_sup = ifelse(nivel_educ & jefe,1,0),
      jefe_pension = ifelse(pension & jefe,1,0),
    ) %>%
    select(id, orden, mujer,jefe,salud,regsalud,nivel_educ,
           pension,pet,desocupado, sub_trans, otros_ing, ayu_inst,
           )
  
}

# 1.4.1 Se preprocesan datos de hogares
train_hogares<- pre_process_hogares(train_hogares)
test_hogares<- pre_process_hogares(test_hogares_org)

# 1.4.2 Se preprocesan datos de personas
train_personas<- pre_process_personas(train_personas)
test_personas<- pre_process_personas(test_personas_org)


# 1.5 Agrupacion de indicadores de personas a hogar

# Se crea funcion para aplicar tanto a train como a test
group_personas_hogar<-  function(data, ...) {
  
  data <- data %>% 
    group_by(id) %>% 
    summarize(nmujeres=sum(mujer,na.rm=T),
              nsalud=sum(salud,na.rm=T),
              neducsup=sum(nivel_educ,na.rm=TRUE),
              npension=sum(pension,na.rm=TRUE),
              npet = sum(pet,na.rm=TRUE),
              ndes = sum(desocupado,na.rm = TRUE),
              jefe_mujer = sum(jefe,na.rm=TRUE),
              jefe_salud_cont = sum(salud,na.rm=TRUE),
              jefe_educ_sup = sum(nivel_educ,na.rm=TRUE),
              sub_transporte = sum(sub_trans, na.rm = T), 
              otros_ing = sum(otros_ing, na.rm = T), 
              ayu_inst = sum(ayu_inst, na.rm = T),
              #ing_arri = sum(ing_arri, na.rm = T)
              
    )
}

# Se agrupan datos de personas por hogar
train_personas_grouped <- group_personas_hogar(train_personas)
test_personas_grouped <- group_personas_hogar(test_personas)

# Se juntan las bases de datos
train <- left_join(train_hogares, train_personas_grouped, by='id')
test <- left_join(test_hogares, test_personas_grouped, by='id')

# 2 Indicadores de proporción o porcentaje
train <- train %>%
  mutate(
    porc_mujer = nmujeres/nper,
    porc_salud = nsalud/nper,
    porc_educ_sup = neducsup/npet,
    porc_pension = npension/npet,
    porc_pet = npet/nper,
    porc_des = ndes/npet
  )
test <- test %>%
  mutate(
    porc_mujer = nmujeres/nper,
    porc_salud = nsalud/nper,
    porc_educ_sup = neducsup/npet,
    porc_pension = npension/npet,
    porc_pet = npet/nper,
    porc_des = ndes/npet
  )

# 3. Exportación de bases de datos
write.csv(train, "stores/train_procesado_m.csv", row.names = FALSE)
write.csv(test, "stores/test_procesado_m.csv", row.names = FALSE)

