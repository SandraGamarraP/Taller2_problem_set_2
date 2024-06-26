############################################
########## Problem set 2            ########
###### Big Data y Maching Learning #########
###### Paula Osorio:201327186   ############
###### Sandra Gamarra: 202225782 ###########
###### Erika M. Macías:  Educacion continua#  
###### Ingrith Sierra: 201720654    ########
############################################


#rm(list = ls())

# Especificar Directorio

# directorio_destino <- "C:/Users/sandr/Documents/GitHub/BIG DATA/"

# Cargando librerias y paquetes necesario

library(pacman)
p_load(rvest, 
       tidyverse, 
       ggplot2, 
       robotstxt, 
       psych, 
       stargazer, 
       boot, 
       openxlsx, 
       rio, 
       GGally, 
       scales)


# Elaboramos las Estadísticas Descriptivas de la base de Entrenamiento


Tabla_descriptivas2 <- train[c("porc_mujer","porc_salud", 
                            "porc_educ_sup", "porc_pension",
                             "porc_pet", "porc_des", "Pobre")]

estadisticas_train <- data.frame(sapply(Tabla_descriptivas2, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_train, file = "Tabla_descriptivas_train2.xlsx")

# Estadistivas para los que son es pobre

Tabla_pobres <- Tabla_descriptivas2[Tabla_descriptivas2$Pobre == 1, ]

estadisticas_pobres_train <- data.frame(sapply(Tabla_pobres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_pobres_train, file = "Tabla_pobres1.xlsx")

# Estadísticas para los No pobres

Tabla_no_pobres <- Tabla_descriptivas2[Tabla_descriptivas2$Pobre == 0, ]

estadisticas_no_pobres_train <- data.frame(sapply(Tabla_no_pobres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_no_pobres_train, file = "Tabla_no_pobres1.xlsx")


# Elaboramos las Estadísticas Descriptivas de la base de testeo o prueba

Tabla_descriptivas3 <- test[c("porc_mujer","porc_salud", 
                              "porc_educ_sup", "porc_pension",
                              "porc_pet", "porc_des", "Pobre")]

estadisticas_test <- data.frame(sapply(Tabla_descriptivas3, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_test, file = "Tabla_descriptivas_test2.xlsx")


# Gráfico de dispersión de las variables

g_1<- train[c("porc_mujer","porc_salud", 
               "porc_educ_sup", "porc_pension",
               "porc_pet", "porc_des", "Pobre")]

g_1$Pobre <- factor(g_1$Pobre, labels = c("No pobre", "Pobre"))

grafico_1 <- ggpairs(g_1, columns = 2:6, ggplot2::aes(colour = Pobre)) +
  theme_minimal() + 
  labs(title = "Matriz de dispersión de las variables") + 
  scale_y_continuous(labels = scales::number_format()) + 
  scale_x_continuous(labels = scales::number_format()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 8))

graphics.off()
print(grafico_1)

###########

