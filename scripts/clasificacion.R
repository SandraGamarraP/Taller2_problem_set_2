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

directorio_destino <- "stores/"

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
        openxlsx
)

train <- read.csv("stores/train_definitivo.csv")
test <- read.csv("stores/test_definitivo.csv")

