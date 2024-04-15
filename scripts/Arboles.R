###CARTs###
require(pacman)
p_load("tidyverse", "ggplot2" )
p_load("rpart")
data_train <- train
dim(data_train)
head(data_train)
summary(data_train)
p_load("modeldata")
mytree <- rpart(Ingtotug ~ edad + edad_2 + mujer + exp_trabajo_actual + superior + horas_trab_semana, data = data_train, control = list(maxdepth=3))
p_load("rpart.plot")
mytree
prp(mytree, under=TRUE, branch.lty = 2, yesno =2,faclen=0, varlen=15,tweak=1.2, clip.facs = TRUE, box.palette = "green", compress=FALSE, ycompress = FALSE, node.fun = function(x, labs, digits, varlen)paste("Ingtotug/n", format(round(exp(mytree$frame$yval),0),nsmall=0, big.mark=",")))

