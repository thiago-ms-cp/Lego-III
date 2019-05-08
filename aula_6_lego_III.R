## =========================================
##
##      AULA 6 - MODELOS MULTINOMIAIS 
##
## =========================================

pacman::p_load(tidyverse, VGAM, nnet,
               effects, sjPlot, inspectdf)

## abrir base ==============================

data("marital.nz")

glimpse(marital.nz)
summary(marital.nz)

## pacote nnet ============================

fit_nnet <- multinom(mstatus ~ age, 
                     data = marital.nz)
summary(fit_nnet)

## pacote vglm  ===========================

fit_vgam <- vglm(mstatus ~ age, 
                 multinomial(refLevel = 1), 
                 data = marital.nz)
summary(fit_vgam)

library(ggeffects)

mydf <- ggpredict(fit_nnet, terms = "age")
g1 <- ggplot(mydf, aes(x, predicted, color = response.level)) + 
  geom_line()

g1 + theme(legend.position = "bottom")

## outro exemplo ==========================

require(MASS)

data(housing)
glimpse(housing)
summary(housing)

model1 <- multinom(Sat ~ Infl + Type + Cont,
                   weights = Freq, 
                   data = housing) 

plot(Effect("Infl", model1))
plot(Effect("Infl", model1), multiline =T)
plot(Effect("Infl", model1),style="stacked") 

mydf <- ggpredict(model1, terms = "Infl")
g2 <- ggplot(mydf, aes(x, predicted, color = response.level)) + 
  geom_line()






