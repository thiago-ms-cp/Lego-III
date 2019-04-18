##=============================================================
## 
##  Aula 4 - Lego III (Efeitos, dignósticos e interações)
##
##=============================================================
rm(list = ls())
gc()
pacman::p_load(tidyverse, sjPlot, janitor, pscl, pROC, heatmapFit)

source("https://sites.google.com/a/stonybrook.edu/mperess/r0.r")

# load the data
statadata <- read.dta("https://sites.google.com/a/stonybrook.edu/mperess/teaching/classdata/civwar.dta")

## selecionar variaveis ===========================================

statadata <- statadata %>% 
  dplyr::select(war, lpop, western, muslim, polity2, gdpen, ethfrac) %>% 
  mutate(western = factor(western))


## modelo logistico ===============================================

logit1 <- glm(war ~ lpop + western + muslim + polity2 + gdpen + ethfrac, 
              data = statadata, family = "binomial")
summary(logit1)


##================================================================
## Diagnósticos 
##=================================================================

## McFadden ======================================================

library(pscl) # pacote com pseudo-R2
pR2(logit1)


## Tabela de contingência =========================================


probabilities <- logit1 %>% 
  predict(statadata, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == statadata$war, na.rm = T)

# Proporção correta das predicoes = 0.86

## Curva de ROC ==================================================

library(pROC)

statadata <- statadata %>% 
  dplyr::select(war, lpop, western, muslim, polity2, gdpen, ethfrac) %>% 
  filter(complete.cases(.))

rocplot <- roc(war ~ fitted(logit1), data = statadata)
plot.roc(rocplot, legacy.axes = F)
auc(rocplot)

## Heatmap esarey ===============================================

pred <- predict(logit1, type = "response")
heatmapFit::heatmap.fit(statadata$war, pred, reps = 1000)

names(statadata)


##=================================================================
## probabilidades preditas e efeitos marginais Haenmer (2012) 
##=================================================================

get_model_data(logit1, terms = "western", type = "pred")
get_model_data(logit1, terms = "polity2", type = "pred")

plot_model(logit1, type = "eff",
           terms = "western")

plot_model(logit1, type = "eff",
           terms = "polity2")


## Interacoes ===================================================

install.packages("webuse") # pacote com base
webuse::webuse("nhanes2")

mod3 <- glm(highbp ~ sex * agegrp + bmi, 
            data = nhanes2, family = binomial)

plot_model(mod3, type = "pred", terms = c("agegrp ", "sex"))


