## =======================================
##
##      Aula 10: Modelos de Contagem
##
## =======================================

## instalar e abrir pacotes ======================

pacman::p_load(tidyverse, rio, janitor, sjPlot,
               stargazer, inspectdf, Metrics,
               DHARMa, dataverse)

## baixar base ===================================

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

(dataset <- get_dataset("doi:10.7910/DVN/ARKOTI"))

writeBin(get_file("PESenergy.csv", "doi:10.7910/DVN/ARKOTI"), 
         "PESenergy.csv")

PESenergy <- import("PESenergy.csv")

## ===============================================
# Problema:
# TV coverage of energy
# policy is a function of six terms 
# for presidential speeches, an indicator for
# the Arab oil embargo, an indicator for the 
# Iran hostage crisis, the price
# of oil, presidential approval, and the 
# unemployment rate.
## ===============================================

glimpse(PESenergy)
summary(PESenergy)
inspect_na(PESenergy, show_plot = T)

## distribuicao variavel dependente =================

PESenergy %>% 
  ggplot(aes(Energy)) + geom_density()

PESenergy %>% 
  ggplot(aes(Energy)) + geom_histogram()

PESenergy %>% 
  summarise(media = mean(Energy),
            var = sd(Energy))


## Modelo Poisson =====================================

energy.poisson <- glm(Energy~rmn1173+grf0175+grf575+jec477+
                      jec1177+jec479+embargo+hostages+oilc+Approval+Unemploy,
                    family=poisson(link=log),data=PESenergy)

stargazer(energy.poisson, type = "text")

## odds ratio =================================

exp(energy.poisson$coefficients[-1])
100*(exp(energy.poisson$coefficients[-1])-1)

## probabilities ==============================

plot_model(energy.poisson)

plot_model(energy.poisson, terms = "Approval",
           type = "pred")

ggeffects::ggpredict(energy.poisson, terms = "Approval")

# teste sobredispersao ========================

library(AER)
options(scipen = 999)
dispersiontest(energy.poisson, trafo = NULL, alternative = c("greater", "two.sided", "less"))
# c = 0: equidispersion

library(DHARMa)
sim_fmp <- simulateResiduals(energy.poisson)
testOverdispersion(sim_fmp)
plotSimulatedResiduals(sim_fmp)



## Binomial Negativo =========================

library(MASS)
library(stargazer)
library(broom)
library(metrics)

energy.nb<-glm.nb(Energy~rmn1173+grf0175+grf575+jec477+
                    jec1177+jec479+embargo+hostages+oilc+Approval+Unemploy,
                  data=PESenergy)

sim_cmp <- simulateResiduals(energy.nb)
testOverdispersion(sim_cmp)
plotSimulatedResiduals(sim_cmp)

stargazer(energy.poisson, energy.nb, type = "text")

teste <- augment(energy.nb) %>% 
  dplyr::select(Energy, .fitted) 

teste1 <- augment(energy.poisson) %>% 
  dplyr::select(Energy, .fitted) 

plot_model(energy.nb, type = "diag")
plot_kfold_cv(PESenergy, fit = energy.poisson)
plot_kfold_cv(PESenergy, fit = energy.nb)
