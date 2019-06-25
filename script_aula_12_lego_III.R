## ============================================
##
##     SCRIPT AULA 12 - SOBREVIVENCIA
##
## ============================================

pacman::p_load(survival, KMsurv, survminer ,simPH,
               tidyverse, rms, ggfortify,
               janitor, stargazer)

getHdata(prostate)



## =====================================
## Variaveis:
## rx - dose de estrogeno, 
## age - idade, 
## wt - peso,
## pf - performance, 
## status - censura,
## dtime - tempo
## =====================================

## conhecendo os dados =============================

glimpse(prostate)
summary(prostate)

prostate %>% 
  tabyl(status)

prostate <- prostate %>% 
  mutate(status = ifelse(status != "alive", 1, 0))  

## conhecendo a variavel dependente ===============

modelo1 <- survfit(Surv(dtime, status) ~ 1,
                   conf.type = "log-log", data = prostate)

modelo1

summary(modelo1)

simPH::cox(modelo1, nsim = 100)

stargazer(modelo1, type = "text")
ggsurvplot(modelo1)

g1 <- autoplot(modelo1, censor.shape = "", conf.int = T)

library(plotly)

ggplotly(g1)


## associação bivariada ==================================

modelo2 <- survfit(Surv(dtime, status) ~ rx,
                   conf.type = "log-log", data = prostate)

autoplot(modelo2, censor.shape = "", conf.int = T)

autoplot(modelo2, censor.shape = "", conf.int = F)

modelo3 <- survfit(Surv(dtime, status) ~ pf,
                   conf.type = "log-log", data = prostate)

autoplot(modelo3, censor.shape = "", conf.int = F)


## regressoes ============================================

prostate <- prostate %>% 
  filter(complete.cases(.))

modelo4 <- coxph(Surv(dtime, status) ~ rx + age + wt + 
                 pf, data = prostate)


## diagnosticos ==========================================

# martingale

prostate$resid_mart <- residuals(modelo4, type = "martingale")

prostate %>% 
  ggplot(aes(x = age, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  labs(title = "age") +
  theme_bw() + theme(legend.key = element_blank())


# Schoenfeld 

test.ph <- cox.zph(modelo4)

# p < 0.05 = não proporcional

test.ph

ggcoxzph(test.ph)


# outliers

ggcoxdiagnostics(modelo4, type = , linear.predictions = TRUE)

# interação entre tempo e idade 

modelo5 <- coxph(Surv(dtime, status) ~ rx + age*dtime + wt + 
                   pf, data = prostate)


test.ph <- cox.zph(modelo5)
test.ph





