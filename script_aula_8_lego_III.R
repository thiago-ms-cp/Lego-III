## ============================================
##
##           AULA 8: Modelos Ordinais
##
## ============================================

pacman::p_load(tidyverse, MASS, VGAM, ordinal,janitor,
               sjPlot)


## exemplo 1: ideologia e partido: simular base ========


party <- factor(rep(c("Rep","Dem"), c(407, 428)), 
                levels=c("Rep","Dem"))  
rpi <- c(30, 46, 148, 84, 99) # cell counts
dpi <- c(80, 81, 171, 41, 55) # cell counts
ideology <- c("Very Liberal","Slightly Liberal","Moderate","Slightly Conservative","Very Conservative")
pol.ideology <- factor(c(rep(ideology, rpi), 
                         rep(ideology, dpi)), levels = ideology)
dat <- data.frame(party,pol.ideology)


## fit proportional odds model ==========================

pom <- polr(pol.ideology ~ party, data=dat)
summary(pom)

## odds ratio ===========================================

exp(coef(pom))
100*(exp(pom$coefficients)-1)

## pseudo R2 =============================================

var(pom$lp)/(var(pom$lp) + (pi^2)/3) # R2

## predicted probabilities ===============================

predict(pom,newdata = data.frame(party="Dem"),type="p")
predict(pom,newdata = data.frame(party="Rep"),type="p")


## Example 3: A study looks at factors that 
## influence the decision of whether to apply 
## to graduate school. 

library(foreign)

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta") %>% 
  as.data.frame()
  

m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)
summary(m)

## odds ratio =========================================

exp(coef(m))

plot_model(m)

## predicted probabilities ============================

plot_model(m, terms = "gpa",
           type = "pred")

predict(m, data.frame(pared=1, 
                public = 1, gpa = mean(dat$gpa)), type="p")

predict(m, data.frame(pared=0, 
                      public = 1, gpa = mean(dat$gpa)), type="p")

ggeffects::ggeffect(m, terms = "pared")

## Mesmos dados de partido e ideologia ======================

party <- factor(rep(c("Rep","Dem"), c(407, 428)), 
                levels=c("Rep","Dem"))  
rpi <- c(30, 46, 148, 84, 99) # cell counts
dpi <- c(80, 81, 171, 41, 55) # cell counts
ideology <- c("Very Liberal","Slightly Liberal","Moderate","Slightly Conservative","Very Conservative")
pol.ideology <- factor(c(rep(ideology, rpi), 
                         rep(ideology, dpi)), levels = ideology)
dat <- data.frame(party,pol.ideology)


dat <- dat %>% 
  mutate(pol.ideology = ordered(factor(pol.ideology)))

## Modelos proporcionais e não proporcionais ==================

mod1 <- vglm(pol.ideology ~ party,family=cumulative(parallel = T),
             data=dat)

summary(mod1)
mod2 <- vglm(pol.ideology ~ party, family=cumulative,
             data=dat)

summary(mod2)

anova(mod1, mod2, type = 1)

