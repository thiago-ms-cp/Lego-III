##===============================================
##
##    Aula 3 Lego II - Modelos Logísticos
##
##===============================================

dir()

pacman::p_load(tidyverse, arm, 
               haven, janitor,
               sjPlot)


## abrir base ===================================

eseb14 <- read_sav("eseb_2014.sav")


## modificar variáveis =========================

names(eseb14) 

eseb14 <- eseb14 %>% 
  clean_names(.) %>% 
  dplyr::select(sexo = d2_sexo, idade = d1a_idade, voto = q5p2b, 
                renda = d20a_fxrendfam,
         religiao = d24_religiao) %>% 
  mutate(renda = ifelse(renda %in% c(98:9999), NA, renda)) %>%
  mutate(religiao = ifelse(religiao %in% c(95:99), NA, religiao)) %>% 
  mutate(religiao = case_when(religiao == 3 ~ "cat",
                           religiao == 5 ~ "evan",
                           TRUE ~ "outros")) %>%
  mutate(voto = ifelse(voto %in% c(50:9999), NA, voto))  %>% 
  mutate(voto = ifelse(voto == 2, 1, 0))  
  

## modelo linear =======================

modelo_lin <- lm(voto ~ renda, 
                 data = eseb14)

summary(modelo_lin)

## modelo generalizado logit ==========

fit_log <- glm(voto ~ renda, 
                  family = binomial(link = "logit"),
                  data = eseb14)

summary(fit_log)

## modelo generalizado probit ==========

fit_prob <- glm(voto ~ renda, 
                      family = binomial(link = "probit"),
                      data = eseb14)

summary(fit_prob)

## interpretacao dos coeficientes ======

invlogit(1.33 - 0.37*1) # primeira faixa de renda
invlogit(1.33 - 0.37*5) # quinta faixa de renda

## odds ratio ===========================

exp(coef(fit_log))

## 1-0.68 = -32% de chances de votar em dilma

## com sjplot ===========================

get_model_data(fit_log, type = "pred")

## incluir religiao =====================

fit_log2 <- glm(voto ~ renda + religiao, 
               family = binomial(link = "logit"),
               data = eseb14)

summary(fit_log2)

get_model_data(fit_log2, type = "pred")

# plotar modelo geral ==================

plot_model(fit_log2)

# plotar relacao X e Y ==================

plot_model(modelo_lin, type = "pred", terms = "renda")
plot_model(fit_log2, type = "pred", terms = "renda")
plot_model(fit_log2, type = "pred", terms = "religiao")


