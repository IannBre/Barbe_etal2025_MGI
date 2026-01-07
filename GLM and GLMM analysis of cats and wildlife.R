-------
title: "Non-lethal management of Felis catus on a protected island: Outcomes of a successful ongoing socio-ecological strategy in Martín García Island Reserve, Argentina"
author: "Ian Barbe, Lucía Inés Rodríguez-Planes, María del Rosario Jacoby, Andrea Szmelc, Gloria Domínguez, Nazareno Asín, María Eugenia Cueto, María Marcela Orozco"
CA_email: "marcelaorozco.vet@gmail.com"
year: 2026
about: "GLM and GLMM cats and fauna analysis"
---

library(tidyverse)
library(readxl)
library(lme4)
library(ggeffects)
library(performance)
library(emmeans) 
library(writexl)
library(magrittr) 
library(MuMIn) 
library(glmmTMB) 
library(MASS) 
library(pscl)
library(gridExtra)


# GLMM Sighted cats #########################

#Medium zone
cats <- read_excel("Fig3.SightedCats18-21_GLMM.xlsx") %>%
  rename(abundance = 1) %>%
  mutate(year = factor(year)) %>%
  mutate (index = abundance/effort)%>%
  filter(zone == "Medium")

#Low zone ---
cats_exp <- read_excel("Fig3.SightedCats18-21_GLMM.xlsx") %>%
  rename(abundance = 1) %>%
  mutate(year = factor(year)) %>%
  mutate (index = abundance/effort)%>%
  filter(zone != "High") %>% 
  filter(zone == "Low")

#year as character
cats_exp$year <- as.character(cats_exp$year)
class(cats_exp$year)
cats_exp$abundance

##GLMM medium zone model for sighted cats--
library(lme4)
library(AICcmodavg)
library(ggeffects)
library(emmeans)
library(performance)

# Modelo nulo
m0 <- glmer(
  abundance ~ offset(log(effort)) + (1 | transect),
  family = poisson,
  data = cats
)

# Año como efecto fijo
m1 <- glmer(
  abundance ~ year + offset(log(effort)) + (1 | transect),
  family = poisson,
  data = cats
)

#Model selection
cand_models <- list(m0, m1)
mod_names <- c("Null", "Year")

aictab(
  cand.set = cand_models,
  modnames = mod_names,
  second.ord = TRUE
)

aictab

#best model and predictions
best_cat_model <- m1
summary(best_cat_model)

cats_pred <- ggpredict(
  best_cat_model,
  terms = "year",
  type = "random"   # incluye variabilidad del efecto aleatorio
)

cats_pred

#eemeans
emms_cats <- emmeans(
  best_cat_model,
  ~ year,
  type = "response"
)

# Predicted means + 95% CI
summary(emms_cats)

# Pairwise comparisons between years (Tukey)
pairs(emms_cats, adjust = "tukey")
confint(pairs(emms_cats, adjust = "tukey"))

#Chequeos
check_overdispersion(best_cat_model)
check_model(best_cat_model)


### L. wiegmannii lizard GLM ###################

l.w_lizard <- read_excel("Fig4.Lw_lizard18-21_GLM.xlsx") %>%
  rename(abundance = 1) %>%
  mutate(year = factor(year)) %>%
  mutate(index = abundance/effort)

#model
mod_lizard1 <- glm(abundance ~ year +  offset(log(effort)),
                   family = "poisson", data = l.w_lizard)


check_overdispersion(mod_lizard1)
check_model(mod_lizard1)

#'[Binomial negativa'] 
mod_lizard2 <- glmmTMB(abundance ~ year,data=l.w_lizard,
                       family="nbinom2") 

check_overdispersion(mod_lizard2)
check_model(mod_lizard2)

#AIC
AIC(mod_lizard1,mod_lizard2)

summary(mod_lizard2)
l.w_lizard$predicted <- predict(mod_lizard2, type= "response") 
lizard_predindex <- l.w_lizard$predicted/l.w_lizard$effort

lizard_predict <- ggpredict(mod_lizard2, terms = "year", type = "fixed")
lizard_predict

## emmeans
mod_lizard2 %>% 
  emmeans(list(pairwise~year), type = "response")

#CI
emms_lizard <- emmeans(mod_lizard2, "year", type="response")
summary(emms_lizard)
confint(pairs(emms_lizard))

### Caprimulgus spp. GLM#################################

nightjars <- read_excel("Fig4b.Nightjars18-21_GLM.xlsx") %>% 
  rename(abundance = 1) %>%
  mutate(year = factor(year))

#model
mod_ng <- glm(abundance ~ year +  offset(log(effort)),
              family = "poisson", data = nightjars)
check_overdispersion(mod_ng)
check_model(mod_ng)

summary(mod_ng)

nightjars$predicted <- predict(mod_ng, type= "response") 
nightjars_predindex <- nightjars$predicted/nightjars$effort

# predicted values
nightjars_predict <- ggpredict(mod_ng, terms = "year", type = "fixed")
nightjars_predict

## emmeans
mod_ng %>% 
  emmeans(list(pairwise~year), type = "response")
#CI
emms_ng <- emmeans(mod_ng, "year", type="response")
summary(emms_ng) 
confint(pairs(emms_ng))

## Tegu lizard GLMM ###################################################################
tegu <- read_excel("R data/Fig5.Tegu18-21_GLMM.xlsx")%>%
  rename(abundance = 1) %>%
  mutate(year = factor(year))

##GLMM model
m_full <- glmer(
  abundance ~ year * zone + (1 | transect) + offset(log(effort)),
  family = poisson,
  data = tegu
)

m_add <- glmer(
  abundance ~ year + zone + (1 | transect) + offset(log(effort)),
  family = poisson,
  data = tegu
)

m_year <- glmer(
  abundance ~ year + (1 | transect) + offset(log(effort)),
  family = poisson,
  data = tegu
)

m_zone <- glmer(
  abundance ~ zone + (1 | transect) + offset(log(effort)),
  family = poisson,
  data = tegu
)

m_null <- glmer(
  abundance ~ 1 + (1 | transect) + offset(log(effort)),
  family = poisson,
  data = tegu
)

#models
models <- list(
  full = m_full,
  additive = m_add,
  year = m_year,
  zone = m_zone,
  null = m_null
)

aictab(
  cand.set = models,
  modnames = names(models)
)

#best model
best_tegu <- m_add  

check_overdispersion(best_tegu)
summary(best_tegu)

#emeans year
emms_tegu_year <- emmeans(best_tegu, ~ year, type = "response")
summary(emms_tegu_year)

pairs(emms_tegu_year, type = "response")
confint(pairs(emms_tegu_year))

#emmeans zone
emms_tegu_zone <- emmeans(best_tegu, ~ zone, type = "response")
summary(emms_tegu_zone)

pairs(emms_tegu_zone, type = "response")
confint(pairs(emms_tegu_zone))


