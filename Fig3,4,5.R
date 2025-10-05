-------
title: "Non-lethal management of Felis catus on a protected island: Outcomes of a successful ongoing socio-ecological strategy in Martín García Island Reserve, Argentina"
author: "Ian Barbe, Lucía Inés Rodríguez-Planes, María del Rosario Jacoby, Andrea Szmelc, Gloria Domínguez, Nazareno Asín, María Eugenia Cueto, María Marcela Orozco"
author_email: "iannbarbe@gmail.com"
year: 2025
about: "GLM and GLMM cats and fauna analysis"
-------

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

setwd("")

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

mod_cats <- glmer(abundance ~ year + (1|transect) + offset(log(effort)),
                  family = "poisson", data = cats)

check_overdispersion(mod_cats)
check_model(mod_cats)
summary(mod_cats)

## Likelihood Ratio Test
mod_cats %>% 
  update(.~.-year) %>% 
  anova(mod_cats, test = "Chisq") #AIC

#predicted values
cats$predicted <- predict(mod_cats, type= "response") 
cats$pred_index <- cats$predicted/cats$effort

#predicted per year
year_predicted <- predict(mod_cats, type="response")
year_predicted

#random predicted values with CI 
cats_predict_random <- ggpredict(mod_cats, terms = "year", type = "random")
cats_predict_random

#emmeans
mod_cats %>% 
  emmeans(list(pairwise~year), type="response"   )

# CI
emms_cats <- emmeans(mod_cats, "year", type="response")  
summary(emms_cats)
confint(pairs(emms_cats))

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
tegu <- read_excel("Fig5.Tegu18-21_GLMM.xlsx")%>%
  rename(abundance = 1) %>%
  mutate(year = factor(year))

##GLMM model

m_sat <- glmer(abundance ~ year*zone + (1|transect) + offset(log(effort)),
               family = "poisson",
               data = tegu)
check_overdispersion(m_sat)
summary(m_sat)

## Likelihood Ratio Test

m_sat %>% 
  update(.~. -year:zone) %>% 
  anova(m_sat, test = "Chisq") # removing the interaction does not significantly reduce the deviance value.It is discarded 
m_sat %>% 
  update(.~. -year:zone - zone) %>% # The zone influences the deviance.
  anova(m_sat, test = "Chisq")
m_sat %>% 
  update(.~. -year:zone - year) %>% # The year influences the deviance.
  anova(m_sat, test = "Chisq")

#best model
mod_tegu <- glmer(abundance ~ year + zone + (1|transect) + offset(log(effort)),
                  family = "poisson", data = tegu)

check_overdispersion(mod_tegu)
check_model(mod_tegu)
summary(mod_tegu)


## predicted values + CI
tegu$predicted <- predict(mod_tegu, type= "response") 
tegu$pred_index <- tegu$predicted/tegu$effort
tegu_predict <- ggpredict(mod_tegu,terms = c("year","zone"))

## emmeans YEAR
mod_tegu %>% 
  emmeans(list(pairwise~year),
          type = "response")
#CI year
emms_tegu_year <- emmeans(mod_tegu, "year", type="response")  
summary(emms_tegu_year)
confint(pairs(emms_tegu_year))

#emmeans ZONE
mod_tegu %>% 
  emmeans(list(pairwise~zone),
          type = "response")
#CI zone
emms_tegu_zone <- emmeans(mod_tegu, "zone", type="response") 
summary(emms_tegu_zone) 
confint(pairs(emms_tegu_zone))