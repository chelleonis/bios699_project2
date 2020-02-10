#longitudinal 
library(lme4)
library(lmerTest)
library(dplyr)
library(survey)

#https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/
#random, year/ID?

smoke_glmix <- smoke %>% 
  mutate(outcome = case_when(ecigs_everysome == "none" ~ 0,
                             ecigs_everysome == "everyday or some days" ~ 1)) %>%
  mutate(wt_long = case_when(wave == 1 ~ R01_A_PWGT,
                             wave == 2 ~ R02_A_PWGT,
                             wave == 3 ~ R03_A_AWGT,
                             wave == 4 ~ R04_A_A01WGT))  %>%
  filter(!is.na(VARSTRAT))

smoke_glmix$wt_long[is.na(smoke_glmix$wt_long)] <- 1



#naive LMM

naive_glmm <- glmer(formula = outcome ~ 1 + wave + menthol_cig + (1 | PERSONID), 
                   data = smoke_glmix, family = binomial)

summary(naive_glmm)

#too big doesn't work wtf
#test <- glmer(formula = outcome ~ 1 + wave + cigs_everysome_ESTD + (1 | PERSONID), 
#                    data = smoke_glmix, family = binomial)

#summary(test)
#better LMM

#according to flowchart , all-waves weight for cohort (for wave)
#glmix_wts <- svydesign(ids =~PERSONID, data = smoke_glmix,weights =~wt_long)
#cross_glm_w1 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_2, family = binomial, 
#                       design = glmix_wts)
#summary(cross_glm_w1)

glmm_wt_model <- glmer(formula = outcome ~ 1 + menthol_cig + (1 | PERSONID), 
                       data = smoke_glmix, weights = wt_long, family = binomial)
summary(glmm_wt_model)



#it's kinda bad, let's try something else

library(svylme)
#lol doesn't work either fuck
glmix_wts <- svydesign(ids =~PERSONID, strata =~VARSTRAT, 
                       data = smoke_glmix,weights =~wt_long)

glmm_wt_model_1 <- svy2lme(outcome ~ 1 + menthol_cig + (1 | PERSONID), 
                           design = glmix_wts)
summary(glmm_wt_model_1)


