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

smoke_glmix_small <- smoke_glmix %>% 
  filter(cigs_everysome_ESTD == "established everyday or some days")



#naive LMM

naive_glmm <- glmer(formula = outcome ~ 1 + educat + menthol_cig + (1 | PERSONID), 
                   data = smoke_glmix_reduced, family = binomial)
summary(naive_glmm)

#too big doesn't work
#test <- glmer(formula = outcome ~ 1 + wave + cigs_everysome_ESTD + (1 | PERSONID), 
#                    data = smoke_glmix, family = binomial)

#summary(test)
#better LMM

#according to flowchart , all-waves weight for cohort (for wave)
#glmix_wts <- svydesign(ids =~PERSONID, data = smoke_glmix,weights =~wt_long)
#cross_glm_w1 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_2, family = binomial, 
#                       design = glmix_wts)
#summary(cross_glm_w1)

glmm_wt_model <- glmer(formula = outcome ~ 1 + educat + wave + menthol_cig + (1 | PERSONID), 
                       data = smoke_glmix_reduced, weights = wt_long, family = binomial)
summary(glmm_wt_model)



#it's kinda bad, let's try something else
#variables:
# male, age, education, income, education, income,


library(svylme)

#reduced
smoke_glmix_reduced <- smoke_glmix %>% 
  filter(cigs_everysome_ESTD == "established everyday or some days") %>%
  select(-c(R01_A_PWGT,R02_A_PWGT,R03_A_SWGT,R03_A_AWGT,R04_A_S01WGT,R04_A_A01WGT)) %>%
  filter(!is.na(wt_long)) %>%
  mutate(any_ecig )

#entries with missing weights were excluded :\

glmix_wts <- svydesign(ids =~PERSONID, strata =~VARSTRAT, 
                       data = smoke_glmix_reduced,weights =~wt_long)

glmm_wt_model_1 <- svy2lme(outcome ~ 1 + menthol_cig + (1 | PERSONID), 
                           design = glmix_wts)
summary(glmm_wt_model_1)

## MACHINE LEARNING STUFF HERE

library(caret)
library(glmnet)

data(Hitters, package = "ISLR")

Hitters = na.omit(Hitters)
cv_5 = trainControl(method = "cv", number = 5)

hit_elnet = train(Salary ~., data= Hitters, method = "glmnet", trControl = cv_5)

pred = predict(hit_elnet,)

#filter variables that aren't nonsense

smoke_glmix_ML <- smoke_glmix %>% 
  filter(cigs_everysome_ESTD == "established everyday or some days") %>%
  select(wave,menthol_cig,cigs_30day, ecigs_everysome, male, agegrp, RE, educat,income) %>%
  filter(ecigs_everysome != "") %>%
  filter(male != "") %>%
  filter(agegrp != "") %>%
  filter(RE != "") %>%
  filter(educat != "")
  
smoke_glmix_income <- smoke_glmix_ML %>%
  filter(income != "")
#evaluate accuracy of model eventually

smoke_glmix_income = na.omit(smoke_glmix_income)

cv_10 = trainControl(method = "cv", number = 10)
smoke_elnet <- train(RE ~ ecigs_everysome, data = smoke_glmix_income,
                    method = "glmnet", trControl = cv_10)

