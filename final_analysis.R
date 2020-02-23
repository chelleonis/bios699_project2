library(lme4)
library(lmerTest)
library(dplyr)
library(survey)

smoke_final <- smoke %>% 
  mutate(outcome = case_when(ecigs_everysome == "none" ~ 0,
                             ecigs_everysome == "everyday or some days" ~ 1)) %>%
  mutate(wt_long = case_when(wave == 1 ~ R01_A_PWGT,
                             wave == 2 ~ R02_A_PWGT,
                             wave == 3 ~ R03_A_AWGT,
                             wave == 4 ~ R04_A_A01WGT))  %>%
  filter(!is.na(VARSTRAT))

smoke_final_3 <- smoke_final %>% filter(wave != 4) %>%
  filter(!is.na(wt_long))

smoke_glmix_reduced <- smoke_final %>% 
  select(-c(R01_A_PWGT,R02_A_PWGT,R03_A_SWGT,R03_A_AWGT,R04_A_S01WGT,R04_A_A01WGT)) %>%
  filter(!is.na(wt_long)) %>%
  filter(cigs_everysome_ESTD == "established everyday or some days")


big_surv <- svydesign(ids =~PERSONID, data = smoke_glmix_reduced ,weights =~wt_long,
                      strata =~VARSTRAT)

overall_glm <- svyglm(outcome ~ menthol_cig + male + educat + income, 
                      data = smoke_glmix_reduced, family = binomial,
                       design = big_surv)
summary(overall_glm)

hi_surv <- svydesign(ids =~PERSONID, data = smoke_final_3 ,weights =~wt_long,
                      strata =~VARSTRAT)

og_3 <- svyglm(outcome ~ menthol_cig + male + educat + income, 
                      data = smoke_final_3, family = binomial,
                      design = hi_surv)
summary(og_3)


#need to convert wave to factor

smoke_final_33 <- smoke_final_3 %>% mutate(f_wave = as.factor(wave))


naive_glmm <- glmer(formula = outcome ~ 1 + educat + agegrp + income + RE + male +
                       menthol_cig + (1|PERSONID), 
                    data = smoke_final_33, family = binomial)
summary(naive_glmm)

library(tableone)

CreateTableOne(vars = c("menthol_cig", "male", "educat", "income"), strata = c("wave"),
               data = smoke_glmix_reduced)




#last try
library(WeMix)

smoke_glmix_reduced$one <- 1

meme <- mix(outcome ~ 1 + menthol_cig + wave + (1 | PERSONID),
            data = smoke_glmix_reduced, 
            weights = c("one","wt_long"))

data(sleepstudy)
ss1 <- sleepstudy

ss1$W1 <- ifelse(ss1$Subject %in% c(308,309,310),2,1)
ss1$W2 <- 1

two_level <- mix(Reaction ~ Days  + (1|Subject),data = ss1, weights = c("W2"))

summary(two_level)

