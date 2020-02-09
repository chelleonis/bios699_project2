library(survey)
library(dplyr)

smoke <- read.csv('C:/Users/typer321/Documents/bios699_project2/data/PATH data file_BSTT699_2020.csv')

#convert from words into numbers for analysis reasons(?)

#wave 1 data only cuz i'm lazy as fuck

#regular gm
smoke_glm <- smoke %>% 
  mutate(outcome = case_when(ecigs_everysome == "none" ~ 0,
                             ecigs_everysome == "everyday or some days" ~ 1))

#no accounting for years at all, nor survey weights
worst_glm <- glm(outcome ~ menthol_cig , data = smoke_glm, family = binomial)
summary(bad_glm)



#cross section yr 1: (no weights)
ok_glm <- glm(outcome ~ menthol_cig , data = smoke_glm_2, family = binomial)
summary(ok_glm)

#cross section year 1: (WITH WEIGHTS)
cs1_surv <- svydesign(ids =~PERSONID, data = smoke_glm_2,weights =~R01_A_PWGT)
cross_glm_w1 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_2, family = binomial, 
                       design = cs1_surv)
summary(cross_glm_w1)

#note, repeat for all 4 lol

cs2_surv <- svydesign(ids =~PERSONID, data = smoke_glm_2,weights =~R02_A_PWGT)
cross_glm_w2 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_2, family = binomial, 
                       design = cs2_surv)
summary(cross_glm_w2)

#example for no. #2
#unweighted
ok_glm <- glm(outcome ~ menthol_cig , data = smoke_glm_2, family = binomial)
summary(ok_glm)

#weighted
cs2_surv <- svydesign(ids =~PERSONID, data = smoke_glm_2,weights =~R02_A_PWGT)
cross_glm_w2 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_2, family = binomial, 
                       design = cs2_surv)
summary(cross_glm_w2)