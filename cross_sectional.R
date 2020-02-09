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

cs2_surv <- svydesign(ids =~PERSONID, data = smoke_glm_2,weights =~R01_A_PWGT)
cross_glm_w2 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_2, family = binomial, 
                       design = cs2_surv)
summary(cross_glm_w2)

#example for no. #2
#unweighted
smoke_glm_3 <- smoke_glm %>% filter(wave == 2)%>% 
  mutate(wave = as.integer(wave))

ok_glm_2 <- glm(outcome ~ menthol_cig , data = smoke_glm_3, family = binomial)
summary(ok_glm_2)

#weighted
cs2_surv <- svydesign(ids =~PERSONID, data = smoke_glm_3,weights =~R02_A_PWGT)
cross_glm_w2 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_3, family = binomial, 
                       design = cs2_surv)
summary(cross_glm_w2)

#wave 3
#unweighted
smoke_glm_4 <- smoke_glm %>% filter(wave == 3)%>% 
  mutate(wave = as.integer(wave))

ok_glm_3 <- glm(outcome ~ menthol_cig , data = smoke_glm_4, family = binomial)
summary(ok_glm_3)

#weighted
cs3_surv <- svydesign(ids =~PERSONID, data = smoke_glm_4,weights =~R03_A_SWGT)
cross_glm_w3 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_4, family = binomial, 
                       design = cs3_surv)
summary(cross_glm_w3)

#wave 4
#unweighted

smoke_glm_5 <- smoke_glm %>% filter(wave == 4)%>% 
  mutate(wave = as.integer(wave))
#different since has na
smoke_glm_5$R04_A_S01WGT[is.na(smoke_glm_5$R04_A_S01WGT)] <- 1

ok_glm_4 <- glm(outcome ~ menthol_cig , data = smoke_glm_5, family = binomial)
summary(ok_glm_4)

#weighted
cs4_surv <- svydesign(ids =~PERSONID, data = smoke_glm_5,weights =~R04_A_S01WGT)
cross_glm_w4 <- svyglm(outcome ~ menthol_cig, data = smoke_glm_5, family = binomial, 
                       design = cs4_surv)
summary(cross_glm_w4)


#confidence interval function:

ci_95 <- function(summary_obj) {
  
  
  
  return(1)
}

