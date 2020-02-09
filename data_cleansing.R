#Goal: to understand whether menthol flavoring in cigarettes 
#impacts the likelihood of initiating e-cigarette use among smokers

#codings
# exposure (cigs_everysome_ESTD), 


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

smoke_glm_2 <- smoke_glm %>% filter(wave == 1)


#cross section yr 1:
ok_glm <- glm(outcome ~ menthol_cig , data = smoke_glm_2, family = binomial)
summary(ok_glm)

#note, repeat for all 4 lol


test <- svyglm(formula)


#longitudinal 
library(lme4)
library(lmerTest)

#random, year/ID?