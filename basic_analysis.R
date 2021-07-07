#descriptive analysis for each category

smoke <- read.csv('C:/Users/typer321/Documents/bios699_project2/data/PATH data file_BSTT699_2020.csv')

length(unique(smoke$PERSONID))
length(smoke$PERSONID)

library(dplyr)


smoke_desc <- smoke %>% group_by(wave) %>%
  summarise(n = n())

counts_cigs <- smoke %>% group_by(wave) %>%
  count(cigs_everysome_ESTD)

counts_menthol <- smoke %>% group_by(wave) %>%
  count(menthol_cig)

counts_ecigs <- smoke %>% group_by(wave) %>%
  count(ecigs_everysome)

count_smoke <- smoke %>% group_by(wave) %>%
  filter(!is.na(menthol_cig)) %>%
  count(ecigs_everysome)


#this should mostly be covariate analysis
#and descrptive statistics

#piecharts for descriptive stats
table(smoke$wave)
table(smoke$menthol_cig)
table(smoke$ecigs_everysome)
table(smoke$cigs_everysome_ESTD)


#group by wave
#then see how many people are menthol and that stuff

#graph that includes % of people who mentholed at that time
