

smoke <- read.csv('C:/Users/typer321/Documents/bios699_project2/data/PATH data file_BSTT699_2020.csv')


library(dplyr)


smoke_desc <- smoke %>% group_by(wave) %>%
  summarise(mean_smoke = mean(cigs_everysome_ESTD),
            n_smoke = n(cigs_everysome_ESTD),
            mean_smoke = mean(menthol_cig),
            n_smoke = n(menthol_cig),
            mean_smoke = mean(ecigs_everysome),
            n_smoke = n(ecigs_everysome)
            )

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