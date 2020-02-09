

smoke <- read.csv('C:/Users/typer321/Documents/bios699_project2/data/PATH data file_BSTT699_2020.csv')
#convert from words into numbers for analysis reasons(?)

#filter NAs

#test <- ifelse(smoke$ecigs_everysome == "everyday or some days",1,0)


#wave 1 data only cuz i'm lazy as fuck

#regular glm, ignoring weights

bad_fit <- glm( , data = smoke, link = logit)

#applying weights

test <- svyglm(formula, )