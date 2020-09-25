
library(tidyverse)

acs_long <- read_csv("data/acs_long.csv")
acs_long$ageband[acs_long$age_group == 1] <- '0-17'
acs_long$ageband[acs_long$age_group == 2] <- '18-29'
acs_long$ageband[acs_long$age_group == 3] <- '30-39'
acs_long$ageband[acs_long$age_group == 4] <- '40-49'
acs_long$ageband[acs_long$age_group == 5] <- '50-64'
acs_long$ageband[acs_long$age_group == 6] <- '65-74'
acs_long$ageband[acs_long$age_group == 7] <- '75+'

acs_long <- acs_long[rowSums(is.na(acs_long)) != ncol(acs_long),]

write.csv(acs_long, 'data/acs_long.csv', na = "")
