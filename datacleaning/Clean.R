# Import hospitalizations data 

library(haven)
library(tidyverse)
library(RStata)
Hospitalizations_wave1 <- read_dta("data/Hospitalizations_wave1.dta")
Hospitalizations_wave1$state <- tolower(Hospitalizations_wave1$location_name)
View(Hospitalizations_wave1)


Hospitalizations_wave2 <- read_dta("data/Hospitalizations_wave2.dta")
Hospitalizations_wave2$state <- tolower(Hospitalizations_wave2$location_name)
View(Hospitalizations_wave2)

#Import ACS Data
acs_long <- read_csv("data/acs_long.csv")

#sum by state and insurance 
acs_long <- acs_long %>%
  group_by(state, insurance) %>%
  mutate(totalpop_stateinsurance = sum(pop, na.rm = TRUE))

# sum by state and agegroup
acs_long <- acs_long %>%
  group_by(state, age_group) %>%
  mutate(totalpop_stateagegroup = sum(pop, na.rm=TRUE))

acs_long <- acs_long %>%
  mutate(ins_ratio_by_age = pop/totalpop_stateagegroup , na.rm = TRUE)

#Import cdc rates
prejune <- read_csv('data/prejune_cdc_rate.csv')
postjune <- read_csv('data/postjune_cdc_rate.csv')

#merge cdc rates onto acs_long
prejune <- left_join(acs_long, prejune, by='age_group')
postjune <- left_join(acs_long, postjune , by = 'age_group')

#Merge acs and hospitalizations

Wave1 <- left_join(prejune, Hospitalizations_wave1, by= 'state')
Wave2 <- left_join(postjune, Hospitalizations_wave2, by ='state')

#calculate hospitalization

Wave1 <- Wave1 %>%
  mutate(hosp_Medium = totaladmis_mean * ins_ratio_by_age *hospitalization)


Wave1 <- Wave1 %>% 
  mutate(hosp_Low = totaladmis_lower * ins_ratio_by_age * hospitalization)


Wave1 <- Wave1 %>% 
  mutate(hosp_High = totaladmis_upper * ins_ratio_by_age * hospitalization)

#Calculate ICU

Wave1 <- Wave1 %>% 
  mutate(icu_Medium = totalnewicu_mean * ins_ratio_by_age * icu )

Wave1 <- Wave1 %>% 
  mutate(icu_Low = totalnewicu_lower * ins_ratio_by_age * icu)

Wave1 <- Wave1 %>% 
  mutate(icu_High = totalnewicu_upper * ins_ratio_by_age *icu)

# Reshape the data so that we can select IHME conditions

Wave1_icu <- gather(Wave1, IHME, ICUs, icu_Medium, icu_Low, icu_High, factor_key = FALSE)

Wave1_icu$IHME[Wave1_icu$IHME == 'icu_Medium'] <- 'Medium'
Wave1_icu$IHME[Wave1_icu$IHME == 'icu_Low'] <- 'Low'
Wave1_icu$IHME[Wave1_icu$IHME == 'icu_High'] <- 'High'
Wave1_icu <- select(Wave1_icu, -c(hosp_Medium, hosp_Low, hosp_High))


Wave1_hosp <- gather(Wave1, IHME, Hospitalizations, hosp_Medium, hosp_Low, hosp_High, factor_key = FALSE)

Wave1_hosp$IHME[Wave1_hosp$IHME == 'hosp_Medium'] <- 'Medium'
Wave1_hosp$IHME[Wave1_hosp$IHME == 'hosp_Low'] <- 'Low'
Wave1_hosp$IHME[Wave1_hosp$IHME == 'hosp_High'] <- 'High'
Wave1_hosp <- select(Wave1_hosp, -c(icu_High, icu_Medium, icu_Low))

Wave1 <- right_join(Wave1_hosp, 
                  Wave1_icu %>% select(c(state, IHME, age_group, insurance, ICUs)), 
                  by = c('state', 'IHME', 'age_group', 'insurance'))

rm(Wave1_hosp)
rm(Wave1_icu)

Wave1<- Wave1 %>%
  mutate(nonicuhosp = Hospitalizations - ICUs)

Wave1_hosp_plot <- ggplot(Wave1, aes( x = location_name , y = Hospitalizations , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))

Wave1_nonicuhosp_plot <- ggplot(Wave1, aes( x = location_name , y = nonicuhosp , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of Non ICU Hospitalizations')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))


Wave1_icu_plot <- ggplot(Wave1, aes( x = location_name , y = ICUs , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of ICU')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(Wave1, (location_name == 'California' | location_name == 'Texas') & IHME == 'Medium'), aes(x = location_name, y = ICUs, fill = insurance))+
  geom_bar(stat = 'identity')


# Testing below 
ggplot(filter(Wave1, state != 'united states' & IHME == 'hosp_High'), aes(x = location_name, y = Hospitalizations, fill = insurance))+
  geom_bar(stat = 'identity')
  

ggplot(filter(Wave1, state != 'united states'), aes( x = location_name , y = icu_High , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of ICU')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(Wave1, state != 'united states'), aes( x = location_name , y = hosp_High , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))


