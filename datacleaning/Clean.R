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


Wave2 <- Wave2 %>%
  mutate(hosp_Medium = totaladmis_mean * ins_ratio_by_age *hospitalization)

Wave2 <- Wave2 %>% 
  mutate(hosp_Low = totaladmis_lower * ins_ratio_by_age * hospitalization)


Wave2 <- Wave2 %>% 
  mutate(hosp_High = totaladmis_upper * ins_ratio_by_age * hospitalization)


#Calculate ICU

Wave1 <- Wave1 %>% 
  mutate(icu_Medium = totalnewicu_mean * ins_ratio_by_age * icu )

Wave1 <- Wave1 %>% 
  mutate(icu_Low = totalnewicu_lower * ins_ratio_by_age * icu)

Wave1 <- Wave1 %>% 
  mutate(icu_High = totalnewicu_upper * ins_ratio_by_age *icu)


Wave2 <- Wave2 %>% 
  mutate(icu_Medium = totalnewicu_mean * ins_ratio_by_age * icu )

Wave2 <- Wave2 %>% 
  mutate(icu_Low = totalnewicu_lower * ins_ratio_by_age * icu)

Wave2 <- Wave2 %>% 
  mutate(icu_High = totalnewicu_upper * ins_ratio_by_age *icu)

# Reshape the data so that we can select IHME conditions

#Reshape Wave1 
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


# Reshape Wave 2
Wave2_icu <- gather(Wave2, IHME, ICUs, icu_Medium, icu_Low, icu_High, factor_key = FALSE)

Wave2_icu$IHME[Wave2_icu$IHME == 'icu_Medium'] <- 'Medium'
Wave2_icu$IHME[Wave2_icu$IHME == 'icu_Low'] <- 'Low'
Wave2_icu$IHME[Wave2_icu$IHME == 'icu_High'] <- 'High'
Wave2_icu <- select(Wave2_icu, -c(hosp_Medium, hosp_Low, hosp_High))


Wave2_hosp <- gather(Wave2, IHME, Hospitalizations, hosp_Medium, hosp_Low, hosp_High, factor_key = FALSE)

Wave2_hosp$IHME[Wave2_hosp$IHME == 'hosp_Medium'] <- 'Medium'
Wave2_hosp$IHME[Wave2_hosp$IHME == 'hosp_Low'] <- 'Low'
Wave2_hosp$IHME[Wave2_hosp$IHME == 'hosp_High'] <- 'High'
Wave2_hosp <- select(Wave2_hosp, -c(icu_High, icu_Medium, icu_Low))

Wave2 <- right_join(Wave2_hosp, 
                    Wave2_icu %>% select(c(state, IHME, age_group, insurance, ICUs)), 
                    by = c('state', 'IHME', 'age_group', 'insurance'))

Wave2<- Wave2 %>%
  mutate(nonicuhosp = Hospitalizations - ICUs)


rm(Wave2_hosp)
rm(Wave2_icu)

#Drop unneeded vars in Wave 1 and 2

Wave1 <- select(Wave1, -c(totalnewicu_mean, totalnewicu_lower, totalnewicu_upper, totaldeaths_mean, 
                          totaldeaths_lower, totaldeaths_upper, totaladmis_mean, totaladmis_lower, totaladmis_upper))


Wave2 <- select(Wave2, -c(totalnewicu_mean, totalnewicu_lower, totalnewicu_upper, totaldeaths_mean, 
                          totaldeaths_lower, totaldeaths_upper, totaladmis_mean, totaladmis_lower, totaladmis_upper))


Wave2<- Wave2 %>%
  mutate(nonicuhosp = Hospitalizations - ICUs)



#Merge Wave1 and Wave2

Wave1 <- Wave1 %>% 
  rename(Hospitalizations_wave1 = 'Hospitalizations',
         ICUs_wave1 = 'ICUs',
         nonicuhosp_wave1 = 'nonicuhosp')


Wave2 <- Wave2 %>% 
  rename(Hospitalizations_wave2 = 'Hospitalizations',
         ICUs_wave2 = 'ICUs',
         nonicuhosp_wave2 = 'nonicuhosp')

Combined <-right_join(Wave1, 
                      Wave2 %>% select(c(state, IHME, age_group, insurance, Hospitalizations_wave2, ICUs_wave2, nonicuhosp_wave2 )),
                      by = c('state', 'IHME', 'age_group', 'insurance'))

#Add Combined Hosp, ICU, nonicuhosp

Combined <- Combined %>%
  mutate(Hospitalizations_combined = Hospitalizations_wave1 + Hospitalizations_wave2 )

Combined <- Combined %>%
  mutate(ICUs_combined = ICUs_wave1+ ICUs_wave2)

Combined <- Combined %>%
  mutate(nonicuhosp_combined = nonicuhosp_wave1+nonicuhosp_wave2)

#Reshape the Waves

Combined_hospitalizations <- gather(select(Combined, -starts_with(c('ICUs_','nonicuhosp'))), 
                                    Wave, Hospitalizations, 
                                    Hospitalizations_wave1, Hospitalizations_wave2, Hospitalizations_combined, factor_key = FALSE)

Combined_hospitalizations$Wave[Combined_hospitalizations$Wave == 'Hospitalizations_wave1'] <- 'First'
Combined_hospitalizations$Wave[Combined_hospitalizations$Wave == 'Hospitalizations_wave2'] <- 'Second'
Combined_hospitalizations$Wave[Combined_hospitalizations$Wave == 'Hospitalizations_combined'] <- 'Combined'



Combined_icu <- gather(select(Combined, -starts_with(c('Hospitalizations', 'nonicuhosp'))),
                       Wave, ICUs, 
                       ICUs_wave1, ICUs_wave2, ICUs_combined, factor_key = FALSE)


Combined_icu$Wave[Combined_icu$Wave == 'ICUs_wave1'] <- 'First'
Combined_icu$Wave[Combined_icu$Wave == 'ICUs_wave2'] <- 'Second'
Combined_icu$Wave[Combined_icu$Wave == 'ICUs_combined'] <- 'Combined'


Combined <- right_join(Combined_hospitalizations, 
                   Combined_icu %>% select(c(state, IHME, age_group, insurance, ICUs, Wave)),
                   by = c('state' , 'IHME', 'age_group', 'insurance', 'Wave'))

Combined <- test %>%
  mutate(nonicuhosp = Hospitalizations - ICUs)

rm(Combined_hospitalizations)
rm(Combined_icu)

#Test Plots 
ggplot(filter(test, (location_name == 'California' ) & IHME == 'Medium' & Wave == 'combined'), aes(x = location_name, y = ICUs, fill = insurance))+
  geom_bar(stat = 'identity')


ggplot(filter(Combined, (location_name == 'California' ) & IHME == 'Medium'), aes(x = location_name, y = ICUs_combined, fill = insurance))+
  geom_bar(stat = 'identity')


ggplot(filter(Wave2, (location_name == 'California' ) & IHME == 'Medium'), aes(x = location_name, y = ICUs_wave2, fill = insurance))+
  geom_bar(stat = 'identity')
  
hosp_plot <- ggplot(test, aes( x = location_name , y = Hospitalizations , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))

#Make Plot Objects 



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


Wave2_hosp_plot <- ggplot(Wave2, aes( x = location_name , y = Hospitalizations , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))

Wave2_nonicuhosp_plot <- ggplot(Wave2, aes( x = location_name , y = nonicuhosp , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of Non ICU Hospitalizations')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))


Wave2_icu_plot <- ggplot(Wave2, aes( x = location_name , y = ICUs , fill = insurance))+
  geom_bar(stat = 'identity')+
  xlab('State') + ylab('Number of ICU')+labs(fill = 'Insurance')+
  theme(axis.text.x = element_text(angle = 90))







# Testing below 


ggplot(filter(Wave1, (location_name == 'California' | location_name == 'Texas') & IHME == 'Medium'), aes(x = location_name, y = ICUs, fill = insurance))+
  geom_bar(stat = 'identity')


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


