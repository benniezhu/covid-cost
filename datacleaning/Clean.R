# Import hospitalizations data 

library(haven)
library(tidyverse)
library(RStata)
library(readr)

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

Combined <- Combined %>%
  mutate(nonicuhosp = Hospitalizations - ICUs)

rm(Combined_hospitalizations)
rm(Combined_icu)


#Add Inpatient Non ICU Cost 

#Import the data from multipler sheets 

#the medicare sheet is wide ; we will use it to reshape and make an assumption column on which we can match the other subsequent cost
nonicucost_uninsured_medicare <- read_csv("data/nonicucost_uninsured_medicare.csv")

nonicucost_uninsured_charges <- read_csv("data/nonicucost_uninsured_charges.csv")


#merge medicare and reshape

Combined <- right_join(Combined, 
                   nonicucost_uninsured_medicare,
                   by = 'insurance')

Combined <- gather(Combined, inpatientcost_assumption, nonicucost, nonicucost_low, nonicucost_high, factor_key = FALSE)

Combined$inpatientcost_assumption[Combined$inpatientcost_assumption == 'nonicucost_low'] <- 'Low'
Combined$inpatientcost_assumption[Combined$inpatientcost_assumption == 'nonicucost_high'] <- 'High'

#merge in the charges costs 

Combined <- right_join(Combined,
                   nonicucost_uninsured_charges,
                   by = c('insurance', 'inpatientcost_assumption'))

Combined <- gather(Combined, key = 'uninsured_as', value = 'nonicucost', nonicucost, nonicucost_charges , factor_key = FALSE )

Combined$uninsured_as[Combined$uninsured_as == 'nonicucost'] <- 'Medicare'
Combined$uninsured_as[Combined$uninsured_as == 'nonicucost_charges'] <- 'Charges'

Combined <- Combined %>%
  rename(
    nonicucost_pervisit = nonicucost
  )

Combined <- Combined %>%
  mutate(nonicucost = nonicuhosp*nonicucost_pervisit)



# Merge in the icu costs 
icucost <- read_csv("data/icucost.csv")

Combined <- right_join(Combined, 
                   icucost,
                   by = c('insurance', 'uninsured_as'))

Combined <- Combined %>%
  mutate(icucost = icucost_pervisit* ICUs)

# Calculate Total Hospitalizations Cost

Combined <- Combined %>%
  mutate(hospitalizationcost = nonicucost+icucost)

# remove unneeded dataframes 
rm(list=(ls()[ls()!="Combined"]))

#Add OOP
oopcost <- read_csv('data/oopcost.csv')


Combined <- right_join(Combined,
                       oopcost,
                       by = c('uninsured_as', 'insurance', 'inpatientcost_assumption'))

Combined <- Combined %>%
  mutate(icucost_oop = ICUs * icucost_oop_pervisit)

Combined <- Combined %>%
  mutate(nonicucost_oop = nonicuhosp * nonicucost_oop_pervisit)

Combined <- Combined %>%
  mutate(icucost_reimbursed = icucost - icucost_oop)

Combined <- Combined %>%
  mutate(nonicucost_reimbursed = nonicucost - nonicucost_oop)

Combined_hospitalizations <- gather(select(Combined, -c('nonicucost' , 'icucost', 'hospitalizationcost', 'icucost_oop', 'icucost_reimbursed')),
                                    oop_or_reimbursed ,nonicucost,
                                    nonicucost_reimbursed, nonicucost_oop, factor_key = FALSE)

Combined_hospitalizations$oop_or_reimbursed[Combined_hospitalizations$oop_or_reimbursed == 'nonicucost_reimbursed'] <- 'Reimbursed'
Combined_hospitalizations$oop_or_reimbursed[Combined_hospitalizations$oop_or_reimbursed == 'nonicucost_oop'] <- 'OOP'

Combined_icu <- gather(select(Combined, -c('nonicucost', 'icucost', 'hospitalizationcost', 'nonicucost_oop', 'nonicucost_reimbursed')),
                       oop_or_reimbursed, icucost,
                       icucost_reimbursed, icucost_oop, factor_key = FALSE)

Combined_icu$oop_or_reimbursed[Combined_icu$oop_or_reimbursed == 'icucost_reimbursed'] <- 'Reimbursed'
Combined_icu$oop_or_reimbursed[Combined_icu$oop_or_reimbursed == 'icucost_oop'] <- 'OOP'


Combined <- right_join(Combined_hospitalizations,
                   Combined_icu %>% select(c(state, IHME, age_group, inpatientcost_assumption, insurance, uninsured_as, Wave, oop_or_reimbursed, icucost)),
                   by = c('state', 'IHME', 'age_group', 'insurance', 'oop_or_reimbursed', 'Wave', 'inpatientcost_assumption', 'uninsured_as'))

Combined$payer <- paste(Combined$insurance , Combined$oop_or_reimbursed, sep = " ")

Combined <- Combined %>%
  mutate(hospitalizationcost = nonicucost + icucost)

#Add Full or reduced OOP
Combined <- Combined %>%
  mutate(hospitalizationcost_reduced = hospitalizationcost)

Combined$hospitalizationcost_reduced <- if_else(Combined$insurance == 'uninsured' & Combined$oop_or_reimbursed == 'OOP', Combined$hospitalizationcost*0.2,
                                                Combined$hospitalizationcost)


Combined <- Combined %>%
  mutate(nonicucost_reduced = nonicucost)

Combined$nonicucost_reduced <- if_else(Combined$insurance == 'uninsured' & Combined$oop_or_reimbursed == 'OOP', Combined$nonicucost * 0.2, 
                                       Combined$nonicucost)

Combined <- Combined %>%
  mutate(icucost_reduced = icucost)

Combined$icucost_reduced <- if_else(Combined$insurance == 'uninsured' & Combined$oop_or_reimbursed == 'OOP', Combined$icucost*0.2,
                                    Combined$icucost)

#Capitalize Payer 
Combined$payer <- str_to_title(Combined$payer)

#Capitalze op in Oop
Combined$payer[Combined$payer == "Medicaid Oop"] <- "Medicaid OOP"
Combined$payer[Combined$payer == "Uninsured Oop"] <- "Uninsured OOP"
Combined$payer[Combined$payer == "Private Oop"] <- "Private OOP"
Combined$payer[Combined$payer == "Medicare Oop"] <- "Medicare OOP"



#Add the difference between normal and reduced for uninsured OOP and add it back into uninsured (re)imbursed
library('collapse')

Combined_uninsured_oop <- Combined %>% 
  subset( payer == 'Uninsured OOP')


Combined_uninsured_oop$nonicucost <- Combined_uninsured_oop$nonicucost * 0.8
Combined_uninsured_oop$icucost <- Combined_uninsured_oop$icucost * 0.8
Combined_uninsured_oop$hospitalizationcost <- Combined_uninsured_oop$hospitalizationcost * 0.8


Combined_uninsured_oop <- Combined_uninsured_oop %>%
  group_by(IHME, Wave, state, location_name, oop_or_reimbursed, insurance, inpatientcost_assumption, uninsured_as) %>%
  select_at( c('hospitalizationcost', 'icucost' ,'nonicucost', 'pop'))  %>% fsum


Combined_uninsured_oop$oop_or_reimbursed = 'Reimbursed'

#rename vars to reduced 

library("plyr")

Combined_uninsured_oop <- Combined_uninsured_oop %>%
  rename(c(
    'hospitalizationcost' = 'hospitalizationcost_reduced',
    'nonicucost' = 'nonicucost_reduced' ,
    'icucost' = 'icucost_reduced')
)

Combined_uninsured_oop$payer <- paste(Combined_uninsured_oop$insurance , Combined_uninsured_oop$oop_or_reimbursed, sep = " ")
Combined_uninsured_oop$payer <- str_to_title(Combined_uninsured_oop$payer)

Combined <- rbind.fill(Combined, Combined_uninsured_oop)

#drop X1
Combined <- select(Combined, -X1)

#capitalize insurance
Combined$insurance <- str_to_title(Combined$insurance)

#Rename Payer uninsured reimbursed to uninsured uncompensated 
Combined$payer[Combined$payer == "Uninsured Reimbursed"] <- "Uninsured Uncompensated"

#Export data to a csv 
write.csv(Combined, 'data/Combined.csv')

# remove unneeded dataframes 
rm(list=(ls()[ls()!="Combined"]))

