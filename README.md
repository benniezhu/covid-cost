# Covid-Cost Simulator

## Overview

This dashboard is based on the [Commonwealth Fund Publication](https://www.commonwealthfund.org/blog/2020/who-will-pay-covid-19-hospital-care-looking-payers-across-states)



Any inquiries can be directed to [Benjamin Zhu](mailto:bz22@nyu.edu)


## Files

`ui.R` contains the code for the user interface of the dashboard.

`server.R` contains the code for the server end of the dashboard.

`Combined.csv` is the dataset that powers the dashboard. This was recently updated using IHME data on October 27, 2020

### Data

`acs_long.csv` is population data by state, age group, and insurance coverage.

`Hospitalizations_wave1.dta` and `Hospitalizations_wave2` are data on hospital and ICU admissions from the [Institute for Health Metrics and Evaluation](https://covid19.healthdata.org/global?view=total-deaths&tab=trend). Wave1 contains data from January through May and Wave2 contains data from June through October.

`prejune_cdc_rate.csv` and `postjune_cdc_rate.csv` contain the relative proportion of hospitalizations and icu admissions by age group for Wave1 and Wave2, respectively.     

`nonicucost_uninsured_charges.csv` and `nonicucost_uninsured_medicare.csv` contain cost estimates for hospital admissions that do not require an ICU admission, treating uninsured cost estimates as hospital charges or the Medicare rate, respectively.

`icucost.csv` contains cost estimates for ICU admissions.

`oopcost.csv` contains out of pocket cost estimates.

`States.csv` contains a list of the 50 states plus DC and the US as a whole.

### Data Cleaning

`Add_Agecat_ACS_Long.R` adds the ageband variable to population data from the American Community Survey

`Clean.R` takes the various data files and generates the final Combined dataset.
