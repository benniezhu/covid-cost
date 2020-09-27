library(shiny)
library(shinydashboard)
library(tidyverse)
#import states
library(readr)
States <- read_csv("data/States.csv")

#Import Data

acs_long <- read_csv("data/acs_long.csv")

#Shiny UI

shinyUI(
  dashboardPage(
    dashboardHeader(title = "State Cost of Covid"),
 
    dashboardSidebar(
      selectInput(inputId = "State", "Please Select States",
                  choices = States$State,
                  selected = c('California' , 'Texas', 'New York', 'Florida'),
                  multiple = TRUE
                  ),
      
      selectInput(inputId = "Wave", "Please Select Wave",
                  choices = c("First", "Second", "Combined")),
      
      selectInput("IHME", "Please Select IHME Projection",
                  choices = c('Low', 'Medium', 'High')),
      
      selectInput(inputId = "inpatientcost", "Inpatient Cost (Private Insurance Only)",
                  choices = c("Low", "High")),
     
      selectInput(inputId = 'uninsuredinpatientcost' , "Treat Uninsured Inpatient Cost as",
                  choices = c("Medicare", "Charges")),
      
      selectInput(inputId = 'uninsuredoop', "Out of Pocket Payment Assumption",
                  choices = c('Reduced', 'Full'))
      
  ),
  
  dashboardBody(
    fluidRow(
      box(title = 'Total Hospitalizations', 
          plotOutput("hosp_plot")),
      
      box(title = 'Total Hospitalizations Cost by Coverage and Payer',
          plotOutput("hosp_cost_plot")),
      
      box(title = 'Non-ICU Hospitalizations',
          plotOutput("nonicuhosp_plot")),
      
      box(title = 'Non-ICU Cost by Coverage and Payer',
          plotOutput("nonicuhosp_cost_plot")),
      
      
      box(title = 'ICU Admissions',
          plotOutput("icu_plot")),
      
      box(title = 'ICU Admissions Cost by Coverage and Payer',
          plotOutput("icu_cost_plot")),
      
    )
  )
)
)
