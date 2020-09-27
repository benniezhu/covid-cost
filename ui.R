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
                  choices = c('Low', 'Medium', 'High'))
      
  ),
  
  dashboardBody(
    fluidRow(
      box(title = 'Total Hospitalizations', 
          plotOutput("hosp_plot"),
          ),
      
      box(title = 'Non-ICU Hospitalizations',
          plotOutput("Wave1_nonicuhosp_plot")),
      
      box(title = 'ICU Admissions',
          plotOutput("Wave1_icu_plot"),
          )
    )
  )
)
)
