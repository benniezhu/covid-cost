library(shiny)
library(shinydashboard)

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
      
      
      selectInput("IHME", "Please Select IHME Projection",
                  choices = c('Low', 'Medium', 'High'))
      
  ),
  
  dashboardBody(
    fluidRow(
      box(title = 'Hospitalizations', 
          plotOutput("Wave1_hosp"),
          ),
      
      box(plotOutput("Wave1_Hosp"))
    )
  )
)
)
