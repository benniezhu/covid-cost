library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

#Import States
States <- read_csv("States.csv")


#Shiny UI

shinyUI(
  dashboardPage(
    dashboardHeader(title = "State Cost of Covid",
                  
                    
                    dropdownMenu(type = 'messages',
                                 icon = icon('info'),
                                 badgeStatus = NULL,
                                 messageItem(
                                   icon = icon('mail'), #use mail as the icon because then it removes the icon
                                   from = '' ,
                                   message = a("Based on: Who Will Pay for COVID-19 Hospital Care: Looking at Payers Across States", href = "https://www.commonwealthfund.org/blog/2020/who-will-pay-covid-19-hospital-care-looking-payers-across-states")
                                 ),
                                 
                                 messageItem(
                                   icon = icon('mail'),
                                   from = "",
                                   message = a('Code and Documentation', href = "https://github.com/benniezhu/covid-cost")
                                 ),
                                 
                                 messageItem(
                                   icon = icon('mail'),
                                   from = "",
                                   message = a('Questions, suggestions, and other issues can be directed to Benjamin Zhu', href = 'mailto:bz22@nyu.edu' )
                                 ))
                    
                    ),
 
    dashboardSidebar(
      

      
      selectInput(inputId = "State", "Please Select States",
                  choices = States$State,
                  selected = c('California' , 'Texas', 'New York', 'Florida'),
                  multiple = TRUE
                  ),
      
      selectInput(inputId = "Wave", "Please Select Wave",
                  choices = c("First", "Second", "Combined"),
                  selected = 'Combined'),
      
      selectInput("IHME", "Please Select IHME Projection",
                  choices = c('Low', 'Medium', 'High'),
                  selected = 'Medium'),
      
      selectInput(inputId = "inpatientcost", "Inpatient Cost (Private Insurance Only)",
                  choices = c("Low", "High"),
                  selected = 'High'),
     
      selectInput(inputId = 'uninsuredinpatientcost' , "Treat Uninsured Inpatient Cost as",
                  choices = c("Medicare", "Charges")),
      
      selectInput(inputId = 'uninsuredoop', "Uninsured Out of Pocket Payment Assumption",
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
            plotOutput("icu_cost_plot"))
                )
            ),
     
      )
)
  


