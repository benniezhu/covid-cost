library(shiny)
library(shinydashboard)

shinyServer(function(input,output){
  
  #Import Data
  
  Combined <- read_csv("Combined.csv")
  
  output$hosp_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes( x = location_name , y = Hospitalizations/1000 , fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$nonicuhosp_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes( x = location_name , y = nonicuhosp/1000 , fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of Non ICU Hospitalizations')+labs(fill = 'Insurance')+
      theme(axis.text.x = element_text(angle = 90))
  })
  

  output$icu_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost),
           aes(x = location_name, y = ICUs/1000, fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of ICU Admissions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))
  })  
  
  output$nonicuhosp_cost_plot <-renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes(x = location_name, y = nonicucost/1000000, fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Cost of Non-ICU Admissions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))
  })
  
  output$icu_cost_plot <-renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes(x = location_name, y = icucost/1000000, fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Cost of ICU Admissions in $Millions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))
  })
  
  output$hosp_cost_plot <-renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes(x = location_name, y = hospitalizationcost/1000000, fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Cost of Hospital Admissions Admissions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))
  })
  
  
})




