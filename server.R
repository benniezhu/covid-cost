library(shiny)
library(shinydashboard)

shinyServer(function(input,output){
  
  #Import Data
  
  # Comment the following line out for local testing atm 
  Combined <- read_csv("Combined.csv")
  
  output$hosp_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes( x = location_name , y = Hospitalizations/1000 , fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
      theme(axis.text.x = element_text(angle = 90))+
      scale_fill_manual(breaks = c("Medicaid", "Medicare" , "Private" , "Uninsured"),
                        values = c("red4", "green4", "blue4" , "purple4"))
  })
  
  output$nonicuhosp_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes( x = location_name , y = nonicuhosp/1000 , fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of Non ICU Hospitalizations')+labs(fill = 'Insurance')+
      theme(axis.text.x = element_text(angle = 90))+
      scale_fill_manual(breaks = c("Medicaid", "Medicare" , "Private" , "Uninsured"),
                        values = c("red4", "green4", "blue4" , "purple4"))
  })
  

  output$icu_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost),
           aes(x = location_name, y = ICUs/1000, fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of ICU Admissions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))+
      scale_fill_manual(breaks = c("Medicaid", "Medicare" , "Private" , "Uninsured"),
                        values = c("red4", "green4", "blue4" , "purple4"))
  })  
  
  output$nonicuhosp_cost_plot <-renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes_string(x = "location_name", y = if_else(input$uninsuredoop == 'Reduced' ,"nonicucost_reduced/1000000", "nonicucost/1000000"), fill = "payer"))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Cost of Non-ICU Admissions in $Millions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))+
      scale_fill_manual(breaks = c("Medicaid Reimbursed", "Medicaid OOP", "Medicare Reimbursed" , "Medicare OOP", "Private Reimbursed" , "Private OOP" , "Uninsured Uncompensated", "Uninsured OOP"),
                        values = c("red4", "red" ,"green4", "green", "blue4" , "blue" ,"purple4", "purple" ))
  })
  
  output$icu_cost_plot <-renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes_string(x = "location_name", y = if_else(input$uninsuredoop == 'Reduced' , "icucost_reduced/1000000", "icucost/1000000"), fill = "payer"))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Cost of ICU Admissions in $Millions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))+
      scale_fill_manual(breaks = c("Medicaid Reimbursed", "Medicaid OOP", "Medicare Reimbursed" , "Medicare OOP", "Private Reimbursed" , "Private OOP" , "Uninsured Uncompensated", "Uninsured OOP"),
                        values = c("red4", "red" ,"green4", "green", "blue4" , "blue" ,"purple4", "purple" ))
  })
  
  output$hosp_cost_plot <-renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave & inpatientcost_assumption == input$inpatientcost & uninsured_as == input$uninsuredinpatientcost), 
           aes_string(x = "location_name", y = if_else(input$uninsuredoop == 'Reduced' ,"hospitalizationcost_reduced/1000000", "hospitalizationcost/1000000" ), fill = "payer"))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Cost of Hospital Admissions Admissions in $Millions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))+
      scale_fill_manual(breaks = c("Medicaid Reimbursed", "Medicaid OOP", "Medicare Reimbursed" , "Medicare OOP", "Private Reimbursed" , "Private OOP" , "Uninsured Uncompensated", "Uninsured OOP"),
                        values = c("red4", "red" ,"green4", "green", "blue4" , "blue" ,"purple4", "purple" ))
  })
  
  
})




