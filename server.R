library(shiny)
library(shinydashboard)

shinyServer(function(input,output){

  
  output$hosp_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave), aes( x = location_name , y = Hospitalizations , fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$Wave1_nonicuhosp_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave ), aes( x = location_name , y = nonicuhosp , fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of Non ICU Hospitalizations')+labs(fill = 'Insurance')+
      theme(axis.text.x = element_text(angle = 90))
  })
  

  output$Wave1_icu_plot <- renderPlot({
    ggplot(filter(Combined, location_name %in% input$State & IHME == input$IHME & Wave == input$Wave), aes(x = location_name, y = ICUs, fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of ICU Admissions') + labs(fill = 'Insurance')+
      theme(axis.text =  element_text(angle = 90))
  })  
})




