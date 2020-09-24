library(shiny)
library(shinydashboard)

shinyServer(function(input,output){

  
  output$Wave1_hosp <- renderPlot({

    ggplot(filter(Wave1, location_name == input$State & IHME == input$IHME), aes( x = location_name , y = Hospitalizations , fill = insurance))+
      geom_bar(stat = 'identity')+
      xlab('State') + ylab('Number of Hospitalizations')+labs(fill = 'Insurance')+
      theme(axis.text.x = element_text(angle = 90))
  })
  
})