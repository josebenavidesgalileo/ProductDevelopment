#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$`slider-io` <- renderText({paste0(c('output slider input',input$`Slider-input`),
                                           collapse = '')})
    
    output$`slider-io2` <- renderText({input$`Slider-input2`})
    
    output$`Select-io` <- renderText({input$`select_input`})
    
    output$`Select-io2` <- renderText({input$`select_input2`})
    
    output$`DateIO` <- renderText({as.character(input$`dateInput`)})
    
    output$`DateIO2` <- renderText({paste(as.character(input$dateRange), collapse = " to ")})

    output$`numericIO` <- renderText({input$`numericInput`})
    
    output$`checkboxIO` <- renderText({input$`checkboxInput`})
   
    output$`TextIO` <- renderText({input$`TextInput`})
    
    output$`checboxGroupIO` <- renderText({input$`checboxGroupInput`})
    
    output$`TextAreaIO` <- renderText({input$`TextAreaINput`})
    
    output$`buttonIO` <- renderText({input$`ActioButton`})
    
    output$`linkIO` <- renderText({input$`ActionLink`})
    
    
})


