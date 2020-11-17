#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("LAB 1"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
                sidebarPanel(
                        sliderInput("Slider-input", "seleccione un valor :",
                                    min = 0, max = 1000,
                                    value = 500),
                        sliderInput("Slider-input2", "seleccione rango :",
                                    min = 0, max = 1000,
                                    value = c(0,500),
                                    animate = TRUE),
                        
                        selectInput('select_input','seleccione un carro :',
                                    choices=rownames(mtcars),
                                    selected = "Datsun 710",
                                    multiple = FALSE),
                        
                        selectInput('select_input2','seleccione un carro :',
                                    choices=rownames(mtcars),
                                    selected = "Datsun 710",
                                    multiple = TRUE),
                        dateInput('dateInput',"Ingrese Fecha:",
                                  value = today(),
                                  min = today() - 60,
                                  max = today() + 30,
                                  language = 'es',
                                  weekstart = 1),
                        
                        dateRangeInput('dateRange',
                                       label = 'Ingrese rango feha ',
                                       start = today() - 2, 
                                       end = today() + 2,
                                       separator = 'a'),
                        
                        numericInput('numericInput','Ingrese Numero',value =0),
                        
                        checkboxInput('checkboxInput','seleccione si verdadero',value = FALSE),
                        
                        checkboxGroupInput('checboxGroupInput','seleccione opctiones',
                                           choices = LETTERS[1:5]),
                        
                        
                        textInput("TextInput","Ingrese Texto:"),
                        
                        textAreaInput("TextAreaINput","Ingrese parrafo"),
                        
                        actionButton("ActioButton","Ok"),
                        
                        actionLink("ActionLink","Siguiente")
                        
                        
                        
                ),

                # Show a plot of the generated distribution
                mainPanel(
                           h2("Slider sencillo"),
                           verbatimTextOutput("slider-io"),
                           h2("Slider input rango"),
                           verbatimTextOutput("slider-io2"),
                           h2("Select Input"),
                           verbatimTextOutput("Select-io"),
                           h2("Select Input Multiple"),
                           verbatimTextOutput("Select-io2"),
                           h2("Select Date"),
                           verbatimTextOutput("DateIO"),
                           h2("Select Date range"),
                           verbatimTextOutput("DateIO2"),
                           h2("Select numeric"),
                           verbatimTextOutput("numericIO"),
                           h2("Select checkbox"),
                           verbatimTextOutput("checkboxIO"),
                           h2("Select checkbox group"),
                           verbatimTextOutput("checboxGroupIO"),
                           h2("Select Text"),
                           verbatimTextOutput("TextIO"),
                           h2("Select Text Area"),
                           verbatimTextOutput("TextAreaIO"),
                           verbatimTextOutput("buttonIO"),
                           verbatimTextOutput("linkIO")
                           
                )
    )
))
