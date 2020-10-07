#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("cargar archivo DT"),

    tabsetPanel(tabPanel("cargar archivo",
                         sidebarLayout(
                             sidebarPanel(
                                 h2("Subir Archivo"),
                                 fileInput("UploadFile1",label="cargar archivo ...",buttonLabel = "cargar")
                             ),
                             mainPanel(
                                 tableOutput("Contenido_archivo_1")
                                 
                             )
                         )),
                
                tabPanel("Cargar Archivo DT",
                         sidebarLayout(
                             sidebarPanel(
                                 h2("Subir Archivo"),
                                 fileInput("UploadFile2",
                                           label="cargar archivo ...",
                                           buttonLabel = "cargar",
                                           accept =c(".csv",".tsv"))
                             ),
                             mainPanel(
                                 DT::dataTableOutput("Contenido_archivo_2")
                                 
                             )
                         )),
                
                tabPanel("DT Option",
                         fluidRow(column(width=12,
                                         DT::dataTableOutput("tabla1")
                                         )
                                  ),
                         fluidRow(column(width=12,
                                         DT::dataTableOutput("tabla2")
                                         )
                                  ),
                         fluidRow(column(width=12,
                                         DT::dataTableOutput("tabla3")
            
                                        )
                             
                                ),
                         
                        ),
                tabPanel("Clicks",
                         fluidRow(column(width=12,
                                         h2("click en una fila"),
                                         dataTableOutput("tabla4"),
                                         verbatimTextOutput("tabla4SingleClick")
                         )
                         ),
                         fluidRow(column(width=12,
                                         h2("click en multiples filas"),
                                         dataTableOutput("tabla5"),
                                         verbatimTextOutput("tabla5SingleClick")
                         )
                         ),
                         fluidRow(column(width=12,
                                         h2("click en una columna"),
                                         dataTableOutput("tabla6"),
                                         verbatimTextOutput("tabla6SingleClick")
                         )
                         ),
                         
                         fluidRow(column(width=12,
                                         h2("click multiples columnas"),
                                         dataTableOutput("tabla7"),
                                         verbatimTextOutput("tabla7SingleClick")
                         )
                         ),
                         fluidRow(column(width=12,
                                         h2("click una cell"),
                                         dataTableOutput("tabla8"),
                                         verbatimTextOutput("tabla8SingleClick")
                         )
                         ),
                         fluidRow(column(width=12,
                                         h2("click multiples cells"),
                                         dataTableOutput("tabla9"),
                                         verbatimTextOutput("tabla9SingleClick")
                                        )
                                 )
                    )
             )
    ))
