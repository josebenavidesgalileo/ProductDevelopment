#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("graficas en shinny"),
    
    tabsetPanel(
        tabPanel("Plot User Interaction",
                 plotOutput("plot_click_options",
                            click = "clk",
                            dblclick = "dclk",
                            hover ="mhover",
                            brush = "mbrush"
                            
                 ),
                 verbatimTextOutput("click_data"),
                 tableOutput("mtcars_tbl")
        )
    )
    
 )
)
