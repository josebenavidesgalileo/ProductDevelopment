#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
mtcars$name <- row.names(mtcars)


selected_points <- mtcars[0, ]

selected_pointsdbl <- mtcars[0, ]


selected_pointsbrush <- mtcars[0, ]

selected_pointshover <- mtcars[0, ]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
   
    
    output$plot_click_options <- renderPlot({
       
        ggplot(mtcars, aes(x = mpg, y = wt)) +
            geom_point(colour="magenta") +
            geom_point(data = selectedhover(),colour = "gray") +
            geom_point(data = selected(), colour = "green") +
            geom_point(data = selectedbrush(),colour = "blue")
        
       
    })
    
    
    selectedhover <- reactive({
        
       
        
        if(!is.null(input$mhover$x))
        {
            
            selected_pointshover <<- rbind(selected_pointshover, nearPoints(mtcars, input$mhover))
            
        }
        
        if(!is.null(input$dclk$x))
        {
            remove <- mtcars[0, ]
            remove <- rbind(remove, nearPoints(mtcars, input$dclk))
            selected_pointshover <<- selected_pointshover[!(selected_pointshover$name %in% remove$name),]
            
        }
        
        
        
    
        return(selected_pointshover)
    })
    
    
    selected <- reactive({
        
        if(!is.null(input$dclk$x))
        {
            remove <- mtcars[0, ]
            remove <- rbind(remove, nearPoints(mtcars, input$dclk))
            selected_points <<- selected_points[!(selected_points$name %in% remove$name),]
            
        }
        
        
        
        
        if(!is.null(input$clk$x))
        {
            
            selected_points <<- rbind(selected_points, nearPoints(mtcars, input$clk))
            
            
        }
        
        
       
        return(selected_points)
    })
    
    
    
    selecteddbl <- reactive({
        
        
        
        
        
        
        if(!is.null(input$dclk$x))
        {
            
            
            selected_pointsdbl <<- rbind(selected_pointsdbl, nearPoints(mtcars, input$dclk))
           
            
        }
        
        return(selected_pointsdbl)
    })
    
    
    
    selectedbrush <- reactive({
        
        
        if(!is.null(input$clk$x))
        {
            remove <- mtcars[0, ]
            remove <- rbind(remove, nearPoints(mtcars, input$clk))
            selected_pointsbrush <<- selected_pointsbrush[!(selected_pointsbrush$name %in% remove$name),]
            
        }
        
        if(!is.null(input$dclk$x))
        {
            remove <- mtcars[0, ]
            remove <- rbind(remove, nearPoints(mtcars, input$dclk))
            selected_pointsbrush <<- selected_pointsbrush[!(selected_pointsbrush$name %in% remove$name),]
            
        }
        
        
        if(!is.null(input$mbrush$xmin))
        {
            
            mtcars_df <-cbind(row.names(mtcars),mtcars)
          
            selected_pointsbrush <<- rbind(selected_pointsbrush, brushedPoints(mtcars_df,brush=input$mbrush))
            
        }
        
        
        str(selected_pointsbrush)
        return(selected_pointsbrush)
    })
    
    output$mtcars_tbl <- renderTable({
        
       
            mtcars_df <-cbind(row.names(mtcars),mtcars)
    
            
            df <- brushedPoints(mtcars_df,brush=input$mbrush)
            
            if(nrow(df) != 0)
            {
                
                df
                
            }
            else 
            {
                NULL   
            }
        
    })

})
