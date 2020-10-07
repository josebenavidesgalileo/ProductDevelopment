#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

   archivo.carga_1 <- reactive({
   
      if(is.null(input$UploadFile1)){
        
        return(NULL)
        
      }
    
     
      ext <- strsplit(input$UploadFile1$name,split="[.]")[[1]][2]
      
      if(ext== "csv")
      {
        file_data <- readr::read_csv(input$UploadFile1$datapath)
        return(file_data)
      }
      
      if(ext== "tsv")
      {
        file_data <- readr::read_tsv(input$UploadFile1$datapath)
        return(file_data)
      }
      
     
      return(NULL)
   })
   
   output$Contenido_archivo_1 <- renderTable({
      archivo.carga_1()
   })
   
   
   archivo.carga_2 <- reactive({
     
     if(is.null(input$UploadFile2)){
       
       return(NULL)
       
     }
     
     
     ext <- strsplit(input$UploadFile2$name,split="[.]")[[1]][2]
     
     if(ext== "csv")
     {
       file_data <- readr::read_csv(input$UploadFile2$datapath)
       return(file_data)
     }
     
     if(ext== "tsv")
     {
       file_data <- readr::read_tsv(input$UploadFile2$datapath)
       return(file_data)
     }
     
     
     return(NULL)
   })
   
   output$Contenido_archivo_2 <- DT::renderDataTable({
     
     archivo.carga_2() %>% DT::datatable(filter="top")
     
   })
   
   output$tabla1 <- DT::renderDataTable({
     diamonds %>% 
       datatable() %>% 
       formatCurrency("price") %>%
       formatString(c("x","y","z"),suffix = "mm")
   })
   
   output$tabla2 <- DT::renderDataTable({
     mtcars %>% DT::datatable(options = list(pageLength=5,
                                             lengthmenu= c(5,10,15)
                                             ),
                              filter = 'top'
                              )
   })
   
   output$tabla3 <- DT::renderDataTable({
              iris %>% datatable(extensions = "Buttons",
                                 options=list(dom="Bftrip",
                                              buttons=c("csv")
                                              ),
                                 rownames = FALSE
                                )
   })
   
   
   output$tabla4 <- renderDataTable({
                                     mtcars %>% datatable(selection = "single")                      
                                    })
   
   output$tabla4SingleClick <- renderText({
            input$tabla4_rows_selected
   })
   
   
   output$tabla5 <- renderDataTable({
     mtcars %>% datatable()                      
   })
   
   output$tabla5SingleClick <- renderText({
     input$tabla5_rows_selected
   })
   
   
   output$tabla6 <- renderDataTable({
     mtcars %>% datatable(selection = list(mode="single",
                                           target="column"))                      
   })
   
   output$tabla6SingleClick <- renderText({
     input$tabla6_columns_selected
   })
   
   output$tabla7 <- renderDataTable({
     mtcars %>% datatable(selection = list(mode="multiple",
                                           target="column"))                      
   })
   
   output$tabla7SingleClick <- renderText({
     input$tabla7_columns_selected
   })
   
   output$tabla8 <- renderDataTable({
     mtcars %>% datatable(selection = list(mode="single",
                                           target="cell"))                      
   })
   
   output$tabla8SingleClick <- renderText({
     input$tabla8_cells_selected
   })
   
   output$tabla9 <- renderDataTable({
     mtcars %>% datatable(selection = list(mode="multiple",
                                           target="cell"))                      
   })
   
   output$tabla9SingleClick <- renderText({
     input$tabla9_cells_selected
   })
   
   

})


