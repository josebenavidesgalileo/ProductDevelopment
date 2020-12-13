#setwd("~/XrJ Dreams/Xperiments/appCovid19Analisis")

library(shinyjs)
library(shinyBS)
library(leaflet)
library(purrr)
library(leaflet.extras)
library(RMySQL)
library(pool)
library(scales)
library(hrbrthemes)
#install.packages("plotly")
#install.packages("shinyBS")
#install.packages("shinythemes")
#install.packages("ggfortify")

library(shinydashboard)
library(flexdashboard)
library(shinyWidgets)
library(formattable)
library(lubridate)
library(ggfortify)
library(forecast)
require(reshape2)
library(quantmod)
require(ggplot2)
library(tseries)
library(readxl)
require(plotly)
library(dplyr)
library(tidyr)
library(shiny)
library(httr)
library(xts)
library(DT)









############################################################
############################################################
##################  X Shiny    #############################
############################################################
############################################################


shinyServer(function(input, output, session) {
#  observe({
#    if(input$showpanel == TRUE) {
#      js$showSidebar()
#    }
#    else {
#      js$hideSidebar()
#    }
#  })


all_cons <- dbListConnections(MySQL())
    
    
    for(con in all_cons)
        +  dbDisconnect(con)
    
    
    
    
    cancel.onSessionEnded <- session$onSessionEnded(function() {
        all_cons <- dbListConnections(MySQL())
        
        
        for(con in all_cons)
            +  dbDisconnect(con)
        
    })
    
    mydb = dbPool(RMySQL::MySQL(), 
                  user='root', 
                  password='test123', 
                  dbname='Covid', 
                  host='airflow-main_db_1',
                  port=3306)


#dfALLSumarizado <- read.csv('StagingArea/ALLSumarizado.csv')
dfALLSumarizado = as.data.frame(mydb %>% tbl("ALLSumarizado"))
names(dfALLSumarizado)[names(dfALLSumarizado) == "Pais.Region"] <- "Pais/Region"

dfTSConfirmed<- read.csv('StagingArea/SerieMundialConfirmados.csv',check.names=FALSE)
names(dfTSConfirmed)[names(dfTSConfirmed) == ""] <- "Dia"

dfTSMuertes<- read.csv('StagingArea/SerieMundialMuertes.csv',check.names=FALSE)
names(dfTSMuertes)[names(dfTSMuertes) == ""] <- "Dia"

dfTSRecuperados<- read.csv('StagingArea/SerieMundialRecuperados.csv',check.names=FALSE)
names(dfTSRecuperados)[names(dfTSRecuperados) == ""] <- "Dia"
#### End  header -Para series de tiempo


guatemalaLatLong <- read_excel("StagingArea/GuatemalaLatLong.xlsx")
### Quitar valores departamentos que no tengan valores mayores a 0, es decir, que no tengan casos
guatemalaLatLong <- guatemalaLatLong[!is.na(guatemalaLatLong$valores>0), ]



#################### INICIO de comentarios opcionales
#### Por el momento no es necesario utilizar el siguiente codigo, 
### ya que se encuentra en el archivo de carga y genera datos


#Leer desde archivo CSV
#    dfCovid19_HOPKINS <- read.csv('03-25-2020.csv',stringsAsFactors = FALSE)
#dfALLSumarizado <- read.csv('ALLSumarizado.csv',stringsAsFactors = FALSE)


### Renombrar columnas a espaniol
#    names(dfCovid19_HOPKINS)[names(dfCovid19_HOPKINS) == "Province_State"] <- "Provincia"
#    names(dfCovid19_HOPKINS)[names(dfCovid19_HOPKINS) == "Country_Region"] <- "Pais/Region"
#    names(dfCovid19_HOPKINS)[names(dfCovid19_HOPKINS) == "Last_Update"] <- "Actualizacion"
#     names(dfCovid19_HOPKINS)[names(dfCovid19_HOPKINS) == "Confirmed"] <- "Confirmados"
#     names(dfCovid19_HOPKINS)[names(dfCovid19_HOPKINS) == "Deaths"] <- "Muertes"
#     names(dfCovid19_HOPKINS)[names(dfCovid19_HOPKINS) == "Recovered"] <- "Recuperados"
#     
#     ### Obtener dataframe con el conteo de todo, ya resumido
#     dfSumaGeneral <- dfCovid19_HOPKINS %>%
#       group_by(`Pais/Region`) %>%
#       select(`Pais/Region`, `Confirmados`,`Recuperados` ,`Muertes`) %>%
#       summarise_all(funs(sum))

### Calcular, agregar columnas de porcentajes a un nuevo dataFrame y Formatear a tipo porcentaje %

#dfPorcentajes <- dfSumaGeneral

###########    
#################### FIN de comentarios opcionales


dfPorcentajes <- dfALLSumarizado

dfPorcentajes$"% Muertes" <- percent((dfPorcentajes$Muertes/dfPorcentajes$Confirmados))
dfPorcentajes$"% Recuperados" <- percent(dfPorcentajes$Recuperados/dfPorcentajes$Confirmados)
dfPorcentajes$"% EnTratamiento*" <-percent(
  (dfPorcentajes$Confirmados-dfPorcentajes$Recuperados-dfPorcentajes$Muertes)/dfPorcentajes$Confirmados) 

### Reemplazando valors NaN por cero. Esto se produce cuando se divide 0/0, debido a las operaciones anteriores.
dfPorcentajes <- replace(dfPorcentajes, is.na(dfPorcentajes), 0)

#Crear Dataframes especificos para Centroamerica
listaPaisesCA<-c("Guatemala","Costa Rica","El Salvador","Panama","Nicaragua", "Honduras","Mexico")
dfPaisesCA<-filter(dfPorcentajes, dfPorcentajes$`Pais/Region` %in% listaPaisesCA)

dfGuatemala<-filter(dfPorcentajes, dfPorcentajes$`Pais/Region` =="Guatemala")

#####
dfGuatemala$OtraCausa <- 1

dfGuatemala$"EnTratamiento" <-
  (dfGuatemala$Confirmados-dfGuatemala$Recuperados-dfGuatemala$Muertes)
dfGuatemala$"EnTratamiento" <- dfGuatemala$"EnTratamiento"-dfGuatemala$OtraCausa

#### Para series de tiempo
### Para Centroamerica
#### Se observa que la primera fecha de contagio ocurre en Mexico, el 28 de febrero.
##### Dicha fecha tiene la posición 38 del arreglo, por esa razón comienza en 38
###### Hasta el final, es decir, hasta la cantidad de filas. [38:,]
dfTS_Centroamerica <- dfTSConfirmed  %>%
  select("Dia",listaPaisesCA)
numeroFilas <- nrow(dfTS_Centroamerica)

dfTS_Centroamerica <- dfTS_Centroamerica[38:numeroFilas,]

### 


          

    
   
    
    CasesTypes = as.data.frame(mydb %>% tbl("Types"))
    data = as.data.frame(mydb %>% tbl("Totals"))
    datadetial = as.data.frame(mydb %>% tbl("Cases"))
    
    dataCofirmed = filter(as.data.frame(mydb %>% tbl("Totals")),Type == "C")
    dataRecover = filter(as.data.frame(mydb %>% tbl("Totals")),Type == "R")
    dataDead = filter(as.data.frame(mydb %>% tbl("Totals")),Type == "D")
  
   #definir el color para los typos de caso 
    pal <- colorFactor(
        palette = c('blue', 'red','green'),
        domain = CasesTypes$Type)
    
    
    
    vCountry = "Guatemala"
    v = reactiveValues( plottt = NULL)
    
    
    

    #create the map
    output$mymap <- renderLeaflet({
        
        proxy <- leafletProxy("mymap", data = data)
        proxy %>% clearMarkers()
        
        leaflet(filter(data,Type==input$Typos)) %>% 
            setView(lng = 0, lat = 0, zoom = 2)  %>% #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = filter(data,Type==input$Typos), 
                       layerId = ~ Country,
                       lat = ~ Lat, lng = ~ Long, 
                       weight = 1,
                       radius = ~sqrt(total) *10, 
                       color = ~pal(input$Typos),
                       label = ~as.character(paste0(as.integer(total/1000),"K")),
                       popup = ~as.character(paste0("Total: ", sep = " ", total)),
                       fillOpacity = 0.5)
    })
    
    observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks
        p <- input$mymap_shape_click
        #vCountry <<- unique(filter(datadetial,Lat == p$lat , Long == p$lng)['Country'])[1,1]
        #browser()
        output$TimeFrame = renderPlot({plotseries(p$id)})
    })
    
    plotseries <- function (n)({
        result = NULL
        
        result = filter(datadetial,Country==n & Type == input$Typos)
        
        ggplot(result, aes(x=as.Date(result$RegDate), y=Count)) +
            geom_line( color="steelblue") +  
            xlab("") +
            theme_ipsum() +
            ggtitle(n) +
            theme(axis.text.x=element_text(angle=60, hjust=1)) +
            scale_x_date(
                labels = date_format("%Y-%m"),
                breaks = "1 month") 
        
        
    })
    
    output$TimeFrame = renderPlot({
        
            plotseries('Guatemala')
        
        })

  
  observeEvent(input$btnGuatemala, {
      updateTabItems(session, "miBarraDeLado", "Guatemala")
    })
  observeEvent(input$btnCentroamerica, {
    updateTabItems(session, "miBarraDeLado", "Centroamerica")
  })
  observeEvent(input$btnOtrosPaises, {
    updateTabItems(session, "miBarraDeLado", "OtrosPaises")
  })
  observeEvent(input$btnCentrosdeAyuda, {
    updateTabItems(session, "miBarraDeLado", "CentrosdeAyuda")
  })
  observeEvent(input$btnMapa, {
    updateTabItems(session, "miBarraDeLado", "MapaGuatemala")
  })
  
  
  
  
  ## Boton informacion  
  observeEvent(input$btnInformacion, {
    txtInformacion <- "La actualización de datos se lleva a cabo por medio de las fuentes oficiales 
    del centro \"CSSE at Johns Hopkins University\" y también del centro  \"European Centre for
    Disease Prevention and Control  \". <br> <br>Para Centroamérica los casos reportados
    se van actualizando en tiempo real conforme a los comunicados oficiales de cada pais. <br><br>
    Link de fuentes de datos: <br>
    <a href=\"https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases\">European 
    Centre for Disease Prevention and Control</a>
    <br>
    <a href=\"https://github.com/CSSEGISandData/COVID-19\">Data Repository by Johns Hopkins CSSE</a>
    <br>
    <a href=\"https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports\">World Health Organization</a>
    
    <br>
    <br>
    Si deseas reportar nuevos casos, por favor escribenos a \"502analitycs@gmail.com\". No olvides incluir 
    la referencia de la noticia oficial por medio de un link.<br><br>
    
    <a href=\"https://drive.google.com/file/d/1QCBcBPpeJjIFHKqcnOvZ_PDQdUAACA0m/view?usp=sharing\">VER MANUAL DE USUARIO</a>
    <br><br>
    
    Saludos desde Guatemala, <br> <br>  <b> Ing. Erick J. Pineda Amézquita</b> <br>
    502analitycs@gmail.com"
    
    showModal(modalDialog(
      title = "Información sobre la fuente de datos",
      HTML('<span style="color:#5181c9; font-size: 15px; font-weight:bold; font-family:sans-serif ">' , txtInformacion,'<span>'),
      footer = modalButton("Enterado")
      #,div("This is an important message!", style="font-size:160%")
    )
    )
  })  ##FIN Button Info
  
  
  ## Ejemplo para usar renderPlot    
  output$distPlot <- renderPlot({
    valorY <- dfPaisesCA$Confirmados
    ggplot(dfPaisesCA, aes(x = dfPaisesCA$`Pais/Region`, y =valorY  )) 
    +geom_col(fill = "#263e63")
  })### FIN output$distPlot
  ## Ejemplo de funcion  
  plot_trends <- function(){
    dfPaisesCA %>% 
      filter(name == input$name) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_col()
  }  # FIN plot_trends
  ## Ejemplo de group by con media  
  output$duration_table <- renderTable({
    dfCovid19_HOPKINS %>%
      #dfPaisesCA %>%
      #filter(
      #  `Pais/Region` == input$Pais
      #) %>%
      #group_by(Muertes) %>%
      #group_by(input$parametro) %>%
      group_by(Confirmados) %>%
      summarize(
        nb_sighted = n(),
        avg_duration_min = mean(Muertes) / 60,
        median_duration_min = median(Confirmados) / 60
      )
  })   # FIN duration_table
  
  
  
  
  
  ######################### FUNCIONES ################################  
  
  
  obtenerXTSSeriePaisIndivual <- function(dataSet, nombrePais,esIndividual,primerDiaDetectado){
    
    fechas <- as.Date(dataSet$Dia,format = "%m/%d/%y")
    cantidadFilas <- nrow(dataSet) ## Esto es para obtener el dia final :O
    cantidadColumnas <- ncol(dataSet)
    
    if(esIndividual){
      xps <-  nombrePais
      variableTsConfirmados <- get(xps,dataSet)
      variableTsConfirmados<- as.numeric(variableTsConfirmados)
      
      variableTsRecuperados <- get(xps,dfTSRecuperados)
      variableTsRecuperados<- as.numeric(variableTsRecuperados)
      
      variableTsMuertes <- get(xps,dfTSMuertes)
      variableTsMuertes <- as.numeric(variableTsMuertes)
      
      
        
      #cuerpo <- dfTSConfirmed$Guatemala[primerDiaDetectado:cantidadFilas]
      
      ## Buscar el dia que sea mayor a cero, nos dara el valor del primer reporte de contagio
      primerDiaDetectado <- which(variableTsConfirmados>0)[1]
      dfCantidadConfirmados <- variableTsConfirmados[primerDiaDetectado:cantidadFilas]
      dfCantidadMuertos <- variableTsMuertes[primerDiaDetectado:cantidadFilas]
      dfCantidadRecuperados <- variableTsRecuperados[primerDiaDetectado:cantidadFilas]
      
      #cuerpo <- variableTsConfirmados[primerDiaDetectado:cantidadFilas]
      cuerpo <- data.frame(dfCantidadConfirmados,dfCantidadMuertos,dfCantidadRecuperados)
      names(cuerpo) <- c("Confirmados","Fallecidos","Recuperados")
      #cuerpo <- data.frame(cuerpo,variableTsConfirmados)
      
      
    } else {
      
      cuerpo <- dataSet[primerDiaDetectado:cantidadFilas,2:cantidadColumnas]   ##Equivale a dfTSConfirmed[,2:]
    }
    
    ## Obtiene el valor de las fechas a partir de elementos TRUE
    fechas <- fechas[primerDiaDetectado:cantidadFilas]
    fechas <- as.Date(fechas,format = "%m/%d/%y")
    
    xtsSeriePaisIndividual <- xts(x = cuerpo, order.by = fechas)
    
    #names(xtsSeriePaisIndividual)[1]
    
    
    
    #if(esIndividual){
    #  names(xtsSeriePaisIndividual)[1]<-paste(nombrePais) 
    #} 
    
    
    return(xtsSeriePaisIndividual)
    
  } ## FIN funcion obtenerXTSSeriePaisIndivual
  
  
  obtenerGraficaGgplot2 <- function(dataSet_O_xts, nombrePais,espaciadoDias){
    diaFin <- nrow(dataSet_O_xts)
    #espaciadoDias <- "10"
    txtEspaciadoDias <- paste(espaciadoDias,"day")
    
    r <- ggplot(broom::tidy(dataSet_O_xts),
                aes(x=index,y=value, group = 1, color=series, type = 'scatter', mode = 'lines'#coco=
                    ,text=paste(series, "<br>Fecha: ", index, "<br>Cantidad: ", scales::comma(as.numeric(value)),"<br>","Dia: ",c(1:diaFin)
                    )#Fin paste
                ) ##Fin aes
    ) + geom_line(size=1) + geom_point() +
      scale_y_continuous(labels = scales::comma)+
      scale_x_date(date_breaks = txtEspaciadoDias, date_labels = paste("%b %d","\nDia:" ,seq(1,diaFin,by=as.numeric(espaciadoDias))) )+
      theme(axis.text.x=element_text(angle=45, hjust=1,size = 10),  legend.position="top") #+
      #theme(plot.title = element_text(size=22))
    
    
    
    r <-ggplotly(r, tooltip = c("text")#,dynamicTicks = TRUE
                 ) %>% 
      #rangeslider() %>% layout(hovermode = "x")%>% 
      plotly::config(displayModeBar = F)  %>%
      layout(autosize = T,legend = list(orientation = "v", x = 0, y = 1)  ,
             #title=nombrePais,
             title=list(text = nombrePais, y = 1,x=0.7),
             xaxis = list(title = "Días", automargin=T, ticksuffix = " ",fixedrange=TRUE)
             ,yaxis = list(title = "Cantidad Confirmados", automargin=T, ticksuffix = "   ",fixedrange=TRUE)
             
             
             
      #,height = '100%', width = '100%'
      )  #%>% layout(
         #xaxis2 = list(seq(1,diaFin,by=as.numeric(espaciadoDias)))
         #)
    
    
    return(r)
    
  }  ## FIN obtenerGraficaGgplot2
  
  
  
  graficarTimeSeries <- function(dataSet){
    #dfTSConfirmed <- dfTSConfirmed[,1:10]
    cantidadColumnas <- ncol(dataSet)
    
    fechas <- as.Date(dataSet$Dia,format = "%m/%d/%y")
    cuerpo <- dataSet[,2:cantidadColumnas]   ##Equivale a dfTSConfirmed[,2:]
    
    xtsSerieGenerica <- xts(x = cuerpo, order.by = fechas)
    
    ##Con una sola linea funciona
    #tidy(xtsSerieGuatemala) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line()
    
    r <- ggplot(broom::tidy(xtsSerieGenerica),
                aes(x=index,y=value, group = 1, color=series, type = 'scatter', mode = 'lines'
                    ,text=paste("Pais: ",series,"<br>Dia: ", index, "<br>Cantidad: ", scales::comma(as.numeric(value))
                    )#Fin paste
                ) ##Fin aes
    ) + geom_line(size=1) + geom_point() +
      scale_y_continuous(labels = scales::comma)+
      scale_x_date(date_breaks = "7 day", date_labels = "%b %d")+
      theme(axis.text.x=element_text(angle=45, hjust=1,size = 12),  legend.position="top")
    
    
    
    r <-ggplotly(r, tooltip = c("text")) %>% plotly::config(displayModeBar = F)  %>%
      layout(legend = list(orientation = "v", x = 0.05, y = 1)  ,
             # title="Guatemala",
             xaxis = list(title = "Días", automargin=T, ticksuffix = "%",fixedrange=TRUE)
             ,yaxis = list(title = "Cantidad", automargin=T, ticksuffix = "   ",fixedrange=TRUE)
      )
    
  }
  
  
  #### Función reactiva, para ordenar paises en forma descendente 
  mostrarPaisesSerieVertical <- reactive({
    dfPorcentajes %>% 
      #filter(`Pais/Region` == input$sliderSerieVertical) %>% 
      #arrange(-desc(`Pais/Region`)) %>%
      arrange(desc(Confirmados)) %>% 
      head(input$sliderSerieVertical)
  })
  
  
  ################## FIN FUNCIONES ###############################
  
  
  
  #### Graficar paises CA por tipo categoria
  output$paisesCA_porTipo_singular <- plotly::renderPlotly({
    #output$paisesCA_porTipo_singular <- renderPlot({  
    #output$paisesCA_porTipo_singular <- renderDT({  
    Pais<-dfPaisesCA$`Pais/Region`
    
    #Cantidad <- sym(input$parametro)
    
    xps <-  input$parametro
    Cantidad <- get(xps,dfPaisesCA)
    Cantidad<- as.numeric(Cantidad)
    Titulo <- paste(Cantidad,"Centroamérica y México",sep = " ")
    
    
    #xtreme <- ggplot(dfPaisesCA, aes(x = Pais, y =  Cantidad)) +
    # geom_col(fill = "#7ca0d6")+labs(title=Titulo, x ="Pais", y = "Cantidad") +
    #  theme(axis.text.x=element_text(angle=45, hjust=1))+
    # geom_text(
    #  aes(label = Cantidad, y = Cantidad),
    #  position = position_dodge(10.0),
    # vjust = 0,show.legend = FALSE
    #) 
    #xtreme
    
    
    #objetoGgplot <- ggplot(dfPaisesCA, aes(x= Pais,  y=Cantidad, group=Cantidad)) + 
    #  geom_bar(aes(y = Cantidad, fill = factor(..x..)), stat="sum") +
    #  geom_text(position = position_dodge(3.0),aes( label = scales::percent(Cantidad),
    #                  y= as.numeric(`% Muertes`) ), stat= "sum", vjust = -.5) +
    #  labs(title=Titulo, y = "Percent", fill="day") +
    #    #facet_grid(~sex) +
    # theme(axis.text.x=element_text(angle=45, hjust=1))#+
    #scale_y_continuous(labels = scales::percent)
    
    
    
    if (xps == "% Recuperados" || xps == "% Muertes"){
      #objetoGgplot <- ggplot(dfPaisesCA, aes(x= Pais,  y=Cantidad, group=Cantidad)) + 
      objetoGgplot <- ggplot(dfPaisesCA, 
                             aes(x= reorder(Pais,-Cantidad),  group=Cantidad
                                             ,text=paste("Pais: ",Pais, "<br>Cantidad: ", scales::comma(as.numeric(Cantidad))
                                             )#Fin paste
                                ) ##Fin aes
                             ) + 
        geom_bar(aes(y = Cantidad, fill = Pais), stat="sum") +
        geom_text(position = position_dodge(3.0),
                  aes( label = scales::percent(Cantidad),
                       y= Cantidad ), stat= "sum", vjust = -.5) +
        labs(title=Titulo, y = "Cantidad en %", fill="Pais",x="Paises Centroamericanos") +
        #facet_grid(~sex) +
        theme(axis.text.x=element_text(angle=45, hjust=1))+
        scale_y_continuous(labels = scales::percent)
      
    }else {
      
      #objetoGgplot <- ggplot(dfPaisesCA, aes(x= Pais,  y=Cantidad, group=Cantidad)) + 
      objetoGgplot <- ggplot(dfPaisesCA, 
                             aes(x= reorder(Pais,-Cantidad),  group=Cantidad
                                 
                             ) ##Fin aes
                        ) +
        geom_bar(aes(y = Cantidad, fill = Pais), stat="sum") +
        geom_text(position = position_dodge(3.0),
                  aes( label = scales::comma(Cantidad),
                       y= Cantidad ), stat= "sum", vjust = -.5) +
        labs(title=Titulo, y = "Cantidad", fill="Pais",x="Paises Centroamericanos") +
        #facet_grid(~sex) +
        theme(axis.text.x=element_text(angle=45, hjust=1))+
        scale_y_continuous(labels = scales::comma)
    }
    
    
    #objetoGgplot
    return(ggplotly(objetoGgplot, tooltip = c("")) %>% plotly::config(displayModeBar = F)  %>%
             layout(legend = list(orientation = "h", x = 0.01, y = 5.2)
                    ,xaxis = list(automargin=T, ticksuffix = " ",fixedrange=TRUE)
                    ,yaxis = list(automargin=T, ticksuffix = "  ",fixedrange=TRUE)
                    
                    )
           )
    
  } ) #FIN paisIndividual
  
  
  
  ## Graficar Guatemala tres barras
  output$paisGuatemala_Multibarras <- plotly::renderPlotly({
    
    ## quitar columnas
    dfPaisesCA_Numeros <-select (dfGuatemala,-c("% EnTratamiento*","% Muertes","% Recuperados"))
    dfPaisesCA_Porcentajes <-select (dfGuatemala,-c("Confirmados","Muertes","Recuperados","EnTratamiento","OtraCausa"))
    Titulo <- paste("Datos Guatemala",sep = " ")
    
    
    
    
    if(input$selector_gt_tipo == "En numeros"){
      
      dfContenidoGather <-  dfPaisesCA_Numeros %>%
        gather("Type", "Value",-`Pais/Region`) 
      
      myObjetoGGPlot <- ggplot(dfContenidoGather, aes(x=Type, y=Value, group=`Pais/Region`, fill=factor(Type)
                               ,text=paste("Pais: ",`Pais/Region`, "<br>Tipo: ", Type,"<br>Cantidad: ",Value
                               )#Fin paste
      ) ##Fin aes
      ) +##Fin gplot                        
                 
        geom_bar(position = "dodge", stat = "identity") +
        labs(title=Titulo, x ="Pais", y = "Cantidad",fill="Categoria")+
        theme(axis.text.x=element_blank(),  legend.position="right",legend.direction = "horizontal")+
        #theme(axis.text.x=element_text(angle=45, hjust=1, size = 7),  legend.position="top")+
        facet_grid(~`Pais/Region`)    +
        geom_text(size=3.19,aes( label = scales::comma(as.integer(Value))
                                 #               ,y= Value )
        )
        ,
        stat= "sum", vjust = -.5,position = position_dodge(0.001)) +
        scale_y_continuous(labels = scales::comma)+
        theme(panel.spacing = unit(0.05, "lines"))
      
      
      
      
    }else {
      
      
      dfContenidoGather <-  dfPaisesCA_Porcentajes %>%
        gather("Type", "Value",-`Pais/Region`) 
      
      myObjetoGGPlot <- ggplot(dfContenidoGather, dynamicTicks = TRUE,aes(x=Type, y=Value, group=`Pais/Region`, fill=factor(Type)
                                                      ,text=paste("Pais: ",`Pais/Region`, "<br>Tipo: ", Type,"<br>Cantidad: ", Value
                                                      )#Fin paste
      ) ##Fin aes
                               
                               ) +
        geom_bar(position = "dodge", stat = "identity") +
        labs(title=Titulo, x ="Guatemala", y = "Cantidad",fill="Categoria")+
        theme(axis.text.x=element_blank(),  legend.position="top")+
        #theme(axis.text.x=element_text(angle=45, hjust=1,size = 7),  legend.position="top")+
        facet_grid(~`Pais/Region`)    +
        geom_text(size=3.19,aes( label = scales::percent(as.numeric(Value))
                                 #                         , y= Value 
        )
        , stat= "sum", vjust = -.5,position = position_dodge(0.001)) +
        scale_y_continuous(labels = scales::percent)    +
        theme(panel.spacing = unit(0.05, "lines"))
      
      
      
      
      
      
    }
    
    
    return(ggplotly(myObjetoGGPlot, tooltip = c("text"),dynamicTicks = TRUE) %>% plotly::config(displayModeBar = F)  %>%
             layout(legend = list(orientation = "h", x = 0, y = -0.2),
                    xaxis = list(title = "Datos segun clasificación", automargin=T, ticksuffix = "%",fixedrange=TRUE)
                    ,yaxis = list(title = " ", automargin=T, ticksuffix = "   ",fixedrange=TRUE)
                    
                    ) #%>% ##Fin Layout
           #rangeslider() %>%
            # layout(hovermode = "x")
           )
    
    
  } ) ##FIn generalGuatemala 3 barras
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Graficar todos los paises de CA con tres barras
  output$paisesCA_Multibarras <- plotly::renderPlotly({
    
    ## quitar columnas
    dfPaisesCA_Numeros <-select (dfPaisesCA,-c("% EnTratamiento*","% Muertes","% Recuperados"))
    dfPaisesCA_Porcentajes <-select (dfPaisesCA,-c("Confirmados","Muertes","Recuperados"))
    Titulo <- paste("Comparación entre paises",sep = " ")
    
    
    
    
    if(input$selector_ca_tipo == "En numeros"){
      
      dfContenidoGather <-  dfPaisesCA_Numeros %>%
        gather("Type", "Value",-`Pais/Region`) 
      
      myObjetoGGPlot <- ggplot(dfContenidoGather, aes(x=Type, y=Value, group=`Pais/Region`, fill=factor(Type)
                               #No lo agarra or el geomText supongo  
                               ,text=paste("País:",`Pais/Region`,"<BR>Valor:",scales::comma(as.numeric(Value)))
      )) +
        geom_bar(position = "dodge", stat = "identity") +
        labs(title=Titulo, x ="Pais", y = "Cantidad",fill="Categoria")+
        theme(axis.text.x=element_blank(),  legend.position="center",legend.direction = "horizontal")+
        #theme(axis.text.x=element_text(angle=45, hjust=1, size = 7),  legend.position="top")+
        facet_grid(~`Pais/Region`)    +
        geom_text(size=2.5,aes( label = scales::comma(as.integer(Value))
                                 #               ,y= Value )
        )
        ,
        stat= "sum", vjust = -.5,position = position_dodge(0.001)) +
        scale_y_continuous(labels = scales::comma)+
        theme(panel.spacing = unit(0.05, "lines"))
      
      
      
      
    }else {
      
      
      dfContenidoGather <-  dfPaisesCA_Porcentajes %>%
        gather("Type", "Value",-`Pais/Region`) 
      
      myObjetoGGPlot <- ggplot(dfContenidoGather, aes(x=Type, y=Value, group=`Pais/Region`, fill=factor(Type)
                                                      #No lo agarra or el geomText supongo  
                                                      ,text=paste("País:",`Pais/Region`,"<BR>Valor:",Value)
                                                      )) +
        geom_bar(position = "dodge", stat = "identity") +
        labs(title=Titulo, x ="Pais", y = "Cantidad",fill="Categoria")+
        theme(axis.text.x=element_blank(),  legend.position="top")+
        #theme(axis.text.x=element_text(angle=45, hjust=1,size = 7),  legend.position="top")+
        facet_grid(~`Pais/Region`)    +
        geom_text(size=3.19,aes( label = scales::percent(as.numeric(Value))
                                 #                         , y= Value 
        )
        , stat= "sum", vjust = -.5,position = position_dodge(0.001)) +
        scale_y_continuous(labels = scales::percent)    +
        theme(panel.spacing = unit(0.05, "lines"))
      
      
      
      
      
      
    }
    
    
    return(ggplotly(myObjetoGGPlot, tooltip = c("text")) %>% plotly::config(displayModeBar = F)  %>%
             layout(legend = list(orientation = "h", x = 0, y = -0.1)
                    ,xaxis = list(title = " ",automargin=T, ticksuffix = " ",fixedrange=TRUE)
                    ,yaxis = list(title = " ",automargin=T, ticksuffix = " ",fixedrange=TRUE)
                    ))
    
    
    

    
    
    
    
  } ) ##FIn todosCentroamerica
  
  
  
  
  
  ## Graficar tabla paises CA  
  output$tablaPaisesCA <- DT::renderDT({
    
    #output$general <- renderTable({
    # output$general <- plotly::renderPlotly({
    #dfPaisesCA
    formatoComma=c("Confirmados","Muertes", "Recuperados")
    formatoPorcentaje=c("% Muertes", "% Recuperados","% EnTratamiento*")
    
    datatable(dfPaisesCA,filter="none") %>% 
      formatPercentage(formatoPorcentaje, 2) %>% 
      formatCurrency(formatoComma,currency = "", interval = 3, mark = ",") %>% 
      formatRound(formatoComma,0)
      
    
  }) ##FIN output$general
  
  
  
  
  
  
  
  #### Mundial: Graficar una barra por pais, del top10
  output$paisesMundial_porTipo_singular <- plotly::renderPlotly({
    
    #Obtnertop10
    dfPaisesTop10 <-  dfPorcentajes[order(-dfPorcentajes$Confirmados),][1:10,]
    
    xpsEsTipoPorcentaje <-  FALSE
    
    if(input$selector_xt_individual_tipo == "Cantidad de confirmados"){
      #dfContenido  <- dfPaisesConfirmados 
      Cantidad <- dfPaisesTop10$Confirmados
      xpsEsTipoPorcentaje <-  FALSE
    }else if (input$selector_xt_individual_tipo == "Cantidad de muertes"){
      #dfContenido  <- dfPaisesMuerte
      Cantidad <- dfPaisesTop10$Muertes
      xpsEsTipoPorcentaje <-  FALSE
    } else if (input$selector_xt_individual_tipo == "Cantidad de recuperados"){
      #dfContenido  <- dfPaisesRecuperados
      Cantidad <- dfPaisesTop10$Recuperados
      xpsEsTipoPorcentaje <-  FALSE
    }else if (input$selector_xt_individual_tipo == "% Recuperados"){
      Cantidad <- dfPaisesTop10$`% Recuperados`
      xpsEsTipoPorcentaje <-  TRUE
      Cantidad <- round(Cantidad, 2)
    }else if (input$selector_xt_individual_tipo == "% Muertes"){
      Cantidad <- dfPaisesTop10$`% Muertes`
      Cantidad <- round(Cantidad, 2)
      xpsEsTipoPorcentaje <-  TRUE
    }
    
    
    Cantidad<- as.numeric(Cantidad)  
    Pais<-dfPaisesTop10$`Pais/Region`
    
    
    Titulo <- paste(Cantidad,"Top 10 de paises",sep = " ")
    
    #ggplot(dfPaisesTop10, aes(x = reorder(Pais,-Cantidad) , y =  Cantidad, 
    #                               text = paste("Pais: ", Pais,
    #                                             "<br>Cantidad:", Cantidad)
    #                                )
    #       ) + geom_col(fill = "#7ca0d6")+labs(title=Titulo, x ="Pais", y = "Cantidad") +
    #            geom_text(
    #              aes(label = Cantidad, y = Cantidad ),
    #              position = position_dodge(8.9),
    #              vjust = 0
    #            )+ 
    #            theme(axis.text.x=element_text(angle=45, hjust=1))+
    #            labs(title=Titulo, x ="Pais", y = "Cantidad")
    
    
    if (xpsEsTipoPorcentaje){
      objetoGgplot <- ggplot(dfPaisesTop10, 
                             aes(x= reorder(Pais,-Cantidad),  y=Cantidad, group=Cantidad
                             ,text=paste("Pais: ",Pais, "<br>Cantidad: ", scales::percent(as.numeric(Cantidad))
                                                )#Fin paste
      ) ##Fin aes
      )+ ##fin gglot
        
                                                
        geom_bar(aes(y = Cantidad, fill = Pais), stat="identity") +
        geom_text(position = position_dodge(3.0),
                  aes( label = scales::percent(Cantidad),
                       y= Cantidad ), stat= "identity", vjust = -.5) +
        labs(title=Titulo, y = "Cantidad en %",x = "Pais", fill="Pais") +
        #facet_grid(~sex) +
        theme(axis.text.x=element_text(angle=45, hjust=1))+
        scale_y_continuous(labels = scales::percent)
      
    }else {
      
      objetoGgplot <- ggplot(dfPaisesTop10, aes(reorder(Pais,-Cantidad),   group=Cantidad
                                                ,text=paste("Pais: ",Pais, "<br>Cantidad: ", scales::comma(as.numeric(Cantidad))
                                                )#Fin paste
      ) ##Fin aes
      )+ ##fin gglot
        geom_bar(aes(y = Cantidad, fill = Pais), stat="identity") +
        geom_text(position = position_dodge(3.0),
                  aes( label = scales::comma(Cantidad),
                       y= Cantidad ), stat= "identity", vjust = -.5) +
        labs(title=Titulo, y = "Cantidad", x = "Pais",fill="Pais") +
        #facet_grid(~sex) +
        
        theme(axis.text.x=element_text(angle=45, hjust=1),legend.position="none")+
        scale_y_continuous(labels = scales::comma)
    }
    
    
    return(ggplotly(objetoGgplot, tooltip = c("text")) %>% plotly::config(displayModeBar = F)  %>%
             layout(legend = list(orientation = "h", x = 0.01, y = 5.2)
                    ,xaxis = list(automargin=T, ticksuffix = " ",fixedrange=TRUE)
                    ,yaxis = list(automargin=T, ticksuffix = "  ",fixedrange=TRUE)
                    
             )
    )
    
    
    
    
    
  } ) ## Fin todosPaises
  
  
  
  ### Mundial: Graficar mulibarras de Top 10 de paises
  output$paisesMundial_Multibarras <- plotly::renderPlotly({
    dfPaisesConfirmadosTop10 <-  dfPorcentajes[order(-dfPorcentajes$Confirmados),][1:10,]
    
    #dfPaisesConfirmadosTop10$"% Muertes" <- percent((dfPaisesConfirmadosTop10$Muertes/dfPaisesConfirmadosTop10$Confirmados))
    #dfPaisesConfirmadosTop10$"% Recuperados" <- percent(dfPaisesConfirmadosTop10$Recuperados/dfPaisesConfirmadosTop10$Confirmados)
    #dfPaisesConfirmadosTop10$"% EnTratamiento*" <-percent(
    #  (dfPaisesConfirmadosTop10$Confirmados-dfPaisesConfirmadosTop10$Recuperados-dfPaisesConfirmadosTop10$Muertes)/dfPaisesConfirmadosTop10$Confirmados) 
    
    ### Reemplazando valors NaN por cero. Esto se produce cuando se divide 0/0, debido a las operaciones anteriores.
    #dfPaisesConfirmadosTop10 <- replace(dfPaisesConfirmadosTop10, is.na(dfPaisesConfirmadosTop10), 0)
    
    
    
    dfPaisesConfirmadosTop10_Numeros <-select (dfPaisesConfirmadosTop10,-c("% EnTratamiento*","% Muertes","% Recuperados"))
    dfPaisesConfirmadosTop10_Porcentajes <-select (dfPaisesConfirmadosTop10,-c("Confirmados","Muertes","Recuperados"))
    Titulo <- paste("Comparación entre paises",sep = " ")
    
    #if(input$selector_xt_todos_tipo == "En numeros"){
    # dfContenido  <- dfPaisesConfirmadosTop10_Numeros
    #}else {
    #  dfContenido  <- dfPaisesConfirmadosTop10_Porcentajes 
    #}
    
    #    dfContenido %>%
    #     gather("Type", "Value",-`Pais/Region`) %>%
    #    ggplot(aes(`Pais/Region`, Value, fill = Type)) +
    #   geom_bar(position = "dodge", stat = "identity") +
    #  labs(title=Titulo, x ="Pais", y = "Cantidad")+
    # theme(axis.text.x=element_text(angle=45, hjust=1),legend.position='top', legend.box = "horizontal")
    
    
    if(input$selector_xt_todos_tipo == "En numeros"){
      
      
      dfContenidoGather <-  dfPaisesConfirmadosTop10_Numeros %>%
        gather("Type", "Value",-`Pais/Region`) 
      
      myObjetoGGPlot <- ggplot(dfContenidoGather, aes(x=Type, y=Value, group=`Pais/Region`, fill=factor(Type)
                                                          ,text=paste("Pais: ",`Pais/Region`, "<br>Cantidad: ", scales::comma(as.numeric(Value))
                                                          )#Fin paste
                                                      ) ##Fin aes
      ) + 
        geom_bar(position = "dodge", stat = "identity") +
        labs(title=Titulo, x ="Pais", y = "Cantidad",fill="Categoria")+
        theme(axis.text.x=element_blank(),  legend.position="top")+
        #theme(axis.text.x=element_text(angle=45, hjust=1, size = 7),  legend.position="top")+
        facet_grid(~`Pais/Region`)    +
        geom_text(size=2.3,aes( label = scales::comma(as.integer(Value))
                                #        ,y= Value 
        )
        , 
        stat= "sum", vjust = -.5,position = position_dodge(0.001)) +
        scale_y_continuous(labels = scales::comma)+
        theme(panel.spacing = unit(0.05, "lines"))
      
      return(ggplotly(myObjetoGGPlot, tooltip = c("y", "text")) %>% plotly::config(displayModeBar = F)  %>%
               layout(legend = list(orientation = "h", x = 0, y = -0.2)
                      ,
                      xaxis = list(title = " ", automargin=T, ticksuffix = "%",fixedrange=TRUE)
                      ,yaxis = list(title = " ", automargin=T, ticksuffix = "   ",fixedrange=TRUE)
                      
                      )
             
             )
      
      
      
      
    }else {
      
      dfContenidoGather <-  dfPaisesConfirmadosTop10_Porcentajes  %>%
        gather("Type", "Value",-`Pais/Region`) 
      
      myObjetoGGPlot <- ggplot(dfContenidoGather, aes(x=Type, y=Value, group=`Pais/Region`, fill=factor(Type)
                                                      ,text=paste("Pais: ",`Pais/Region`, "<br>Cantidad: ", scales::percent(as.numeric(Value))
                                                      )#Fin paste
      ) ##Fin aes
      )+                                               
        geom_bar(position = "dodge", stat = "identity") +
        labs(title=Titulo, x ="Pais", y = "Cantidad",fill="Categoria")+
        theme(axis.text.x=element_blank(),  legend.position="top")+
        #theme(axis.text.x=element_text(angle=45, hjust=1,size = 7),  legend.position="top")+
        facet_grid(~`Pais/Region`,labeller=label_wrap_gen(width = 15, multi_line = TRUE),scales = "free", space = "free")    +
        geom_text(size=2.3,angle=45,aes( label = scales::percent(as.numeric(Value))
                                         #                             ,y= Value 
        )
        , 
        stat= "sum", vjust = -0.5,position = position_dodge(0.001)) +
        scale_y_continuous(labels = scales::percent)    +
        theme(panel.spacing = unit(0.05, "lines"))
      
      return(ggplotly(myObjetoGGPlot, tooltip = c("text")) %>% plotly::config(displayModeBar = F)  %>%
               layout(legend = list(orientation = "h", x = 0, y = -0.2)
             ,xaxis = list(title = " ", automargin=T, ticksuffix = "%",fixedrange=TRUE)
             ,yaxis = list(title = " ", automargin=T, ticksuffix = "   ",fixedrange=TRUE)
               )
             )
      
      
      
      
      
    }
    
    
    
    
    
    
  } ) ##FIn paisesMundial_Multibarras
  
  
  ###Mundial: Graficar tabla  
  
  output$tablaPaisesMundial <- DT::renderDataTable({
    
    dfPaisesConfirmadosTop10 <-  dfPorcentajes[order(-dfPorcentajes$Confirmados),]
    
    dfPaisesConfirmadosTop10$"% Muertes" <- percent((dfPaisesConfirmadosTop10$Muertes/dfPaisesConfirmadosTop10$Confirmados))
    dfPaisesConfirmadosTop10$"% Recuperados" <- percent(dfPaisesConfirmadosTop10$Recuperados/dfPaisesConfirmadosTop10$Confirmados)
    dfPaisesConfirmadosTop10$"% EnTratamiento*" <-percent(
      (dfPaisesConfirmadosTop10$Confirmados-dfPaisesConfirmadosTop10$Recuperados-dfPaisesConfirmadosTop10$Muertes)/dfPaisesConfirmadosTop10$Confirmados) 
    
    ### Reemplazando valors NaN por cero. Esto se produce cuando se divide 0/0, debido a las operaciones anteriores.
    dfPaisesConfirmadosTop10 <- replace(dfPaisesConfirmadosTop10, is.na(dfPaisesConfirmadosTop10), 0)
    
    
    formatoComma=c("Confirmados","Muertes", "Recuperados")
    formatoPorcentaje=c("% Muertes", "% Recuperados","% EnTratamiento*")
    
    datatable(dfPaisesConfirmadosTop10,filter="none") %>% 
      formatPercentage(formatoPorcentaje, 2) %>% 
      formatCurrency(formatoComma,currency = "", interval = 3, mark = ",") %>% 
      formatRound(formatoComma,0)
    
    
  }) ## FIN tablaPaisesMundial  
  
  
  
  
  
  
  
  
  output$plotSerieGuatemala <- plotly::renderPlotly({
    miXTS <- obtenerXTSSeriePaisIndivual(dfTSConfirmed, "Guatemala", TRUE,0)
    obtenerGraficaGgplot2(miXTS,"Guatemala",7)
  }) ### Fin Plot Serie de tiempo Guatemala
  
  
  
  output$plotSerieUnicoPais <- plotly::renderPlotly({
    
    miXTS <- obtenerXTSSeriePaisIndivual(dfTSConfirmed, input$selector_cualquier_pais, TRUE)
    obtenerGraficaGgplot2(miXTS,input$selector_cualquier_pais,15)
    
    #graficaSeriePaisIndivual(dfTSConfirmed, "El Salvador")
    
  }) ### Fin Plot plotSerieUnicoPais
  
  
  
  output$plotSerieCentroamerica <- plotly::renderPlotly({
    #### Notar que para centroamerica el Dataset ya comienza desde la fila 1
    miXTS <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "Centroamerica", FALSE,1)
    #obtenerGraficaGgplot2(miXTS,dfTS_Centroamerica[2:ncol(dfTS_Centroamerica)],9)
    obtenerGraficaGgplot2(miXTS,names(dfTS_Centroamerica),9)
    
  }) ### Fin Plot Serie de tiempo mundial
  
  
  
  output$plotSerieMundialTop10 <- plotly::renderPlotly({
    miXTS <- obtenerXTSSeriePaisIndivual(dfTSConfirmed[,1:10], "Internacional", FALSE,3)
    obtenerGraficaGgplot2(miXTS,"Internacional",15)

  }) ### Fin Plot Serie de tiempo mundial
  
  
  
  #####Box Values #############
  
  output$infoBoxConfirmados <- renderInfoBox({
    infoBox(
      "CONFIRMADOS",icon = icon("ambulance"),
      color = "light-blue", fill = TRUE,
      value = tags$p( dfGuatemala$Confirmados, 
                      style = "font-size: 200%;") 
    )
  })
  
  output$infoBoxRecuperados <- renderInfoBox({
    infoBox(
      "RECUPERADOS",  icon = icon("blind"),
      color = "green", fill = TRUE,
      value = tags$p(dfGuatemala$Recuperados , 
                     style = "font-size: 200%;") 
    )
  })
  output$infoBoxFallecidos <- renderInfoBox({
    infoBox(
      "FALLECIDOS",  icon = icon("bible"), #, lib = "glyphicon"
      color = "purple", fill = TRUE,
      value = tags$p(dfGuatemala$Muertes, 
                     style = "font-size: 200%;") 
    )
  })
  
  output$infoBoxTratamiento <- renderInfoBox({
    infoBox(
      "ACTIVOS",  icon = icon("capsules"),
      color = "navy", fill = TRUE,
      value = tags$p(dfGuatemala$EnTratamiento,
                     style = "font-size: 200%;") 
    )
  })
  

  
  output$infoBoxOtrosCasos <- renderInfoBox({
    infoBox(
      "OTROS*",  icon = icon("car-crash"),
      color = "olive", fill = TRUE,
      value = tags$p(dfGuatemala$OtraCausa,
                     style = "font-size: 219%;") 
      #,"Tenía Covid pero falleció por otra causa"
    )
  })  
  
  #gaugeOutput(outputId, width = "100%", height = "200px")
  
  #renderGauge(expr, env = parent.frame(), quoted = FALSE)
  
  output$gaugeEnTratamiento <- renderGauge({
    gauge(round(dfGuatemala$`% EnTratamiento*`*100,2), min = 0, max = 100, symbol = '%', label = paste("En tratamiento"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
  })
  
  
  output$gaugeRecuperados <- renderGauge({
    gauge(round(dfGuatemala$`% Recuperados`*100,2), min = 0, max = 100, symbol = '%', label = paste("Recuperados"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
  })
  
  output$gaugeMuertes <- renderGauge({
    gauge(round(dfGuatemala$`% Muertes`*100,2), min = 0, max = 100, symbol = '%', label = paste("Fallecidos"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
  })
  
  output$gaugeOtrasMuertes <- renderGauge({
    gauge(round((dfGuatemala$OtraCausa/dfGuatemala$Confirmados)*100,2), min = 0, max = 100, symbol = '%', label = paste("Otra causa "),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
  })
  
  
  output$confirmados <- renderText({dfGuatemala$confirmados})
  
  
  output$plot_serieVertical <- plotly::renderPlotly({
    
    myObjectGGplot <- ggplot(mostrarPaisesSerieVertical(),
                             aes(forcats::fct_reorder(`Pais/Region`,Confirmados),Confirmados
                                 ,text=paste("Pais: ",`Pais/Region`,"<br>Confirmados: ", 
                                             scales::comma(as.numeric(Confirmados) )))
    ) + geom_col(colour = "blue",fill = "#4DADFB") +
      labs(title="Barras horizontales", y = "Cantidad", x = "Pais")+
      scale_y_continuous(labels = scales::comma)+
      geom_text(size=3.19,aes(label = scales::comma(as.numeric(Confirmados)),
                y=19000)
                )+
      
      coord_flip()
      
    
    myObjectGGplot <- ggplotly(tooltip = c("text")) %>% 
      plotly::config(displayModeBar = F)  %>%
      layout(legend = list(orientation = "h", x = 0.4, y = 1.2)
             ,xaxis = list(automargin=T, ticksuffix = " ",fixedrange=TRUE)
             ,yaxis = list(automargin=T, ticksuffix = "  ",fixedrange=TRUE)
             )
    
    
    
    
    
  })
  
  
  output$pronosticoPais <- renderPlot({
    miXTS <- obtenerXTSSeriePaisIndivual(dfTSConfirmed, "Guatemala",TRUE,1)
    
    armModel <- auto.arima(miXTS)
    fcrfa <- forecast(armModel,h=5)
    
    
    tu <-  ggplot2::autoplot(fcrfa)    +
      ggplot2::ggtitle("Pronóstico de contagiados") +
      ggplot2::labs(y = "Cantidad de confirmados", x = "Dias transcurridos desde el primer contagio")#+
      #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1)) +
      #scale_y_continuous(breaks = round(seq(min(coredata(miXTS)), max(coredata(miXTS)), by = 10),1))
    
    tu
    
    
    
    
  }) #Fin pronosticoPais
  

  
  
  map = leaflet(data = guatemalaLatLong) %>% addTiles() %>% setView( -89.780620, 15.393619, zoom = 7.49) %>% 
    addTiles() %>% 
    
    addCircleMarkers(
      lng = ~long, lat = ~lat, 
      radius = ~sqrt(valores), popup = paste("",guatemalaLatLong$Departamento, "<br>",guatemalaLatLong$valores," confirmados"),
      fillColor = 'red', color = 'red', weight = 1
    )
  
  output$myMap = renderLeaflet(map)
  
  
  
  
  output$hope <- plotly::renderPlotly({
    
    miXTSGT <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "Guatemala", TRUE,1)
    miXTSElsa <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "El Salvador", TRUE,1)
    miXTSHondu <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "Honduras", TRUE,1)
    miXTSCosta <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "Costa Rica", TRUE,1)
    miXTSNica <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "Nicaragua", TRUE,1)
    miXTSPanam <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "Panama", TRUE,1)
    miXTSMexido <- obtenerXTSSeriePaisIndivual(dfTS_Centroamerica, "Mexico", TRUE,1)
    
    dfmiXTSGT  <- data.frame(date=index(miXTSGT), coredata(miXTSGT))
    dfmiXTSElsa  <- data.frame(date=index(miXTSElsa), coredata(miXTSElsa))
    dfmiXTSHondu  <- data.frame(date=index(miXTSHondu), coredata(miXTSHondu))
    dfmiXTSCosta  <- data.frame(date=index(miXTSCosta), coredata(miXTSCosta))
    dfmiXTSNica  <- data.frame(date=index(miXTSNica), coredata(miXTSNica))
    dfmiXTSPanam  <- data.frame(date=index(miXTSPanam), coredata(miXTSPanam))
    dfmiXTSMexido  <- data.frame(date=index(miXTSMexido), coredata(miXTSMexido))
    
    
    # "Guatemala\nDia" <- 1:nrow(dfmiXTSGT)
    # "El Salvador\nDia" <- 1:nrow(dfmiXTSElsa)
    # "Honduras\nDia" <- 1:nrow(dfmiXTSHondu)
    # "Costa Rica\nDia" <- 1:nrow(dfmiXTSCosta)
    # "Nicaragua\nDia" <- 1:nrow(dfmiXTSNica)
    # "Panama\nDia" <- 1:nrow(dfmiXTSPanam)
    # "Mexico\nDia" <- 1:nrow(dfmiXTSMexido)
    
    "gtDia" <- 1:nrow(dfmiXTSGT)
    "elsaDia" <- 1:nrow(dfmiXTSElsa)
    "hondDia" <- 1:nrow(dfmiXTSHondu)
    "costaDia" <- 1:nrow(dfmiXTSCosta)
    "nicaDia" <- 1:nrow(dfmiXTSNica)
    "panaDia" <- 1:nrow(dfmiXTSPanam)
    "mexiDia" <- 1:nrow(dfmiXTSMexido)
    
    
    
    r1 <- geom_line(data = dfmiXTSGT,aes(x=gtDia,y=Confirmados,Fechas=date,color="Guatemala",group = 1
                                   ,text=paste("Guatemala", "<br>Fecha: ", date, "<br>Cantidad: ", scales::comma(as.numeric(Confirmados)),"<br>","Dia: ",gtDia)#Fin paste
                                   ),size=1) 
    
    r2 <- geom_line(data = dfmiXTSElsa,aes(x=elsaDia,y=Confirmados,Fechas=date,color="El Salvador",group = 2
                                         ,text=paste("El Salvador", "<br>Fecha: ", date, "<br>Cantidad: ", scales::comma(as.numeric(Confirmados)),"<br>","Dia: ",elsaDia)#Fin paste
    ),size=1) 
    
    r3 <- geom_line(data = dfmiXTSHondu,aes(x=hondDia,y=Confirmados,Fechas=date,color="Honduras",group = 2
                                           ,text=paste("Honduras", "<br>Fecha: ", date, "<br>Cantidad: ", scales::comma(as.numeric(Confirmados)),"<br>","Dia: ",hondDia)#Fin paste
    ),size=1) 
    
    r4 <- geom_line(data = dfmiXTSCosta,aes(x=costaDia,y=Confirmados,Fechas=date,color="Costa Rica",group = 2
                                            ,text=paste("Costa Rica", "<br>Fecha: ", date, "<br>Cantidad: ", scales::comma(as.numeric(Confirmados)),"<br>","Dia: ",costaDia)#Fin paste
    ),size=1) 
    
    r5 <- geom_line(data = dfmiXTSNica,aes(x=nicaDia,y=Confirmados,Fechas=date,color="Nicaragua",group = 5
                                            ,text=paste("Nicaragua", "<br>Fecha: ", date, "<br>Cantidad: ", scales::comma(as.numeric(Confirmados)),"<br>","Dia: ",nicaDia)#Fin paste
    ),size=1) 
    
    r6 <- geom_line(data = dfmiXTSPanam,aes(x=panaDia,y=Confirmados,Fechas=date,color="Panama",group = 2
                                            ,text=paste("Panama", "<br>Fecha: ", date, "<br>Cantidad: ", scales::comma(as.numeric(Confirmados)),"<br>","Dia: ",panaDia)#Fin paste
    ),size=1) 
    
    r7 <- geom_line(data = dfmiXTSMexido ,aes(x=mexiDia,y=Confirmados,Fechas=date,color="Mexico",group = 2
                                            ,text=paste("Mexico", "<br>Fecha: ", date, "<br>Cantidad: ", scales::comma(as.numeric(Confirmados)),"<br>","Dia: ",mexiDia)#Fin paste
    ),size=1) 
    
    
    
    #r10 <- scale_x_date(date_breaks = "7 day", date_labels = "%b %d")
    #r11 <- theme(axis.text.x=element_text(angle=45, hjust=1,size = 12),  legend.position="top")
    
    
    r0 <- ggplot()
    
    if (input$selectorHope=="Si"){
      tux <-  mget(paste0('r', 0:7)) %>%
        purrr::reduce(`+`)
      
    }else {
      tux <-  mget(paste0('r', 0:6)) %>%
        purrr::reduce(`+`)
    }
     
    
    tux <- tux + scale_y_continuous(labels = scales::comma)
     
     ggplotly(tux, tooltip = c("text")#,dynamicTicks = TRUE
     ) %>%
       #rangeslider() %>% layout(hovermode = "x")%>%
       plotly::config(displayModeBar = F)  %>%
       layout(autosize = T,legend = list(orientation = "v", x = 0, y = 1)  ,
              #title=nombrePais,
              title=list(text = "Comparación en Centroamérica", y = 1,x=0.7),
              xaxis = list(title = "Dias transcurridos desde el primer contagio", automargin=T, ticksuffix = " ",fixedrange=TRUE)
              ,yaxis = list(title = "Cantidad Confirmados", automargin=T, ticksuffix = "   ",fixedrange=TRUE)

       )###### 
     
     
    
    
  }) ### Fin Plot Serie Desde First Dia :v
  
  
  
  
  
  
  
  
  #observe({
  #  print(input$myMap_bounds)
  #  print(input$myMap_shape_click)
  #})
  
    
  
  
})## FIN del FIN ;)
