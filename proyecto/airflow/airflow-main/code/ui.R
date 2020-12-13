#install.packages("devtools")
#devtools::install_github("daattali/shinyjs")
#devtools::install_github("ebailey78/shinyBS")
#install.packages("shinyBS")
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(shinyBS)


library(shinyWidgets)
require(plotly)
library(shiny)
library(DT)
#install.packages("shinydashboard")
#install.packages("flexdashboard")
library(shinydashboard)
library(flexdashboard)


############################################################
############################################################
##################  X Shiny    #############################
############################################################
############################################################



header <- dashboardHeader(
  title = " 502 Analitycs", 
  tags$li(class = "dropdown", #actionButton("home", "Home")
          actionBttn(
            inputId = "btnGuatemala",
            label = "GT",
            style = "gradient",
            color = "primary" #
            #style='padding:4px; font-size:80%'
          )),
  
  tags$li(class = "dropdown", #actionButton("home", "Home")
              actionBttn(
              inputId = "btnCentroamerica",
              label = "CA",
              style = "gradient",
              color = "royal" # “primary”, “warning”, “danger”, “success”, “royal”
                #style='padding:4px; font-size:80%'
            )),
          
          
  tags$li(class = "dropdown", #actionButton("home", "Home")
              actionBttn(
              inputId = "btnOtrosPaises",
              label = "Internacional",
              style = "gradient",
              color = "danger" #
              #style='padding:4px; font-size:80%'
            )),
          
  
  
  #### Activar despues ###
  
  tags$li(class = "dropdown", #actionButton("home", "Home")
         actionBttn(
           inputId = "btnMapa",
           label = "Mapa",
           style = "unite",
           # “material-flat”, “pill”, “float”, “unite”
           color = "primary" #
           #style='padding:4px; font-size:80%'
         ))
  ,
   
  #“simple”, “bordered”, “minimal”, “stretch”, “jelly”, “gradient”, “fill”, “material-circle”, “material-flat”, “pill”, “float”, “unite”       
  tags$li(class = "dropdown", #actionButton("home", "Home")
                actionBttn(
                inputId = "btnCentrosdeAyuda",
                label = "Info",
                style = "gradient",
                # “material-flat”, “pill”, “float”, “unite”
                color = "primary" #
                #style='padding:4px; font-size:80%'
              ))     
          
  
  ,
  dropdownMenu(type = "messages",headerText= "Información y contacto",
               messageItem(
                 from = "Autor",
                 message = "Ing. José Pineda Amézquita",
                 href = "https://gt.linkedin.com/in/erick-j-pineda-am%C3%A9zquita-1808801a4"
               ),
               messageItem(
                 from = "Dudas",
                 message = "502analitycs@gmail.com",
                 icon = icon("question"),
                 time = "Última vez 13:45",
                 href = "https://forms.gle/cXLLNNVDxn224V328"
               ),
               messageItem(
                 from = "Manual",
                 message = "502 Analitycs",
                 icon = icon("life-ring"),
                 time = "2020-03-19",
                 href = "https://drive.google.com/file/d/1QCBcBPpeJjIFHKqcnOvZ_PDQdUAACA0m/view?usp=sharing"
               ),
               messageItem(
                 from = "Donar 1 Cafecito",
                 message = "502 Analitycs",
                 icon = icon("coffee"),
                 href = "https://forms.gle/cXLLNNVDxn224V328"
                 
               )
  ) ## Fin Drop1
#  ,
#  dropdownMenu(type = "notifications",headerText= "3 Notificaciones",
#               notificationItem(
#                 text = "3 Actualizaciones pendientes (última revisión)",
#                 icon("users")
#               ),
#               notificationItem(
#                 text = "+9 sugerencias recibidas (última revisión)",
#                 icon("truck"),
#                 status = "success"
#               ),
#               notificationItem(
#                 text = "% Créditos en la nube",
#                 icon = icon("exclamation-triangle"),
#                 status = "warning"
#               )
#  )
#  ,
  
#  dropdownMenu(type = "tasks", badgeStatus = "success",headerText= "Resumen Global",
#               taskItem(value = 93, color = "green",
#                        "Recuperados China"
#               ),
#               taskItem(value = 34, color = "aqua",
#                        "Recuperados Irán"
#               ),
#               taskItem(value = 30, color = "yellow",
#                        "Recuperados Switzerland"
#               ),
#               taskItem(value = 5, color = "red",
#                        "Recuperados EU",
#                        actionBttn(
#                          inputId = "btnInformacion")
#               )
#  )
  
)




sidebar <- dashboardSidebar(width = 43,
  
  actionBttn(
    inputId = "btnInformacion",
    label = "INF",
    style = "material-circle",
    color = "primary" #
    #style='padding:4px; font-size:80%'
  )
  ,
  #tags$style(type='text/css', "#btnInformacion { horizontal-align: top; }"),
  #tags$style(type='text/css', "#btnInformacion { padding-bottom: 435px; }"),
  tags$style(type='text/css', "#btnInformacion { margin-bottom: 1px; }"),
  
  #tagList(
  #  tags$h4("Secciones de consulta", align = "center")
  #  )
  #,
  
  tags$head(
    tags$style(HTML("
                    
                    
                    @import url('https://fonts.googleapis.com/css2?family=Cinzel:wght@900&family=Montserrat&display=swap');
                    @import url('https://fonts.googleapis.com/css2?family=Special+Elite&display=swap');
                    
                    h1 {
                    font-family: 'Cinzel', serif;
                    font-weight: 250;
                    line-height: 1;
                    color: #2a59b0;
                    }
                    
                    h2 {
                    font-family:'Montserrat', sans-serif' ;
                    font-weight: 300;
                    line-height: 1;
                    color: #4ba7d1;
                    }
                    
                    h3 {
                    font-family:'Special Elite', cursive;
                    font-weight: 250;
                    line-height: 1;
                    color: #e37827;
                    }
                    
                    h4 {
                    font-family: 'Cinzel', serif;
                    font-weight: 100;
                    line-height: 1;
                    color: #2a59b0;
                    }
                    
                    
                    h5 {
                    font-family:'Special Elite', cursive;
                    font-weight: 100;
                    line-height: 1;
                    color: #3a17ff;
                    }
                    
                    h6 {
                    font-family:'Special Elite', cursive;
                    font-weight: 100;
                    line-height: 1;
                    color: #e37827;
                    }
                    
                    
                    
                    
                    "))
    )
 
  
  
  ,
  
  sidebarMenu(id="miBarraDeLado",
    br(),br(),br(),br(),br(),
    menuItem("", tabName = "Guatemala", icon = icon("home"),badgeLabel = "GT"),
    br(),br(),br(),
    menuItem("", tabName = "Centroamerica", icon = icon("chart-area"),
             badgeLabel = "CA", badgeColor = "green"),
    br(),br(),br(),
    menuItem("", tabName = "OtrosPaises", icon = icon("globe-americas"),
             badgeLabel = "WD", badgeColor = "green"),
    br(),br(),br(),
    menuItem("", tabName = "CentrosdeAyuda", icon = icon("info-circle"),
             badgeLabel = "In", badgeColor = "green")   ,
    br(),br(),br()
    
    #### Activar despues ###
    ,
    menuItem("", tabName = "MapaGuatemala", icon = icon("lemon"),
            badgeLabel = "M", badgeColor = "green")   #
    
    
    #menuItem("Source code", icon = icon("file-code-o"), 
    #        href = "https://github.com/rstudio/shinydashboard/")
  )
  
  #,
  #tagList(
  #  tags$hr(style="border-color: purple;"),
  #  tags$h4("Covid-19 Analityc App, V 3.19", align = "center")
  #  
  #)
  
  
  
    )
















body <- dashboardBody(#ñ
  
  tagList(
    tags$h5("Actualización (
            dom., 19 de abr. de 2020 05:41:05 p. m.
            )", align = "right")
  ),
  
 
  #useShinyjs(),
  #extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse"); 
  #            $(window).trigger("resize"); }'),
  #extendShinyjs(text='shinyjs.showSidebar = function(params) { $("body").removeClass("sidebar-collapse"); 
  #                $(window).trigger("resize"); }'),
  #bsButton("showpanel", "Show/Hide sidebar",icon = icon("toggle-off"), type = "toggle",style = "info", value = TRUE),
  
  
  
  
  
  
  #fluidRow(
  # Clicking this will increment the progress amount
  # box(width = 4, actionButton("count", "Increment progress"))
  #  )
  #,
  
  
  ########### Seccion de Table Panes  
  tabItems(
    ########################
    ### PRIMERA PESTAniA
    ##############################
    
    
    tabItem(tabName = "Guatemala",
            
            h1("Situación actual de Guatemala", align="center"),
             ##infoBoxes with fill=TRUE
            fluidRow(
              
              infoBoxOutput("infoBoxConfirmados",width = 2),
              infoBoxOutput("infoBoxRecuperados",width = 2),
              infoBoxOutput("infoBoxFallecidos",width = 2),
              infoBoxOutput("infoBoxTratamiento",width = 2),
              infoBoxOutput("infoBoxOtrosCasos",width = 2)
            ),
            
            
            #tags$hr(style="border-color: purple;"),
            
            fluidRow(
              
              column(3, 
                     #uiOutput("infoBoxTratamiento"),
                     gaugeOutput("gaugeEnTratamiento",width = "100%", height = "auto"),
                     tags$h5("Tasa de personas en tratamiento", align = "center")
              ),
              column(3, 
                     #uiOutput("infoBoxRecuperados"),
                     gaugeOutput("gaugeRecuperados",width = "100%", height = "auto"),
                     tags$h5("Tasa de recuperación", align = "center")
                     
                     #gaugeOutput("gauge2",width = "100%", height = "auto")
              ),
              column(3, 
                     #uiOutput("infoBoxFallecidos"),
                     gaugeOutput("gaugeMuertes",width = "100%", height = "auto"),
                     tags$h5("Tasa de letalidad por COVID-19", align = "center")
              ),
              
              column(3, 
                     #uiOutput("infoBoxRecuperados"),
                     gaugeOutput("gaugeOtrasMuertes",width = "100%", height = "auto"),
                     tags$h5("Tasa de muertes por otras causas (Tenia Covid pero murió por otra causa)", align = "center")
          
              )
          
            )
            ,
            "Otros*: Personas confirmadas con COVID-19 y que fallecieron por otra causa",
            
            h3("Comportamiento diario desde primer contagiado", align = "center"),
            br(), 
            
            
            plotly::plotlyOutput('plotSerieGuatemala',width = '91%',height = '91%')
            ,
            
           
            
            h3("Comportamiento general de Guatemala", align = "center"),
            "En esta sección puedes analizar las cantidades de casos confirmados, muertes o en tratamiento.
            Asimismo, los valores en porcentajes para conocer las razones de cambio de cada clasificación. ",
            pickerInput(
              inputId = "selector_gt_tipo",
              label = "Selecciona el tipo de resultado que deseas ver como números o porcentajes:", 
              selected = "En numeros", 
              choices = c("En numeros", "En porcentajes"),
              options = list(
                style = "btn-primary")
            ),
            
            #### Grafica 2, barplot multiple
            plotly::plotlyOutput('paisGuatemala_Multibarras',width = '91%',height = '91%')
            
            ### Activar despues
            ### br(), 
            ### plotly::plotlyOutput('plotSerieUnicoPais')
            ###,
            
            #h3("Pronóstico de los siguientes 5 días", align = "center"),
            ### Activar despues
            #plotOutput("pronosticoPais")
            #plotly::plotlyOutput('pronosticoPais')
            #
            
            
    ),
    
    
    
    
    
    tabItem(tabName = "Centroamerica",
            h1("Sección Centroamérica y México",align="center")
            
            ,
            h3("Información por tipo de clasificación", align = "center"),
            br(),
            
            pickerInput(
              inputId = "parametro",
              label = "Seleccionar el tipo de información a observar:", 
              selected = "Confirmados", 
              choices = c("Confirmados","Recuperados","Muertes","% Recuperados","% Muertes"),
              options = list(
                style = "btn-primary")
            ),
            
            #### Grafica 1, barplot singular
            plotly::plotlyOutput('paisesCA_porTipo_singular',width = '91%',height = '91%'),
            #plotOutput("paisesCA_porTipo_singular"),
            #DT::DTOutput("general"),
            #,plotly::plotlyOutput('conPlotly')
            
            
            #tableOutput("general")
            br(),br(),
            h3("Información unificada de países de Centroamérica (Confirmados, Muertes, Recuperados)", align = "center"),
            
            pickerInput(
              inputId = "selector_ca_tipo",
              label = "Selecciona el tipo de resultado que deseas ver como números o porcentajes:", 
              selected = "En numeros", 
              choices = c("En numeros", "En porcentajes"),
              options = list(
                style = "btn-primary")
            ),
            
            #### Grafica 2, barplot multiple
            plotly::plotlyOutput('paisesCA_Multibarras',width = '90%',height = '90%'),
            
            br(),br(), 
            h3("Progreso de la enfermedad en función del tiempo (solo confirmados)", align = "center"),
            "Países centroamericanos y México",
            h5("Observación: La fecha de inicio se establece con la fecha del país que tuvo el primer caso confirmado, 
               a partir de ahi se tiene el conteo diario. Esto es util para conocer el orden de aparición de primeros casos, 
               no para comparar la evolución en cantidad de días."),
            plotly::plotlyOutput('plotSerieCentroamerica',width = '91%',height = '91%')
            ,
            
            
            
            h3("Comparacion desde el primer día de cada país", align = "center"),
            h5("Comportamiento de cada país en función del tiempo desde el primer día de contagio de cada país"),
            br(),br(),
            pickerInput(
              inputId = "selectorHope",
              label = "Incluir a México?", 
              selected = "Si", 
              choices = c("Si", "No"),
              options = list(
                style = "btn-primary")
            ),
            br(),
            plotly::plotlyOutput('hope',width = '91%',height = '91%')
            
            
            
            ,
            
            
            
            
            br(),br(), h3("Información detallada", align = "center"),br(),br(),
            #### Grafica 3, mostrar tabla
            DT::DTOutput("tablaPaisesCA")
            
            
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    tabItem(tabName = "OtrosPaises",
            h1("Sección Internacional",align="center"),
            br(),"*Estás en las sección del top 10 de todos los países con casos confirmados de Covid19", br(),
            h3("Información por tipo de clasificación", align = "center"),
            
            br(),
            pickerInput(
              inputId = "selector_xt_individual_tipo",
              label = "Top 10 de paises por", 
              selected = "En numeros", 
              choices = c("Cantidad de confirmados", "Cantidad de muertes","Cantidad de recuperados",
                          "% Recuperados","% Muertes"),
              options = list(
                style = "btn-primary")
            ),
            
            
            #### Grafica 1 MUNDIAL, barplot singular
            plotly::plotlyOutput('paisesMundial_porTipo_singular',width = '91%',height = '91%'),
            br(),br(),
            h3("Información unificada (Confirmados, Muertes, Recuperados)", align = "center"),
            
            pickerInput(
              inputId = "selector_xt_todos_tipo",
              label = "Selecciona el tipo de resultado que deseas ver como números o porcentajes:", 
              selected = "En numeros", 
              choices = c("En numeros", "En porcentajes"),
              options = list(
                style = "btn-primary")
            ),
            
            #### Grafica 2 MUNDIAL, barplot MULTIPLE
            plotly::plotlyOutput('paisesMundial_Multibarras',width = '91%',height = '91%'),
            br(),br(),
            
            
            
            h3("Progreso de la enfermedad en función del tiempo", align = "center"),
            "Top 10 de países mas afectados en orden de cantidad de confirmados",
            h5("Observación: La fecha de inicio se establece con la fecha del país que tuvo el primer caso confirmado, 
               a partir de ahi se tiene el conteo diario. Esto es util para conocer el orden de aparición de primeros casos, 
               no para comparar la evolución en cantidad de días."),
            
            
            #plotly::plotlyOutput('plotSerieMundialTop10'),
            plotly::plotlyOutput('plotSerieMundialTop10',width = '91%',height = '91%'),
            
            
            
            
            br(),br(),
            h3("Serie de tiempo para cualquier país", align = "center",width = '95%',height = '95%'),
            h5("A continuación puede grafica el proceso de evolución de cada país desde el primer día de contagio."),
            pickerInput(
              inputId = "selector_cualquier_pais",
              label = "Selecciona el pais que deseas ver:", 
              selected = "En numeros", 
              choices = sort(c("US","Spain","Italy","France","Germany","China","United Kingdom","Iran",
                          "Turkey","Belgium","Switzerland","Netherlands","Canada","Brazil","Portugal",
                          "Austria","Russia","Korea, South","Israel","Sweden","Ireland","India","Ecuador",
                          "Chile","Norway","Australia","Denmark","Poland","Peru","Czechia","Japan","Romania",
                          "Pakistan","Malaysia","Philippines","Saudi Arabia","Indonesia","Mexico","United Arab Emirates",
                          "Luxembourg","Serbia","Finland","Panama","Dominican Republic","Qatar","Colombia","Thailand",
                          "Ukraine","Singapore","Greece","South Africa","Belarus","Argentina","Egypt","Algeria",
                          "Iceland","Croatia","Morocco","Moldova","New Zealand","Iraq","Estonia","Hungary","Slovenia",
                          "Lithuania","Kuwait","Azerbaijan","Armenia","Bahrain","Bosnia and Herzegovina","Cameroon",
                          "Kazakhstan","Slovakia","Diamond Princess","North Macedonia","Tunisia","Bulgaria","Uzbekistan",
                          "Latvia","Lebanon","Andorra","Cyprus","Cuba","Costa Rica","Afghanistan","Oman","Uruguay",
                          "Cote d'Ivoire","Burkina Faso","Niger","Bangladesh","Albania","Honduras","Taiwan*","Ghana",
                          "Jordan","Malta","San Marino","Mauritius","Nigeria","Kyrgyzstan","Bolivia","West Bank and Gaza",
                          "Senegal","Vietnam","Montenegro","Kosovo","Georgia","Congo (Kinshasa)","Guinea","Sri Lanka",
                          "Kenya","Venezuela","Djibouti","Brunei","Paraguay","Guatemala","Cambodia","Rwanda","El Salvador",
                          "Trinidad and Tobago","Madagascar","Monaco","Mali","Liechtenstein","Togo","Barbados","Ethiopia",
                          "Jamaica","Congo (Brazzaville)","Uganda","Gabon","Bahamas","Zambia","Guyana","Liberia",
                          "Guinea-Bissau","Benin","Eritrea","Tanzania","Haiti","Burma","Libya","Somalia","Mozambique",
                          "Angola","Antigua and Barbuda","Maldives","Syria","Equatorial Guinea","Sudan","Dominica","Fiji",
                          "Laos","Mongolia","Namibia","Saint Lucia","Grenada","Botswana","Zimbabwe","Eswatini",
                          "Saint Kitts and Nevis","Saint Vincent and the Grenadines","Chad","Seychelles","Belize",
                          "Suriname","Malawi","MS Zaandam","Nepal","Central African Republic","Holy See","Sierra Leone",
                          "Cabo Verde","Mauritania","Nicaragua","Bhutan","Gambia","Sao Tome and Principe","South Sudan",
                          "Western Sahara","Burundi","Papua New Guinea","Timor-Leste","Yemen")),
              options = list(
                style = "btn-primary")
            ),
            plotly::plotlyOutput('plotSerieUnicoPais',width = '95%',height = '95%'),
            
            
            
            
            
            
                  br(),br(),
                  h3("Gráfica de todos los paises por categoría Confirmados", align = "center"),
                  "A continuacion puede seleccionar la cantidad de países a observar.",
                  "Observación:",
                  "Los países están ordenados descendentemente por la cantidad de casos confirmados",
                  br(),br(),
            
            #div(style="height: 200px;",
                sliderInput('sliderSerieVertical', 'Seleccione cuantos paises desea mostrar', 5, 50, 10,width='100%',pre="Cantidad Paises: ")
            #)
            
                  ,
            
            
                  plotly::plotlyOutput('plot_serieVertical',width = '95%',height = '95%')
            ,
            
            
            h3("Información detallada", align = "center"),
            
            #### Grafica 3 MUNDIAL, tabla
            br(),br(),
            DT::dataTableOutput('tablaPaisesMundial')
            
            
    ),
    
    
    
    tabItem(tabName = "CentrosdeAyuda",
            
            tagList(
              "",
              tags$h3("Anda, pueblo mío, entra en tus aposentos, cierra tras ti tus puertas;", align = "center"), 
              tags$h3("escóndete un poquito, por un momento, en tanto que pasa la indignación.", align = "center"),
              tags$h3("- Isaías 26:20", align = "center")
              ),
            
            br(),
            h1("Centros de Ayuda",align="center"),
            
            br(),
            h4("1. Hospital Nacional de Villa Nueva", align = "center"),
            h4("2. Instalaciones del Parque de la industria", align = "center"),
            br(),br(),
            
            
            
            h3("No olvides escribir tus comentarios a 502analitycs@gmail.com", align = "center")
            
            
    )## Fin Tab CentrosDeAyuda
    
    ,
    #### Activar despues ###
    tabItem(tabName = "MapaGuatemala",
            fluidRow(
                        selectInput("Typos", "Typos:",
                                    c("Confirmados"="C",
                                      "Recuperados"="R",
                                      "Muertes"="D"
                                    )),
                        #this will create a space for us to display our map
                        leafletOutput(outputId = "mymap"),
                        plotOutput("TimeFrame")  
            )
     ) ## fin TamMapaGuatemala

    
    
    
    
    
)






)




dashboardPage(header, sidebar, body)