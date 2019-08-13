library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(chron)
library(scales)
library(DT)
library(lubridate)
library(dplyr)
library(magrittr)

load("viajesentorno.RData")

navbarPage(id = "navegacion",
           title = "Viajes en el Valle de Aburrá",
           header = tagList(
               useShinyjs(),
               useShinydashboard(),
               setBackgroundColor(color = c("ghostwhite")),
               tags$link(rel="stylesheet", type="text/css", href="styles.css"),
               tags$style(
                          #Para pie de página
                          "body { font-size: 14px; line-height: 1.1; padding-bottom:30px}",
                          ".checkbox label, .radio label, .checkbox-bs label, .radio-bs label
                          { line-height: 1.6 }",
                          ".radio-inline {line-height: 2}",
                          #landing page recuadros
                          ".landing-page-box {width:100%; height:100%; min-height:22vh; background-color:white;
                          border: 1px solid #AAAAAA; margin-bottom: 1px; float: left; transition: 0.5s ease; position: relative; object-fit: scale-down;}",
                          #landing page icons or pics
                          ".landing-page-icon {width:100%; height:85%; min-height:12vh; background-color: white;
                          border: 0px ; position: absolute; object-fit: scale-down;}",
                          ".landing-page-inicio-icon {width:100%; height:65%; min-height:5vh; background-color: white;
                          border: 0px; position: absolute; object-fit: scale-down;}",
                          #landing-page títulos para recuadros
                          ".landing-page-box-title {font-size: 16px; text-align:center; color: green;
                          font-weight: bold; background-color: none; width:100%; max-height: 20px; margin-top: 10px; }",
                          #landing page buttons
                          ".landing-page-button {text-align:center;
                          background-image:none; color: black; white-space: normal; border-radius: 0;border: 0px;
                          font-size: 16px; min-height: 16vh; position: absolute; margin-bottom: 0px; margin-top: 5px; float: middle;width: 100%; opacity: 0;}",
                          ".landing-page-button-inicio {text-align:center;
                          background-image:none; color: black; white-space: normal; border-radius: 0; border:0px ;
                          font-size: 14px; position: absolute; min-height: 7vh; margin-bottom: 0px; margin-top: 1px; float: middle; width: 100%; opacity:0;}",
                          #Efecto de los landing buttons 
                          ".landing-page-button:hover , .landing-page-button:active , .landing-page-button-inicio:hover, .landing-page-button-inicio:active {opacity: 1; 
                          background-color: #fff; /* fallback */
                          background-color: rgba(255, 255, 255, 0.8);
                          color: green;
                          border-color: #fff; /* fallback */
                          border-color: rgba(255, 255, 255, 0.8); transition: background-color 0.3s ease-in,
                          color 0.3s ease-in;}"
               ),
               
               HTML("<base target='_blank'>")
           ), 
           inverse = TRUE,
           theme = shinythemes::shinytheme(theme = "darkly"), 

# Pestaña de Inicio -------------------------------------------------------

           tabPanel(title = "Inicio", icon = icon("home"), value = "inicio",
                    mainPanel(
                        width = 11, style="margin-left:4%; margin-right:4%",
                        br(),
                        br(),
                        fluidRow(h2(strong("MENÚ"), style="margin-top:0px; color:black;")),
                        br(),
                        fluidRow(h4("Explora la app dando clic en los siguientes recuadros:", style="margin-top:0px; color:black;")),
                        br(),
                        fluidRow(
                            
                            column(4, class="landing-page-column",
                                   div(class="landing-page-box", 
                                       div("Análisis de viajes", class = "landing-page-box-title"),
                                       div(class = "landing-page-icon", style="background-image: url(https://raw.githubusercontent.com/angiepcorreas/App-de-viajes/master/City.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                                       actionButton('jump_to_viajes', 'Análisis descriptivo sobre el origen-destino de los habitantes en el Valle de Aburrá.', 
                                                    class="landing-page-button", 
                                                    icon = icon("arrow-circle-right", "icon-lp")))),
                            
                            column(4, class="landing-page-column",
                                   div(class="landing-page-box", 
                                       div("Datos", class = "landing-page-box-title"),
                                       div(class = "landing-page-icon", style="background-image: url(https://raw.githubusercontent.com/angiepcorreas/App-de-viajes/master/Datapicture.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                                       actionButton('jump_to_data', 'Base de datos', 
                                                    class="landing-page-button", 
                                                    icon = icon("arrow-circle-right", "icon-lp")))),
                            
                            column(4, class="landing-page-column",
                                   div(class="landing-page-box", 
                                       div("Sobre la app", class = "landing-page-box-title"),
                                       div(class = "landing-page-icon", style="background-image: url(https://raw.githubusercontent.com/angiepcorreas/App-de-viajes/master/Info.png);
                     background-size: auto 80%; background-position: center; background-repeat: no-repeat; "),
                                       actionButton('jump_to_about', 'Encuentre aquí información general sobre esta app.', 
                                                    class="landing-page-button", 
                                                    icon = icon("arrow-circle-right", "icon-lp"))))
                        )

                    )
           ),

# Pestaña de Análisis de viajes -------------------------------------------

           tabPanel(title = "Análisis de viajes", icon = icon("subway"), value = "viajes",
                    mainPanel(width = 11, style="margin-left:3.5%; margin-right:1%",
                              
                              #Resumen descriptivo de viajes
                              fluidRow(
                                  valueBox(value = "Regreso a casa", h4("Motivo principal del viaje"), icon = icon("home"), color = "yellow", width = 4),
                                  valueBox(value = round(mean(Viajesnew$NUMERO_VIAJES),2), h4("Transferencias necesarias hasta el destino "), icon = icon("bus-alt"), color = "green", width = 4),
                                  valueBox(value = paste(round(mean(Viajesnew$DURACION),2), "", "minutos"), h4("Tiempo promedio de viaje"), icon = icon("clock"), color = "red", width = 4)
                              ),
                              
                              
                              #Cálculo de tiempo de viaje promedio
                              tags$style(".fa-train {color:#bab6ab}"),
                              h3(p("Medios de transporte",icon("bus",lib = "font-awesome"),icon("train", lib = "font-awesome"),style="color:black;text-align:center")),
                              
                              fluidRow(
                                 box(title = NULL, solidHeader = FALSE, width = 12, status = "success",
                                     column(4, style="border-right: 1px solid black",
                                            pickerInput("muniorigen", label = strong("Elija el municipio de origen:", style = "color:black"), choices = c(levels(Viajesnew$MUNICIPIO_O)), selected = "Medellin", options = list( `live-search` = TRUE, style = "success")),
                                            pickerInput("comunaorigen", label = strong("Elija la comuna de origen:", style = "color:black"), choices = "", options = list( `live-search` = TRUE, style = "success"))
                                     ),
                                     
                                     column(4, 
                                            pickerInput(inputId = "munidestino", label = strong("Elija el municipio de destino:", style = "color:black"),choices = c(levels(Viajesnew$MUNICIPIO_D)), selected = "Medellin",options = list( `live-search` = TRUE, style = "success")),
                                            pickerInput("comunadestino", label = strong("Elija la comuna de destino:", style = "color:black"), choices = "", options = list( `live-search` = TRUE, style = "success"))
                                     ),
                                     
                                     column(4, style = "margin-top: 25px" ,valueBoxOutput("mediatte", width = 12)
                                     ),
                                     
                                     fluidRow(column(width = 8, style="margin-left: 17px",pickerInput(inputId = "mediotte", width = "787px" ,label = strong("Elija el medio de transporte:", style = "color:black", align = "center"),choices = c(levels(Viajesnew$MODO_TTE_E1)), selected = "Auto Particular (Conductor)",options = list( `live-search` = TRUE, style = "success"))),
                                              column(width = 3, p(tags$i(strong("Nota:"),"", "Si la duración aparece como NaN, indica que no se tienen registros con ese origen, destino y medio de transporte."),style = "color:black; text-align: justify"))
                                     )
                                 )
                              ),
                              
                              br(),
                              
                              #Medio de transporte más utilizado
                              tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-to,.js-irs-0 .irs-from ,.js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1a8933; border-top: 1px #1a8933; border-bottom: 1px #1a8933;border-left: 1px #1a8933}")),
                              tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:#8ecc9c}")),
                              
                              fluidRow(
                                 column(8,box(title = strong("Medio de transporte más utilizado"), solidHeader = FALSE ,status = "success", width = 12, plotOutput("frecmedio"))),
                                 column(4,box(title = p("Filtro de gráfico", style = "color:green"), status = "warning" , width = 12,
                                              sliderTextInput(inputId = "filtrohoramedio", label = p("Seleccione las horas del día a observar:", style = "color:black"), choices = c(levels(datosviajes$HORA_O)), selected = c(0,23)),
                                              br(),
                                              pickerInput(inputId = "filtroingresosmedio", label = p("Filtre por ingresos:", style = "color:black"), choices = c(levels(datosviajes$INGRESOS)), selected = c(levels(datosviajes$INGRESOS)), multiple = TRUE, options = list(style ="success"))
                                              
                                              ),
                                        box(width = 12, status = "primary",
                                            h5(p("Las personas que laboran prefieren desplazarse en motocicleta, mientras que los jubilados, 
                                                 desempleados y amas de casa prefieren trasladarse a pie. Por su parte, los estudiantes no 
                                                 utilizan mucho los taxis y esto puede ser atribuido a los ingresos que perciben, los cuales 
                                                 no son muy altos en su etapa de estudiantes.", style = "color: black; text-align:justify"))
                                        )
                                 )
                                 
                              ),
                              
                              tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-to,.js-irs-1 .irs-from ,.js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1a8933; border-top: 1px #1a8933; border-bottom: 1px #1a8933;border-left: 1px #1a8933}")),
                              tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:#8ecc9c}")),
                              
                              #Boxplot de la duración del viaje en función de la hora del día
                              fluidRow(
                                 column(8, box(title = strong("Duración del viaje según la hora del día"), solidHeader = FALSE ,status = "success", width = 12, plotOutput("duracionhoras"))),
                                 column(4, box(title = p("Filtro de gráfico", style = "color:green"), status = "warning", width = 12,
                                               sliderTextInput(inputId = "filtrohora", label = p("Seleccione las horas del día a observar:", style = "color:black"), choices = c(levels(datosviajes$HORA_O)), selected = c(0,23)),
                                               br(),
                                               pickerInput(inputId = "filtromedioduracion", label = p("Filtre por medio de transporte:", style = "color:black"), choices = c(levels(datosviajes$MODO_TTE)), selected = c(levels(datosviajes$MODO_TTE)), multiple = TRUE, options = list(style ="success"))
                                    
                                           ),
                                           box(width = 12, status = "primary",
                                               h5(p("El mayor flujo ocurre en horas de la mañana y finalizando la tarde. 
                                                 Un 26.3% de las personas se desplazan entre las 6 a.m. y las 8 a.m., 
                                                 mientras que el 24.2% lo hace entre las 4 p.m. y 5 p.m., especialmente 
                                                 como retorno a sus hogares luego de la jornada de trabajo.", style = "color: black; text-align:justify"))
                                           )
                                 )
                              ),
                              
                              br(),

                              
                              #Frecuencia de viajes durante las horas del día
                              fluidRow(
                                  column(8, box(title = strong("Frecuencia de viajes durante las horas del día"), solidHeader = FALSE ,status = "success", width = 12, 
                                               plotOutput("plothoras"))),
                                  column(4, box(title = p("Filtro de gráfico", style = "color: green"), status = "warning", width = 12,
                                                pickerInput(inputId = "filtromotivo", label = p("Seleccione los motivos de viaje:", style = "color:black"), choices = c(levels(Viajesnew$MOTIVO_VIAJE)), selected = c(levels(Viajesnew$MOTIVO_VIAJE)), multiple = TRUE, options = list(style ="success")),
                                                br(),
                                                pickerInput(inputId = "filtroestrato", label = p("Seleccione el estrato:", style = "color:black"), choices = c(levels(Viajesnew$ESTRATO_O)), selected = c(levels(Viajesnew$ESTRATO_O)), multiple = TRUE, options = list(style ="success"))
                                     
                                            ),
                                            box(width = 12, status = "primary",
                                                h5(p("Se analizó el flujo de viajes durante el día, y como es de esperarse, 
                                                     el mayor flujo ocurre en horas de la mañana cuando las personas salen a 
                                                     trabajar, en su mayoría; y finalizando la tarde, ya que muchas personas
                                                     retornan a su hogar, por tanto, en dichas franjas horarias es cuando se 
                                                     presenta mayor flujo de tráfico.", style = "color: black; text-align:justify"))
                                            )
                                   )
                              ),
                              
                              hr(),
                              
                              #Diagrama de barras para la frecuencia de motivo del viaje
                              tags$style(".fa-university {color:#bab6ab}"),
                              h3(p("Motivo del viaje",icon("building",lib = "font-awesome"),icon("university", lib = "font-awesome"),style="color:black;text-align:center")),

                              fluidRow(width = 12,box(title = strong("Frecuencia del motivo de viaje"), solidHeader = FALSE, status = "success", width = 12, plotOutput("frecmotivo"))
                              )

                    )
           ),
       

# Panel de datos ----------------------------------------------------------

           tabPanel(title = "Datos", icon = icon("table"), value = "data",
                    
                    mainPanel(width = 11, style="margin-left:3.5%; margin-right:1%",
                              
                       #Información de disponibilidad de datos      
                       fluidRow(
                         column(width = 12, h4("Los datos utilizados en esta aplicación se encuentran disponibles en la página del Área Metropolitana del Valle de Aburrá. 
                                             Los datos fueron extraídos de la encuesta 'Origen-Destino' del año 2017 y contiene información personal, 
                                             destino/origen, medios de transporte, entre otros aspectos de los encuestados. Para acceder a los datos siga el siguiente",
                                            a(href="http://datosabiertos.metropol.gov.co/search/field_topic/movilidad-y-transporte-2", em("enlace"),target="_blank"), ".",
                                            style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"))
                       ),
                       
                       hr(),
                       
                       #Tabla de datos
                       tags$style(".fa-database {color:#bab6ab}"),
                       h3(p("Datos",icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                       fluidRow(
                          column(width = 12,
                                 div(dataTableOutput("datable"),style = "overflow-x: scroll;")
                          )
                                       
                       )
                       
                    )

           ),

# Panel de información sobre la aplicación ----------------------------------------------------------
           
           tabPanel(title = "Sobre la app", icon = icon("info"), value = "about",
                    mainPanel(width = 11, style="margin-left:3.5%; margin-right:1%",
                              
                              #Título
                              fluidRow(column(width = 2),
                                       column(width = 8,
                                              h3(p("Información sobre la app", style = "color:black;text-align:center"))
                                       )
                              ),
                              
                              #Cuadro de información
                              fluidRow(column(width=1,align="center"),
                                       
                                       column(width = 10,style="background-color:lavender;border-radius: 10px",
                                              br(),
                                              p("La elección del medio de transporte de los habitantes ha sido un aspecto importante a estudiar para el desarrollo de proyectos relacionados con la movilidad e infraestructura (de Dios Ortuzar et. al 2011). En los últimos años,
                                                 diferentes estudios relacionados con la predicción del modo de transporte utilizado han cobrado relevancia gracias a la aplicación de técnicas de machine learning que permiten analizar la importancia de características individuales, 
                                                 factores ambientales y condiciones de viaje en la elección del medio de transporte a utilizar (Omrani 2015). Modelos tradicionales como la regresión logística multinomial han sido ampliamente utilizados para la predicción de los modos 
                                                 de transporte (McFadden 1973) debido a la facilidad en su aplicación e interpretación de resultados, sin embargo, diferentes técnicas de machine learning han demostrado ser muy útiles para la clasificación de medios de transporte 
                                                 utilizados en la población gracias a su flexibilidad y simplicidad, como lo exponen Hagenauer y Helbich (2017) en su estudio comparativo de diferentes modelos clasificadores de aprendizaje automático para analizar la elección del modo 
                                                 de transporte.",style="color:black;text-align:justify"),
                                              br(),
                                              p("En este trabajo se presenta la aplicación de diferentes técnicas de Machine Learning para la predicción del medio de transporte elegido por los habitantes del Valle de Aburrá y se compara la eficiencia de clasificación de las diferentes 
                                                 técnicas usadas. Se obtuvieron datos a partir de la encuesta Origen Destino 2017 realizada por el Área Metropolitana (Área Metropolitana 2017) y se tuvieron en cuenta variables asociadas a características propias de los habitantes como: 
                                                 edad, género, escolaridad, ocupación e ingresos; y variables relacionadas con los viajes específicos como por ejemplo la duración del viaje, comuna de origen, comuna de destino, modo de transporte utilizado, entre otros. 
                                                 Finalmente, se construyó una aplicación web utilizando el paquete Shiny de R en donde los usuarios podrán visualizar e interactuar con los datos usados en el análisis.", 
                                                style="color:black;text-align:justify"),

                                              column(width = 4,align="center"
                                              ),
                                              
                                              column(width = 4,align="center",
                                                     br(),
                                                     tags$img(src="https://raw.githubusercontent.com/angiepcorreas/Shinycontest/master/arealogo.jpg", width="200px",height="200px"),
                                                     br(),
                                                     br(),
                                                     p("Para mayor información y búsqueda de los datos ingrese a la página del",
                                                       br(),
                                                       a(href="http://datosabiertos.metropol.gov.co/search/field_topic/movilidad-y-transporte-2", em("Área Metropolitana"),target="_blank"),style="text-align:center;color:black")
                                                     

                                              ),
                                              
                                              column(width = 4, align="center"
                                                
                                              )

                                       ),
                                       column(width=1)
                              )
                    )
                   
           ),
           


# Pie de página -----------------------------------------------------------

           tags$footer(column(5, tags$a(href="https://angiepcorreas.github.io/", tags$b("App developed by Angie Correa & Olga Úsuga"), 
                                        class="externallink", style = "color: black; text-decoration: none")), 
                       column(2, tags$a(href="https://twitter.com/angiepcorreas", tags$b("Twitter"), icon("twitter"),
                                        class="externallink", style = "color: black; text-decoration: none")), 
                       column(3, tags$a(href="https://github.com/angiepcorreas", tags$b("Github"), icon("github"), 
                                        class="externallink", style = "color: black; text-decoration: none")), 
                       column(2,tags$a(href="https://github.com/angiepcorreas/App-de-viajes", tags$b("Source code"), icon("file-code-o"), 
                                       class="externallink", style = "color: black; text-decoration: none")), 
                       style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color: #339966"
           ) 
           
# THE END -----------------------------------------------------------------
           
           
)


