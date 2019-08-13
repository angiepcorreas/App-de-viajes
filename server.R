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

shinyServer(function(input, output, session) {
    

# ObserveEvents para cambiar tabsets de la barra de navegación-------------

    observeEvent(input$jump_to_viajes, {
        updateTabsetPanel(session, "navegacion", selected = "viajes")
    })
    
    
    observeEvent(input$jump_to_data, {
        updateTabsetPanel(session, "navegacion", selected = "data")
    })
    
    observeEvent(input$jump_to_about, {
        updateTabsetPanel(session, "navegacion", selected = "about")
    })
    

#   -----------------------------------------------------------------------
# PESTAÑA DE VIAJES -------------------------------------------------------
#   -----------------------------------------------------------------------


# Cálculos de tiempo de viaje según origen y destino ----------------------

    observe({
        
        Viajesnew.1 <- subset(Viajesnew, Viajesnew$MUNICIPIO_O %in% input$muniorigen)
        Viajesnew.1 <- droplevels(Viajesnew.1)
        updatePickerInput(session, "comunaorigen", choices = c(levels(Viajesnew.1$COMUNA_O))) 
        
        Viajesnew.2 <- subset(Viajesnew, Viajesnew$MUNICIPIO_D %in% input$munidestino)
        Viajesnew.2 <- droplevels(Viajesnew.2)
        updatePickerInput(session, "comunadestino", choices = c(levels(Viajesnew.2$COMUNA_D))) 
        
        
    })
    
    reactivemediatte <- reactive({
        Viajesnew.3 <- subset(Viajesnew, Viajesnew$COMUNA_O %in% input$comunaorigen & Viajesnew$COMUNA_D %in% input$comunadestino & Viajesnew$MODO_TTE_E1 %in% input$mediotte)
        Viajesnew.3 <- droplevels(Viajesnew.3)
        duracionmedia <- round(mean(Viajesnew.3$DURACION),2)
        return(duracionmedia)
        
    })
    
    output$mediatte <- renderValueBox({
        
        valueBox(value = paste(reactivemediatte(),"" ,"minutos"), h4("Tiempo promedio de viaje"), icon = icon("clock"), color = "red", width = 12)
    })
    


# Medio de transporte más utilizado ---------------------------------------

    reactivemodotte <- reactive({
        
        return(subset(datosviajes, as.numeric(levels(HORA_O))[HORA_O] >= input$filtrohoramedio[1] & as.numeric(levels(HORA_O))[HORA_O] <= input$filtrohoramedio[2] & INGRESOS %in% input$filtroingresosmedio))
        
    })
    
    output$frecmedio <- renderPlot({
        
        ggplot(reactivemodotte(), aes(x= MODO_TTE, text=paste("Frecuencia:",round(((..count..)/sum(..count..))*100,1), "%"))) + 
            geom_bar(aes(y = ((..count..)/sum(..count..))), fill = "sandybrown") +    
            scale_y_continuous(labels = percent)+ 
            ylab("Porcentaje")+ theme_bw() + theme(axis.title=element_text(face = "bold"), plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 10))+ 
            coord_flip() + xlab("") + geom_text(aes(label= paste(round((..count../sum(..count..))*100,1), "%"), y = ..count../sum(..count..)), stat = "count" ,vjust=0.5, hjust = -0)
        
    })
    


# Boxplot de la duración del viaje en función de la hora ------------------

    reactiveduracion <- reactive({
        
        return(subset(datosviajes, as.numeric(levels(HORA_O))[HORA_O] >= input$filtrohora[1] & as.numeric(levels(HORA_O))[HORA_O] <= input$filtrohora[2] & MODO_TTE %in% input$filtromedioduracion))
        
    })
    
    output$duracionhoras <- renderPlot({
        
        ggplot(reactiveduracion(), aes(x=HORA_O, y=DURACION)) + 
            geom_boxplot(fill = "#42aaae", alpha = 0.7) + 
            scale_y_continuous(name = "Duración del viaje (en minutos)"
            )+ xlab("")+  theme(axis.text.x = element_text(size = 12, vjust = 1, color = "black"), axis.text.y = element_text(size = 12, color = "black"))+
            stat_summary(fun.y=mean, geom="point", shape = 3 ,size=1, col = "#ae5042")+theme(axis.title=element_text(face = "bold", size = 12))
        
        
    })
    

# Frecuencia de viajes durante las horas del día --------------------------
    
    reactivehoras <- reactive({

        return(subset(Viajesnew, MOTIVO_VIAJE %in% input$filtromotivo & ESTRATO_O %in% input$filtroestrato))
        
    })
    
    output$plothoras <- renderPlot({

         diagram <- ggplot(reactivehoras(), aes(x = times(Time_O), text=paste("Porcentaje:",round(((..count..)/sum(..count..))*100,2), "%"))) + geom_bar(binwidth=1/24/60,aes(y = ((..count..)/sum(..count..))))
        
        diagram + scale_x_chron(format="%H:%M", n = 15) + scale_y_continuous(labels = percent, limits = c(0,0.05)) +
        theme(axis.title=element_text(face = "bold")) + xlab("Hora de viaje") + ylab("Porcentaje")
        
    })


# Frecuencia del motivo del viaje -----------------------------------------

    output$frecmotivo <- renderPlot({
        
        ggplot(Viajesnew, aes(x= MOTIVO_VIAJE, text=paste("Porcentaje:",round(((..count..)/sum(..count..))*100,2), "%"))) + 
            geom_bar(aes(y = ((..count..)/sum(..count..))), fill = "sandybrown")+ scale_y_continuous(labels = percent)+
            xlab("")+ylab("Porcentaje") + theme_bw() +
            theme(axis.title=element_text(face = "bold"), plot.title = element_text(face = "bold")) +
            geom_text(aes(label= paste(round((..count../sum(..count..))*100,2), "%"), y = ..count../sum(..count..)), stat = "count" ,vjust=0)
        
    })
 


#   -----------------------------------------------------------------------
# PESTAÑA DE DATOS --------------------------------------------------------
#   -----------------------------------------------------------------------


# Tabla de datos ----------------------------------------------------------

    output$datable <- renderDataTable({
        
        
        datatable(datosviajes, rownames = FALSE , class = 'cell-border stripe', 
                  
                  options = list(paging = TRUE, pageLength = 15,lengthMenu = c(5,10,15), lengthChange = TRUE,
                                 initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#8ecc9c', 'color': 'black'});",
                                     "}"),
                                 columnDefs=list(list(className='dt-center',targets="_all"))
                  )
                  
         )  %>%
            formatStyle(... = "font-color:black;",color = "black", columns = 1:14)
            
        })

# THE END -----------------------------------------------------------------

})
