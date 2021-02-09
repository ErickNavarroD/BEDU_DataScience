# P25.R

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(viridis)
library(tibble)





# cargamos bases de datos
co2 = read.csv("tco2c.csv", header = TRUE)
cf = read.csv("tcfc.csv", header = TRUE)
countries = read.csv("countries.csv", header = TRUE)

# Seccion UI
ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Proyecto 25"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Histograma", tabName = "hist", icon = icon("dashboard")),
                    menuItem("Serie de tiempos", tabName = "ts", icon = icon("area-chart")),
                    menuItem("Graficos de dispersion", tabName = "disp", icon = icon("area-chart")),
                    menuItem("Visualizaciones", tabName = "vis", icon = icon("circle")),
                    menuItem("Summary", tabName = "summary", icon = icon("table")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table"))
                )
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Histograma
                    tabItem(tabName = "hist",
                            fluidRow(
                                titlePanel("Histogramas de las variables"),
                                mainPanel("Histogramas en blanco significan que solo hay un dato (promedio) para la region."),
                                selectInput("x1", "Seleccione el dataset", 
                                            choices = c("Emisiones de CO2","Cobertura Forestal")),
                                selectInput("x", "Seleccione el pais", 
                                            choices = countries[1]),
                                
                                box(plotOutput("plot1", height = 300)),
                                box(plotOutput("plot2", height = 300)),
                                box(
                                    title = "Controles",
                                    sliderInput("bins", "Numero de intervalos:", 2, 30, 13),
                                    sliderInput("rango", "Periodo", min = 1901, 
                                                max = 2016, value = c(1901, 2016))
                                )
                            )
                    ),
                    
                    # Diagramas de serie de tiempos
                    tabItem(tabName = "ts",
                            fluidRow(
                                titlePanel("Graficos de series de tiempos"),
                                mainPanel("Debido a la falta de datos, el dataset estima la cobertura forestal de algunos paises."),
                                selectInput("x3", "Seleccione el dataset", 
                                            choices = c("Emisiones de CO2","Cobertura Forestal")),
                                selectInput("x2", "Seleccione el pais", 
                                            choices = countries[1]),
                                
                                box(plotOutput("plot3", height = 300)),
                                box(plotOutput("plot4", height = 300)),
                                box(
                                    title = "Controles",
                                    sliderInput("rango1", "Periodo", min = 1901, 
                                                max = 2016, value = c(1901, 2016))
                                )
                            )
                    ),
                    
                    # Diagramas de dispersion
                    tabItem(tabName = "disp",
                            fluidRow(
                                titlePanel("Graficos de dispersion"),
                                mainPanel("Nuevamente la falta de datos en cobertura forestal genera graficos verticales en algunos casos."),
                                selectInput("x5", "Seleccione el dataset", 
                                            choices = c("Emisiones de CO2","Cobertura Forestal")),
                                selectInput("x4", "Seleccione el pais", 
                                            choices = countries[1]),
                                
                                box(plotOutput("plot5", height = 400)),
                                box(
                                    title = "Controles",
                                    sliderInput("rango2", "Periodo", min = 1901, 
                                                max = 2016, value = c(1901, 2016))
                                )
                            )
                    ),
                    
                    tabItem(tabName = "vis",
                            fluidRow(
                                titlePanel("Visualizaciones de datos"),
                                selectInput("x9", "Seleccione el dataset", 
                                            choices = c("Emisiones de CO2","Cobertura Forestal", "Modelos Predictivos")),
                                imageOutput("img2"),
                                imageOutput("img1")


                    )), 
                    
                    
                    tabItem(tabName = "data_table",
                            fluidRow(
                                titlePanel("Tablas de datos"),
                                selectInput("x7", "Seleccione el dataset", 
                                            choices = c("Emisiones de CO2","Cobertura Forestal")),
                                selectInput("x6", "Seleccione el pais", 
                                            choices = countries[1]),
                                titlePanel("Tabla de datos"),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    tabItem(tabName = "summary",
                            fluidRow(
                                titlePanel("Resumen de la base de datos"),
                                selectInput("x8", "Seleccione el dataset", 
                                            choices = c("Emisiones de CO2","Cobertura Forestal")),
                                verbatimTextOutput("summary")),
                            )
                    
                )
            )
        )
    )

# Seccion SERVER ###################################################

server <- function(input, output, session) {
    
    #Actualizador de slider de rango de histogramas
    observeEvent(input$x1, {
        if (input$x1 == "Emisiones de CO2"){
            updateSliderInput(session, "rango", min=1901, max=2016, value=c(1901, 2016)) 
        } else {
            updateSliderInput(session, "rango", min=1990, max=2016, value=c(1990, 2016))
        }

    })
    
    # Histograma Temperatura
    output$plot1 <- renderPlot({
        if (input$x1=="Emisiones de CO2"){
            v = co2 %>% filter(Country == input$x)
            v = v %>% filter(Year >= input$rango[1], Year <= input$rango[2])
        } else {
            v = cf %>% filter(Country == input$x)
            v = v %>% filter(Year >= input$rango[1], Year <= input$rango[2])
        }
        
        bin = seq(min(v[4]), max(v[4]), length.out = input$bins + 1)
        
        ggplot(v, aes(x=Temperature)) + 
            geom_histogram(breaks = bin) +
            labs( xlim = c(0, max(v[4]))) + 
            theme_light() +
            ggtitle("Temperatura") +
            xlab(input$x) + ylab("Frecuencia")
    })
    
    # Histograma Variable (co2 o cf)
    output$plot2 <- renderPlot({

        if (input$x1=="Emisiones de CO2"){
            v = co2 %>% filter(Country == input$x)
            v = v %>% filter(Year >= input$rango[1], Year <= input$rango[2])
            bin = seq(min(v[5]), max(v[5]), length.out = input$bins + 1)
            
            ggplot(v, aes(x=CO2emissions)) + 
                geom_histogram(breaks = bin) +
                labs( xlim = c(0, max(v[5]))) + 
                theme_light() +
                ggtitle(input$x1) +
                xlab(input$x) + ylab("Frecuencia")
            
        } else {
            v = cf %>% filter(Country == input$x)
            v = v %>% filter(Year >= input$rango[1], Year <= input$rango[2])
            bin = seq(min(v[5]), max(v[5]), length.out = input$bins + 1)
            
            ggplot(v, aes(x=ForestCover)) + 
                geom_histogram(breaks = bin) +
                labs( xlim = c(0, max(v[5]))) + 
                theme_light() +
                ggtitle(input$x1) +
                xlab(input$x) + ylab("Frecuencia")}
    })
    
    #Actualizador de slider de rango de series de tiempo
    observeEvent(input$x3, {
        if (input$x3 == "Emisiones de CO2"){
            updateSliderInput(session, "rango1", min=1901, max=2016, value=c(1901, 2016)) 
        } else {
            updateSliderInput(session, "rango1", min=1990, max=2016, value=c(1990, 2016))
        }
        
    })
    
    # serie de tiempo de Temperatura
    output$plot3 <- renderPlot({
        if (input$x3=="Emisiones de CO2"){
            v = co2 %>% filter(Country == input$x2)
            v = v %>% filter(Year >= input$rango1[1], Year <= input$rango1[2])
        } else {
            v = cf %>% filter(Country == input$x2)
            v = v %>% filter(Year >= input$rango1[1], Year <= input$rango1[2])
        }
        
        ggplot(data = v, aes(x = Year, y = Temperature))+
            geom_line(color = "#00AFBB", size = 2) +
            theme_light() + ggtitle(input$x2) +
            xlab("Fecha") + ylab("Temperatura") + stat_smooth(
                color = "#FC4E07", fill = "#FC4E07",
                method = "loess"
            )
    })

    # serie de tiempo de Variable (co2 o cf)
    output$plot4 <- renderPlot({
        if (input$x3=="Emisiones de CO2"){
            v = co2 %>% filter(Country == input$x2)
            v = v %>% filter(Year >= input$rango1[1], Year <= input$rango1[2])
            
            ggplot(data = v, aes(x = Year, y = CO2emissions))+
                geom_line(color = "#00AFBB", size = 2) +
                theme_light() + ggtitle(input$x2) +
                xlab("Fecha") + ylab("Emisiones de CO2")+ stat_smooth(
                    color = "#08DB0F", fill = "#FC4E07",
                    method = "loess"
                )
        } else {
            v = cf %>% filter(Country == input$x2)
            v = v %>% filter(Year >= input$rango1[1], Year <= input$rango1[2])
            
            ggplot(data = v, aes(x = Year, y = ForestCover))+
                geom_line(color = "#00AFBB", size = 2) +
                theme_light() + ggtitle(input$x2) +
                xlab("Fecha") + ylab("Cobertura Forestal")+ stat_smooth(
                    color = "#08DB0F", fill = "#FC4E07",
                    method = "loess"
                )
        }
        
    })
    
    #Actualizador de slider de rango de dispersiones
    observeEvent(input$x5, {
        if (input$x5 == "Emisiones de CO2"){
            updateSliderInput(session, "year", min=1901, max=2016, value=2016) 
        } else {
            updateSliderInput(session, "year", min=1990, max=2016, value=2016)
        }
        
    })
    
    # dispersion temp vs variable
    output$plot5 <- renderPlot({
        if (input$x5=="Emisiones de CO2"){
            v = co2 %>% filter(Country == input$x4)
            v = v %>% filter(Year >= input$rango2[1], Year <= input$rango2[2])
            
            ggplot(data = v, aes(x = CO2emissions, y = Temperature)) +
                geom_point(aes(color = Year)) + scale_color_viridis(option = "D") +
                theme_light() + ggtitle("Temperatura vs Emisiones de CO2") +
                xlab("Emisiones de CO2") + ylab("Temperatura")+ stat_smooth(
                    color = "#08DB0F",
                    method = "loess"
                )
        } else {
            v = cf %>% filter(Country == input$x4)
            v = v %>% filter(Year >= input$rango2[1], Year <= input$rango2[2])
            
            ggplot(data = v, aes(x = ForestCover, y = Temperature)) +
                geom_point(aes(color = Year)) + scale_color_viridis(option = "D") +
                theme_light() + ggtitle("Temperatura vs Cobertura Forestal") +
                xlab("Cobertura Forestal") + ylab("Temperatura")+ stat_smooth(
                    color = "#08DB0F",
                    method = "loess"
                )
        }
        
    })
    
    #Actualizador de imagenes
    output$img1 <- renderImage({
        if (input$x9 == "Emisiones de CO2"){
            return(list(src = "co2.jpeg",contentType = 'image/jpeg',width = 768,height = 400))
        } else {if (input$x9 == "Cobertura Forestal"){
            return(list(src = "cf.png",contentType = 'image/png',width = 768,height = 400))
        } else {
            return(list(src = "pa.png",contentType = 'image/png',width = 768,height = 400))
        }}
        
    }, deleteFile = FALSE)
    
    output$img2 <- renderImage({
        if (input$x9 == "Emisiones de CO2"){
            return(list(src = "co2map.jpeg",contentType = 'image/jpeg',width = 768,height = 400))
        } else {        if (input$x9 == "Cobertura Forestal"){
            return(list(src = "cfmap.png",contentType = 'image/png',width = 768,height = 400))
        } else {
            return(list(src = "pp.png",contentType = 'image/png',width = 768,height = 400))
        }}
        
    }, deleteFile = FALSE)
    
    
    #Data Table
    output$data_table <- renderDataTable( {        
        if (input$x7=="Emisiones de CO2"){
            v = co2 %>% filter(Country == input$x6)
            return(v)
        } else {
            v = cf %>% filter(Country == input$x6)
            return(v)
    }}, 
            options = list(aLengthMenu = c(5,25,50,100), iDisplayLength = 25)
    )
    
    
    output$summary <- renderPrint({
        if (input$x8=="Emisiones de CO2"){
            return(summary(co2))
        } else {
            return(summary(cf))
    }})
    
}

shinyApp(ui, server)