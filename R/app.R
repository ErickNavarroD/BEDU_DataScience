## app.R ##

## Dash board para el data set 'mtcars'

library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)

#Esta parte es el análogo al ui.R
ui <- 
  
  fluidPage(#theme = shinytheme("darkly"),
    
    dashboardPage(
                  
                  dashboardHeader(title = "Reto 8"),
                  
                  dashboardSidebar(
                    
                    sidebarMenu(
                      menuItem("Histograma", tabName = "Hist", icon = icon("chart-bar")),
                      menuItem("Probabilidades", tabName = "Prob3", icon = icon("chess-board")),
                      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                      menuItem("Factores de Ganancia", tabName = "FacMomios", icon = icon("chart-line"))
                    )
                    
                  ),
                  
                  dashboardBody(
                    
                    tabItems(
                      
                      # Histograma
                      tabItem(tabName = "Hist",
                              fluidRow(
                                titlePanel("Histograma de goles por equipo."), 
                                selectInput("eq", "Seleccione el equipo.",
                                            choices = names(SP1)),
                                
                                box(plotOutput("plot1", height = 250)),
                                
                              )
                      ),
                      
                      # Dispersión
                      tabItem(tabName = "Prob3", 
                              fluidRow(
                                titlePanel(h3("Tabla de probabilidad condicional.")),
                                img( src = "Rplot3.png", 
                                     height = 500, width = 600)
                              )
                      ),
                      
                      
                      
                      tabItem(tabName = "data_table",
                              fluidRow(        
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                              )
                      ), 
                      
                      tabItem(tabName = "FacMomios",
                              fluidRow(
                                titlePanel(h3("Imágen de calor para la correlación de las variables")),
                                column(width = 4,
                                       img( src = "Rplot1.png", 
                                            height = 250, width = 300)
                                ),
                                column(width = 3, offset = 2,
                                       img( src = "Rplot2.png", 
                                            height = 250, width = 300)
                                )
                              )
                      )
                      
                    )
                  )
    )
  )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
  library(ggplot2)
  
  #Gráfico de Histograma
  output$plot1 <- renderPlot({
    
    SP1<-select(read.csv("SP1.csv"),FTHG,FTAG)
    
    var <- SP1[,input$eq]
    var.dist<-data.frame(table(var)/length(var))
    
    ggplot(var.dist, aes(x=var.dist$var,y=Freq)) + 
      geom_bar( stat='identity') +
      labs( xlim = c(0, max(var))) + 
      theme_light() + 
      xlab(input$eq) + ylab("Frecuencia") #+
      #facet_wrap("FTAG")
    
  })
  
  #Data Table
  output$data_table <- renderDataTable( {SP1}, 
                                        options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 5)
  )
  
}


shinyApp(ui, server)