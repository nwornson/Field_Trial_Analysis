#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(DT)

direc = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(direc))

df = read.csv('simulated_data.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Field Trial Analysis"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Heatmaps",
                 fluidRow(
                   column(4,
                          selectInput("Trial","Trial",
                 unique(as.character(df$Trial)))
                 )),
        
        plotOutput('trt_Heat'),         
        plotOutput('BL_Heat'),
        plotOutput('H_Heat')
        
        )),

        
        
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$trt_Heat = renderPlot({
      
      df  %>%
        filter(Trial == input$Trial) %>%
        ggplot(aes(x = Row, y = Range)) +
        geom_tile(color = 'black',fill = 'white') +  
        geom_text(aes(label = Treatment), color = "black", size = 6) +
        theme_classic()
    })

    output$BL_Heat = renderPlot({
        
      df  %>%
          filter(Trial == input$Trial) %>%
          ggplot(aes(x = Row, y = Range, fill = baseline)) +
          geom_tile() +
          scale_fill_gradient(low="palevioletred",high="palegreen") + 
          geom_text(aes(label = baseline), color = "black", size = 5)+
        theme_classic()
    })
    
    output$H_Heat = renderPlot({
      
      df  %>%
        filter(Trial == input$Trial) %>%
        ggplot(aes(x = Row, y = Range, fill = harvest)) +
        geom_tile() +
        scale_fill_gradient(low="palevioletred",high="palegreen") +
        geom_text(aes(label = harvest), color = "black", size = 5)+
        theme_classic()
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
