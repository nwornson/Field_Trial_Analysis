

library(shiny)
library(tidyverse)
library(DT)

#direc = rstudioapi::getActiveDocumentContext()$path 
#setwd(dirname(direc))

#df = read.csv('simulated_data.csv')

# functions
source('data_sim.R')

df = field_data()
df_L1 = L1_stats(df)


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
        
        ),
        
        tabPanel(title = 'Win Rates',
                 fluidRow(
                   column(4,
                          selectInput("Treatment","Treatment",
                                      unique(as.character(df$Treatment[test$Treatment!= 'Control'])))
                   )),
        plotOutput('WR_trt')
        
        ),
        
        tabPanel("Summary",
                 fluidRow(
                   column(4,
                          selectInput("Trial","Trial",
                            unique(as.character(df$Trial)))
                   )),
                 dataTableOutput('Summaries')
        ),
        
        tabPanel("Analysis",
                 fluidRow(
                   column(4,
                          selectInput("Trial","Trial",
                                      unique(as.character(df$Trial)))
                   )),
                  dataTableOutput('anova'),
                 dataTableOutput('dunnet')
                 
        )
        
        
        
    ),
    

        
        
    
)

# Define server logic 
server <- function(input, output) {
  
    output$trt_Heat = renderPlot({
      trt_heat(df,input$Trial)
    })

    output$BL_Heat = renderPlot({
      BL_heat(df,input$Trial)
    })
    
    output$H_Heat = renderPlot({
      H_heat(df,input$Trial)
    })
    
    output$WR_trt = renderPlot({
      WR_bar(df_L1,input$Treatment)
    })
    
    output$Summaries = renderDataTable(datatable({
      display_stats(df,input$Trial)},
      options = list(dom = 't')
    ))
    
    output$anova = renderDataTable(datatable({
      display_anova(df,input$Trial)},
      caption = 'Fixed Effect Anova',
      options = list(dom = 't')
    ))
    
    output$dunnet = renderDataTable(datatable({
      display_dunnet(df,input$Trial)},
      caption = 'Dunnets Test',
      options = list(dom = 't')
    ))
}

# Run the application 
shinyApp(ui = ui, server = server)
