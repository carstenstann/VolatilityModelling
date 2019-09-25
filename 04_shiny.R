#########################################################################################
## Project: VolatilityModelling
## Script purpose: Productionize GARCH models in Shiny app
## Date: 24.09.2019
## Author: Carsten Stann
#########################################################################################

library(shiny)
library(shinydashboard)
library(tidyquant)
library(tidyverse)
library(timetk)
library(xts)
library(DT)
library(highcharter)


# UI ------------------------------------------------------------------------------------

dashboardHeader <- dashboardHeader(title = "VaR Forecasting")

dashboardSidebar <- dashboardSidebar(
   
   textInput("symb", "Symbol", "SPY"),
   
   dateRangeInput(inputId = "dates", 
                  label = NULL,
                  start = "2013-01-01",
                  end = NULL,
                  format = "yyyy-mm-dd"),
   
   actionButton("updateData", "Update Data")
)

dashboardBody <- dashboardBody(
   fluidRow(
      box(highchartOutput("plot"), width = 12)
   ),
   
   fluidRow(
      box(DT::dataTableOutput("table"), width = 12)
   )
)

ui <- dashboardPage(dashboardHeader, dashboardSidebar, dashboardBody)

server <- function(input, output) {
   
   dataInput <- reactive({
      
      #Take dependency on input$updateData button
      input$updateData
      
      isolate({
         getSymbols(input$symb, src = "yahoo",
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
      })
   })
   
   output$table <- DT::renderDataTable({ 
      
      input$updateData
      
      isolate({DT::datatable(dataInput())})
      
   })
   
   output$plot <- renderHighchart({
      
      input$updateData
         
      #isolate({ plot(Ad(dataInput()), main = input$symb) })
      isolate({ hchart(dataInput()) })
   
   })
}

shinyApp(ui, server)
