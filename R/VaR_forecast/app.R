#########################################################################################
## Project: VolatilityModelling
## Script purpose: Productionize GARCH models in Shiny app
## Date: 24.09.2019
## Author: Carsten Stann
#########################################################################################

library(shiny)
library(shinydashboard)
library(quantmod)
library(PerformanceAnalytics)
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
                   start = "2015-01-01",
                   end = NULL,
                   format = "yyyy-mm-dd"),
    
    actionButton("updateData", "Refresh data"),
    
    checkboxInput(inputId = "use_log_returns", label = "Use log-returns", value = TRUE)
)

dashboardBody <- dashboardBody(
    fluidRow(
        tabBox(
            title = "Data",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", 
            #height = "250px",
            width = 12,
            tabPanel(
                "Price", 
                highchartOutput("price")
            ),
            tabPanel(
                "Returns",
                highchartOutput("plot_returns")
            )
        )
    ),
    fluidRow(
        box(DT::dataTableOutput("table"), width = 12)
    )
)

ui <- dashboardPage(dashboardHeader, dashboardSidebar, dashboardBody)

server <- function(input, output) {
    
    input_data <- reactive({
        #Take dependency on input$updateData button
        input$updateData
        isolate({
            getSymbols(input$symb, src = "yahoo",
                       from = input$dates[1],
                       to = input$dates[2],
                       auto.assign = FALSE)
        })
    })
    log_return <- reactive({
        input$updateData
        isolate({Return.calculate(Ad(input_data()), method = "log")[-1]})
    })
    simple_return <- reactive({
        input$updateData
        isolate({Return.calculate(Ad(input_data()), method = "discrete")[-1]})
    })
    output$price <- renderHighchart({
        input$updateData
        isolate({hchart(input_data())})
    })
    output$plot_returns <- renderHighchart({
        input$use_log_return
        if(input$use_log_returns == TRUE){
            hchart(log_return(), type = "column")
        } else
            hchart(simple_return(), type = "column")
    })
    output$table <- DT::renderDataTable({ 
        input$updateData
        isolate({DT::datatable(tk_tbl(input_data(), rename_index = "Date"))})
    })
}

shinyApp(ui, server)
