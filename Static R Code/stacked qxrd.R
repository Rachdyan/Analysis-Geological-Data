library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(glue)
library(shiny)
library(purrr)
library(strex)
library(shinyWidgets)
library(tidyr)

stacked_qxrd_data <- read.csv("./data/stacked_qxrd.csv")
names(stacked_qxrd_data)

stacked_qxrd_data <- stacked_qxrd_data %>%
  mutate(color = case_when(Sample == "Con 1" ~ "#F8766D",
                           Sample == "Con 2" ~ "#00BFC4"))

tez <- as.symbol("X2Theta")

plot_ly(stacked_qxrd_data, x = ~X2Theta, y = ~get("Offsetted.Intensity"), 
        split = ~Sample, type = "scatter", mode = "lines", 
        line = list(color = stacked_qxrd_data$color)) %>%
  layout(yaxis = list(title = "Intensity"),
         xaxis = list(title = "2 Theta"),
         legend = list(x = 0.03, y = 0.98, title = list(text='<b> Sample </b>'))
  )

### COBA SHINY
stacked_qxrd_data <- read.csv("./data/stacked_qxrd.csv")


ui <- fluidPage(
  checkboxGroupInput(
    inputId = "sample", 
    label = "Select Sample", 
    choices = c("Con 1", "Con 2"), 
    selected = c("Con 1", "Con 2"),
    inline = T
  ),
  radioButtons('intensity', "Select the Intensity Type", 
               c("Offsetted Intensity" = "Offsetted.Intensity", 
                 "Raw Intensity" = "Raw.Intensity")),
  tableOutput("stacked_qxrd_table"),
  plotlyOutput(outputId = "stacked_qxrd_plot")
)


server <- function(input, output, session){
  last_data <- reactive({
    req(input$sample)
    data <- stacked_qxrd_data %>% filter(Sample %in% input$sample) %>%
      mutate(color = case_when(Sample == "Con 1" ~ "#F8766D",
                               Sample == "Con 2" ~ "#00BFC4"))
  })
  
  # output$stacked_qxrd_table <- renderTable(
  #   last_data()
  # )
  
  output$stacked_qxrd_plot <- renderPlotly(
    plot_ly(last_data(), x = ~X2Theta, y = ~get(input$intensity), 
            split = ~Sample, type = "scatter", mode = "lines", 
            line = list(color = stacked_qxrd_data$color)) %>%
      layout(yaxis = list(title = "Intensity"),
             xaxis = list(title = "2 Theta"),
             legend = list(x = 0.03, y = 0.98, title = list(text='<b> Sample </b>'))
      )
    
  )
}

shinyApp(ui, server)

### BUAT MODULE

stackedqxrdInput <- function(id) {
  ns <- NS(id)
 
  tagList(
    checkboxGroupInput(
      inputId = ns("sample"), 
      label = "Select Sample", 
      choices = c("Con 1", "Con 2"), 
      selected = c("Con 1", "Con 2"),
      inline = T
    ),
    radioButtons(ns('intensity'), "Select the Intensity Type", 
                 c("Offsetted Intensity" = "Offsetted.Intensity", 
                   "Raw Intensity" = "Raw.Intensity"))
  )
}

stackedqxrdServer <- function(id, stacked_qxrd_data){
  moduleServer(id, function(input,output,session){
    
    last_data <- reactive({
      req(input$sample)
      data <- stacked_qxrd_data %>% filter(Sample %in% input$sample) %>%
        mutate(color = case_when(Sample == "Con 1" ~ "#F8766D",
                                 Sample == "Con 2" ~ "#00BFC4"))
    })
    
    plot <- reactive(
      plot_ly(last_data(), x = ~X2Theta, y = ~get(input$intensity), 
              split = ~Sample, type = "scatter", mode = "lines", 
              line = list(color = last_data()$color)) %>%
        layout(yaxis = list(title = "Intensity"),
               xaxis = list(title = "2 Theta"),
               legend = list(x = 0.03, y = 0.98, title = list(text='<b> Sample </b>'))
        )
    )
    
  }
  )
}


ui <- navbarPage("Geology Data Analysis",
                 tabPanel("Xrd Summary",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(
                                                    stackedqxrdInput("stacked_qxrd"),
                                                    width = 2
                                                  ),
                                                  mainPanel(
                                                    plotlyOutput(outputId = "stacked_qxrd_graph"),
                                                    width = 10
                                                  )
                          )
                          )
                 )
)

server <- function(input, output, session) {
  stacked_xrd_plot <- stackedqxrdServer("stacked_qxrd", stacked_qxrd_data)
  output$stacked_qxrd_graph <- renderPlotly(
    stacked_xrd_plot()
  )
}

shinyApp(ui, server)

