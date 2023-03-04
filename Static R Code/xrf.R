
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


xrf_data <- read.csv("./data/xrf.csv", sep =";")

sample <- xrf_data %>% filter(Screen == "4.5x3.35mm")

sample_long <- pivot_longer(sample, 4:length(sample)) %>% filter(name != "SiO2") %>%
  mutate(color = case_when(Sample == "Sample 2" ~ "#F8766D",
                           Sample == "Sample 3" ~ "#00BFC4"))

plot_ly(sample_long, x = ~value, y = ~name, split  = ~Sample, type = 'scatter',
        mode = "markers", marker = list(color = sample_long$color), 
        hovertemplate = paste("<b>%{y}:</b> %{x}")) %>% 
  layout(yaxis = list(title = "", categoryorder = "total ascending"),
         xaxis = list(title = "Percentage", ticksuffix="%"))


### COBA SHINY

xrf_elements <- c("Al2O3", "BaO", "CaO", "Cr2O3", "Fe2O3", "K2O", "MgO", "MnO", 
                  "Na2O", "P2O5", "SO3", "SiO2", "SrO", "TiO2")

ui <- fluidPage(
  checkboxGroupInput(
    inputId = "sample", 
    label = "Select Sample", 
    choices = c("Sample 2", "Sample 3"), 
    selected = c("Sample 2", "Sample 3"),
    inline = T
  ),
  selectInput("screen", "Select Screen", choices = unique(xrf_data$Screen)),
  uiOutput("xrf_elements"),
  tableOutput("test"),
  plotlyOutput("xrf_chart"),
)

server <- function(input, output, session){
  xrf_elements_list <- c("Al2O3", "BaO", "CaO", "Cr2O3", "Fe2O3", "K2O", "MgO", "MnO", 
                         "Na2O", "P2O5", "SO3", "SiO2", "SrO", "TiO2")
  
  all_xrf <- reactive(xrf_data %>%
                        filter(Sample %in% input$sample, Screen == input$screen)
  )
  
  range_choices_xrf <- reactive({
    req(input$screen)
    temp_percent <- all_xrf()[, -c(1:3)]
    range_percent <- sapply(temp_percent, range)
    percent_list <- c()
    for(i in 1:length(xrf_elements_list)){
      temp_list <- glue("{range_percent[1,i]}-{range_percent[2,i]}")
      percent_list <- c(percent_list, temp_list)
    }
    percent_list
  })
  

  selected_elements <- c("Al2O3", "CaO", "MgO", "Na2O", "K2O", "Fe2O3")
  
  output$xrf_elements <- renderUI(
    pickerInput(
      inputId = "select_element",
      label = "Select Elements ",
      choices = xrf_elements_list,
      multiple = T,
      selected = selected_elements,
      options = list(
        `selected-text-format` = "count > 3",
        `actions-box` = TRUE),
      choicesOpt = list(
        subtext = paste("range:", range_choices_xrf())
      )
    )
  )
  
  last_data_xrf <-  reactive({
    req(input$select_element)
    temp <- all_xrf() %>% select(c(1:3) ,all_of(input$select_element))
    pivot_longer(temp, 4:length(temp)) %>% 
      mutate(color = case_when(Sample == "Sample 2" ~ "#F8766D",
                               Sample == "Sample 3" ~ "#00BFC4"))
  }
  )
  
  output$test <- renderTable(last_data_xrf())
  
  output$xrf_chart <- renderPlotly(
    plot_ly(last_data_xrf(), x = ~value, y = ~name, split  = ~Sample, type = 'scatter',
            mode = "markers", marker = list(color = last_data_xrf()$color), 
            hovertemplate = paste("<b>%{y}:</b> %{x}")) %>% 
      layout(yaxis = list(title = "", categoryorder = "total ascending"),
             xaxis = list(title = "Percentage", ticksuffix="%"))
  )
  
}

shinyApp(ui, server)

## BIKIN MODULE

xrfInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(
      inputId = ns("sample"), 
      label = "Select Sample", 
      choices = c("Sample 2", "Sample 3"), 
      selected = c("Sample 2", "Sample 3"),
      inline = T
    ),
    selectInput(ns("screen"), "Select Screen", choices = unique(xrf_data$Screen)),
    uiOutput(ns("xrf_elements"))
  )
}

xrfServer <- function(id, xrf_data){
  moduleServer(id, function(input,output,session){
    xrf_elements_list <- c("Al2O3", "BaO", "CaO", "Cr2O3", "Fe2O3", "K2O", "MgO", "MnO", 
                           "Na2O", "P2O5", "SO3", "SiO2", "SrO", "TiO2")
    
    all_xrf <- reactive(xrf_data %>%
                          filter(Sample %in% input$sample, Screen == input$screen)
    )
    
    range_choices_xrf <- reactive({
      req(input$screen)
      temp_percent <- all_xrf()[, -c(1:3)]
      range_percent <- sapply(temp_percent, range)
      percent_list <- c()
      for(i in 1:length(xrf_elements_list)){
        temp_list <- glue("{range_percent[1,i]}-{range_percent[2,i]}")
        percent_list <- c(percent_list, temp_list)
      }
      percent_list
    })
    
    
    selected_elements <- c("Al2O3", "CaO", "MgO", "Na2O", "K2O", "Fe2O3")
    
    output$xrf_elements <- renderUI(
      pickerInput(
        inputId = session$ns("select_element"),
        label = "Select Elements ",
        choices = xrf_elements_list,
        multiple = T,
        selected = selected_elements,
        options = list(
          `selected-text-format` = "count > 3",
          `actions-box` = TRUE),
        choicesOpt = list(
          subtext = paste("range:", range_choices_xrf())
        )
      )
    )
    
    last_data_xrf <-  reactive({
      req(input$select_element)
      temp <- all_xrf() %>% select(c(1:3) ,all_of(input$select_element))
      pivot_longer(temp, 4:length(temp)) %>% 
        mutate(color = case_when(Sample == "Sample 2" ~ "#F8766D",
                                 Sample == "Sample 3" ~ "#00BFC4"))
    }
    )
    
    plot <- reactive(plot_ly(last_data_xrf(), x = ~value, y = ~name, split  = ~Sample, type = 'scatter',
                                 mode = "markers", marker = list(color = last_data_xrf()$color), 
                                 hovertemplate = paste("<b>%{y}:</b> %{x}")) %>% 
                           layout(yaxis = list(title = "", categoryorder = "total ascending"),
                                  xaxis = list(title = "Percentage", ticksuffix="%"))
                    
    )
  }
  )
}

## COBA MOUDLE


ui <- navbarPage("Geology Data Analysis",
                 tabPanel("Recovery",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(
                                                    xrfInput("xrf")
                                                  ),
                                                  mainPanel(
                                                    plotlyOutput(outputId = "xrf_graph")
                                                  )
                          )
                          )
                 )
)

server <- function(input, output, session) {
  xrf_plot <- xrfServer("xrf", xrf_data)
  output$xrf_graph <- renderPlotly(
    xrf_plot()
  )
}

shinyApp(ui, server)
