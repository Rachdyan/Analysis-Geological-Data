library(plotly)
library(dplyr)
library(strex)
library(glue)
library(shinyWidgets)

assay_size <- read.csv("./data/assay-by-size.csv", sep =";")

sample <- assay_size %>% filter(Screen == "6700", Type == "Exact Size") %>% select(c(1:3), matches("_recovery"))

colnames(sample)[4:ncol(sample)] <- names(sample[4:ncol(sample)]) %>% str_before_first("_")

sample_long <- pivot_longer(sample, 4:length(sample))



## COBA ISI COLOR
colors = c("#F8766D", "#00BFC4")

sample_long$color <- factor(sample_long$Sample, labels = colors)

sample_long$color <- factor(sample_long$Sample, labels = switch(sample_long$Sample,
                                                                 "Sample 2" = "#F8766D",
                                                                 "Sample 3" = "#00BFC4"))

sample_long %>% mutate(color = case_when(Sample == "Sample 2" ~ "#F8766D",
                                         Sample == "Sample 3" ~ "#00BFC4"))


plot_ly(sample_long,
        x = ~name,
        y = ~value,
        type = "bar",
        text = glue("{sample_long$value}%"),
        textangle = 0,
        textfont = list(color = "#FFF"),
        hovertemplate = paste("<b>%{x} Recovery:</b> %{y}"),
        split = ~Sample,
        marker = list(color = sample_long$color,
          line = list(color = 'rgb(8,48,107)', 
                             width = 1.5))) %>% 
  layout(title = "Element Recovery (%)",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = "",
                      ticksuffix="%",
                      range = list(0,100)),
         font=list(size=11),
         hoverlabel = list(font = list(color = "white")),
         uniformtext=list(minsize=6, mode='show')
  )

### COBA SHINY

ui <- fluidPage(
  checkboxGroupInput(
    inputId = "sample", 
    label = "Select Sample", 
    choices = c("Sample 2", "Sample 3"), 
    selected = c("Sample 2", "Sample 3"),
    inline = T
  ),
  selectInput("screen_size", "Select Screen Size", choices = unique(assay_size$Screen)),
  uiOutput("recovery_size_type"),
  uiOutput("recovery_elements"),
  tableOutput("test"),
  plotlyOutput("recovery_chart"),
)

server <- function(input, output, session){
  output$recovery_size_type <- renderUI(
    radioButtons("recovery_size", "Select Size Type", 
                 choices = unique(assay_size$Type[assay_size$Screen == input$screen_size]))
  )
  
  all_recovery <- reactive(assay_size %>% 
                        filter(Sample %in% input$sample, Screen == input$screen_size, Type %in% input$recovery_size) %>%
                        select(c(1:3), matches("_recovery")))
  
  percent_elements <- c("Al", "Au", "Ca", "Ce", "Fe", "Gd", 
                        "Hf", "La", "Mg", "Nd", "Rb", "Si", "Th", "Y", "Zr")
  
  range_choices_recovery <- reactive({
    req(input$recovery_size)
    temp_percent <- all_recovery()[, -c(1:3)]
    range_percent <- sapply(temp_percent, range)
    percent_list <- c()
    for(i in 1:length(percent_elements)){
      temp_list <- glue("{range_percent[1,i]}-{range_percent[2,i]}")
      percent_list <- c(percent_list, temp_list)
    }
    percent_list
  })
  
  percentValues <- c("Al_percentage_recovery", "Au_percentage_recovery", "Ca_percentage_recovery", 
                     "Ce_percentage_recovery", "Fe_percentage_recovery", "Gd_percentage_recovery", 
                     "Hf_percentage_recovery", "La_percentage_recovery", "Mg_percentage_recovery", 
                     "Nd_percentage_recovery", "Rb_percentage_recovery", "Si_percentage_recovery", 
                     "Th_percentage_recovery", "Y_percentage_recovery", "Zr_percentage_recovery"
  )
  
  output$recovery_elements <- renderUI(
    pickerInput(
      inputId = "select_percent",
      label = "Select Elements ",
      choices = setNames(percentValues, percent_elements),
      multiple = T,
      selected = percentValues,
      options = list(
        `selected-text-format` = "count > 3",
        `actions-box` = TRUE),
      choicesOpt = list(
        subtext = paste("range:", range_choices_recovery())
      )
    )
  )
  
  last_data_percent <-  reactive({
    req(input$select_percent)
    temp <- all_recovery() %>% select(c(1:3) ,all_of(input$select_percent))
    colnames(temp)[4:ncol(temp)] <- names(temp[4:ncol(temp)]) %>% str_before_first("_")
    pivot_longer(temp, 4:length(temp)) %>% 
      mutate(color = case_when(Sample == "Sample 2" ~ "#F8766D",
                               Sample == "Sample 3" ~ "#00BFC4"))
  })
  
  output$recovery_chart <- renderPlotly(
    plot_ly(last_data_percent(),
            x = ~name,
            y = ~value,
            type = "bar",
            text = glue("{last_data_percent()$value}%"),
            textangle = 0,
            textfont = list(color = "#FFF"),
            hovertemplate = paste("<b>%{x} Recovery:</b> %{y}"),
            split = ~Sample,
            marker = list(color = last_data_percent()$color,
                          line = list(color = 'rgb(8,48,107)', 
                                      width = 1.5))) %>% 
      layout(title = "Element Recovery (%)",
             barmode = 'group',
             xaxis = list(title = ""),
             yaxis = list(title = "",
                          ticksuffix="%",
                          range = list(0,100)),
             font=list(size=11),
             hoverlabel = list(font = list(color = "white")),
             uniformtext=list(minsize=6, mode='show')
      )
  )
  
  }

shinyApp(ui, server)

#### BIKIN MODULE

recoveryInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(
      inputId = ns("sample"), 
      label = "Select Sample", 
      choices = c("Sample 2", "Sample 3"), 
      selected = c("Sample 2", "Sample 3"),
      inline = T
    ),
    selectInput(ns("screen_size"), "Select Screen Size", choices = unique(assay_size$Screen)),
    uiOutput(ns("select_size_type")),
    uiOutput(ns("select_recovery_elements"))
  )
}

recoveryServer <- function(id, assay_size){
  moduleServer(id, function(input,output,session){
    recovery_elements <- c("Al", "Au", "Ca", "Ce", "Fe", "Gd", 
                          "Hf", "La", "Mg", "Nd", "Rb", "Si", "Th", "Y", "Zr")
    
    recoveryValues <- c("Al_percentage_recovery", "Au_percentage_recovery", "Ca_percentage_recovery", 
                       "Ce_percentage_recovery", "Fe_percentage_recovery", "Gd_percentage_recovery", 
                       "Hf_percentage_recovery", "La_percentage_recovery", "Mg_percentage_recovery", 
                       "Nd_percentage_recovery", "Rb_percentage_recovery", "Si_percentage_recovery", 
                       "Th_percentage_recovery", "Y_percentage_recovery", "Zr_percentage_recovery"
    )
    
    output$select_size_type <- renderUI(
      radioButtons(session$ns("select_size"), "Select Size Type", 
                   choices = unique(assay_size$Type[assay_size$Screen == input$screen_size]))
    )
    
    all_recovery <- reactive(assay_size %>% 
                               filter(Sample %in% input$sample, Screen == input$screen_size, Type %in% input$select_size) %>%
                               select(c(1:3), matches("_recovery")))
    
    range_choices_recovery <- reactive({
      req(input$select_size)
      temp_percent <- all_recovery()[, -c(1:3)]
      range_percent <- sapply(temp_percent, range)
      percent_list <- c()
      for(i in 1:length(recovery_elements)){
        temp_list <- glue("{range_percent[1,i]}-{range_percent[2,i]}")
        percent_list <- c(percent_list, temp_list)
      }
      percent_list
    })
    
    output$select_recovery_elements <- renderUI(
      pickerInput(
        inputId = session$ns("select_elements"),
        label = "Select Elements ",
        choices = setNames(recoveryValues, recovery_elements),
        multiple = T,
        selected = recoveryValues,
        options = list(
          `selected-text-format` = "count > 3",
          `actions-box` = TRUE),
        choicesOpt = list(
          subtext = paste("range:", range_choices_recovery())
        )
      )
    )
    
    last_data_percent <-  reactive({
      req(input$select_elements)
      temp <- all_recovery() %>% select(c(1:3) ,all_of(input$select_elements))
      colnames(temp)[4:ncol(temp)] <- names(temp[4:ncol(temp)]) %>% str_before_first("_")
      pivot_longer(temp, 4:length(temp)) %>% 
        mutate(color = case_when(Sample == "Sample 2" ~ "#F8766D",
                                 Sample == "Sample 3" ~ "#00BFC4"))
    })
    
      plot <- reactive(plot_ly(last_data_percent(),
                  x = ~name,
                  y = ~value,
                  type = "bar",
                  text = glue("{last_data_percent()$value}%"),
                  textangle = 0,
                  textfont = list(color = "#FFF"),
                  hovertemplate = paste("<b>%{x} Recovery:</b> %{y}"),
                  split = ~Sample,
                  marker = list(color = last_data_percent()$color,
                                line = list(color = 'rgb(8,48,107)', 
                                            width = 1.5))) %>% 
            layout(title = "Element Recovery (%)",
                   barmode = 'group',
                   xaxis = list(title = ""),
                   yaxis = list(title = "",
                                ticksuffix="%",
                                range = list(0,100)),
                   font=list(size=11),
                   hoverlabel = list(font = list(color = "white"))
            )
      )
  }
)
}


ui <- navbarPage("Geology Data Analysis",
                 tabPanel("Recovery",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(
                                                    recoveryInput("recovery")
                                                  ),
                                                  mainPanel(
                                                    plotlyOutput(outputId = "recovery_plot")
                                                  )
                          )
                          )
                 )
)

server <- function(input, output, session) {
  recovery_data <- recoveryServer("recovery", assay_size)
  output$recovery_plot <- renderPlotly(
    recovery_data()
  )
}

shinyApp(ui, server)
