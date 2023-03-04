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

xrd_sum_data <- read.csv("./data/xrd_summary.csv") %>% .[,-4]

dput(xrd_sum_data$Mineral)

c("Almandine", "Anastase", 
  "Perovskite", "Zircon")

### COBA FACTOR BUAT URUTAN BAR CHART MANUAL
xrd_sum_data$Mineral  <- factor(xrd_sum_data$Mineral, 
                                c("Quartz", "Ilmenite", "Almandine", "Monazite", 
                                   "Rutile", "Anastase", 
                                   "Perovskite", "Zircon","Hematite"))

### BISA buat shiny 
levelz <- xrd_sum_data$Mineral[order(xrd_sum_data[["percentage_wt"]], decreasing = T)] %>% unique()

levelz <- xrd_sum_data$Mineral[order(xrd_sum_data$percentage_wt, decreasing = T)] %>% unique()

xrd_sum_data$Mineral <- factor(xrd_sum_data$Mineral,levels = levelz)
  
summary(xrd_sum_data)


## BISA
plot_ly(xrd_sum_data, x = ~Sample, y = ~percentage_wt, type = 'bar',
        color = ~Mineral, colors = "Accent",
        hovertemplate = paste("<b>%{x}:</b> %{y}%")) %>%
  layout(yaxis = list(title = "Percentage (wt. %)"),
         barmode = 'stack') 


add_annotations(text = ifelse(xrd_sum_data$percentage_wt > 15, paste(xrd_sum_data$Mineral, "\n", xrd_sum_data$percentage_wt), ""), 
                  x = xrd_sum_data$Sample, 
                  y = unlist(tapply(xrd_sum_data$percentage_wt, xrd_sum_data$Sample, FUN=cumsum))-(xrd_sum_data$percentage_wt/3.5), 
                  showarrow = FALSE)

unlist(tapply(xrd_sum_data$percentage_wt, xrd_sum_data$Sample, FUN=cumsum))-(xrd_sum_data$percentage_wt/2)

wes_palette(n=3, name="BottleRocket1")
tes <- wes_palette(9, name = "Darjeeling1", type = "continuous")
summary(tes) %>% as.character()


### COVA LAIN
df <- data.frame("QuarterYear" = c("2019 Q1","2019 Q2","2019 Q2","2019 Q3","2019 Q3","2019 Q3"), "Size" = c("Medium","Large","Medium","Large","Medium","Small"),
                 "percentage" = c(100,29,71,13,74,13))

plot_ly(df, x = df$QuarterYear,
        y = df$percentage,
        type = 'bar',
        name = df$Size,
        text = paste(df$percentage,"%"),
        textposition = 'top',
        hoverinfo = 'text',
        hovertext = paste('Size: ', df$Size,
                          '<br> % of Total count: ', paste(df$percentage,"%")),
        color = df$Size) %>%
  layout(yaxis = list(title = "% of Count", zeroline = FALSE, 
                      showline = FALSE, ticksuffix = "%"), barmode = 'stack',
         hoverlabel = list(bgcolor= 'white')) %>%
  layout(legend = list(orientation = "h",
                       xanchor = "center", 
                       x = 0.5,
                       y = -0.13)) %>% 
  add_annotations(text = paste0(df$percentage, "%"), x = df$QuarterYear, 
                  y = unlist(tapply(df$percentage, df$QuarterYear, FUN=cumsum))-(df$percentage/2), 
                  showarrow = FALSE)

https://stackoverflow.com/questions/74008769/determine-the-stack-order-in-a-stacked-bar-plot-with-r-and-plotly

https://stackoverflow.com/questions/37824958/stacked-bar-graphs-in-plotly-how-to-control-the-order-of-bars-in-each-stack


### COBA SHINY

xrd_sum_data <- read.csv("./data/xrd_summary.csv") %>% .[,-4]
xrd_sum_elements_list <- xrd_sum_data$Mineral %>% unique()

ui <- fluidPage(
  checkboxGroupInput(
    inputId = "sample", 
    label = "Select Sample", 
    choices = c("Con 1", "Con 2"), 
    selected = c("Con 1", "Con 2"),
    inline = T
  ),
  pickerInput(
    inputId = "select_mineral",
    label = "Select Mineral Phases ",
    choices = c("Quartz", "Ilmenite", "Almandine", "Monazite", "Rutile", "Anastase", 
                "Perovskite", "Zircon", "Hematite"),
    multiple = T,
    selected = c("Quartz", "Ilmenite", "Almandine", "Monazite", "Rutile", "Anastase", 
                 "Perovskite", "Zircon", "Hematite"),
    options = list(
      `selected-text-format` = "count > 3",
      `actions-box` = TRUE)),
  tableOutput("tezz"),
  plotlyOutput("xrd_summary_plot")
)

server <- function(input, output, session){
  all_xrd_sum <- reactive(xrd_sum_data %>%
                            filter(Sample %in% input$sample, Mineral %in% input$select_mineral))
  
  last_xrd_sum <- reactive({
    current_data <- all_xrd_sum()
    factor_level <- current_data$Mineral[order(current_data$percentage_wt, decreasing = T)] %>% unique()
    current_data$Mineral <- factor(current_data$Mineral,levels = factor_level)
    current_data
  })
  
  output$tezz <- renderTable(last_xrd_sum())
  
  output$xrd_summary_plot <- renderPlotly(
    plot_ly(last_xrd_sum(), x = ~Sample, y = ~percentage_wt, type = 'bar',
            color = ~Mineral, colors = "Accent",
            hovertemplate = paste("<b>%{x}:</b> %{y}%")) %>%
      layout(yaxis = list(title = "Percentage (wt. %)"),
             barmode = 'stack') 
  )
}

shinyApp(ui, server)

## COBA MODULE

xrdsumInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(
      inputId = ns("sample"), 
      label = "Select Sample", 
      choices = c("Con 1", "Con 2"), 
      selected = c("Con 1", "Con 2"),
      inline = T
    ),
    pickerInput(
      inputId = ns("select_mineral"),
      label = "Select Mineral Phases ",
      choices = c("Quartz", "Ilmenite", "Almandine", "Monazite", "Rutile", "Anastase", 
                  "Perovskite", "Zircon", "Hematite"),
      multiple = T,
      selected = c("Quartz", "Ilmenite", "Almandine", "Monazite", "Rutile", "Anastase", 
                   "Perovskite", "Zircon", "Hematite"),
      options = list(
        `selected-text-format` = "count > 3",
        `actions-box` = TRUE))
    
  )
}

xrdsumServer <- function(id, xrd_sum_data){
  moduleServer(id, function(input,output,session){
    
    all_xrd_sum <- reactive(xrd_sum_data %>%
                              filter(Sample %in% input$sample, Mineral %in% input$select_mineral))
    
    last_xrd_sum <- reactive({
      req(input$select_mineral)
      current_data <- all_xrd_sum()
      factor_level <- current_data$Mineral[order(current_data$percentage_wt, decreasing = T)] %>% unique()
      current_data$Mineral <- factor(current_data$Mineral,levels = factor_level)
      current_data
    })
    
    plot <- reactive(
      plot_ly(last_xrd_sum(), x = ~Sample, y = ~percentage_wt, type = 'bar',
              color = ~Mineral, colors = "Accent",
              hovertemplate = paste("<b>%{x}:</b> %{y}%")) %>%
        layout(yaxis = list(title = "Percentage (wt. %)"),
               barmode = 'stack') 
    )
    
  }
  )
}


ui <- navbarPage("Geology Data Analysis",
                 tabPanel("Xrd Summary",
                          fluidPage(sidebarLayout(position = "left",
                                                  sidebarPanel(
                                                    xrdsumInput("xrd_sum")
                                                  ),
                                                  mainPanel(
                                                    plotlyOutput(outputId = "xrd_sum_graph")
                                                  )
                          )
                          )
                 )
)

server <- function(input, output, session) {
  xrd_sum_plot <- xrdsumServer("xrd_sum", xrd_sum_data)
  output$xrd_sum_graph <- renderPlotly(
    xrd_sum_plot()
  )
}

shinyApp(ui, server)

