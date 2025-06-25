library(shiny)
library(dplyr)
library(plotly)
library(scales)
library(glue)
library(shinyWidgets)
library(strex)
library(tidyr)
library(purrr)
library(bslib)

source("./R/modules.R")

### FIRST TAB
screen_size <- read.csv("./data/screen-sizing-report.csv", sep = ";")
screen_size <- screen_size %>% arrange(Screen.Size)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

### SECOND & THIRD TAB 
assay_size <- read.csv("./data/assay-by-size.csv", sep = ";")
assay_size <- assay_size %>% mutate(across(4:19, ~ .x %>% str_replace_all(",", "\\.") %>% as.double()))

## FOURTH TAB
xrf_data <- read.csv("./data/xrf.csv", sep =";")

## FIFTH TAB
xrd_sum_data <- read.csv("./data/xrd_summary.csv") %>% .[,-4]

## 6th TAB
stacked_qxrd_data <- read.csv("./data/stacked_qxrd.csv")

## 7th Tab
mass_recovery <- read.csv("./data/mass_recovery.csv", sep = ";")
mass_recovery_tooltip <- "<span><b>Run {Run}</b> \n <b>Fractions Mass</b>: {Fraction.Mags} g</span>"

### APP

## bs_theme(version = 4, bootswatch = "sandstone")

ui <- navbarPage("Rock Sample Analysis",
  theme = bs_theme(version = 3, bootswatch = "sandstone"),
  tabPanel("Screen Sizing",
    fluidPage(sidebarLayout(position = "left",
      sidebarPanel(
        screensizeInput("ss")
      ),
      mainPanel(
        plotlyOutput(outputId = "ss_plot")
      )
    )
    )
  ),
  tabPanel("Grade",
    fluidPage(sidebarLayout(position = "left",
      sidebarPanel(
        graderadarInput("grade")
      ),
      mainPanel(
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), plotlyOutput(outputId = "grade_plot"), plotlyOutput(outputId = "grade_plot2"))
        )
      )
    )
    )
  ),
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
  ),
  tabPanel("XRF",
    fluidPage(sidebarLayout(position = "left",
      sidebarPanel(
        xrfInput("xrf")
      ),
      mainPanel(
        plotlyOutput(outputId = "xrf_plot")
      )
    )
    )
  ),
  tabPanel("XRD Summary",
    fluidPage(sidebarLayout(position = "left",
      sidebarPanel(
        xrdsumInput("xrd_sum")
      ),
      mainPanel(
        plotlyOutput(outputId = "xrd_sum_graph")
      )
    )
    )
  ),
  tabPanel("Stacked QXRD",
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
  ),
  tabPanel("Mass Recovery",
    fluidPage(sidebarLayout(position = "left",
      sidebarPanel(
        h5("Sample B2231 SOW#2"),
        massrecoveryInput("mr"),
      ),
      mainPanel(
        plotlyOutput(outputId = "mr_plot"),
      )
    )
    )
  )
  
  
)


server <- function(input, output, session) {
  ss_data <- screensizeServer("ss", screen_size)
  output$ss_plot <- renderPlotly(
    ss_data()
  )
  
  grade_data <- graderadarServer("grade", assay_size)
  output$grade_plot <- renderPlotly(
    grade_data$ppm_radar_chart()
  )
  output$grade_plot2 <- renderPlotly(
    grade_data$percentage_radar_chart()
  )
  
  recovery_data <- recoveryServer("recovery", assay_size)
  output$recovery_plot <- renderPlotly(
    recovery_data()
  )
  
  xrf_graph <- xrfServer("xrf", xrf_data)
  output$xrf_plot <- renderPlotly(
    xrf_graph()
  )
  
  xrd_sum_plot <- xrdsumServer("xrd_sum", xrd_sum_data)
  output$xrd_sum_graph <- renderPlotly(
    xrd_sum_plot()
  )
  
  stacked_xrd_plot <- stackedqxrdServer("stacked_qxrd", stacked_qxrd_data)
  output$stacked_qxrd_graph <- renderPlotly(
    stacked_xrd_plot()
  )
  
  mr_plot <- massrecoveryServer("mr", mass_recovery)
  output$mr_plot <- renderPlotly(
    mr_plot()
  )
  
  
}

shinyApp(ui, server)
