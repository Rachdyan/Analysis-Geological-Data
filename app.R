library(shiny)
library(dplyr)
library(plotly)
library(scales)
library(glue)

source("./R/modules.R")

### FIRST TAB
screen_size <- read.csv("./data/screen-sizing-report.csv")
screen_size <- screen_size %>% arrange(Screen.Size)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

### SECOND TAB
assay_size <- read.csv("./data/assay-by-size.csv")

ppm_elements <- c("Au", "Ce", "Gd", "Hf", "La", "Nd", "Rb", "Th", "Y", "Zr")
percentage_elements <- c("Al", "Ca", "Fe", "Mg", "Si")
choiceValues_ppm <- c("Au_ppm_grade", "Ce_ppm_grade", "Gd_ppm_grade", "Hf_ppm_grade", 
                      "La_ppm_grade", "Nd_ppm_grade", "Rb_ppm_grade", "Th_ppm_grade", "Y_ppm_grade", "Zr_ppm_grade")
choiceValues_percentage <- c("Al_percentage_grade", "Ca_percentage_grade", "Fe_percentage_grade",
                             "Mg_percentage_grade", "Si_percentage_grade")

### APP

ui <- navbarPage("Geology Data Analysis",
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
}

shinyApp(ui, server)
