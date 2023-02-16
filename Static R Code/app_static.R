library(shiny)
library(dplyr)
library(plotly)
library(scales)
library(glue)


ui <- fluidPage(
  checkboxGroupInput(
    inputId = "sample", 
    label = "Select Sample", 
    choices = unique(screen_size$Sample), 
    selected = c("Sample 2", "Sample 3"),
  ),
  radioButtons('yaxis', "Select the Y-axis", 
               c("% Cum Passing" = "Percentage.Cum.Passing", "% Cum Retained" = "Percentage.Cum.Retained")),
  plotlyOutput(outputId = "screen_sizing"),
  textOutput("radioresult"),
)

server <- function(input, output, session) {
  
  tooltip <- reactive({
    if(input$yaxis == "Percentage.Cum.Passing"){
      "<span style='color:white'><b>Cum Passing:</b> {Percentage.Cum.Passing}%\n <b>Screen Size</b>: {Screen.Size} micron\n <b>Mass Restained:</b> {Mass.Restained} g</span>"
    } else {
      "<span style='color:white'><b>Cum Retained:</b> {Percentage.Cum.Retained}%\n <b>Screen Size</b>: {Screen.Size} micron\n <b>Mass Restained:</b> {Mass.Restained} g</span>"
    }
  })
  
  ylabel <- reactive({
    if(input$yaxis == "Percentage.Cum.Passing"){
      "% Cum Passing"
    } else {
      "% Cum Restained"
    }
  })
  
  output$screen_sizing <- renderPlotly({
    
    req(input$sample)
    if (identical(input$cities, "")) return(NULL)
    
    screen_size_plot <-  ggplot(data = filter(screen_size, Sample %in% input$sample),
                                aes_string(x = "Screen.Size", y = input$yaxis, 
                                           color = "Sample", group = "Sample") + aes(text = glue(tooltip()))) + 
      geom_line(size = 1.5) + geom_point(aes(fill = Sample), size = 2.5, color="black",pch=21)+
      scale_y_continuous(limits = c(0,100), label = label_number(suffix = "%")) +
      scale_x_log10(breaks = 10^(0:4), limits = c(1, 10000), minor_breaks = minor_breaks) +
      labs(x = "Screen Size (microns)", y = ylabel()) + scale_color_manual(
        values =  c("Sample 2"= "#F8766D",
                    "Sample 3"= "#00BFC4")
      ) + scale_fill_manual(
        values =  c("Sample 2"= "#F8766D",
                    "Sample 3"= "#00BFC4")
      ) +
      theme_bw()
    
    ggplotly(screen_size_plot, tooltip = c("text")) %>% fix_legend_plotly()  %>% 
      layout(legend = list(x = 0.05, y = 0.95, title = list(text='<b> Sample </b>')))
  })
}

shinyApp(ui, server)
