library(shiny)
library(dplyr)
library(plotly)
library(scales)
library(glue)

######## 1st TAB FUNCTION AND MODULE

#### To sum ggplot aes
`+.uneval` <- function(a,b) {
  `class<-`(modifyList(a,b), "uneval")
}

### Fix Plotly legend for 1st tab plot
fix_legend_plotly <- function(plotly_data){
  # Get the names of the legend entries
  df <- data.frame(id = seq_along(plotly_data$x$data), legend_entries = unlist(lapply(plotly_data$x$data, `[[`, "name")))
  # Extract the group identifier
  df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
  # Add an indicator for the first entry per group
  df$is_first <- !duplicated(df$legend_group)
  
  for (i in df$id) {
    # Is the layer the first entry of the group?
    is_first <- df$is_first[[i]]
    # Assign the group identifier to the name and legendgroup arguments
    plotly_data$x$data[[i]]$name <- df$legend_group[[i]]
    plotly_data$x$data[[i]]$legendgroup <- plotly_data$x$data[[i]]$name
    # Show the legend only for the first layer of the group 
    if (!is_first) plotly_data$x$data[[i]]$showlegend <- T
  }
  return(plotly_data)
}

### 1st tab Module
screensizeInput <- function(id){
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(
      inputId = ns("sample"), 
      label = "Select Sample", 
      choices = c("Sample 2", "Sample 3"), 
      selected = c("Sample 2", "Sample 3"),
    ),
    radioButtons(ns('yaxis'), "Select the Y-axis", 
                 c("% Cum Passing" = "Percentage.Cum.Passing", "% Cum Retained" = "Percentage.Cum.Retained"))
  )
}

screensizeServer <- function(id, screen_size){
  moduleServer(id, function(input,output,session){
    
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
    
    static_plot <- reactive(ggplot(data = filter(screen_size, Sample %in% input$sample),
                                   aes_string(x = "Screen.Size", y = input$yaxis, 
                                              color = "Sample", group = "Sample") + aes(text = glue(tooltip()))) + 
                              geom_line(linewidth = 1.5) + geom_point(aes(fill = Sample), size = 2.5, color="black",pch=21)+
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
    )
    
    plotly_plot <- reactive(ggplotly(static_plot(), tooltip = c("text")) %>% fix_legend_plotly()  %>% 
                              layout(legend = list(x = 0.05, y = 0.95, title = list(text='<b> Sample </b>')))
    )
  })
}

######## 2nd TAB FUNCTION AND MODULE

### Create Custom RAdar Plot With Closed Line and No Fill
radar_custom <- function(data = data.frame(), r, theta, split, name, color,  ...){
  detect_diff <- c(FALSE, data$Sample[-1] == data$Sample[-length(data$Sample)])
  diff_index <- which(detect_diff == FALSE) %>% .[-1]
  print(diff_index)
  if(is_empty(diff_index)){
    check_sample <- unique(data$Sample)
    print(check_sample)
    
    color_palette <- switch (check_sample,
                             "Sample 2" = "#F8766D",
                             "Sample 3" = "#00BFC4")
    
    print(color_palette)
    
    plot <- plot_ly(data,
                    colors = color_palette,
                    type = 'scatterpolar',
                    mode = "markers",
                    fill = 'toself',
                    fillcolor=rgb(0, 0, 0, 0, maxColorValue = 255),
                    r = c(r, r[1]),
                    theta = c(theta, theta[1]),
                    split = c(split, split[1]),
                    name = c(name, name[1]),
                    color = c(color, color[1]),
                    line = list(color = c(color, color[1])),
                    ...)
    return(plot)
  } else{
    plot <- plot_ly(data,
                    colors = c("#F8766D", "#00BFC4"),
                    type = 'scatterpolar',
                    mode = "markers",
                    fill = 'toself',
                    fillcolor=rgb(0, 0, 0, 0, maxColorValue = 255),
                    r = c(r, r[1], r[diff_index]),
                    theta = c(theta, theta[1], theta[diff_index]),
                    split = c(split, split[1], split[diff_index]),
                    name = c(name, name[1], name[diff_index]),
                    color = c(color, color[1], color[diff_index]),
                    line = list(color = c(color, color[1], color[diff_index])),
                    ...)
    return(plot)
  }
}


### 2nd Tab Module

graderadarInput <- function(id) {
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
    uiOutput(ns("select_ppm_elements")),
    uiOutput(ns("select_percentage_elements"))
  )
}

graderadarServer <- function(id, assay_size){
  moduleServer(id, function(input,output,session){
    
    output$select_size_type <- renderUI(
      radioButtons(session$ns("select_size"), "Select Size Type", 
                   choices = unique(assay_size$Type[assay_size$Screen == input$screen_size]))
    )
    
    all_ppm <- reactive(assay_size %>% 
                          filter(Sample %in% input$sample, Screen == input$screen_size, Type %in% input$select_size) %>%
                          select(c(1:3), matches("ppm_grade")))
    
    all_percentage <- reactive(assay_size %>% 
                                 filter(Sample %in% input$sample, Screen == input$screen_size, Type %in% input$select_size) %>%
                                 select(c(1:3), matches("percentage_grade")))
    
    range_choices_ppm <- reactive({
      req(input$select_size)
      temp_ppm <- all_ppm()[, -c(1:3)]
      range_ppm <- sapply(temp_ppm, range)
      ppm_list <- c()
      for(i in 1:length(ppm_elements)){
        temp_list <- glue("{range_ppm[1,i]}-{range_ppm[2,i]}")
        ppm_list <- c(ppm_list, temp_list)
      }
      ppm_list
    })
    
    range_choices_percentage <- reactive({
      req(input$select_size)
      temp_percentage <- all_percentage()[, -c(1:3)]
      range_percentage <- sapply(temp_percentage, range)
      percentage_list <- c()
      for(i in 1:length(percentage_elements)){
        temp_list <- glue("{range_percentage[1,i]}-{range_percentage[2,i]}")
        percentage_list <- c(percentage_list, temp_list)
      }
      percentage_list
    })
    
    output$select_ppm_elements <- renderUI(
      pickerInput(
        inputId = session$ns("select_ppm"),
        label = "Select Grade (ppm) Data to show",
        choices = setNames(choiceValues_ppm, ppm_elements),
        selected = c("Ce_ppm_grade", "La_ppm_grade", "Nd_ppm_grade", "Rb_ppm_grade", "Th_ppm_grade"),
        multiple = T,
        options = list(
          `selected-text-format` = "count > 3",
          `actions-box` = TRUE),
        choicesOpt = list(
          subtext = paste("range:", range_choices_ppm())
        )
      )
    )
    
    output$select_percentage_elements <- renderUI(
      pickerInput(
        inputId = session$ns("select_percentage"),
        label = "Select Grade (%) Data",
        choices = setNames(choiceValues_percentage, percentage_elements),
        selected = c("Al_percentage_grade", "Ca_percentage_grade", "Fe_percentage_grade",
                     "Mg_percentage_grade"),
        multiple = T,
        options = list(
          `selected-text-format` = "count > 3",
          `actions-box` = TRUE),
        choicesOpt = list(
          subtext = paste("range:", range_choices_percentage())
        )
      )
    )
    
    last_data_ppm <-  reactive({
      req(input$select_ppm)
      temp <- all_ppm() %>% select(c(1:3) ,all_of(input$select_ppm))
      colnames(temp)[4:ncol(temp)] <- names(temp[4:ncol(temp)]) %>% str_before_first("_")
      pivot_longer(temp, 4:length(temp))
    })
    
    last_data_percentage <-  reactive({
      req(input$select_percentage)
      temp <- all_percentage() %>% select(c(1:3) ,all_of(input$select_percentage))
      colnames(temp)[4:ncol(temp)] <- names(temp[4:ncol(temp)]) %>% str_before_first("_")
      pivot_longer(temp, 4:length(temp))
    })
    
    
    list(ppm_radar_chart = reactive(
      radar_custom(last_data_ppm(),
                   r = last_data_ppm()$value,
                   theta = last_data_ppm()$name,
                   split = last_data_ppm()$Sample,
                   name = last_data_ppm()$Sample,
                   color = last_data_ppm()$Sample,
                   fill = "toself",
                   hoveron = 'points',
                   hovertemplate = "%{theta}: %{r} ppm"
      ) %>%
        layout(
          title = list(
            text = 'Assay Grade (ppm) by Size'
          ),
          margin = list(
            t = 75
          ),
          polar = list(
            radialaxis = list(
              angle = 90,
              visible = T,
              showline = T,
              color = '#bfbfbf',
              nticks = 5,
              tickangle = 90
            )
          )
        )
    ),
    percentage_radar_chart = reactive(
      radar_custom(last_data_percentage(),
                   r = last_data_percentage()$value,
                   theta = last_data_percentage()$name,
                   split = last_data_percentage()$Sample,
                   name = last_data_percentage()$Sample,
                   color = last_data_percentage()$Sample,
                   fill = "toself",
                   hoveron = 'points',
                   hovertemplate = "%{theta}: %{r}"
      ) %>%
        layout(
          title = list(
            text = 'Assay Grade (%) by Size'
          ),
          margin = list(
            t = 75
          ),
          polar = list(
            radialaxis = list(
              angle = 90,
              visible = T,
              showline = T,
              color = '#bfbfbf',
              nticks = 5,
              tickangle = 90,
              ticksuffix = '%'
            )
          )
        )
    )
    )
  })
}

