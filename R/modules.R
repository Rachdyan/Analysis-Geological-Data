library(shiny)
library(dplyr)
library(plotly)
library(scales)
library(glue)
library(shinyWidgets)
library(strex)
library(tidyr)
library(purrr)

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
      choices = c("Sample 2", "Sample 3", "Con 1"), 
      selected = c("Sample 2", "Sample 3", "Con 1"),
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
            "Sample 3"= "#00BFC4",
            "Con 1" = "#fcc564")
        ) + scale_fill_manual(
          values =  c("Sample 2"= "#F8766D",
            "Sample 3"= "#00BFC4",
            "Con 1" = "#fcc564")
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
      "Sample 3" = "#00BFC4",
      "Con 1" = "#fcc564",
      "B2231 SOW#2" = "#bfe3ab")
    
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
    color_palette <- c("Sample 2" = "#F8766D",
      "Sample 3" = "#00BFC4",
      "Con 1" = "#fcc564",
      "B2231 SOW#2" = "#bfe3ab")
    
    plot <- plot_ly(data,
      colors = color_palette,
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
      choices = c("Sample 2", "Sample 3", "Con 1", "B2231 SOW#2"), 
      selected = c("Sample 2", "Sample 3"),
      inline = T
    ),
    selectInput(ns("screen_size"), "Select Screen Size", choices = 
        c("6700", "4750", "3350", "2360", "1700", "1180", "850", "600", 
          "425", "300", "212", "106", "75", "53", "38", "Undersize", "Total", 
          "38x300", "Feed Assay")),
    uiOutput(ns("select_size_type")),
    uiOutput(ns("select_ppm_elements")),
    uiOutput(ns("select_percentage_elements"))
  )
}


graderadarServer <- function(id, assay_size){
  moduleServer(id, function(input,output,session){
    ppm_elements <- c("Au", "Ce", "Gd", "Hf", "La", "Nd", "Rb", "Th", "Y", "Zr")
    percentage_elements <- c("Al", "Ca", "Fe", "Mg", "Si")
    choiceValues_ppm <- c("Au_ppm_grade", "Ce_ppm_grade", "Gd_ppm_grade", "Hf_ppm_grade", 
      "La_ppm_grade", "Nd_ppm_grade", "Rb_ppm_grade", "Th_ppm_grade", "Y_ppm_grade", "Zr_ppm_grade")
    choiceValues_percentage <- c("Al_percentage_grade", "Ca_percentage_grade", "Fe_percentage_grade",
      "Mg_percentage_grade", "Si_percentage_grade")
    
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

## 3rd TAB MODULE

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

## 4th TAB

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
    selectInput(ns("screen"), "Select Screen", choices = c("+6.7mm", "6.7x4.75mm", "4.5x3.35mm", "3.35x2.36mm", "2.36x1.7mm", 
      "1.7x1.18mm", "1.18mmx850um", "850x600um", "600x425um", "425x300um", 
      "300x212um", "212x106um", "106x75um", "75x53um", "53x38um", "-38um"
    )),
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


## 5th Tab Module

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

### 6th Tab Modules

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

#### 7TH TAB


massrecoveryInput <- function(id){
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput(
      inputId = ns("mass_type_input"), 
      label = "Select Type", 
      choices = c("Mags", "Middles", "Non-mags"), 
      selected = c("Mags", "Middles", "Non-mags")
    )
  )
}

massrecoveryServer <- function(id, mass_recovery){
  moduleServer(id, function(input,output,session){
    
    mass_recovery_tooltip <- "<span><b>Run {Run}</b> \n <b>Fractions Mass</b>: {Fraction.Mags} g</span>"
    
    static_plot <- reactive(
      filter(mass_recovery, Type %in% input$mass_type_input) %>%
        ggplot(aes(x = Run, y = Fraction.Mags, color = Type, group = Type, text = glue(mass_recovery_tooltip))) + 
        geom_smooth(size = 1.5, method = "loess", se=FALSE) + 
        geom_point(aes(fill = Type), size = 2.5, color="black",pch=21) +
        scale_y_continuous(labels = ~paste(" ", .x)) +
        scale_x_continuous(breaks = c(1, 2, 3)) +
        labs(x = "Run Number", y = "Fraction Mass (g)") +
        scale_color_manual(
          values =  c(
            "Mags"= "#F8766D",
            "Middles"= "#00BFC4",
            "Non-mags" = "#fcc564")) + 
        scale_fill_manual(
          values =  c(
            "Mags"= "#F8766D",
            "Middles"= "#00BFC4",
            "Non-mags" = "#fcc564")) +
        theme_bw() + 
        theme(legend.position = c(0.1, 0.9), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
        facet_grid(type2 ~ ., scales="free_y")
    )
    
    plotly_plot <- reactive({
      plotly_data <-  ggplotly(static_plot(),  tooltip = c("text")) %>%
        layout(legend = list(title ="", orientation = 'h', bgcolor = 'rgba(255,255,255,0)', font = list(size = 10)),
          margin = list(r = 40)
        )
      for(i in 1:length(plotly_data$x$data)){
        if (!is.null(plotly_data$x$data[[i]]$name)){
          plotly_data$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', plotly_data$x$data[[i]]$name)
        }
      }
      plotly_data
    })
  })
}
