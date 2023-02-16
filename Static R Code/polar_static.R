
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

assay_size <- read.csv("./data/assay-by-size.csv")
t(sapply(assay_size[,-c(1,2)], range))

sample <- assay_size %>% filter(Screen == "6700", Type == "Exact Size") %>% select(c(1:3), matches("_grade"))

t(sapply(sample, range))


sample$Al

filter1 <- sample %>% filter(Sample == "Sample 2")
value1 <- c(filter1$Al_percentage_grade, filter1$Ca_percentage_grade, filter1$Fe_percentage_grade, filter1$Mg_percentage_grade)

filter2 <- sample %>% filter(Sample == "Sample 3")
value2 <- c(filter2$Al_percentage_grade, filter2$Ca_percentage_grade, filter2$Fe_percentage_grade, filter2$Mg_percentage_grade)

#### Radar Multiple

my_colors <- c("#F8766D","#00BFC4")

### RADAR FILLED
plot_ly(
  type = 'scatterpolar',
  mode = "markers",
  fill = 'toself',
  marker = list(colorscale="Greys")
)  %>%
  add_trace(
    r = c(value, value[1]),
    theta = c('Al','Ca','Fe', 'Mg', "Al"),
    fillcolor=rgb(0, 0, 0, 0, maxColorValue = 255),
    marker=list(color = "#F8766D"),
    line = list(color = "#F8766D"),
    name = "Sample 2"
  ) %>%
  add_trace(
    r = value2,
    theta = c('Al','Ca','Fe', 'Mg'),
    fillcolor=rgb(0, 191, 196, 100, maxColorValue = 255),
    marker=list(color=rgb(0, 191, 196, maxColorValue = 255)),
    name = "Sample 3"
  )

### FIXX TANPA FILL CUMA LINE AJA

plot_ly(
  type = 'scatterpolar',
  mode = "markers",
  fill = 'toself',
)  %>%
  add_transparant_trace(
    r = value,
    theta =  c('Al','Ca','Fe', 'Mg'),
    color = "#F8766D",
    name = "Sample 2"
  )  %>%
  add_transparant_trace(
    r = value2,
    theta =  c('Al','Ca','Fe', 'Mg'),
    color = "#00BFC4",
    name = "Sample 3"
  )  %>%
  layout(
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


### Line

plot_ly(
  type = 'scatterpolar',
  mode = "markers"
)  %>% 
  add_closed_trace(
    r = value,
    mode = "lines",
    theta = c('Al','Ca','Fe', 'Mg'),
    name = "a"
  ) %>%   add_trace(
    r = value,
    theta = c('Al','Ca','Fe', 'Mg'),
    mode = "markers",
    name = "a"
  )

plot_ly(
    type = 'scatterpolar',
    r = c(39, 28, 8, 7, 28, 39),
    theta = c('A','B','C', 'D', 'E', 'A')
)


add_closed_trace <- function(p, r, theta, ...) 
{
  plotly::add_trace(p, r = c(r, r[1]), theta = c(theta, theta[1]), ...)
}
  
add_transparant_trace <- function(p, r, theta, color, ...) 
{
  plotly::add_trace(p, r = c(r, r[1]), theta = c(theta, theta[1]),
                    line = list(color = color), 
                    marker=list(color = color),
                    fillcolor=rgb(0, 0, 0, 0, maxColorValue = 255),
                    ...)
}

my_colors <- c("#F8766D","#00BFC4")


#### BIKIN FUNCTION RADAR PLOT CUSTOM

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

radar_custom(last_data2_long,
             r = last_data2_long$value,
             theta = last_data2_long$name,
             split = last_data2_long$Sample,
             name = last_data2_long$Sample,
             color = last_data2_long$Sample,
             fill = "toself",
             hoveron = 'points',
             hovertemplate = "%{theta}: %{r} ppm"
)

### 1 Sampel plotly
plot_ly(last_data2_long,
        colors = c("#F8766D"),
        type = 'scatterpolar',
        mode = "markers",
        fill = 'toself',
        fillcolor=rgb(0, 0, 0, 0, maxColorValue = 255),
        r = c(last_data2_long$value, last_data2_long$value[1]),
        theta = c(last_data2_long$name, last_data2_long$name[1]),
        split = c(last_data2_long$Sample, last_data2_long$Sample[1]),
        name = c(last_data2_long$Sample, last_data2_long$Sample[1]),
        color = c(last_data2_long$Sample, last_data2_long$Sample[1]),
        line = list(color = c(last_data2_long$Sample, last_data2_long$Sample[1])),
        showlegend=T
       )


 detect_diff <- c(FALSE, last_data_long$Sample[-1] == last_data_long$Sample[-length(last_data_long$Sample)])
 which(detect_diff == FALSE) %>% .[-1]
 
 detect_diff <- c(FALSE, last_data2_long$Sample[-1] == last_data2_long$Sample[-length(last_data2_long$Sample)])
 diff_index <- which(detect_diff == FALSE) %>% .[-1]
 
 is_empty(diff_index)
# 
# diff(split)
# 
# c(split, unique(split))

### COBA MANUAL SEBELUM SHINY
unique(assay_size$Screen)
ppm_elements <- c("Au", "Ce", "Gd", "Hf", "La", "Nd", "Rb", "Th", "Y", "Zr")
all_ppm <- assay_size %>% filter(Screen == "6700", Type == "Exact Size") %>% select(c(1:3), matches("ppm_grade"))

# Coba 1 sampel
all_ppm2 <- assay_size %>% filter(Sample == "Sample 2", Screen == "6700", Type == "Exact Size") %>% select(c(1:3), matches("ppm_grade")) 

## HITUNG RANGE ELEMENt
range_ppm <- sapply(all_ppm, range)

ppm_list <- c()
for(i in 1:length(ppm_elements)){
  temp_list <- glue("{ppm_elements[i]} ({range_ppm[1,i]}-{range_ppm[2,i]})")
  ppm_list <- c(ppm_list, temp_list)
}

## PILIH ELEMENT
name_selected <- c("Ce_ppm_grade", "La_ppm_grade", "Nd_ppm_grade", "Rb_ppm_grade", "Th_ppm_grade")

## FILTER ELEMENT
last_data <- all_ppm %>% select(c(1:3), all_of(name_selected))
last_data2 <- all_ppm2 %>% select(c(1:3), all_of(name_selected))

## CONVERT DATA FRAME KE VECTOR
value1 <- last_data %>% filter(Sample == "Sample 2") %>% select(4:ncol(last_data)) %>% unlist() %>% as.vector()
value2 <- last_data %>% filter(Sample == "Sample 3") %>% select(4:ncol(last_data)) %>% unlist() %>% as.vector()

theta_name <- names(last_data[4:ncol(last_data)]) %>% str_before_first("_")

colnames(last_data)[4:ncol(last_data)] <- names(last_data[4:ncol(last_data)]) %>% str_before_first("_")
colnames(last_data2)[4:ncol(last_data2)] <- names(last_data2[4:ncol(last_data2)]) %>% str_before_first("_")

## COBA PLOTLY DARI DATAFRAME

##CONVERT KE LONG
last_data_long <- pivot_longer(last_data, 4:length(last_data))
last_data2_long <- pivot_longer(last_data2, 4:length(last_data2))



plot_ly(last_data_long,
        type = "scatterpolar",
        mode = "markers",
        r = ~value,
        theta = ~name,
        split = ~Sample,
        fill = "toself",
        hoveron = 'points')


radar_custom(last_data_long,
             r = last_data_long$value,
             theta = last_data_long$name,
             split = last_data_long$Sample,
             name = last_data_long$Sample,
             color = last_data_long$Sample,
             fill = "toself",
             hoveron = 'points',
             hovertemplate = "%{theta}: %{r} ppm"
             )  %>%
  layout(
    polar = list(
      radialaxis = list(
        angle = 90,
        visible = T,
        showline = F,
        color = '#bfbfbf',
        nticks = 5,
        tickangle = 90
      )
    )
  )

## COBA CUMA ! SAMPEL

radar_custom(last_data2_long,
             r = last_data2_long$value,
             theta = last_data2_long$name,
             split = last_data2_long$Sample,
             name = last_data2_long$Sample,
             color = last_data2_long$Sample,
             fill = "toself",
             hoveron = 'points',
             hovertemplate = "%{theta}: %{r} ppm"
) 


### bikin plotly

base_plot <- plot_ly(
  type = 'scatterpolar',
  mode = "markers"
) 

sample_2_trace <- add_transparant_trace(
  r = value1,
  theta =  theta_name,
  color = "#F8766D",
  name = "Sample 2"
)



  
######### SHINY
ppm_elements <- c("Au", "Ce", "Gd", "Hf", "La", "Nd", "Rb", "Th", "Y", "Zr")
percentage_elements <- c("Al", "ca", "Fe", "Mg", "Si")


ui <- fluidPage(
  checkboxGroupInput(
    inputId = "sample", 
    label = "Select Sample", 
    choices = c("Sample 2", "Sample 3"), 
    selected = c("Sample 2", "Sample 3"),
    inline = T
  ),
  selectInput("screen_size", "Select Screen Size", choices = unique(assay_size$Screen)),
  uiOutput("select_size_type"),
  uiOutput("select_ppm_elements"),
  uiOutput("select_percentage_elements"),
  tableOutput("test"),
  tableOutput("last_table"),
  plotlyOutput("ppm_radar_chart"),
  plotlyOutput("percentage_radar_chart")
)

server <- function(input, output, session) {
  output$select_size_type <- renderUI(
    radioButtons("select_size", "Select Size Type", 
                 choices = unique(assay_size$Type[assay_size$Screen == input$screen_size]))
  )
  
  all_ppm <- reactive(assay_size %>% 
                        filter(Sample %in% input$sample, Screen == input$screen_size, Type %in% input$select_size) %>%
                        select(c(1:3), matches("ppm_grade")))
  
  all_percentage <- reactive(assay_size %>% 
                        filter(Sample %in% input$sample, Screen == input$screen_size, Type %in% input$select_size) %>%
                        select(c(1:3), matches("percentage_grade")))
  
  # 
  # ppm_choices <- reactive({
  #   req(input$select_size)
  #   temp_ppm <- all_ppm()[, -c(1:3)]
  #   range_ppm <- sapply(temp_ppm, range)
  #   ppm_list <- c()
  #   for(i in 1:length(ppm_elements)){
  #     temp_list <- glue("{ppm_elements[i]} ({range_ppm[1,i]}-{range_ppm[2,i]})")
  #     ppm_list <- c(ppm_list, temp_list)
  #   }
  #   ppm_list
  # })
  
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

  
  choiceValues_ppm <- c("Au_ppm_grade", "Ce_ppm_grade", "Gd_ppm_grade", "Hf_ppm_grade", 
                        "La_ppm_grade", "Nd_ppm_grade", "Rb_ppm_grade", "Th_ppm_grade", "Y_ppm_grade", "Zr_ppm_grade")

  choiceValues_percentage <- c("Al_percentage_grade", "Ca_percentage_grade", "Fe_percentage_grade",
                               "Mg_percentage_grade", "Si_percentage_grade")


  output$select_ppm_elements <- renderUI(
    pickerInput(
      inputId = "select_ppm",
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
      inputId = "select_percentage",
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

  
  # output$select_ppm_elements <- renderUI(
  #   checkboxGroupInput(
  #     inputId = "select_ppm", 
  #     label = "Select Grade (ppm) data to show", 
  #     choiceNames = ppm_choices(),
  #     choiceValues = c("Au_ppm_grade", "Ce_ppm_grade", "Gd_ppm_grade", "Hf_ppm_grade", 
  #                       "La_ppm_grade", "Nd_ppm_grade", "Rb_ppm_grade", "Th_ppm_grade", 
  #                       "Y_ppm_grade", "Zr_ppm_grade"),
  #     selected = c("Ce_ppm_grade", "La_ppm_grade", "Nd_ppm_grade", "Rb_ppm_grade", "Th_ppm_grade")
  #   )
  # )
  # 
  
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


  # output$test <- renderTable(all_ppm())
  # output$last_table <- renderTable(last_data())
  
  output$ppm_radar_chart <- renderPlotly(
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
  )
    
    output$percentage_radar_chart <- renderPlotly(
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
}

shinyApp(ui, server)



https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe

https://stackoverflow.com/questions/50138568/removing-traces-by-name-using-plotlyproxy-or-accessing-output-schema-in-reactiv
