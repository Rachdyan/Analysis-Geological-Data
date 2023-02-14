library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(glue)

########

screen_size <- read.csv("./data/screen-sizing-report.csv")

screen_size <- screen_size %>% arrange(Screen.Size)

minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

tooltip <- "<span style='color:white'><b>Cum Passing:</b> {Percentage.Cum.Passing}% \n <b>Screen Size</b>: {Screen.Size} micron \n <b>Mass Restained:</b> {Mass.Restained} g</span>"

 ### Cum Passing
screen_size_plot <- screen_size %>% ggplot(aes(x = Screen.Size, y = Percentage.Cum.Passing, color = Sample, group = Sample, 
                                               text = glue(tooltip))) + 
  geom_line(size = 1.5) + geom_point(aes(fill = Sample), size = 2.5, color="black",pch=21)+
  scale_y_continuous(limits = c(0,100), label = label_number(suffix = "%")) +
  scale_x_log10(breaks = 10^(0:4), limits = c(1, 10000), minor_breaks = minor_breaks) +
  labs(x = "Screen Size (microns)", y = "% Cum Passing") +
  theme_bw() + theme(legend.position = c(0.1, 0.9))


###3 Cum Retained
screen_size_plot <- screen_size %>% ggplot(aes(x = Screen.Size, y = Percentage.Cum.Retained, color = Sample, group = Sample, 
                                               text = glue(tooltip))) + 
  geom_line(size = 1.5) + geom_point(aes(fill = Sample), size = 2.5, color="black",pch=21)+
  scale_y_continuous(limits = c(0,100), label = label_number(suffix = "%")) +
  scale_x_log10(breaks = 10^(0:4), limits = c(1, 10000), minor_breaks = minor_breaks) +
  labs(x = "Screen Size (microns)", y = "% Cum Passing") +
  theme_bw() + theme(legend.position = c(0.1, 0.9))


screen_size_plot <- screen_size %>% ggplot(aes_string(x = "Screen.Size", y = "Percentage.Cum.Retained", color = "Sample", group = "Sample", 
                                               text = glue(tooltip))) + 
  geom_line(size = 1.5) + geom_point(aes(fill = Sample), size = 2.5, color="black",pch=21)+
  scale_y_continuous(limits = c(0,100), label = label_number(suffix = "%")) +
  scale_x_log10(breaks = 10^(0:4), limits = c(1, 10000), minor_breaks = minor_breaks) +
  labs(x = "Screen Size (microns)", y = "% Cum Passing") +
  theme_bw() + theme(legend.position = c(0.1, 0.9))


screen_size_plot

screen_size_plotly <- ggplotly(screen_size_plot, tooltip = c("text")) %>% fix_legend_plotly()
 
screen_size_plotly %>% layout(legend = list(x = 0.05, y = 0.95, title = list(text='<b> Sample </b>')))


### Manual Fix Legend
# Get the names of the legend entries
df <- data.frame(id = seq_along(screen_size_plotly$x$data), legend_entries = unlist(lapply(screen_size_plotly$x$data, `[[`, "name")))
# Extract the group identifier
df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
# Add an indicator for the first entry per group
df$is_first <- !duplicated(df$legend_group)


for (i in df$id) {
  # Is the layer the first entry of the group?
  is_first <- df$is_first[[i]]
  # Assign the group identifier to the name and legendgroup arguments
  screen_size_plotly$x$data[[i]]$name <- df$legend_group[[i]]
  screen_size_plotly$x$data[[i]]$legendgroup <- screen_size_plotly$x$data[[i]]$name
  # Show the legend only for the first layer of the group 
  if (!is_first) screen_size_plotly$x$data[[i]]$showlegend <- T
}

### Function to Fix Legend
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


screen_size_plotly


                                             
                                             
"https://stackoverflow.com/questions/43278035/plotly-build-modifies-legend-and-labels"
