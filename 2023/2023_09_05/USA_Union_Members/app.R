#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
library(tidyverse)
library(leaflet)

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')


# Plots -------------------------------------------------------------------

sector_plot <- function(state_name = "Arizona", selected_year = 2022) {
  states %>% 
    select(state, sector, p_members, year) %>% 
    filter(state == state_name & year == selected_year) %>% 
    group_by(state, sector) %>% 
    summarise(p_members = sum(p_members)) %>% 
    arrange(desc(p_members)) %>% 
    ggplot(aes(x = sector, y = p_members, fill = sector)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "RdYlGn") +
      theme_minimal(
        base_size = 14,
        base_family = "sans"
      ) +
      labs(
        title = paste("Percentage of union members\nby sector in", state_name),
        subtitle = paste("Year:", selected_year),
        y = "Union members (%)"
      ) +
      theme(
        legend.position = "none",
        panel.grid.major = element_line(linetype = 3, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0, size = 16)
      ) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = scales::label_percent(scale = 1))
}

year_members_plot <- function(state_name = "Arizona") {
  states %>% 
    select(state, year, members) %>% 
    filter(state == state_name) %>% 
    group_by(state, year) %>% 
    summarise(members = sum(members)) %>% 
    arrange(desc(members)) %>% 
    ggplot(aes(x = year, y = members)) +
    geom_line(color = "#097AD3") +
    geom_area(fill = "#118AE8", alpha = 0.5) +
    geom_point(color = "#097AD3", size = 1) +
    theme_minimal(
      base_size = 14,
      base_family = "sans"
    ) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_long_scale())) +
    labs(
      title = paste("Evolution of Union Members", state_name, sep = "\n"),
      x = "Year"
    ) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title.y = element_blank(),
      axis.title.x = element_text(face = "bold")
    )
}

# Data Processing and base map ---------------------------------------------------------

data <- states |> 
  select(year, state, p_members, sector) |>
  group_by(state) |> 
  summarise(state = unique(state)) |> 
  arrange(state) |> 
  rename(name = state)

base_map <- function(year_s = 2022) {
  states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')
  
data <- states |> 
  select(year, state, p_members, sector) |>
  filter(year == year_s & sector == "Total") %>% 
  group_by(state) |> 
  summarise(p_members = p_members * 100) |> 
  arrange(state) |> 
  rename(name = state)

states_map <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
states_2 <- states_map@data[-52,]
states_map@data <- states_2
states_map$density <- data$p_members

rm(states_2)

states_map@data <- states_map@data |> 
  rename(p_members = density)

bins <- c(0, 1, 5, 10, 15, 20, 25, 30)
nb.cols <- length(bins)
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.cols)
pal <- colorBin(mycolors, domain = states_map$p_members, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g&#37 union members",
  states_map$name, states_map$p_members
) %>% lapply(htmltools::HTML)

map <- leaflet(states_map) |> 
  setView(-96, 37.8, 4) |> 
  addProviderTiles("CartoDB.Positron") %>% addPolygons(
  fillColor = ~pal(p_members), 
  weight = 2, 
  opacity = 1, 
  color = "white", 
  dashArray = "3", 
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 2,
    color = "#A4A4A4",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) |> 
  addLegend(pal = pal, values = ~p_members, opacity = 0.7, title = NULL,
            position = "bottomright")

}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tabPanel("Interactive map",
             div(class="outer",
             tags$head(includeCSS("styles.css")),
             leafletOutput("mymap", width = "100%", height = "100%"),
             absolutePanel(id = "title", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                           width =400, height = "auto",
                           
                           h2("United States of America's Union Members Map"),
                           plotOutput("sector", height = 200),
                           selectInput(
                             "state_choose",
                             label = "Choose a State",
                             choices = c(data$name),
                             selected = "Arizona"),
                           sliderInput("year_select", label = "Choose a year:", min = 1983, max = 2022, value = 2022, step = 1, sep = "",
                                       ticks = FALSE),
                           plotOutput("year", height = 200),
                           
                          )
                           
            )
            )
    )

server <- function(input, output) {

    output$mymap <- renderLeaflet({
      base_map(year_s = input$year_select)
    })
    
    observe({
      input$year_select
    })
    
    output$sector <- renderPlot({
      sector_plot(state_name = as.character(input$state_choose), selected_year = input$year_select)
    })
    
    output$year <- renderPlot({
      year_members_plot(state_name = as.character(input$state_choose))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
