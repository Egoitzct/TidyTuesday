#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

state_name_etymology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/state_name_etymology.csv')

library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(shiny)
library(rvest)

map <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
capitals <- read_csv("world_usa_coord.csv")
capitals <- capitals %>% 
    select(usa_state, usa_state_latitude, usa_state_longitude) %>% 
    filter(!is.na(usa_state)) %>% 
    rename(latitude = usa_state_latitude) %>% 
    rename(longitude = usa_state_longitude) %>% 
    rename(name = usa_state) %>% 
    filter(name != "Puerto Rico" & name != "District of Columbia")

state_name_etymology <- state_name_etymology %>% 
    rename(name = state) %>% 
    full_join(., capitals, by = "name")





state_name_etymology$meaning[6] <- "Probably named for the fictional Island of California ruled by Queen Calafia in the 16th-century novel Las sergas de Esplandián by Garci Rodríguez de Montalvo."
# Map and used functions ------------------------------------------------------

base_map <- leaflet(map) %>% 
    setView(-96, 37.8, 4) %>% 
    addProviderTiles("CartoDB.PositronNoLabels") %>% 
    addTiles() %>% 
    addCircleMarkers(lng = ~capitals$longitude,
                     lat = ~capitals$latitude,
                     label = ~capitals$name,
                     layerId = ~capitals$name,
                     color = "#FF9E00")

paragraph_text <- function(state_name = "Alabama") {
    used_state <- state_name_etymology %>% 
        filter(name == state_name) %>% 
        select(-longitude, -latitude)
    
    meaning <- paste("The original meaning is:", used_state$meaning)
    return(meaning)
}

orig_language <- function(state_name = "Alabama") {
    used_state <- state_name_etymology %>% 
        filter(name == state_name) %>% 
        select(-longitude, -latitude)
    return(used_state$language)
}

date_year <- function(state_name = "Alabama") {
    used_state <- state_name_etymology %>% 
        filter(name == state_name) %>% 
        select(-longitude, -latitude)
    return(as.character(used_state$date_named))
}

paragraph <- function(state_name = "Alabama") {
    year <- date_year(state_name)
    
    year_2 <- paste("The date on which the name was first attested is", year)
    
    year_3 <- paste(year_2, ".", sep = "")
    
    name <- orig_language(state_name)
    
    name_2 <- paste("The original language is", name)
    
    name_3 <- paste(name_2, ".", sep = "")
    
    year_name <- paste(year_3, name_3)
    
    return(year_name)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tabPanel("Etimology_Map",
             div(class = "outer",
                 tags$head(includeCSS("styles.css")),
                 leafletOutput("mymap", width = "100%", height = "100%"),
                 absolutePanel(id = "title", class = "panel panel-default",
                               fixed = TRUE, draggable = TRUE, top = 60,
                               left = "auto", right = 20, bottom = "auto",
                               width = 400, height = "auto",
                               
                               h2("Etymology Map of the USA's States"),
                               h3(htmlOutput("title_etym")),
                               div(id = "paragraph", htmlOutput("paragraph_etym")),
                               div(id = "paragraph", htmlOutput("meaning_etym"))
                               )
                 )
             )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$mymap <- renderLeaflet({
        base_map
    })
    
    output$title_etym <- renderText(
        ifelse(class(input$mymap_marker_click$id) == "character",
               input$mymap_marker_click$id,
               "Alabama"))
    
    output$paragraph_etym <- renderText(
        paragraph(ifelse(class(input$mymap_marker_click$id) == "character",
               input$mymap_marker_click$id,
               "Alabama")))
    
    output$meaning_etym <- renderText(
        paragraph_text(ifelse(class(input$mymap_marker_click$id) == "character",
                              input$mymap_marker_click$id,
                              "Alabama")))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
