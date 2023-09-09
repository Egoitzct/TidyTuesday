# Load data and packages --------------------------------------------------

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv')
state_name_etymology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/state_name_etymology.csv')

library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)

map <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
capitals <- readr::read_csv("https://raw.githubusercontent.com/jasperdebie/VisInfo/master/us-state-capitals.csv")
state_name_etymology <- state_name_etymology %>% 
  rename(name = state) %>% 
  full_join(., capitals, by = "name") %>% 
  rename(capital = description)

# Map ---------------------------------------------------------------------

base_map <- leaflet(map) %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~state_name_etymology$longitude,
                   lat = ~state_name_etymology$latitude) 
