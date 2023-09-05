# Load data and packages ---------------------------------------------------------------

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

library(tidyverse)
library(maps)
library(leaflet)
library(RColorBrewer)

glimpse(demographics)
glimpse(wages)
glimpse(states)


# Data processing ---------------------------------------------------------

plot_data <- states |> 
  select(year, state, p_members) |>
  group_by(state) |> 
  summarise(p_members = sum(p_members)) |> 
  arrange(state) |> 
  rename(region = state) 

# Plot --------------------------------------------------------------------

states_map <- map_data("state")
plot_data$region <- tolower(plot_data$region)
plot_data_map <- inner_join(states_map, plot_data, by = "region")

ggplot(plot_data_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = p_members), color = "white") +
  scale_fill_continuous(name="Union Members (%)",
                        low = "lightblue", high = "darkblue", limits = c(5, 70),
                        breaks=c(5,10,15,20,25,30,35,40,45,50,55,60,65), na.value = "grey50") +
  labs(title = "USA Union Members") +
  theme_classic() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        plot.title = element_text(color="black", size=20, face="bold", hjust=0.5, vjust=-0.25),
        legend.key.size = unit(1, "cm"),
        legend.background = element_rect(fill="#E5F1FF", size=0.5, linetype="solid", color="darkblue")
        )

# Interactive_map ---------------------------------------------------------

plot_data <- states |> 
  select(year, state, p_members) |>
  group_by(state) |> 
  summarise(p_members = sum(p_members)) |> 
  arrange(state) |> 
  rename(name = state) 

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
states_2 <- states@data[-52,]
states@data <- states_2
states$density <- plot_data$p_members

states@data <- states@data |> 
  rename(p_members = density)

m <- leaflet(states) |> 
  setView(-96, 37.8, 4) |> 
  addProviderTiles("CartoDB.Positron")

bins <- c(0, 10, 20, 30, 40, 50, 60, 70)
nb.cols <- length(bins)
mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)
pal <- colorBin(mycolors, domain = states$p_members, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g&#37 union members",
  states$name, states$p_members
) %>% lapply(htmltools::HTML)

m |> addPolygons(
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
