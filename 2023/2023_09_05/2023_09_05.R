# Load data and packages ---------------------------------------------------------------

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

library(tidyverse)
library(maps)


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
