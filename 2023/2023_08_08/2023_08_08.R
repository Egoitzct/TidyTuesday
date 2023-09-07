# Load_data_and_packages --------------------------------------------------

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv')
sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/seasons.csv')

library(tidyverse)
library(ggtext)
library(gganimate)
library(ggforce)
library(scales)

head(episodes)
head(sauces)
head(seasons)

plot_data <- sauces %>% 
  select(season, scoville, sauce_number) %>% 
  janitor::clean_names() %>% 
  distinct() %>% 
  mutate(
    sauce_number = factor(sauce_number)
  ) %>% 
  group_by(season)
  
colorCount <- length(unique(plot_data$sauce_number))
getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))

animacion <- plot_data %>% 
  ggplot(aes(sauce_number, scoville, color = sauce_number, fill = sauce_number)) +
  geom_bar(stat = "identity") +
  theme_light(
    base_size = 14,
    base_family = "sans"
  ) +
  labs(
    x = "Number of the sauce from 1 (least hot)\nto 10 (hottest)",
    y = "Rating in Scoville Heat Units",
    title = "Changes of the Sauces Scoville puntuation\nin the Hot Ones show",
    subtitle = "Season: {closest_state}"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid.major = element_line(linetype = 5, color = "black"),
    panel.grid.major.x = element_blank(),
    zoom.x = element_rect(fill = "gray"),
    zoom.y = element_rect(fill = "gray"),
    zoom = element_rect(fill = "gray"),
    legend.position = "none",
    plot.title = element_text(
      face = "bold", size = 17, margin = margin(t = 0.75, b = 0.5, unit = "cm")
    ),
    plot.subtitle = element_text(
      face = "bold", size = 15, color = "gray", margin = margin(b = 0.5, unit = "cm")
    ),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  facet_zoom(ylim = c(0, 1000000), shrink = TRUE) +
  scale_fill_manual(values = getPalette(colorCount)) +
  scale_color_manual(values = getPalette(colorCount)) +
  scale_y_continuous(minor_breaks = NULL,
                     labels = label_number(scale_cut = cut_long_scale())) +
  transition_states(factor(season), state_length = 0)

animate(animacion, width = 700, height = 432, fps = 25, duration = 15,
        rewind = FALSE)

anim_save(filename = "animation.gif", animation = animacion, width = 1800, height = 1300, fps = 25, duration = 15,
        rewind = FALSE, path = "~/Documents/GitHub/TidyTuesday/2023/2023_08_08/", res = 300)
