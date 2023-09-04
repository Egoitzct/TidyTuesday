# Load_data ---------------------------------------------------------------

population <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

library(tidyverse)
library(janitor)
library(ggrepel)
library(scales)
library(plotly)

glimpse(population)

data <- population |> 
  filter(refugees >= 100) |> 
  select(year, coo_name, coo_iso, coa_name, coa_iso, refugees, asylum_seekers) |> 
  group_by(coa_iso)

data <- data |> 
  select(year, coa_name, coa_iso, refugees, asylum_seekers) |> 
  group_by(coa_iso)

data_plot <- data |>
  group_by(coa_name) |> 
  summarise(refugees_sum = sum(refugees),
            asylum_sum = sum(asylum_seekers)) |> 
  ungroup()

data_plot <- data_plot |> 
  filter(refugees_sum >= 10000 & asylum_sum >= 10000)

data_plot_1 <- data_plot |> 
  filter(refugees_sum >= 7000000)

data_plot_2 <- data_plot |> 
  filter(refugees_sum < 7000000) |> 
  adorn_totals("row") |> 
  filter(coa_name == "Total")

data_plot_2$coa_name[1] <- "Others"

data_plot_1 <- data_plot_1 |> 
  arrange(refugees_sum)

data_plot <- rbind(data_plot_1, data_plot_2)

# Plot --------------------------------------------------------------------

plot <- data_plot_1 |> 
  mutate(coa_name=factor(coa_name, levels=coa_name)) |> 
  ggplot(aes(x = coa_name, y = refugees_sum)) +
  geom_segment(aes(xend=coa_name, yend=0)) +
  geom_point(size = 4, color = "#0FDEE8") +
  coord_flip() +
  xlab("") +
  ylab("Total number of refugees (World`s first 10 countries)") +
  theme_bw() +
  geom_label_repel(aes(x = coa_name, label = refugees_sum)) +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = label_number(scale_cut = cut_long_scale()))

interactive_plot <- data_plot_1 |> 
  mutate(coa_name=factor(coa_name, levels=coa_name)) |> 
  ggplot(aes(x = coa_name, y = refugees_sum)) +
  geom_segment(aes(xend=coa_name, yend=0)) +
  geom_point(size = 4, color = "#FF7E0F") +
  coord_flip() +
  xlab("") +
  ylab("Total number of refugees (World`s first 10 countries)") +
  theme_bw() +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = label_number(scale_cut = cut_long_scale()))

interactive_plot <- ggplotly(interactive_plot)

interactive_plot

# Save the plots ----------------------------------------------------------

ggsave("Total_number_of_refugees.png", plot = plot, device = "png",
       path = "~/Documents/GitHub/TidyTuesday/2023/2023_08_22")

library(htmlwidgets)

saveWidget(interactive_plot, file="Total_number_of_refugees_interactive.html", selfcontained = FALSE,
           libdir = "lib")
