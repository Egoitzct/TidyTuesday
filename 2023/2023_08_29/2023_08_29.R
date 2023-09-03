fair_use_cases <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv')
fair_use_findings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_findings.csv')

library(tidyverse)

glimpse(fair_use_cases)
glimpse(fair_use_findings)


# data_exploring ------------------------------------------

cases <- fair_use_cases |> 
  filter(outcome == "Fair use found" | outcome == "Fair use not found") |> 
  select(year, categories, fair_use_found) |> 
  mutate(
    education = case_when(
      stringr::str_detect(categories, "Education") ~ "education_related",
      TRUE ~ "Non_education_related"
      )
  ) |> 
  filter(education == "education_related") |> 
  select(year, fair_use_found) |> 
  group_by(year, fair_use_found) |>
  summarise(n = n()) |> 
  ungroup()|> 
  filter(year >= 1950)


# ploting -----------------------------------------------------------------

library(ggrepel)


position <- c()

position <-  ifelse(cases$fair_use_found == TRUE, append(position, 1), append(position, -1))

cases_plot <- cases |> 
  mutate(
    position =  position
  ) 

redondear_decada <- function(x) {
  return(x - x %% 10)
}

cases_plot <- cases_plot |> 
  mutate(
    year_decade = redondear_decada(year)
  )

cases_plot_found <- cases_plot[cases_plot$fair_use_found == TRUE, ]
cases_plot_not <- cases_plot[cases_plot$fair_use_found != TRUE, ]

position_plot_found <- cases_plot_found |> 
  group_by(year_decade) |> 
  summarise(position_sum = sum(position))

position_plot_not <- cases_plot_not |> 
  group_by(year_decade) |> 
  summarise(position_sum = sum(position))

position_plot <- merge(position_plot_found, position_plot_not, by = "year_decade", all = TRUE)

ggplot(position_plot, aes(x=year_decade)) +
  theme_classic() +
  geom_area(aes(x = year_decade, y = position_sum.x), fill="#79DD79", alpha=0.7) +
  geom_line(aes(x = year_decade, y = position_sum.x), color="#2FB92F") +
  geom_point(aes(x = year_decade, y = position_sum.x), color="#2FB92F", size=1.5) +
  geom_hline(yintercept=0, color="black", linewidth=0.3)+
  geom_area(aes(x = year_decade, y = position_sum.y), fill="#E73A3A", alpha=0.7) +
  geom_line(data=position_plot[!is.na(position_plot$position_sum.y),], aes(x = year_decade, y = position_sum.y), color="#C81717") +
  geom_point(aes(x = year_decade, y = position_sum.y), color="#C81717", size=1.5) +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom"
  ) +
  geom_text(aes(x=year_decade,y=-0.5,label=year_decade, fontface="bold"),size=3, color='black') +
  geom_point(aes(x=year_decade, y=0), size=1) +
  geom_segment(aes(x=year_decade, y=position_sum.x, yend=0.15, xend=year_decade), color="#2FB92F", linetype="dashed") +
  geom_segment(aes(x=year_decade, y=position_sum.y, yend=0.15, xend=year_decade), color="#C81717", linetype="dashed") +
  geom_label_repel(aes(x=year_decade, y=position_sum.x, label=paste(position_sum.x, "fair use found")), size = 2, color="#79DD79") +
  geom_label_repel(aes(x=year_decade, y=position_sum.y, label=paste((-position_sum.y), "fair use case\nnot found")), size = 2, color="#FF8C8C") +
  ggtitle("Fair use cases in education from 1950 to 2020") +
  theme(
    plot.title = element_text(color="black", size=20, face="bold", hjust=0.5, vjust=-0.25)
  ) +
  annotate("label", x = 1967, y = 4.25, label="Fair use is a longstanding and vital aspect of American\ncopyright law. The goal of the Index is to make the\nprinciples and application of fair use more accessible and\nunderstandable to the public by presenting\na searchable database of court opinions,\nincluding by category and type of use (e.g., music,\ninternet/digitization, parody).")
