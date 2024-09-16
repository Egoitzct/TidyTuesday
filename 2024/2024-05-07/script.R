library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)
library(ggbump)
library(waffle)
library(ggsci)

rolling_stone <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-07/rolling_stone.csv')

glimpse(rolling_stone)

rolling_stone_clean <- rolling_stone %>% 
  select(rank_2003, rank_2012, rank_2020, genre, sort_name, album) %>% 
  filter(rank_2003 <= 9) %>% 
  arrange(rank_2003) %>% 
  pivot_longer(
    cols = !album,
    names_to = c(".value", "year"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>% 
  select(album, year, rank) %>%
  filter(!is.na(rank)) %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>% 
  group_by(album) %>% 
  na.omit()

font <- "Gudea"
font_add_google(family=font, font, db_cache = TRUE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
theme_set(theme_minimal(base_family = font, base_size = 10))
bg <- "#F4F5F1"
txt_col <- "black"
showtext_auto(enable = TRUE)

p1 <- rolling_stone_clean %>% 
  ggplot() +
  geom_hline(yintercept = 5, linetype = "solid", size = .25) +
  geom_point(data = rolling_stone_clean,
             aes(x = year, y = rank, color = album), shape = 16) +
  geom_line(aes(x = year, y = rank, color = album)) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  geom_text(data = rolling_stone_clean,
            aes(x = year, y = rank, color = album, label = rank),
            hjust = -.5, vjust = .5, size = 2.5, family = "Gudea", fontface = "bold") +
  scale_color_met_d(name = "Redon") +
  scale_x_date(date_labels = "%y") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
                     labels = c("", "5", "","","","","","", "")) +
  facet_wrap(~ factor(album, levels = c("What's Going On", "Sgt. Pepper's Lonely Hearts Club Band",
                                        "Rubber Soul", "Revolver", "Pet Sounds", "London Calling",
                                        "Highway 61 Revisited", "Exile on Main St.", "Blonde on Blonde"))) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=7),
    strip.text.x = element_text(face="bold"),
    plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold")
  )


p1

glimpse(rolling_stone_clean)

artist_appear <- as.data.frame(table(rolling_stone$clean_name))

artist_appear <- artist_appear %>% 
  rename(
    artist = Var1,
    n = Freq
  ) %>% 
  arrange(desc(n)) %>% 
  slice(1:21)

glimpse(artist_appear)

top_20 <- c(artist_appear[["artist"]])

top_20 <- top_20[ !top_20 == "Various Artists"]

top_artist <- rolling_stone %>% 
  filter(clean_name %in% top_20) %>% 
  filter(type != "Compilation" & type != "Greatest Hits") %>% 
  group_by(clean_name) %>% 
  select(rank_2003, rank_2012, rank_2020, album, clean_name) %>% 
  pivot_longer(
    cols = !c(clean_name, album),
    names_to = c(".value", "year"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>% 
  arrange(clean_name) %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>% 
  na.omit()

color <- met.brewer("Degas", n = 21)

p2 <- top_artist %>% 
  ggplot(aes(x = year, y = rank, group = album)) +
  geom_bump(linewidth = 0.6, color = "gray85", smooth = 6) +
  geom_bump(aes(color = album), linewidth = 0.8, smooth = 6) +
  geom_point(aes(x = year, y = rank, color = album), size = 1) +
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Accent"))(114)) +
  scale_x_date(date_labels = "%y") +
  facet_wrap(~ factor(clean_name, levels = as.vector(top_20))) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=7),
    strip.text.x = element_text(face="bold"),
    plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold")
  )

p2

artist_year <- top_artist %>% 
  select(-rank) %>% 
  group_by(year, clean_name) %>% 
  reframe(name = clean_name, year = year, n = sum(n())) %>% 
  distinct() %>% 
  select(year, name, n)

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

p3 <- ggplot(artist_year, aes(fill = name, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, expand = c(0, 0)) +
  scale_fill_igv() +
  coord_equal() +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.spacing = unit(0.5, 'cm'),
    legend.key.height= unit(0.5, 'cm'),
    legend.key.width= unit(0.7, 'cm'),
    legend.text = element_text(family = body_font,
                               size=13,
                               face = 'plain',
                               color = "grey10"),
  )

p3




