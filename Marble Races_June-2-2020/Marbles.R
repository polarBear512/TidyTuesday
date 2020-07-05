library(tidyverse)
library(skimr)
library(patchwork)
library(scales)
library(viridis)
library(stringr)



marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')
marbles <- marbles %>%
  mutate(race_type = if_else(str_detect(race, "R"), "Race", "Qualification"))

#remove qualification rounds, keep only races
races <- marbles %>%
  filter(!str_detect(race, "Q"))

#keep only qualification rounds
qualification_rounds <- marbles %>%
  filter(str_detect(race, "Q"))

#each marble has four races
races %>%
  group_by(marble_name) %>%
  count(race)

#there are 8 races
races %>%
  count(site)

#add column for avg speed for race to make it easier to compare speed. Different tracks
#prim and mary keep na?
speed <- marbles %>%
  mutate(average_speed = number_laps*(track_length_m/time_s)) %>%
  group_by(marble_name, team_name) %>%
  summarise(avg = mean(average_speed, na.rm = TRUE)) %>%
  arrange(team_name, avg) %>%
  ungroup() %>%
  mutate(avg_speed_diff = lag(avg, order_by = team_name) - avg) %>%
  mutate(team_name = fct_reorder(team_name, avg_speed_diff, sum))

#investigate why marbles Mary, Wospy, and Sublime return NA values
dnf <- races %>%
  select(marble_name, 
         team_name, 
         time_s,
         track_length_m,
         number_laps, 
         avg_time_lap) %>%
  filter(marble_name == "Mary" | marble_name == "Wospy" | marble_name == "Sublime")

#leads to changing na.rm to TRUE

speed_plot <- speed %>% 
  ggplot(aes(x = team_name, y = avg, label = marble_name)) + 
  geom_point() +
  geom_line() + 
  coord_flip() + 
  geom_label(aes(colour = factor(team_name))) + 
  scale_colour_viridis_d(guide = FALSE) +
  labs(x = "Average Speed", 
       y = "Teams", 
       title = "How do Teams Stack Up in Terms of Speed", 
       subtitle = "Mary holding back Team Primary? Limers Don't Leave Each Other Behind") +
  theme_bw()

speed_plot

#how do marbles stack up in terms of qualification
qualification_overview <- marbles %>%
  filter(race_type == "Qualification") %>%
  group_by(race) %>%
  mutate(position = row_number()) %>%
  ungroup() %>%
  count(marble_name, position) %>%
  arrange(marble_name)

qual_overview_plot <- qualification_overview %>% 
  ggplot(aes(x = n, y = marble_name)) +
  geom_col(aes(fill = position)) +
  scale_fill_viridis(option = "cividis") +
  labs(x = "Race Number", 
       y = "Marble Name", 
       title = "A Glance at Marble Qualifying",
       subtitle = "Might Wospy be the Best at Qualifying?") +
  theme_bw()

#return df with average qualifying finishes
qualifier_rankings <- marbles %>%
  filter(race_type == "Qualification") %>%
  mutate(pole = str_remove(pole, "P")) %>%
  group_by(marble_name) %>%
  mutate(avg = (mean(as.numeric(pole)))) %>%
  distinct(marble_name, avg) %>%
  arrange(marble_name)

qualifier_rankings_plot <- qualifier_rankings %>%
  ggplot(aes(x = avg, y = reorder(marble_name, desc(marble_name)))) +
  geom_point() +
  labs(x = "Average Qualifying Finish",
       y = "Marble Name", 
       title = "Average Qualifying Finishes for Each Marble", 
       subtitle = "Don't Mess with Smoggy and Prim") +
  theme_bw()

all_plots <- speed_plot + qualifier_rankings_plot / qual_overview_plot +
  plot_annotation("A Glance at Which Marbles are the Fastest and Qualification Rounds") +
  plot_layout(heights = 5, width = 20)

ggsave(filename = "Fastest Marbles.png", plot = speed_plot)
ggsave(filename = "Qualification Overview.png", plot = qual_overview_plot)
ggsave(filename = "Qualifiers Plot.png", plot = qualifier_rankings_plot)
