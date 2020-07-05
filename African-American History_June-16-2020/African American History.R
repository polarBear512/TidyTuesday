library(tidyverse)
library(skimr)
library(plotly)
library(glue)


blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

skim(slave_routes)

ship_name <- slave_routes %>%
  replace_na(list(ship_name = "Unknown")) %>%
  count(ship_name, sort = TRUE) %>%
  mutate(ship_name = fct_reorder(ship_name, n)) 

ship_name_plot <- ggplot(data = subset(ship_name, ship_name %in% ship_name [1:11]), aes(x = ship_name, y = n, label = n)) +
  geom_col(position = "dodge") + 
  coord_flip() +
  labs(title = "Ten Most Common Ship Names Plus Unknown Names", 
       subtitle = "17% of Ship Names Are Unknown",
       x = "Count",
       y = "Ship Name")

#ships by decade
ships_by_decade <- slave_routes %>%
  filter(!is.na(ship_name)) %>%
  mutate(ship_name = fct_lump(ship_name, 10)) %>%
  count(ship_name, 
        decade = 10 * (year_arrival %/% 10))

ships_decade_plot <- ships_by_decade %>%
  ggplot(aes(x = decade, y = n)) + 
  geom_line() +
  facet_wrap(~ship_name, scales = "free_y") +
  labs(title = "Ten Most Used Ships During Each Century",
       subtitle = "S Antônio May Have Been Active Each Century",
       x = "Decade",
       y = "Count") +
  theme_bw()
  

explore_discovery <- blackpast %>%
  filter(subject == "Exploration and Discovery")

timeline_plot <- ggplot(data = explore_discovery, aes(x = year, y = subject, colour = era, text = events)) + 
  geom_point() +
  labs(title = "A Timeline of Exploration and Discovery Events from Blackpast Dataset",
       x = "Year",
       y = "", 
       colour = "ERA") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
        axis.text.y = element_blank(), 
        legend.title = element_text(hjust = .25))

timeline_plotly <- ggplotly(timeline_plot, tooltip = "events") 
timeline_plotly

ggsave("Timeline of Exploration and Discovery Events.png", plot = timeline_plot)
htmlwidgets::saveWidget(timeline_plotly, "Timeline of Exploration and Discovery Events.html")

