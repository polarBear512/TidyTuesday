#tidy tuesday 2020-06-09
#used David Robinson's tidycast as a guide to learn using text with ggplot and plotly and using Glue Package
library(tidyverse)
library(plotly)
library(glue)

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

#clean data

firsts <- firsts %>%
  mutate(person = str_remove(person, "[\\[\\(].*"),
         person = str_trim(person))

firsts_plot <- firsts %>%
  ggplot(aes(x = year, y = reorder(category, desc(category)), color = category, text = glue("{year }: { accomplishment }\n{ person }"))) +
  geom_point() + 
  labs(title = "African-American Achievements Timeline",
       subtitle = "Source: https://en.wikipedia.org/wiki/List_of_African-American_firsts",
       x = "Year",
       y = "Category"
       ) +
  theme(axis.text.y = element_text(vjust = 0.5)) +
  theme_bw()


firsts_plot_plotly <- ggplotly(firsts_plot, tooltip = "text")
firsts_plot_plotly

htmlwidgets::saveWidget(firsts_plot_plotly, file = "African American Achievements Timeline.html")
ggsave("African American Achievements Timeline.png", plot = firsts_plot)