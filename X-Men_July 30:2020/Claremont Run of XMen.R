library(tidyverse)
library(plotly)
library(glue)
library(patchwork)
library(ggraph)
library(igraph)

#comic_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/comic_bechdel.csv')
#character_visualization <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')
#xmen_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/xmen_bechdel.csv')
covers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/covers.csv')
#issue_collaborators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/issue_collaborators.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/locations.csv')

#explore covers
#timeline of narrative captions

timeline_data <- covers %>%
  filter(!is.na(narrative_captions)) %>%
  mutate(issue_caption = glue("Issue {issue}: {narrative_captions} "))


plot_captions <- ggplot(data = timeline_data, aes(x = issue, y = 0, colour = cover_artist, text = issue_caption)) +
  geom_point() + 
  labs(x = "Issue",
       y = "",
       colour = "Cover Artist",
       title = "A Timeline of Narrative Captions During Claremon Run of X-Men") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

plotly_captions <- ggplotly(plot_captions, tooltip = "issue_caption")
plotly_captions  



#who dances the most: remove NA, separate character into mutants/human, cover for extra dance partners
dancing_queen <- characters %>%
  filter(!is.na(dancing_with_which_character)) %>% 
  select(issue, character, dancing_with_which_character) %>%
  separate(character, into = c("Mutant", "Identity"), sep = "=", fill = "right") %>%
  separate(dancing_with_which_character, into = c("Partner_1", "Partner_2"), sep = ",", fill = "right")

#remove *
dancing_queen$Partner_1 <- str_replace_all(string = dancing_queen$Partner_1, pattern = "\\*", replacement = "")
dancing_queen$Partner_2 <- str_replace_all(string = dancing_queen$Partner_2, pattern = "\\*", replacement = "")

dancer <- dancing_queen %>%
  group_by(Mutant) %>%
  count() %>%
  ungroup() %>%
  mutate(Mutant = fct_reorder(Mutant, desc(n)))
  
dancer_plot <- ggplot(data = dancer, aes(x = Mutant, y = n)) +
  geom_col() +
  labs(y = "Count",
       title = "Which X-Men Character Likes to Dance the Most",
       subtitle = "Havok is the DANCING QUEEN!") +
  scale_colour_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


dance_partners <- dancing_queen %>%
  mutate(pair = paste0(Identity, " ", Partner_1)) %>%
  select(Identity, Partner_1, pair, issue)

dance_partners_ggraph <- graph_from_data_frame(dance_partners)


plot_dance_partners <- ggraph(dance_partners_ggraph, layout = "fr") +
  geom_edge_arc(aes(edge_colour = issue)) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  labs(subtitle = "X-Men Dancing Partners") +
  theme_void()
  

#most common locations
locations %>%
  count(location, sort = TRUE)
#shows us that there are many locations especially with only 1 value that ideally should be combined with others (example: Yellowstone). will ignore

common_locations <- locations %>%
  group_by(location) %>%
  mutate(n = n()) %>%
  dplyr::distinct(location, .keep_all = TRUE) %>%
  arrange(desc(n)) %>%
  top_n(10)

all_dancing_plots <- dancer_plot + plot_dance_partners



ggsave(filename = "xmendancingplots.png")

