library(tidyverse)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

species <- coffee_ratings %>%
  group_by(species) %>%
  count(species, sort = TRUE) %>%
  ggplot(aes(x = species, y = n)) + 
  geom_col()

#distribution of points

ggplot(data = coffee_ratings, aes(x = total_cup_points)) + 
  geom_histogram()

#boxplot of flavours for arabica

arabica_beans <- coffee_ratings %>%
  filter(species == "Arabica") %>%
  select(species, 
         aroma, 
         flavor, 
         aftertaste,
         acidity, 
         body,
         balance,
         uniformity, 
         clean_cup,
         sweetness, 
         cupper_points, 
         ) %>%
  janitor::clean_names(case = "upper_camel") %>%
  pivot_longer(cols = Aroma:CupperPoints, 
               names_to = "Variable",
               values_to = "Rank", 
               values_drop_na = TRUE)



arabica_tasting <- ggplot(data = arabica_beans, aes(x = Variable, y = Rank)) + 
  geom_boxplot() +
  labs(title = "How do Different Coffee Taste Variables Stack Up for Arabica Beans", 
       subtitle = "Data From Coffee Quality Institute") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 7)) 

ggsave(filename = "Boxplot of Coffee Tasting Variables.png", plot = arabica_tasting, width = 8)

#check country names combine same country names into one 
coffee_ratings %>%
  distinct(country_of_origin) %>%
  arrange(country_of_origin)



coffee_ratings <- coffee_ratings %>%
  mutate(
    country_of_origin = case_when(
      str_detect(country_of_origin, "United States") ~ "United States", 
      TRUE ~ country_of_origin
    )
  )
  

coffee_quantity_by_country <- coffee_ratings %>%
  filter(!is.na(country_of_origin)) %>%
  count(Country = fct_lump(country_of_origin, 10), sort = TRUE) %>%
  mutate(Country = fct_reorder(Country, n)) %>%
  ggplot(aes(x = n, y = Country)) +
  geom_col(fill="#f68060", alpha=.6, width=.4) + 
  labs(title = "Highest Coffee Producing Countries", 
       caption = "Data From Coffee Institute",
       x = "Total", 
       y = "Country of Origin") +
  theme_bw()

ggsave("Coffee Quantity by Country Plot.png", plot = coffee_quantity_by_country)



