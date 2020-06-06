library(tidyverse)
library(skimr)
library(scales)
library(stringr)
library(patchwork)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

#clean cocktails: make glass and ingredient columns lowercase
cocktails <- cocktails %>%
  mutate(
    glass = str_to_lower(glass),
    ingredient = str_to_lower(ingredient)
  ) 
  

#find most common glass types
glass_types <- cocktails %>%
  distinct(row_id, .keep_all = TRUE) %>%
  count(glass, sort = TRUE) %>%
  mutate(glass = fct_reorder(glass, n),
         Percentage = (n/(sum(n))))


percentage_glass_types_plot <- ggplot(glass_types, aes(x = Percentage, y = glass)) + 
  geom_col()  +
  labs(title = "Most Common Glass Types",
       x = "Percentage of Glass Types", 
       y = "Glass Types") +
  theme_bw()
percentage_glass_types_plot

top_ten_glass_types <- glass_types %>%
  top_n(10) %>%
  ggplot(aes(x = n, y = glass)) +
  geom_col() +
  labs(title = "Top Ten Glass Types",
       x = "Total", 
       y = "Glass Types") +
  theme_bw()

png(top_ten_glass_types)

all_plots <- top_ten_glass_types + percentage_glass_types_plot +
  plot_annotation(title = "Exploratory Analysis of Glass Types From Cocktails Dataset")

png(filename = "Glass Types")







