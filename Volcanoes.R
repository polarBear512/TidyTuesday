library(tidyverse)
library(sf)
library(viridis)
library(maps)
library(rnaturalearth)

volcanoes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

#get world map
world_map <- map_data("world")

#data frame with major rocks
#major rocks df
major_rocks <- volcanoes %>%
  select(volcano_name, primary_volcano_type, latitude, longitude, 
         major_rock_1, 
         major_rock_2, 
         major_rock_3, 
         major_rock_4,
         major_rock_5)

#create map showing primary rock types (major_rock_1)

rock_type_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_point(data = major_rocks, aes(x = longitude, y = latitude, colour = major_rock_1)) +
  xlab("Longitude") + ylab("Latitude") + labs(colour = "Primary Rock") +
  ggtitle("Primary Rock Types of Volcanoes") + 
  theme_bw() +
  scale_colour_viridis_d(option = "magma")
  


