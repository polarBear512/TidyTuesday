library(tidyverse)
library(readxl)
library(sf)
library(viridis)
library(maps)
library(tmap)
library(rnaturalearth)
#library(leaflet)
#get the data

volcanoes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')


world_map <- map_data("world")
world_map_sf <- st_as_sf(world_map, coords = c("long", "lat"), crs = 4326) #converts world map into an sf object

#major rocks df
major_rocks <- volcanoes %>%
  select(volcano_name, primary_volcano_type, latitude, longitude, major_rock_1)

major_rocks_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "seagreen4", alpha = .4) +
  geom_point(data = major_rocks, aes(x = longitude, y = latitude, colour = major_rock_1)) +
  labs(x = "Longitude", y ="Latitude",
       colour = "Primary Major Rock Type") +
  ggtitle("Major Rock Types of Volcanoes")

ggsave(filename = "Major_Rocks_Type_Plot.png")

#find which volcanoes have erupted the most
most_eruptions <- eruptions %>%
  count(volcano_name, sort = TRUE)

sum_volcanoes <- eruptions %>%
  group_by(volcano_name) %>%
  count() %>%
  left_join(volcanoes)

#making world map using Molleweide projection

#another way to get map
earth <- ne_countries(scale = "medium", returnclass = "sf") #returns sf class

moll <- st_transform(world_map_sf, crs = "+proj=moll")
ggplot(data = earth) + 
  geom_sf() + 
  coord_sf(crs = "+proj=moll") + 
  geom_point(data = major_rocks, aes(x = longitude, y = latitude, colour = major_rock_1)) +
  labs(x = "Longitude", y ="Latitude",
       colour = "Primary Major Rock Type") +
  ggtitle("Major Rock Types of Volcanoes")

ggsave(filename = "Molleweide Projection for Major Rock Types.png")


