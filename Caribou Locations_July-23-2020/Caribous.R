library(tidyverse)
library(skimr)
library(gganimate)
library(raster)
library(bcmaps)
library(patchwork)


individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

#find caribou that are pregnant and are with a calf

data <- individuals %>%
  filter(pregnant == "TRUE" & with_calf == TRUE)

#filter for caribou which are pregnant and with a calf
loc_data <- locations %>%
  filter(animal_id == c("KE_car024", "HR_car033", "QU_car036"))

#get map data 
province <- c("British Columbia")
canada <- getData(name = "GADM", country = "CAN", level = 1)
bc_map_data <- canada[canada$NAME_1 %in% province,]



pregnant_withcalf_caribou <- ggplot(bc_map_data, aes(x = long, y = lat, group = group)) +
  geom_path() +
  geom_polygon(fill = "white") +
  geom_line(data = loc_data, aes(x = longitude, y = latitude, colour = animal_id, group = animal_id)) +
  labs(x = "Longitude", 
       y = "Latitude",
       title = "Tracking The Three Caribou Who Were Pregnant and With a Calf",
       subtitle = "Data from Movebank",
       colour = "Animal_ID")

pregnant_withcalf_caribou

#graph doesn't tell much. will try by removing BC map

plot_caribous <- ggplot(data = loc_data, aes(x = longitude, y = latitude, shape = animal_id, colour = season)) +
  geom_point() +
  geom_path() +
  labs(x = "Longitude",
       y = "Latitude", 
       title = "Tracking Three Caribou Who Were Pregnant and With a Calf",
       subtitle = "Data from Movebank", 
       colour = "Season",
       shape = "Animal_ID") +
  theme_bw()

plot_caribous  

#create map of predators
#combine grizzly and grizzly bear

locations_sf <- locations %>%
  dplyr::select(animal_id, timestamp, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  transform_bc_albers()
 
death_from_predators <- individuals %>%
  transmute(
    cause_of_death = case_when(
      str_detect(death_cause, "Wolf") ~ "Wolf",
      str_detect(death_cause, "Grizzly") ~ "Grizzly",
      str_detect(death_cause, "Unknown predator") ~ "Unknown Predator",
      str_detect(death_cause, "Suspected wolf predation") ~ "Suspected Wolf Predation",
      str_detect(death_cause, "Suspected predation") ~ "Suspected Predation",
      TRUE ~ "Other"
    ),
    animal_id, sex, study_site
  ) 
  

list_of_death_causes <- c("Wolf", "Suspected Predation", "Unknown Predation", "Grizzly", "Suspected Wolf Predation")

death_from_predators <- death_from_predators %>%
  filter(cause_of_death %in% list_of_death_causes) %>%
  left_join(locations_sf) %>%  #locations_sf
  group_by(animal_id) %>%
  filter(timestamp == max(timestamp)) %>%
  ungroup()

#convert death_from_predators to sf object

death_from_predators <- st_as_sf(death_from_predators)


loc_of_predator_deaths <- ggplot() + 
  geom_sf(data = bc_neighbours(), color = "white", fill = "gray70") + 
  geom_sf(data = watercourses_5M(), color = "dodgerblue1") +
  geom_sf(data = death_from_predators, aes(colour = cause_of_death)) +
  labs(x = "Longitude", 
       y = "Latitude",
       colour = "Cause of Death",
       title = "Looking at the Locations of Predator Related Death Causes",
       subtitle = "Data From MoveBank") 

png("Caribou Related Graphs.png")
plot_caribous + loc_of_predator_deaths

ggsave("Tracking Caribou Who Are Pregnant and With A Calf.png", plot = plot_caribous)
ggsave("Location of Predator Deaths.png", plot = loc_of_predator_deaths)




  
  

