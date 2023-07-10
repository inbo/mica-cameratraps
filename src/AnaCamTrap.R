### ----------------------------------------------------------------------------
### Analysis of camera trap images
### Last update: 2022-11-25
### ----------------------------------------------------------------------------

library(readr)
library(openxlsx)
library(lubridate)
library(tidyverse)

observations <- 
  read_csv("input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230601061026/observations.csv", 
                         col_types = cols(comments = col_character())) %>% 
  mutate(Year = year(timestamp))

deployments <-  read_csv("input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230601061026/deployments.csv")

media <- read_csv("input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230601061026/media.csv")

obs_dep <- observations %>% 
  left_join(deployments, "deploymentID")


start_obs <- observations %>% 
  filter(locationName %in% c("Mica Herne",
                              "Mica Viane"))

start_obs %>% 
  group_by(scientific_name) %>% 
  summarise(sum(count))

setup_date <- obs_dep %>% 
  group_by(locationName) %>% 
  summarise(min(timestamp, na.rm = TRUE),
            max(timestamp, na.rm = TRUE))

# Subsets
midden_obs <- observations %>% 
  filter(location_name %in% c("Mica Sint-Laureins Oostpolderkreek",  
                              "Mica Sint-Laureins Grenspost Camping",
                              "Mica Sint-Laureins Roeselarekreek",
                              "Mica Sint-Laureins Vrouwkeshoekkreek"))

midden_obs %>% 
  group_by(scientific_name) %>% 
  summarise(sum(count))

hoogstraten <- obs_dep %>% 
  filter(locationName %in% c("B_HS_val 1_markweg",
                             "B_HS_val 2_processiepark",
                             "B_HS_val 3_meander ANB",
                             "B_HS_val 4_Oosteneind",
                             "B_HS_val 5_zeemansbrug",
                             "B_HS_val 6_keerschot"))

nederland <- obs_dep %>% 
  filter(str_detect(locationName, "NL."))

# Analyses
species <- obs_dep %>%
  group_by(scientificName) %>% 
  summarise(sum(count)) %>% 
  select(word = scientificName, freq = "sum(count)") %>% 
  drop_na()

rownames(species) <- species$word

# library
library(treemap)

treemap(species,
        index="word",
        vSize="freq",
        type="index",
        title = "",
        palette = "BuGn"
)



# Update VMM 2022

obs_dep %>%
  filter(observationType != "unclassified") %>% 
  ggplot(aes(x = observationType)) +
  geom_histogram(stat = "count", fill = "forestgreen") +
  xlab("") + ylab("") +
  facet_wrap(Year)

# Update 2023

subset <- observations %>% 
  filter(scientificName %in% c("Ondatra zibethicus", "Myocastor coypus")) %>% 
  left_join(deployments, "deploymentID") %>% 
  select(timestamp, scientificName, count, locationName, longitude, latitude) %>% 
  arrange(locationName, timestamp)

write.xlsx(subset, file = "./output/camera_observations.xlsx")


