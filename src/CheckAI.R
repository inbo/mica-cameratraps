## Checking AI accuracy 2022-02-15

library(googlesheets4)
library(freqtables)
library(tidyverse)

Axel <- read_sheet("https://docs.google.com/spreadsheets/d/1sYod7aLhlD6Eg-e_jo6qT7O57LOaZc4zy-aagmDZP2Q/edit#gid=2025723555",
                   col_types = "cncccncncn") 
Frank <- read_sheet("https://docs.google.com/spreadsheets/d/1sYod7aLhlD6Eg-e_jo6qT7O57LOaZc4zy-aagmDZP2Q/edit#gid=2025723555",
                    range = "Frank")
Emma <- read_sheet("https://docs.google.com/spreadsheets/d/1sYod7aLhlD6Eg-e_jo6qT7O57LOaZc4zy-aagmDZP2Q/edit#gid=2025723555",
                    range = "Emma")

full_data <- Axel %>% 
  add_row(Frank) %>% 
  add_row(Emma)

full_data %>% 
  filter(`Volgens AI` == "Lege sequentie") %>% 
  freq_table(`Correcte soort`)

test <- full_data %>% 
  filter(`Volgens AI` %in% c("Anas platyrhynchos", "Anas strepera", "Anser", "Ardea",
                             "Fulica atra", "Gallinula chloropus")) %>% 
  mutate(correct = ifelse(`Volgens AI` == `Correcte soort`, "ja", "nee")) %>% 
  select(-link, -Link)

test %>%
  group_by(correct) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) #AI voor 80,7% correct

test2 <- full_data %>% 
  filter(!`Volgens AI` %in% c("Anas platyrhynchos", "Anas strepera", "Anser", 
                              "Ardea", "Fulica atra", "Gallinula chloropus",
                             "Rodentia", "Lege sequentie")) %>% 
  mutate(correct = ifelse(`Volgens AI` == `Correcte soort`, "ja", "nee")) %>% 
  select(-link, -Link)



## Checking AI accuracy 2023-08-09
Axel2 <- read_sheet("https://docs.google.com/spreadsheets/d/1zkXqCjusoYUmi1WfI3mOMcbn7BcTqO4aIxqfwsFujNo/edit#gid=1594870574") 
Emma2 <- read_sheet("https://docs.google.com/spreadsheets/d/1zkXqCjusoYUmi1WfI3mOMcbn7BcTqO4aIxqfwsFujNo/edit#gid=1594870574",
                   range = "Emma")
Sanne2<- read_sheet("https://docs.google.com/spreadsheets/d/1zkXqCjusoYUmi1WfI3mOMcbn7BcTqO4aIxqfwsFujNo/edit#gid=1594870574",
                    range = "Sanne")

full_data2 <- Axel2 %>% 
  add_row(Emma2) %>% 
  add_row(Sanne2)

full_data2 %>% 
  filter(`scientificName` == "Lege sequentie") %>% 
  freq_table(`Correcte soort`)

testP2 <- full_data2 %>% 
  filter(`scientificName` %in% c("Anas platyrhynchos", "Anas strepera", "Anser", "Ardea",
                             "Fulica atra", "Gallinula chloropus")) %>% 
  mutate(correct2 = ifelse(`scientificName` == `Correcte soort`, "ja", "nee"))

testP2 %>%
  group_by(correct2) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) #AI voor 73,4% correct