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


test2 <- full_data %>% 
  filter(!`Volgens AI` %in% c("Anas platyrhynchos", "Anas strepera", "Anser", 
                              "Ardea", "Fulica atra", "Gallinula chloropus",
                             "Rodentia", "Lege sequentie")) %>% 
  mutate(correct = ifelse(`Volgens AI` == `Correcte soort`, "ja", "nee")) %>% 
  select(-link, -Link)
