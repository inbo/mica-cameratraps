### ----------------------------------------------------------------------------
### Checking AI accurateness
### ----------------------------------------------------------------------------

library(readr)
library(lubridate)
library(plotly)
library(crosstalk)
library(DT)
library(freqtables)
library(tidyverse)

observations <- 
  read_csv("./input/mica-muskrat-and-coypu-20220131140731/observations.csv", 
           col_types = cols(comments = col_character()))


preview <- observations %>% 
  filter(classifiedBy == "Western Europe species model Version 1")  
#  freq_table(scientificName)

ai_set <- unique(preview$deploymentID) 

preview2 <- observations %>% 
  filter(classifiedBy != "Western Europe species model Version 1",
         deploymentID %in% ai_set) %>%  
  freq_table(scientificName)

selection1 <- observations %>% 
  select(sequenceID, classifiedBy, scientificName, count) %>% 
  filter(classifiedBy == "Western Europe species model Version 1",
         scientificName == "Rodentia") %>%
  mutate(link = paste0("https://www.agouti.eu/#/project/86cabc14-d475-4439-98a7-e7b590bed60e/annotate/sequence/", sequenceID))

birds <- c("Anas platyrhynchos", "Anas strepera", "Anser", "Ardea",
           "Fulica atra", "Gallinula chloropus")

selection2 <- observations %>% 
  select(sequenceID, classifiedBy, scientificName, count) %>% 
  filter(classifiedBy == "Western Europe species model Version 1",
         scientificName %in% birds) %>%
  slice_sample(n = 150) %>%
  mutate(link = paste0("<a href='","https://www.agouti.eu/#/project/86cabc14-d475-4439-98a7-e7b590bed60e/annotate/sequence/",sequenceID,"' target='_blank'>","https://www.agouti.eu/#/project/86cabc14-d475-4439-98a7-e7b590bed60e/annotate/sequence/",sequenceID,"</a>"))

selection3 <- observations %>% 
  select(sequenceID, classifiedBy, scientificName, count) %>% 
  filter(classifiedBy == "Western Europe species model Version 1",
         !(scientificName %in% c(birds, "Rodentia")),
         !(is.na(scientificName))) %>%
  mutate(link = paste0("https://www.agouti.eu/#/project/86cabc14-d475-4439-98a7-e7b590bed60e/annotate/sequence/", sequenceID))

selection4 <- observations %>% 
  select(sequenceID, classifiedBy, scientificName, count) %>% 
  filter(classifiedBy == "Western Europe species model Version 1",
         is.na(scientificName)) %>%
  slice_sample(n = 150) %>%
  mutate(link = paste0("https://www.agouti.eu/#/project/86cabc14-d475-4439-98a7-e7b590bed60e/annotate/sequence/", sequenceID))

selection <- selection1 %>% 
  bind_rows(selection2) %>% 
  bind_rows(selection3) %>%
  bind_rows(selection4)

library(googlesheets4)
(ss <- gs4_create("Sample AI Agouti", sheets = selection))



## Create second dataset for review
# 663 rodentia
# 150 common birds:"Anas platyrhynchos", "Anas strepera", "Anser", "Ardea",
# "Fulica atra", "Gallinula chloropus"
# 150 other animals
# 150 blanks

new-observations <- 
  read_csv("./input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230601061026/observations.csv", 
           col_types = cols(comments = col_character()))

preview <- observations %>% 
  filter(classifiedBy == "Western Europe species model Version 4")  %>% 
  freq_table(scientificName)
