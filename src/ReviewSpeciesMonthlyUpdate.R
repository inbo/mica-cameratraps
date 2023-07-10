### ----------------------------------------------------------------------------
### Monthly update of species that need to be reviewed
### Life MICA
### ----------------------------------------------------------------------------

library(googlesheets4)
library(googledrive)
library(tidyverse)

observations <- 
  read_csv("./input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230306142500/observations.csv", 
           col_types = cols(comments = col_character()))

test <- observations %>% 
  group_by(scientificName, classificationMethod) %>% 
  summarise(count = n()) %>% 
  mutate(Reviewed_by = NA)

gs4_create(name = paste("Species list", Sys.Date()), sheets = test)

drive_mv(paste("Species list", Sys.Date()), path = "G:/Mijn Drive/PRJ_FAUNABEHEER/INBOPRJ-14891 - MICA/beeldmateriaal")

library(httr)    
set_config(use_proxy(url="10.3.100.207",port=8080))
