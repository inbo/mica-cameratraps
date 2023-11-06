### ----------------------------------------------------------------------------
### Analysis of camera trap images
### Last update: 2022-11-25
### ----------------------------------------------------------------------------
rm(list = ls())
library(readr)
library(openxlsx)
library(lubridate)
library(tidyverse)


observations <- 
  read_csv("input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230922141344/observations.csv", 
                         col_types = cols(comments = col_character())) %>% 
  mutate(Year = year(timestamp))

deployments <-  read_csv("input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230922141344/deployments.csv")

media <- read_csv("input/mica-management-of-invasive-coypu-and-muskrat-in-europe-20230710120748/media.csv")

obs_dep <- observations %>% 
  left_join(deployments, "deploymentID")

start_obs <- obs_dep %>% 
  filter(locationName %in% c("Mica Herne",
                              "Mica Viane"))

start_obs %>% 
  group_by(scientificName) %>% 
  summarise(sum(count))

setup_date <- obs_dep %>% 
  group_by(locationName) %>% 
  summarise(min(timestamp, na.rm = TRUE),
            max(timestamp, na.rm = TRUE)) #2019-09-18 08:24:59 - 2023-05-03 10:47:24

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

#Update 09/2023
#location map visualization
library(sf)
library(mapview)

subsetMV<-st_as_sf(subset,coords = c("longitude","latitude"),crs=4326)
mapview(subsetMV,zcol="scientificName")

subsetAllSN <- observations %>% 
  left_join(deployments, "deploymentID") %>% 
  select(timestamp, scientificName, count, locationName, longitude, latitude) %>% 
  arrange(locationName, timestamp)

#De Luizen
B_DL_obs <- subsetAllSN %>% 
  filter(locationName %in% c("B_DL_val 1_beek nieuwe vijvers",
                              "B_DL_val 2_emissaire",
                              "B_DL_val 3_dikke boom",
                              "B_DL_val 4_grensgracht",
                              "B_DL_val 4_grote vijver",
                              "B_DL_val 5_beek kleine vijver",
                              "B_DL_val 5_eilandje kleine vijver"))

B_DL_obs %>% 
  group_by(scientificName) %>% 
  summarise(sum(count))%>%
  select(word = scientificName, freq = "sum(count)") %>% 
  na.omit() %>%
  mutate(Percentage = freq/sum(freq)) %>%
  filter(word %in% c("Ondatra zibethicus","Myocastor coypus")) #Ondatra accounted for 0,2% and Myocastor for 0,5% of all the observations


#De Markvallei
B_DM_obs <- subsetAllSN %>% 
  filter(locationName %in% c("B_DM_val 1_weerstation",
                             "B_DM_val 2_Aloam",
                             "B_DM_val 3_waterzuivering"))

B_DM_obs %>% 
  group_by(scientificName) %>% 
  summarise(sum(count))%>%
  select(word = scientificName, freq = "sum(count)") %>% 
  na.omit() %>%
  mutate(Percentage = freq/sum(freq)) %>%
  filter(word %in% c("Ondatra zibethicus","Myocastor coypus")) #Ondatra accounted for 0,2% and Myocastor for 0,008% of all the observations


#Hoogstraten
B_HS_obs <- subsetAllSN %>% 
  filter(locationName %in% c("B_HS_val 1_markweg",
                             "B_HS_val 2_processiepark",
                             "B_HS_val 3_meander ANB",
                             "B_HS_val 4_Oosteneind",
                             "B_HS_val 5_zeemansbrug",
                             "B_HS_val 6_keerschot"))

B_HS_obs %>% 
  group_by(scientificName) %>% 
  summarise(sum(count))%>%
  select(word = scientificName, freq = "sum(count)") %>% 
  na.omit() %>%
  mutate(Percentage = freq/sum(freq)) %>%
  filter(word %in% c("Ondatra zibethicus","Myocastor coypus")) #Ondatra accounted for 0,7% of all the observations



#Meetjesland
B_ML_obs <- subsetAllSN %>% 
  filter(locationName %in% c("B_ML_val 01_vrouwkeshoek",
                             "B_ML_val 02_camping grenspost",
                             "B_ML_val 03_De Val",
                             "B_ML_val 04_Roeselaerekreek",
                             "B_ML_val 05_molenkreek",
                             "B_ML_val 06_Oostpolderkreek",
                             "B_ML_val 07_Sint-Anna",
                             "B_ML_val 08_grenspost vrouwkeshoek",
                             "B_ML_val 09_kapitale damm",
                             "B_ML_val 10_Sint-Barbara"))

B_ML_obs %>% 
  group_by(scientificName) %>% 
  summarise(sum(count)) %>%
  select(word = scientificName, freq = "sum(count)") %>% 
  na.omit() %>%
  mutate(Percentage = freq/sum(freq)) %>%
  filter(word %in% c("Ondatra zibethicus","Myocastor coypus")) #Ondatra accounted for 7% and Myocastor for 0,03% of all the observations


#Nederland
nederland <- subsetAllSN %>% 
  filter(str_detect(locationName, "NL."))

nederland %>% 
  group_by(scientificName) %>% 
  summarise(sum(count))%>%
  select(word = scientificName, freq = "sum(count)") %>% 
  na.omit() %>%
  mutate(Percentage = freq/sum(freq)) %>%
  filter(word %in% c("Ondatra zibethicus","Myocastor coypus")) #Ondatra accounted for 0,1% of all the observations

#Duitsland lake Dümmer
duitslandLD <- subsetAllSN %>% 
  filter(str_detect(locationName, "D_LD_lake Dümmer"))

#Duitsland Mica
duitslandMICA <- subsetAllSN %>% 
  filter(str_detect(locationName, "D_MICA"))

duitsland %>% 
  group_by(scientificName) %>% 
  summarise(sum(count)) %>%
  select(word = scientificName, freq = "sum(count)") %>% 
  na.omit() %>%
  mutate(Percentage = freq/sum(freq)) %>%
  filter(word %in% c("Ondatra zibethicus","Myocastor coypus")) #Ondatra accounted for 24% and Myocastor for 7% of all the observations

#totaal
species<-subsetAllSN %>%
  group_by(scientificName) %>% 
  summarise(sum(count)) %>% 
  select(word = scientificName, freq = "sum(count)") %>%
  na.omit() %>%
  mutate(Percentage = freq/sum(freq)) %>%
  filter(word %in% c("Ondatra zibethicus","Myocastor coypus")) #Ondatra accounted for 6% and Myocastor for 1% of all the observations
  
species<-subsetAllSN %>%
  group_by(scientificName) %>% 
  summarise(sum(count)) %>% 
  select(word = scientificName, freq = "sum(count)") %>%
  na.omit() %>%
  mutate(Percentage = freq/sum(freq))

species[order(species$Percentage,decreasing = TRUE),]

rownames(species) <- species$word

library(treemap)

treemap(species,
        index="word",
        vSize="freq",
        type="index",
        title = "",
        palette = "BuGn"
)

#observed trough the year 

library(ggplot2)
library(dplyr)
install.packages("viridis")
library(viridis)

#Ondatra zibethicus

subsetOZ <- observations %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  left_join(deployments, "deploymentID") %>% 
  select(timestamp, scientificName, count, locationName, longitude, latitude) %>% 
  arrange(locationName, timestamp)

subsetOZ2<-subsetOZ %>%
  mutate(location = if_else(locationName %in% c("B_DL_val 1_beek nieuwe vijvers",
                                                "B_DL_val 2_emissaire",
                                                "B_DL_val 3_dikke boom",
                                                "B_DL_val 4_grensgracht",
                                                "B_DL_val 4_grote vijver",
                                                "B_DL_val 5_beek kleine vijver",
                                                "B_DL_val 5_eilandje kleine vijver"), "B_DL",
                            if_else(locationName %in% c("B_DM_val 1_weerstation",
                                                        "B_DM_val 2_Aloam",
                                                        "B_DM_val 3_waterzuivering"), "B_DM",
                                    if_else(locationName %in% c("B_HS_val 1_markweg",
                                                                "B_HS_val 2_processiepark",
                                                                "B_HS_val 3_meander ANB",
                                                                "B_HS_val 4_Oosteneind",
                                                                "B_HS_val 5_zeemansbrug",
                                                                "B_HS_val 6_keerschot"),"B_HS",
                                            if_else(locationName %in% c("B_ML_val 01_vrouwkeshoek",
                                                                        "B_ML_val 02_camping grenspost",
                                                                        "B_ML_val 03_De Val",
                                                                        "B_ML_val 04_Roeselaerekreek",
                                                                        "B_ML_val 05_molenkreek",
                                                                        "B_ML_val 06_Oostpolderkreek",
                                                                        "B_ML_val 07_Sint-Anna",
                                                                        "B_ML_val 08_grenspost vrouwkeshoek",
                                                                        "B_ML_val 09_kapitale damm",
                                                                        "B_ML_val 10_Sint-Barbara"), "B_ML",
                                                    if_else(locationName %in% c("D_LD_lake Dümmer"), "D_LD",
                                                            if_else(locationName %in% c("D_MICA 323",
                                                                                        "D_MICA 324",
                                                                                        "D_MICA 325",
                                                                                        "D_MICA 326",
                                                                                        "D_MICA 327",
                                                                                        "D_MICA 328",
                                                                                        "D_MICA 329"),"D_MICA",
                                                                    if_else(locationName %in% c("NL_cam 331_boukwyk Harkema",
                                                                                                "NL_cam 332_Tjongervallei Tjonger",
                                                                                                "NL_cam 333_Zevenaarpad Tjongervallei",
                                                                                                "NL_cam 335_Sondelerdijk Sondel",
                                                                                                "NL_cam 336_nationaal park Lauwersmeer Bantswei",
                                                                                                "NL_cam 337_Serhuizum",
                                                                                                "NL_cam 338_Bremerwei Bakkeveen",
                                                                                                "NL_cam 339_Frieseweg Rutten",
                                                                                                "NL_cam 340_slijkenburg lindedijk"),"NL","test"))))))))
subsetOZ2 %>% 
  ggplot(aes(timestamp, group=location, color=location)) + 
  geom_freqpoly(binwidth = 86400) +
  labs(x="time", y="number of Ondatra zibethicus observed") +
  scale_color_manual(values=c("black", "darkred", "red","orange","blue","green","deeppink","grey"))



subsetOZ %>% 
  count(week = floor_date(timestamp, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

#Myocastor coypus

subsetMC <- observations %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  left_join(deployments, "deploymentID") %>% 
  select(timestamp, scientificName, count, locationName, longitude, latitude) %>% 
  arrange(locationName, timestamp)

subsetMC2<-subsetMC %>%
  mutate(location = if_else(locationName %in% c("B_DL_val 1_beek nieuwe vijvers",
                                                "B_DL_val 2_emissaire",
                                                "B_DL_val 3_dikke boom",
                                                "B_DL_val 4_grensgracht",
                                                "B_DL_val 4_grote vijver",
                                                "B_DL_val 5_beek kleine vijver",
                                                "B_DL_val 5_eilandje kleine vijver"), "B_DL",
                            if_else(locationName %in% c("B_DM_val 1_weerstation",
                                                        "B_DM_val 2_Aloam",
                                                        "B_DM_val 3_waterzuivering"), "B_DM",
                                    if_else(locationName %in% c("B_HS_val 1_markweg",
                                                                "B_HS_val 2_processiepark",
                                                                "B_HS_val 3_meander ANB",
                                                                "B_HS_val 4_Oosteneind",
                                                                "B_HS_val 5_zeemansbrug",
                                                                "B_HS_val 6_keerschot"),"B_HS",
                                           if_else(locationName %in% c("B_ML_val 01_vrouwkeshoek",
                                                                       "B_ML_val 02_camping grenspost",
                                                                       "B_ML_val 03_De Val",
                                                                       "B_ML_val 04_Roeselaerekreek",
                                                                       "B_ML_val 05_molenkreek",
                                                                       "B_ML_val 06_Oostpolderkreek",
                                                                       "B_ML_val 07_Sint-Anna",
                                                                       "B_ML_val 08_grenspost vrouwkeshoek",
                                                                       "B_ML_val 09_kapitale damm",
                                                                       "B_ML_val 10_Sint-Barbara"), "B_ML",
                                                   if_else(locationName %in% c("D_LD_lake Dümmer"), "D_LD",
                                                           if_else(locationName %in% c("D_MICA 323",
                                                                                       "D_MICA 324",
                                                                                       "D_MICA 325",
                                                                                       "D_MICA 326",
                                                                                       "D_MICA 327",
                                                                                       "D_MICA 328",
                                                                                       "D_MICA 329"),"D_MICA","")))))))
subsetMC2 %>% 
  ggplot(aes(timestamp, group=location, color=location)) + 
  geom_freqpoly(binwidth = 86400) +
  labs(x="time", y="number of Myocastor coypus observed") +
  scale_color_manual(values=c("red", "green", "black"))

subsetMC %>% 
  count(week = floor_date(timestamp, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

#observed through the day

#Ondatra zibethicus

subsetOZ %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()
#OZ voornamelijk waargenomen in de nacht (tussen 18u en 4u)

B_DL_obs %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

B_DM_obs %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

B_HS_obs %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

B_ML_obs %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

nederland %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

duitslandLD %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

duitslandMICA %>% 
  filter(scientificName %in% c("Ondatra zibethicus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

#Myocastor coypus

subsetMC %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()
#MC voornamelijk waargenomen in de nacht (tussen 18u en 5u)

B_DL_obs %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

B_DM_obs %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

B_HS_obs %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

B_ML_obs %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

nederland %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

duitslandLD %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

duitslandMICA %>% 
  filter(scientificName %in% c("Myocastor coypus")) %>% 
  mutate(hour = hour(timestamp)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()


