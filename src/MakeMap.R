## Map project areas

library(sf)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

countries <- st_read("./input/Europa_landen_WGS1984.shp")

countries <- countries %>% 
  select(NAME, geometry) %>% 
  filter(NAME %in% c("Deutschland", "Belgique-België", "Nederland"))

leaflet(countries) %>% 
  addPolygons(color = c("#373956", "#71c3da", "#19b4aa"),
              fillOpacity = 1,
              weight = 1) %>% 
  addCircleMarkers(lng = c(4.741801900599858, 3.5743262676459073,
                     3.903916099567944, 5.642211570754315,
                     7.075946157521134, 5.831708430328752,
                     6.0019211084358535, 10.263121572513155,
                     8.334345147465388, 6.87704006447081,
                     4.873681359170746),
             lat = c(51.44858550342504, 51.2709571450602,
                     50.783819503366175, 51.18323675978413,
                     53.007380247552504, 53.131765081625765,
                     51.889727583440234, 52.752969128628735,
                     52.51632351354496, 52.61106411086103,
                     52.763858043232055),
             color = "#f9bf32",
             fillOpacity = 1,
             radius = 3) %>% 
  setMapWidgetStyle(list(background = "white"))

# Blauw, Geel, Groen, Zwartgrijs
c("#71c3da", "#f9bf32", "#19b4aa", "#373956")






