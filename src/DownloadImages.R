## Function to download all images of certain species

download_images <- function(species, folder) {
  #' Create folder with all images of specific species
  #'
  #' @param species Character string. Species of interest.
  #' 
  #' @param folder Character string. Folder found under input folder.
  
  library(tidyverse)
  
  output_folder <- paste0("./output/", species)
  
  if(!dir.exists(output_folder)){
    dir.create(output_folder)
  }
  
  multimedia <- readr::read_csv(paste0("./input/", folder, "/media.csv"), 
                         col_types = cols(comments = col_character()))
  
  observations <- readr::read_csv(paste0("./input/", folder, "/observations.csv"), 
                           col_types = cols(comments = col_character())) %>% 
    filter(scientificName == species) %>% 
    left_join(multimedia, by = "sequenceID")
  
  for (i in 1:nrow(observations)){
    filename <- paste0(species,i,".jpeg")
    
    download.file(observations$filePath[i],
                  destfile = paste0("./output/", species, "/", filename),
                  method = 'curl', 
                  overwrite = TRUE)
  }
}


download_images("Myocastor coypus", 
      "mica-management-of-invasive-coypu-and-muskrat-in-europe-20221012105236")



