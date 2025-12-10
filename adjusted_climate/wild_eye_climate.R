### adjusting the climate files using Bonnies method
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Bring in wild eye data temp files -------------------------------------------------------

#site <- "PolkinghorneA_LockWstWS"
#site <- "Glover_G_GMH_WS"
#site <- "PolkinghorneA_KingWS"
#site <- "PopeB_WBooWS"

temperature <- read_csv(paste0("D:/work/RiskWise/met_files/wild_eye_farm_data/", site, "_10_12_25.csv"))
#temperature <- read_csv("D:/work/RiskWise/met_files/wild_eye_farm_data/Glover_G_GMH_WS_Temp10_12_25_v2.csv")

## Split the date clm 

names(temperature)
temperature <- temperature %>% rename(date_T = `...1`, Temperature_C = "Temperature (°C)") 

str(temperature)

temperature <- temperature %>% mutate(date_hms = dmy_hms(date_T))

temperature <- temperature %>% mutate(year = year(date_hms),
                                      date = date(date_hms),
                                      day = yday(date_hms)
                                      ) %>% 
  select(date, year, day, Temperature_C )



temperature_min_max <- temperature %>%  group_by(date, day) %>% 
  summarise(mint = min(Temperature_C),
            maxt = max(Temperature_C)) %>% 
  mutate(site = site)
         
temperature_min_max

write.csv(temperature_min_max ,
          paste0(
          "D:/work/RiskWise/met_files/wild_eye_farm_data/", site, ".csv"), row.names = FALSE )



###############################################################################
#Merge all the site into one file

#site <- "PolkinghorneA_LockWstWS" # PolkinghorneA_LockWstWS
#site <- "Glover_G_GMH_WS" # Glover_G_GMH_WS_Reformat
#site <- "PolkinghorneA_KingWS" PolkinghorneA_KingWS
#site <- "PopeB_WBooWS" # PopeB_WBooWS



site1 <- "PolkinghorneA_LockWstWS"
site2 <- "Glover_G_GMH_WS_Reformat"
site3 <- "PolkinghorneA_KingWS"
site4 <- "PopeB_WBooWS"




site1_Tempertaure <- read_csv(paste0("D:/work/RiskWise/met_files/wild_eye_farm_data/", site1, ".csv")) %>% 
  mutate(latitude = "-33.587979",
         longitude = "135.666561")


site2_Tempertaure <- read_csv(paste0("D:/work/RiskWise/met_files/wild_eye_farm_data/", site2, ".csv")) %>% 
  mutate(latitude = "-33.554869",
         longitude = "135.836610")


site3_Tempertaure <- read_csv(paste0("D:/work/RiskWise/met_files/wild_eye_farm_data/", site3, ".csv")) %>% 
  mutate(latitude = "-33.477542",
         longitude = "135.801123")


site4_Tempertaure <- read_csv(paste0("D:/work/RiskWise/met_files/wild_eye_farm_data/", site4, ".csv")) %>% 
  mutate(latitude = "-33.331867",
         longitude = "135.598628")
####
Wild_eye_Tempertaure <- rbind(site1_Tempertaure, site2_Tempertaure,site3_Tempertaure, site4_Tempertaure )



### Plot
Wild_eye_Tempertaure

Wild_eye_Tempertaure %>% 
ggplot(aes(x = date , y = mint   )) +
  geom_point(colour = "blue")+
  geom_point(aes(x = date , y = maxt ), colour = "green")+
  facet_wrap(.~site)+
  theme_bw()+
  labs(x = "Date", y = "Tempertaure (C)")
  


### Create a map data

library(leaflet)


Wild_eye_Tempertaure_coords <- Wild_eye_Tempertaure %>% 
  distinct(site, .keep_all = TRUE ) %>% 
  select(site , latitude,   longitude)

Wild_eye_Tempertaure_coords$latitude <- as.double(Wild_eye_Tempertaure_coords$latitude)
Wild_eye_Tempertaure_coords$longitude <- as.double(Wild_eye_Tempertaure_coords$longitude)

# Create a basic leaflet map
leaflet(Wild_eye_Tempertaure_coords) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~paste("<b>", site)
    #popup = ~paste("<b>", site, "</b><br/>Value:", value)
  )


### Add to this with the names of the sites as label not pop up and avaialble data range