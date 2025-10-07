### Just the plots using harvest data 

library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

file_path1  <- "D:/work/Riskwise/early_sowing/" #(my computer)
#file_path1  <- "N:/work/Riskwise/early_sowing/" #(vitrual computer)
site        <- "Lock"
file_path2  <- "/For_Decile_cal/"
crop <- "Wheat/"
crop2 <- "canola/"
file_name   <- "APSIM_yld_Decile_GermDate_Lock18046_No_summary.csv"



Early_sowing_Lock <- read_csv(paste0(
  file_path1,
  site,
  file_path2,
  crop,
  file_name))


## Wheat Plots ###

## both yields 
str(Early_sowing_Lock)
Early_sowing_Lock_summary <- Early_sowing_Lock %>% 
  group_by(DOY_sowing) %>% 
  summarise(Yield_mean = mean(Yield, na.rm = TRUE),
            FrostHeatYield_mean = mean(FrostHeatYield, na.rm = TRUE))
str(Early_sowing_Lock_summary)

Early_sowing_Lock_summary <- Early_sowing_Lock_summary %>%  mutate()


plot3_point <- Early_sowing_Lock_summary %>% 
  ggplot(aes(x =DOY_sowing,  Yield_mean))+
  geom_point()+
  geom_point(data= Early_sowing_Lock_summary, 
             aes(x =DOY_sowing, FrostHeatYield_mean), colour ="blue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule. \nYield and Frost Damage Functions Yield",
       subtitle = "Wheat crop, unadjusted climate file",
       y = "Mean Yield for Sowing Date",
       x ="Sowing date expressed as DOY",
       caption = "Blue = Mean Frost Yield, Black = Mean Yield"
  )
plot3_point





Early_sowing_Lock <- read_csv(paste0(
  file_path1,
  site,
  file_path2,
  crop,
  file_name))


## Canola Plots ###

Early_sowing_Lock_canola <- read_csv(paste0(
  file_path1,
  site,
  file_path2,
  crop2,
  file_name))

## both yields 
str(Early_sowing_Lock_canola)
Early_sowing_Lock_canola_summary <- Early_sowing_Lock_canola %>% 
  group_by(DOY_sowing) %>% 
  summarise(Yield_mean = mean(Yield, na.rm = TRUE),
            FrostHeatYield_mean = mean(FrostHeatYield, na.rm = TRUE))
str(Early_sowing_Lock_canola_summary)




plot3_point <- Early_sowing_Lock_canola_summary %>% 
  ggplot(aes(x =DOY_sowing,  Yield_mean))+
  geom_point()+
  geom_point(data= Early_sowing_Lock_summary, 
             aes(x =DOY_sowing, FrostHeatYield_mean), colour ="blue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule. \nYield and Frost Damage Functions Yield",
       subtitle = "Canola crop, unadjusted climate file",
       y = "Mean Yield for Sowing Date",
       x ="Sowing date expressed as DOY",
       caption = "Blue = Mean Frost Yield, Black = Mean Yield"
  )
plot3_point
