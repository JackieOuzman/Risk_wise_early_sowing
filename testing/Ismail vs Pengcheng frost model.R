
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

### Compare wheat frost pen from ismail and Pengcheng

Pengcheng_wheat <- read_csv("N:/work/RiskWise/early_sowing/Lock/For_Decile_cal/Wheat/APSIM_yld_Decile_GermDate_Lock18046_No_summary.csv")
Ismail_wheat <- read_csv("N:/work/RiskWise/early_sowing/Lock/For_Decile_cal/Wheat_Ismail/APSIM_yld_Decile_GermDate_Lock18046_No_summary.csv")

names(Pengcheng_wheat)
str(Ismail_wheat)


Pengcheng_wheat <- Pengcheng_wheat %>% select(
  "StartDate"     ,
  "year"    ,
  "sowing_date" ,
  "Yield"  ,
  "FrostHeatYield" , 
  "DOY_sowing"
) %>% 
  rename( "Yield_Pengcheng" =  "Yield",
          "FrostHeatYield_Pengcheng" = "FrostHeatYield")


Ismail_wheat <- Ismail_wheat %>% select(
  "StartDate"     ,
  "year"    ,
  "sowing_date" ,
  "Yield"  ,
  "FrostHeatYield" , 
  "DOY_sowing"
) %>% 
  rename( "Yield_Ismail" =  "Yield",
          "FrostHeatYield_Ismail" = "FrostHeatYield")

names(Pengcheng_and_Ismail_wheat)


## make data long first 
Pengcheng_and_Ismail_wheat_long <- Pengcheng_and_Ismail_wheat %>%
  pivot_longer(
    cols = contains("yield"), # Select columns to pivot
    names_to = "yield_Type",     # Name of the new column for original column names
    values_to = "yield_Value" )   # Name of the new column for values


plot2_hisrogram <-  ggplot(Pengcheng_and_Ismail_wheat_long, aes(x=yield_Value,color=yield_Type, fill = yield_Type)) +
  geom_histogram( )+
  labs(title = "Lock Histogram Wheat Yield - Frost adjustment and No adjustment",
       subtitle = "Climate file not adjusted",
       x = "Yield",
       y = "Frequency") +
  theme_minimal()+
  facet_wrap(. ~ yield_Type)
plot2_hisrogram

## average data
names(Pengcheng_and_Ismail_wheat_long)

Pengcheng_and_Ismail_wheat_ave <- Pengcheng_and_Ismail_wheat_long %>% 
  group_by(yield_Type, DOY_sowing) %>% 
  summarise(  mean_yld = mean(yield_Value, na.rm = TRUE))


names(Pengcheng_and_Ismail_wheat_ave)

Pengcheng_and_Ismail_wheat_ave %>% 
  ggplot(aes(x =as.factor(DOY_sowing),  mean_yld, group =yield_Type, colour= yield_Type ))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  facet_wrap(.~yield_Type)+
  
  
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule.
       \nIsmail Yield and Frost penalty Vs Pengcheng Yield and Frost penalty",
       subtitle = "Wheat crop, unadjusted climate file",
       y = "Mean Yield for Sowing Date",
       x ="Sowing date expressed as DOY"
  )


Pengcheng_and_Ismail_wheat_ave %>% 
  ggplot(aes(x =as.factor(DOY_sowing),  mean_yld, group =yield_Type, colour= yield_Type ))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  #facet_wrap(.~yield_Type)+
  
  
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule.
       \nIsmail Yield and Frost penalty Vs Pengcheng Yield and Frost penalty",
       subtitle = "Wheat crop, unadjusted climate file",
       y = "Mean Yield for Sowing Date",
       x ="Sowing date expressed as DOY"
  )
