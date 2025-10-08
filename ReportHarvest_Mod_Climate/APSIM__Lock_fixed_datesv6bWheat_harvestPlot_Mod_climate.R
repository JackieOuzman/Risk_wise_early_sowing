library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


Early_sowing_Lock_met_decile <- read_csv(paste0(
            "N:/work/Riskwise/early_sowing/Lock/For_Decile_cal/Wheat/",
            "/APSIM_yld_Decile_Lock18046_FROSTY_No_summary", ".csv"))
           
Early_sowing_Lock_met_decile <- Early_sowing_Lock_met_decile %>% 
  mutate(Yield = Yield/10)
 

## Plots ###
path_saved_files <- file_path_input_data<-file.path("N:","work", "Riskwise", "early_sowing", "Lock", "Results" , "Wheat")


## both yields 
str(Early_sowing_Lock_met_decile)
Early_sowing_Lock_met_decile_long <- Early_sowing_Lock_met_decile %>%
  pivot_longer(
    cols = contains("yield"), # Select columns to pivot
    names_to = "yield_Type",     # Name of the new column for original column names
    values_to = "yield_Value"    # Name of the new column for values
  )

str(Early_sowing_Lock_met_decile_long)




plot1_box <- Early_sowing_Lock_met_decile_long %>% 
  ggplot(aes(x =DOY_sowing, yield_Value, group = DOY_sowing ))+
  geom_boxplot()+
  facet_wrap(.~yield_Type)+
  geom_point(data= Early_sowing_Lock_met_decile_long %>% filter(year == 2023), 
             aes(x =DOY_sowing, yield_Value), colour ="red")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  
  #ylim(0,600)+
  labs(title = "Yield vs early sowing dates Lock 18046.\nMust sow rule",
       subtitle = "Canola crop, Adjusted climate file",
       y = "Yield",
       x ="Sowing date expressed as DOY",
       caption = "Red dot = 2023."
  )
plot1_box

ggsave(plot = plot1_box,
       filename = paste0(path_saved_files,"/Box_yield_vs_Sowing_dates_Lock_wheat_FROSTY", ".png" ),
       width = 20, height = 12, units = "cm")




###############################################################################
str(Early_sowing_Lock_met_decile)

Early_sowing_Lock_met_decile_summary <- Early_sowing_Lock_met_decile %>% 
  group_by(DOY_sowing) %>% 
  summarise(Yield_mean = mean(Yield, na.rm = TRUE),
            FrostHeatYield_mean = mean(FrostHeatYield, na.rm = TRUE))
str(Early_sowing_Lock_met_decile_summary)

Early_sowing_Lock_met_decile_summary_long <- Early_sowing_Lock_met_decile_summary %>%
  pivot_longer(
    cols = contains("yield"), # Select columns to pivot
    names_to = "yield_Type",     # Name of the new column for original column names
    values_to = "yield_Value"    # Name of the new column for values
  )


str(Early_sowing_Lock_met_decile_summary_long)

plot3_point <- Early_sowing_Lock_met_decile_summary_long %>% 
  ggplot(aes(x =DOY_sowing,  yield_Value, colour = yield_Type))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  
  
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule.",
       subtitle = "Wheat crop, Adjusted climate file",
       y = "Mean Yield for Sowing Date",
       x ="Sowing date expressed as DOY"
       
  )
plot3_point
ggsave(plot = plot3_point,
       filename = paste0(path_saved_files,"/Frostyield_and_yldvs_Sowing_dates_Lock_WheatFROSTY", ".png" ),
       width = 20, height = 12, units = "cm")



################################################################################
str(Early_sowing_Lock_met_decile_long)
plot2_hisrogram <-  ggplot(Early_sowing_Lock_met_decile_long, 
                           aes(x=yield_Value,color=yield_Type, fill = yield_Type)) +
  geom_histogram( )+
  labs(title = "Lock Histogram Canola Yield - Frost adjustment and No adjustment",
       subtitle = "Climate file Adjusted",
       x = "Yield",
       y = "Frequency") +
  theme_minimal()+
  facet_wrap(. ~ yield_Type)
plot2_hisrogram


ggsave(plot = plot2_hisrogram,
       filename = paste0(path_saved_files,"/histogram_facet_Frostyield_and_yldvs_Sowing_dates_Lock_WheatFROSTY", ".png" ),
       width = 20, height = 12, units = "cm")
