library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


Early_sowing_Lock_met_decile_wheat <- read_csv(paste0(
            "N:/work/Riskwise/early_sowing/Lock/For_Decile_cal/Wheat/",
            "/APSIM_yld_Decile_Lock18046_No_summary", ".csv"))
           
           
Early_sowing_Lock_met_decile_wheat <- Early_sowing_Lock_met_decile_wheat %>% 
  mutate( Yield =  Yield/10)




#### some info on frost days
str(Early_sowing_Lock_met_decile_wheat)
Ave_Sum_frost_days_sowing_date <- Early_sowing_Lock_met_decile_wheat %>% 
  group_by(CommonDate_sowing) %>% 
  summarise(
    av_frost_days = mean(FrostHeatDamageFunctions.FrostEventNumber, na.rm = TRUE),
    sum_frost_days = sum(FrostHeatDamageFunctions.FrostEventNumber, na.rm = TRUE)
  )

Ave_Sum_frost_days_sowing_date

plot4_point <- Ave_Sum_frost_days_sowing_date %>% 
  ggplot(aes(x = CommonDate_sowing,  av_frost_days))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  labs(title = "Mean Number frost days vs early sowing dates Lock 18046.\nMust sow rule.",
       subtitle = "Wheat crop, unadjusted climate file",
       y = "Mean number of frost days for Sowing Date",
       x ="Sowing date common year"
  )
plot4_point

plot5_point <- Ave_Sum_frost_days_sowing_date %>% 
  ggplot(aes(x = CommonDate_sowing,  sum_frost_days))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  labs(title = "Sum Number frost days vs early sowing dates Lock 18046.\nMust sow rule.",
       subtitle = "Wheat crop, unadjusted climate file",
       y = "Sum number of frost days for Sowing Date",
       x ="Sowing date common year"
  )
plot5_point



ggsave(plot = plot4_point,
       filename = paste0(path_saved_files,"/Mean_no_FrostDaysvs_Sowing_dates_Lock_Wheat", ".png" ),
       width = 20, height = 12, units = "cm")

ggsave(plot = plot5_point,
       filename = paste0(path_saved_files,"/Sum_no_FrostDaysvs_Sowing_dates_Lock_Wheat", ".png" ),
       width = 20, height = 12, units = "cm")


### for sowing date 13-May

Bonnie_mean_sowing_date_Sum_frost_days <- Early_sowing_Lock_met_decile_wheat %>% 
  filter(CommonDate_sowing == "2000-05-13") %>% 
  group_by(CommonDate_sowing) %>% 
  summarise(
    count_years = n(),
    av_frost_days = mean(FrostHeatDamageFunctions.FrostEventNumber, na.rm = TRUE),
    sum_frost_days = sum(FrostHeatDamageFunctions.FrostEventNumber, na.rm = TRUE)
  )

Bonnie_mean_sowing_date_Sum_frost_days
###############################################################################
## Plots ###
str(Early_sowing_Lock_met_decile_wheat)



path_saved_files <- file_path_input_data<-file.path("N:","work", "Riskwise", "early_sowing", "Lock", "Results" , "Wheat")

Early_sowing_Lock_met_decile_wheat_long <- Early_sowing_Lock_met_decile_wheat %>% 
  pivot_longer(
    cols = contains("yield"), # Select columns to pivot
    names_to = "yield_Type",     # Name of the new column for original column names
    values_to = "yield_Value"    # Name of the new column for values
  )




# non adjusted yld
plot1_box <- Early_sowing_Lock_met_decile_wheat_long %>% 
  ggplot(aes(x =DOY_sowing, yield_Value, group = DOY_sowing))+
  geom_boxplot()+
  #geom_point(alpha =0.2)+
  geom_point(data= Early_sowing_Lock_met_decile_wheat_long %>% filter(year == 2023), 
             aes(x =DOY_sowing, yield_Value), colour ="red")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        panel.border = element_rect(colour = "blue", fill=NA, linewidth=1))+
  facet_wrap(.~yield_Type)+
  
  #ylim(0,600)+
  labs(title = "Yield vs early sowing dates Lock 18046.\nMust sow rule.",
       subtitle = "Wheat crop, unadjusted climate file. Irrigation at sowing",
       y = "Yield",
       x ="Sowing date expressed as DOY",
       caption = "Red dot = 2023."
  )
plot1_box


ggsave(plot = plot1_box,
       filename = paste0(path_saved_files,"/Box_yield_vs_Sowing_dates_Lock_Wheat", ".png" ),
       width = 20, height = 12, units = "cm")

################################################################################




################################################################################
## both
str(Early_sowing_Lock_met_decile_wheat)
Early_sowing_Lock_met_decile_wheat_summary <- Early_sowing_Lock_met_decile_wheat %>% 
  group_by(DOY_sowing) %>% 
  summarise(Yield_mean = mean(Yield, na.rm = TRUE),
            FrostHeatYield_mean = mean(FrostHeatYield, na.rm = TRUE))
str(Early_sowing_Lock_met_decile_wheat_summary)

Early_sowing_Lock_met_decile_wheat_summary_long <- Early_sowing_Lock_met_decile_wheat_summary %>% 
  pivot_longer(
    cols = contains("yield"), # Select columns to pivot
    names_to = "yield_Type",     # Name of the new column for original column names
    values_to = "yield_Value"    # Name of the new column for values
  )



plot3_point <- Early_sowing_Lock_met_decile_wheat_summary_long %>% 
  ggplot(aes(x =DOY_sowing,  yield_Value, colour = yield_Type))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  
  
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule. \nYield and Frost Damage Functions Yield",
       subtitle = "Wheat crop, unadjusted climate file",
       y = "Mean Yield for Sowing Date",
       x ="Sowing date expressed as DOY"
  )
plot3_point
ggsave(plot = plot3_point,
       filename = paste0(path_saved_files,"/Box_Frostyield_and_yldvs_Sowing_dates_Lock_Wheat", ".png" ),
       width = 20, height = 12, units = "cm")


################################################################################
## histogram
str(ES_Lock_met_decile_harvest_germ)


str(Early_sowing_Lock_met_decile_wheat_long)


plot1_hisrogram <-  ggplot(Early_sowing_Lock_met_decile_wheat_long, aes(x=yield_Value, color=yield_Type, fill = yield_Type)) +
  geom_histogram( alpha=0.2, position="identity")+
  labs(title = "Lock Histogram Wheat Yield - Frost adjustment and No adjustment",
       subtitle = "Climate file not adjusted",
       x = "Yield",
       y = "Frequency") +
  theme_minimal()
  
plot1_hisrogram


plot2_hisrogram <-  ggplot(Early_sowing_Lock_met_decile_wheat_long, aes(x=yield_Value,color=yield_Type, fill = yield_Type)) +
  geom_histogram( )+
  labs(title = "Lock Histogram Wheat Yield - Frost adjustment and No adjustment",
       subtitle = "Climate file not adjusted",
       x = "Yield",
       y = "Frequency") +
  theme_minimal()+
  facet_wrap(. ~ yield_Type)
plot2_hisrogram


ggsave(plot = plot1_hisrogram,
       filename = paste0(path_saved_files,"/histogram_Frostyield_and_yldvs_Sowing_dates_Lock_Wheat", ".png" ),
       width = 20, height = 12, units = "cm")
ggsave(plot = plot2_hisrogram,
       filename = paste0(path_saved_files,"/histogram_facet_Frostyield_and_yldvs_Sowing_dates_Lock_Wheat", ".png" ),
       width = 20, height = 12, units = "cm")
