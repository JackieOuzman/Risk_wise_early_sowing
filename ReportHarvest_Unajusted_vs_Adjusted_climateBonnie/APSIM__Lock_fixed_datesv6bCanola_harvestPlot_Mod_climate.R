library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


Early_sowing_Lock_met_decile_FROSTY <- read_csv(paste0(
            "N:/work/Riskwise/early_sowing/Lock/For_Decile_cal/Canola",
            "/APSIM_yld_Decile_Lock18046_FROSTY_No_summary", ".csv"))
           
Early_sowing_Lock_met_decile_FROSTY <- Early_sowing_Lock_met_decile_FROSTY %>% 
  mutate(Yield = Yield/10)
 

Early_sowing_Lock_met_decile <- read_csv(paste0(
  "N:/work/Riskwise/early_sowing/Lock/For_Decile_cal/Canola/",
  "/APSIM_yld_Decile_Lock18046_No_summary", ".csv"))

Early_sowing_Lock_met_decile <- Early_sowing_Lock_met_decile %>% 
  mutate(Yield = Yield/10)


################################################################################
str(Early_sowing_Lock_met_decile_FROSTY)
str(Early_sowing_Lock_met_decile)

Early_sowing_Lock_met_decile1 <- Early_sowing_Lock_met_decile %>% 
  select(year, 
         StartDate, 
         decile, 
         Yield, 
         FrostHeatYield , 
         FrostHeatDamageFunctions.FrostEventNumber, 
         sowing_date, DOY_sowing, CommonDate_sowing) %>% 
  rename(Yield_std_climate = Yield,
         FrostHeatYield_std_climate = FrostHeatYield,
         FrostEventNumber_std_climate= FrostHeatDamageFunctions.FrostEventNumber) %>% 
  mutate(for_join = paste0(year,"_", StartDate))

Early_sowing_Lock_met_decile_FROSTY1 <- Early_sowing_Lock_met_decile_FROSTY %>% 
  select(year, 
         StartDate, 
         Yield, 
         FrostHeatYield , 
         FrostHeatDamageFunctions.FrostEventNumber) %>% 
  rename(Yield_Mod_climate = Yield,
         FrostHeatYield_Mod_climate = FrostHeatYield,
         FrostEventNumber_Mod_climate= FrostHeatDamageFunctions.FrostEventNumber) %>% 
  mutate(for_join = paste0(year,"_", StartDate)) %>% 
  select(-year, -StartDate)

str(Early_sowing_Lock_met_decile1)
str(Early_sowing_Lock_met_decile_FROSTY1)

Early_sowing_Lock_met_mod_std <- left_join(Early_sowing_Lock_met_decile1, 
                                           Early_sowing_Lock_met_decile_FROSTY1)


###############################################################################
### Frost days in 2023
count_frost_2023 <- Early_sowing_Lock_met_mod_std %>% 
  filter(year == 2023) 
str(count_frost_2023)

count_frost_2023_summary <- count_frost_2023 %>% 
  group_by(StartDate, year) %>% 
  summarise(
    frost_numb_std_climate = sum(FrostEventNumber_std_climate),
    frost_numb_Mod_climate = sum(FrostEventNumber_Mod_climate))
count_frost_2023_summary

### Frost days in all year


count_frost_summary <- Early_sowing_Lock_met_mod_std %>% 
  group_by(StartDate, CommonDate_sowing) %>% 
  summarise(
    frost_numb_std_climate = sum(FrostEventNumber_std_climate),
    frost_numb_Mod_climate = sum(FrostEventNumber_Mod_climate))
count_frost_summary

#Plot of all years
summary_data_long <-  count_frost_summary %>% 
  pivot_longer(
    cols = starts_with("frost"),
    names_to = "climate_file",
    values_to = "frost_count"
  )
summary_data_long <- ungroup(summary_data_long)

str(summary_data_long)
plot_frost_count <- summary_data_long %>% 
  ggplot(aes(x=CommonDate_sowing,frost_count, colour =  climate_file))+
  geom_point()+
  theme_classic()

plot_frost_count
## Plots ###
path_saved_files <- file_path_input_data<-file.path("N:","work", "Riskwise", "early_sowing", "Lock", "Results" , "Canola")

ggsave(plot = plot_frost_count,
       filename = paste0(path_saved_files,"/plot_frost_count_Lock_canola_All", ".png" ),
       width = 20, height = 12, units = "cm")





## both yields 
str(Early_sowing_Lock_met_mod_std)
Early_sowing_Lock_met_mod_std_long <- Early_sowing_Lock_met_mod_std %>%
  pivot_longer(
    cols = contains("yield"), # Select columns to pivot
    names_to = "yield_Type",     # Name of the new column for original column names
    values_to = "yield_Value"    # Name of the new column for values
  )

str(Early_sowing_Lock_met_mod_std_long)




plot1_box <- Early_sowing_Lock_met_mod_std_long %>% 
  ggplot(aes(x =DOY_sowing, yield_Value, group = DOY_sowing ))+
  geom_boxplot()+
  facet_wrap(.~yield_Type)+
  geom_point(data= Early_sowing_Lock_met_mod_std_long %>% filter(year == 2023), 
             aes(x =DOY_sowing, yield_Value), colour ="red")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  
  #ylim(0,600)+
  labs(title = "Yield vs early sowing dates Lock 18046.\nMust sow rule",
       subtitle = "Canola crop",
       y = "Yield",
       x ="Sowing date expressed as DOY",
       caption = "Red dot = 2023."
  )
plot1_box

ggsave(plot = plot1_box,
       filename = paste0(path_saved_files,"/Box_yield_vs_Sowing_dates_Lock_canola_All", ".png" ),
       width = 20, height = 12, units = "cm")




###############################################################################
str(Early_sowing_Lock_met_mod_std)
str(Early_sowing_Lock_met_mod_std_long)

Early_sowing_Lock_met_mod_std_long_summary <- Early_sowing_Lock_met_mod_std_long %>% 
  group_by(CommonDate_sowing,  yield_Type) %>% 
  summarise(Yield_mean = mean(yield_Value, na.rm = TRUE))


Early_sowing_Lock_met_mod_std_long_summary <- ungroup(Early_sowing_Lock_met_mod_std_long_summary)
str(Early_sowing_Lock_met_mod_std_long_summary)



plot3_point <- Early_sowing_Lock_met_mod_std_long_summary %>% 
  ggplot(aes(x =CommonDate_sowing,  Yield_mean, colour = yield_Type))+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  
  
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule.",
       subtitle = "Canola crop",
       y = "Mean Yield for Sowing Date",
       x =""
       
  )
plot3_point
ggsave(plot = plot3_point,
       filename = paste0(path_saved_files,"/Frostyield_and_yldvs_Sowing_dates_Lock_CanolaAll", ".png" ),
       width = 20, height = 12, units = "cm")



################################################################################
str(Early_sowing_Lock_met_mod_std_long)
plot2_hisrogram <-  ggplot(Early_sowing_Lock_met_mod_std_long, 
                           aes(x=yield_Value,color=yield_Type, fill = yield_Type)) +
  geom_histogram( )+
  labs(title = "Lock Histogram Canola Yield - Frost adjustment and No adjustment",
       subtitle = "Climate All",
       x = "Yield",
       y = "Frequency") +
  theme_minimal()+
  facet_wrap(. ~ yield_Type)
plot2_hisrogram


ggsave(plot = plot2_hisrogram,
       filename = paste0(path_saved_files,"/histogram_facet_Frostyield_and_yldvs_Sowing_dates_Lock_CanolaAll", ".png" ),
       width = 20, height = 12, units = "cm")
