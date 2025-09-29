library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
#on virtual machine its N not D
file_path1  <- "N:/work/Riskwise/early_sowing/"

#file_path1  <- "D:/work/Riskwise/early_sowing/"
site        <- "Lock"
file_path2  <- "/APSIM_runs/Canola/"
file_name   <- "Lock_fixed_Pengcheng_HF_rule_canola.Daily.csv"

  
Early_sowing_Lock <- read_csv(paste0(
  file_path1,
  site,
  file_path2,
  file_name))
  


Decile_18046 <- read_csv(paste0(
  file_path1,
  site,
  "/Deciles/",
  "deciles per year_Lock18046.csv"))
  

str(Early_sowing_Lock)
str(Decile_18046)

### get year from date clm

Early_sowing_Lock <- Early_sowing_Lock %>%
  mutate(year = year(Clock.Today))
#create a sowing date clm
Early_sowing_Lock <- Early_sowing_Lock %>% 
  mutate(sowing_date1 = paste0(StartDate,"-", year)) %>% 
  mutate(sowing_date = as.Date(sowing_date1,format = "%d-%B-%Y") ) %>% 
  select(-sowing_date1)

check <- Early_sowing_Lock %>% select( Clock.Today,  SimulationName, StartDate,year, sowing_date )
str(check)
Check_max_date <- max( check$sowing_date   )
Check_max_date #should be 2024 -07-31

something_wrong <- Early_sowing_Lock %>% filter(year == 2016)

### merge the data

Early_sowing_Lock_met_decile <- left_join(Early_sowing_Lock, Decile_18046, by = "year")
str(Early_sowing_Lock_met_decile)


## side step add germination to one data files
str(Early_sowing_Lock_met_decile)
germination_date <- Early_sowing_Lock_met_decile %>% 
  filter(Canola.Phenology.CurrentStageName == "Germination") %>% 
  select(year, StartDate, Clock.Today ) %>% 
  rename(germination_date = Clock.Today)


### get the harvest rows only
Early_sowing_Lock_met_decile_harvest <- Early_sowing_Lock_met_decile %>% 
   filter(Canola.Phenology.CurrentStageName == "HarvestRipe") %>% 
   select(year, StartDate, decile, Yield, FrostHeatDamageFunctions.FrostHeatYield, Canola.Phenology.CurrentStageName, Clock.Today, sowing_date ) %>% 
  rename(harvest_date = Clock.Today,
         FrostHeatYield = FrostHeatDamageFunctions.FrostHeatYield) %>% 
  select(-Canola.Phenology.CurrentStageName)

ES_Lock_met_decile_harvest_germ <- left_join(
  Early_sowing_Lock_met_decile_harvest, germination_date)

ES_Lock_met_decile_harvest_germ <- ES_Lock_met_decile_harvest_germ %>% 
  select(StartDate ,  year, sowing_date, germination_date, harvest_date, decile, FrostHeatYield,  Yield)

str(ES_Lock_met_decile_harvest_germ)





## cal some stats for each decile what is the bottom 10% quantile 

Quantile_sow_date_decile <- Early_sowing_Lock_met_decile %>% 
  group_by(StartDate,decile ) %>%
  summarize(quantile10Yld=quantile(Yield,probs=0.10),
            quantile50Yld=quantile(Yield,probs=0.5),
            quantile90Yld=quantile(Yield,probs=0.90),
            
            quantile10FrostYld=quantile(FrostHeatDamageFunctions.FrostHeatYield,probs=0.10),
            quantile50FrostYld=quantile(FrostHeatDamageFunctions.FrostHeatYield,probs=0.5),
            quantile90FrostYld=quantile(FrostHeatDamageFunctions.FrostHeatYield,probs=0.90))


Quantile_sow_date_decile <- Quantile_sow_date_decile %>% 
  mutate(site ="Lock_18046",
        Yrs_included_decile =  "1957 to 2024",
        GS_def_decile = "1/4 to 1/11",
        APSIM_sow_rule = "fixed sowing date") %>% 
  rename(Sowing_date =StartDate)
Quantile_sow_date_decile



write_csv(Quantile_sow_date_decile, 
          file =paste0(
            "N:/work/Riskwise/early_sowing/Lock/For_Decile_cal/Canola/",
                       "/APSIM_yld_Decile_Fixed_sow_date_Lock18046_quantile", ".csv"))









## before writing out the un summaries data with germination date. Just add a few things to help plot
str(ES_Lock_met_decile_harvest_germ)



ES_Lock_met_decile_harvest_germ <- ES_Lock_met_decile_harvest_germ %>% 
  mutate(
         DOY_sowing =      lubridate::yday( sowing_date),
         DOY_germination = lubridate::yday(germination_date),
         DOY_harvest =     lubridate::yday( harvest_date),
         CommonDate_sowing = ymd(paste0("2000-",str_sub(as.character(sowing_date),-5))),
         CommonDate_germination = ymd(paste0("2000-",str_sub(as.character(germination_date),-5))),
         CommonDate_harvest = ymd(paste0("2000-",str_sub(as.character(harvest_date),-5))),
         )

           
           
write_csv(ES_Lock_met_decile_harvest_germ, 
          file =paste0(
            "N:/work/Riskwise/early_sowing/Lock/For_Decile_cal/Canola/",
            "/APSIM_yld_Decile_GermDate_Lock18046_No_summary", ".csv"))


###############################################################################
## Plots ###
str(ES_Lock_met_decile_harvest_germ)

path_saved_files <- file_path_input_data<-file.path("N:","work", "Riskwise", "early_sowing", "Lock", "Results" , "Canola")

# non adjusted yld
plot1_box <- ES_Lock_met_decile_harvest_germ %>% 
  ggplot(aes(x =as.factor(DOY_sowing), Yield), group)+
  geom_boxplot()+
  #geom_point(alpha =0.2)+
  geom_point(data= ES_Lock_met_decile_harvest_germ %>% filter(year == 2023), 
             aes(x =as.factor(DOY_sowing), Yield), colour ="red")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        panel.border = element_rect(colour = "blue", fill=NA, linewidth=1))+
  
  ylim(0,600)+
  labs(title = "Yield vs early sowing dates Lock 18046.\nMust sow rule. No frost rule used or penalty applied",
       subtitle = "Canola crop, unadjusted climate file",
       y = "Yield",
       x ="Sowing date expressed as DOY",
       caption = "Red dot = 2023."
  )
plot1_box


ggsave(plot = plot1_box,
       filename = paste0(path_saved_files,"/Box_yield_vs_Sowing_dates_Lock_Canola", ".png" ),
       width = 20, height = 12, units = "cm")

################################################################################
## adjusted yld
plot2_box <- ES_Lock_met_decile_harvest_germ %>% 
  ggplot(aes(x =as.factor(DOY_sowing),  FrostHeatYield), group)+
  geom_boxplot()+
  #geom_point(alpha =0.2)+
  geom_point(data= ES_Lock_met_decile_harvest_germ %>% filter(year == 2023), 
             aes(x =as.factor(DOY_sowing), Yield), colour ="red")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        panel.border = element_rect(colour = "blue", fill=NA, linewidth=1))+
  
  ylim(0,600)+
  labs(title = "Yield vs early sowing dates Lock 18046.\nMust sow rule. FrostHeatDamageFunctions penalty applied",
       subtitle = "Canola crop, unadjusted climate file",
       y = "Yield",
       x ="Sowing date expressed as DOY",
       caption = "Red dot = 2023."
  )
plot2_box


ggsave(plot = plot2_box,
       filename = paste0(path_saved_files,"/Box_Frostyield_vs_Sowing_dates_Lock_Canola", ".png" ),
       width = 20, height = 12, units = "cm")



################################################################################
## both
str(ES_Lock_met_decile_harvest_germ)
ES_Lock_met_decile_harvest_germ_summary <- ES_Lock_met_decile_harvest_germ %>% 
  group_by(DOY_sowing) %>% 
  summarise(Yield_mean = mean(Yield, na.rm = TRUE),
            FrostHeatYield_mean = mean(FrostHeatYield, na.rm = TRUE))
str(ES_Lock_met_decile_harvest_germ_summary)


plot3_point <- ES_Lock_met_decile_harvest_germ_summary %>% 
  ggplot(aes(x =as.factor(DOY_sowing),  Yield_mean))+
  geom_point()+
  geom_point(data= ES_Lock_met_decile_harvest_germ_summary, 
             aes(x =as.factor(DOY_sowing), FrostHeatYield_mean), colour ="blue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  
  
  labs(title = "Mean Yield vs early sowing dates Lock 18046.\nMust sow rule. \nYield and Frost Damage Functions Yield",
       subtitle = "Canola crop, unadjusted climate file",
       y = "Mean Yield for Sowing Date",
       x ="Sowing date expressed as DOY",
       caption = "Blue = Mean Frost Yield, Black = Mean Yield"
  )
plot3_point
ggsave(plot = plot3_point,
       filename = paste0(path_saved_files,"/Box_Frostyield_and_yldvs_Sowing_dates_Lock_Canola", ".png" ),
       width = 20, height = 12, units = "cm")


################################################################################
## histogram
str(ES_Lock_met_decile_harvest_germ)

## make data long first 
ES_Lock_met_decile_harvest_germ_long <- ES_Lock_met_decile_harvest_germ %>%
  pivot_longer(
    cols = contains("yield"), # Select columns to pivot
    names_to = "yield_Type",     # Name of the new column for original column names
    values_to = "yield_Value"    # Name of the new column for values
  )

str(ES_Lock_met_decile_harvest_germ_long)


plot1_hisrogram <-  ES_Lock_met_decile_harvest_germ_long %>% 
  filter( yield_Value!=0) %>% 
  ggplot( aes(x=yield_Value, color=yield_Type, fill = yield_Type)) +
  geom_histogram( alpha=0.2, position="identity")+
  labs(title = "Lock Histogram Canola Yield - Frost adjustment and No adjustmentn\nZero yield filtered out",
       subtitle = "Climate file not adjusted",
       x = "Yield",
       y = "Frequency") +
  theme_minimal()
  
plot1_hisrogram


plot2_hisrogram <- ES_Lock_met_decile_harvest_germ_long %>% 
  filter( yield_Value!=0) %>% 
  ggplot( aes(x=yield_Value,color=yield_Type, fill = yield_Type)) +
  geom_histogram( )+
  labs(title = "Lock Histogram Wheat Yield - Frost adjustment and No adjustment\nZero yield filtered out",
       subtitle = "Climate file not adjusted",
       x = "Yield",
       y = "Frequency") +
  theme_minimal()+
  facet_wrap(. ~ yield_Type)

  plot2_hisrogram


ggsave(plot = plot1_hisrogram,
       filename = paste0(path_saved_files,"/histogram_Frostyield_and_yldvs_Sowing_dates_Lock_canola", ".png" ),
       width = 20, height = 12, units = "cm")
ggsave(plot = plot2_hisrogram,
       filename = paste0(path_saved_files,"/histogram_facet_Frostyield_and_yldvs_Sowing_dates_Lock_canola", ".png" ),
       width = 20, height = 12, units = "cm")
