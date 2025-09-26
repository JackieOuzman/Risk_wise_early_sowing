library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

file_path1  <- "X:/Riskwi$e/Dry_sowing/"
site        <- "Lock"
file_path2  <- "/APSIM_runs/"
file_name   <- "Lock_fixed.Daily.csv"

  
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
Check_max_date

### merge the data

Early_sowing_Lock_met_decile <- left_join(Early_sowing_Lock, Decile_18046, by = "year")
str(Early_sowing_Lock_met_decile)


## side step add germination to one data files
germination_date <- Early_sowing_Lock_met_decile %>% 
  filter(Wheat.Phenology.CurrentStageName == "Germination") %>% 
  select(year, StartDate, Clock.Today ) %>% 
  rename(germination_date = Clock.Today)


### get the harvest rows only
Early_sowing_Lock_met_decile_harvest <- Early_sowing_Lock_met_decile %>% 
   filter(Wheat.Phenology.CurrentStageName == "HarvestRipe") %>% 
   select(year, StartDate, decile, Yield_Adj_t_ha,  Yield, Wheat.Phenology.CurrentStageName, Clock.Today, sowing_date ) %>% 
  rename(harvest_date = Clock.Today) %>% 
  select(-Wheat.Phenology.CurrentStageName)

ES_Lock_met_decile_harvest_germ <- left_join(
  Early_sowing_Lock_met_decile_harvest, germination_date)

ES_Lock_met_decile_harvest_germ <- ES_Lock_met_decile_harvest_germ %>% 
  select(StartDate ,  year, sowing_date, germination_date, harvest_date, decile, Yield_Adj_t_ha,  Yield)

str(ES_Lock_met_decile_harvest_germ)





## cal some stats for each decile what is the bottom 10% quantile 

Quantile_sow_date_decile <- Early_sowing_Lock_met_decile %>% 
  group_by(StartDate,decile ) %>%
  summarize(quantile10=quantile(Yield_Adj_t_ha,probs=0.10),
            quantile50=quantile(Yield_Adj_t_ha,probs=0.5),
            quantile90=quantile(Yield_Adj_t_ha,probs=0.90))


Quantile_sow_date_decile <- Quantile_sow_date_decile %>% 
  mutate(site ="Lock_18046",
        Yrs_included_decile =  "1957 to 2024",
        GS_def_decile = "1/4 to 1/11",
        APSIM_sow_rule = "fixed sowing date") %>% 
  rename(Sowing_date =StartDate)
Quantile_sow_date_decile



write_csv(Quantile_sow_date_decile, 
          file =paste0(
            "X:/Riskwi$e/Dry_sowing/Lock/For_Decile_cal/", 
                       "/APSIM_yld_Decile_Fixed_sow_date_Lock18046", ".csv"))









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
          file =paste0("X:/Riskwi$e/Dry_sowing/Lock/for_Decile_cal", 
                       "/APSIM_yld_Decile_GermDate_Lock18046_No_summary", ".csv"))


###############################################################################
## Plots ###
str(ES_Lock_met_decile_harvest_germ)

path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Results" )

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
       subtitle = "Wheat crop, unadjusted climate file",
       y = "Yield",
       x ="Sowing date expressed as DOY",
       caption = "Red dot = 2023."
  )
plot1_box


ggsave(plot = plot1_box,
       filename = paste0(path_saved_files,"/Box_yield_vs_Sowing_dates_Lock_Wheat", ".png" ),
       width = 20, height = 12, units = "cm")

