library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(DBI)
library(RSQLite)


#on virtual machine its N not D
file_path1  <- "N:/work/Riskwise/early_sowing/"

#file_path1  <- "D:/work/Riskwise/early_sowing/"
site        <- "Lock"
file_path2  <- "/APSIM_runs/Barley/"
file_name   <- "Lock_fixed_Pengcheng_HF_rule_barley at harvest.db"
#"D:\work\RiskWise\early_sowing\Lock\APSIM_runs\Wheat\Lock_fixed_Pengcheng_HF_rule.db"
con <- dbConnect(SQLite(),  "N:/work/RiskWise/early_sowing/Lock/APSIM_runs/Barley/Lock_fixed_Pengcheng_HF_rule_barley at harvest.db")
dbListTables(con)
data <- dbGetQuery(con, "SELECT * FROM HarvestReport")
dbDisconnect(con)
rm(con)


names(data)

Early_sowing_Lock <-data
rm(data)






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

check <- Early_sowing_Lock %>% select( Clock.Today,  StartDate,year, sowing_date )
str(check)
Check_max_date <- max( check$sowing_date   )
Check_max_date #should be 2024 -07-31
rm( Check_max_date,check )

### merge the data

Early_sowing_Lock_met_decile <- left_join(Early_sowing_Lock, Decile_18046, by = "year")
str(Early_sowing_Lock_met_decile)




### get the harvest rows only
Early_sowing_Lock_met_decile <- Early_sowing_Lock_met_decile %>% 
  
  select(year, 
         StartDate, 
         decile, 
         Yield, 
         ##FrostHeatDamageFunctions.FrostHeatYield, 
         ##FrostHeatDamageFunctions.FrostEventNumber,
         
         Clock.Today, 
         sowing_date,
         InCropRainfall,
         AllYearRainfall,
         InCropMeanSoilWater) #%>% 
  
  #rename(FrostHeatYield = FrostHeatDamageFunctions.FrostHeatYield) 



Early_sowing_Lock_met_decile <-Early_sowing_Lock_met_decile %>% 
  #mutate(harvest_date = as.Date(Clock.Today,format = "%Y-%B-%d %H:%M:%S") ) #"%d-%B-%Y"
  mutate(harvest_date =  as.POSIXct(Clock.Today, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(harvest_date = as.Date(harvest_date)) 
  

str(Early_sowing_Lock_met_decile)

Early_sowing_Lock_met_decile <- Early_sowing_Lock_met_decile %>% 
  mutate(
    DOY_sowing =      lubridate::yday( sowing_date),
    DOY_harvest =     lubridate::yday( harvest_date),
    CommonDate_sowing = ymd(paste0("2000-",str_sub(as.character(sowing_date),-5))),
    CommonDate_harvest = ymd(paste0("2000-",str_sub(as.character(harvest_date),-5))),
  )



write_csv(Early_sowing_Lock_met_decile, 
          file =paste0(
            "N:/work/Riskwise/early_sowing/Lock/For_Decile_cal/Barley/",
            "/APSIM_yld_Decile_Lock18046_No_summary", ".csv"))


###############################################################################