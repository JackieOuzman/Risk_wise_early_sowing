### adjusting the climate files using Bonnies method
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Download Daily climate files -------------------------------------------------------

climate <- read.table("D:/work/RiskWise/met_files/LOCK_18046.sim", 
                      skip = 22, header = TRUE, sep ="")
climate <- climate [-1,]
### need to make a clm that has all the dates not just day of year and year
str(climate)
climate$rain <- as.character(climate$rain)
climate$rain <- as.double(climate$rain)

#create a date clm # note you need to be aware of the last day of downloaded met data

## Assign dates, site detial and format -------------------------------------------------
download_date <- read_csv("D:/work/RiskWise/met_files/LOCK_18046.sim",
                          col_names = FALSE, skip = 7)
download_date <-download_date[1,1] #just the row with the download date
download_date <-stringr::str_extract(download_date, "[[:digit:]]+") #just the numbers
download_date <- as.Date(as.character(download_date),format="%Y%m%d")

#minus one day from download
download_date <- lubridate::ymd(download_date) - days(1)
str(download_date)

download_date <-"20250403" # "2025/04/03"
download_date <- as.Date(as.character(download_date),format="%Y%m%d")
download_date



climate <- climate %>% 
  mutate(date = seq(as.Date("1900/1/1"), download_date, "days"))
# set date as a date
climate <- climate %>% 
  mutate(year = year(date),
         month =month(date),
         day_of_month = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0("Lock","_", 018046))
str(climate)

climate$maxt <- as.numeric(climate$maxt)
climate$mint <- as.numeric(climate$mint)


# Rules to create a frosty version of Daily climate files -------------------------------------------------------

str(climate)

climate <- climate %>% 
  mutate(
    mint_new = case_when(
    mint <=       -1.0 ~        (mint+ -1),
    between(mint, -1.01, 2.0) ~ (mint+ -2),
    mint >=        2.1 ~        (mint),
    .default = -999
  ))


################################################################################

climate_2023 <- climate %>% filter(year == 2023 ) %>% 
  filter( month == 4 | month == 5 |month == 6| month == 7 |month == 8) %>% 
  select(year, date, month, month_name, mint, mint_new)

str(climate_2023)

climate_2023_long <- climate_2023 %>% pivot_longer(
  cols = starts_with("mint"),
  names_to = "mintemp_type",
  values_to = "mintemp"
)
str(climate_2023_long)

climate_2023_long %>% 
ggplot(aes(x = date , y = mintemp, colour = mintemp_type )) +
  geom_point()



################################################################################
## how to save the file in the same format reday for APSIM

str(climate)
climate1 <- climate %>% 
    select(year:code, mint_new) %>% 
    select(- mint) %>% 
    rename(mint = mint_new  )
str(climate1)    
climate1 <- climate1 %>% 
  select(year:maxt,mint, rain: code)



write.csv(climate1 ,
          "D:/work/RiskWise/met_files/LOCK_adjusted.csv", row.names = FALSE )
