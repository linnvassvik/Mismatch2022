library(dplyr)
library(readxl)
library(lubridate)

#eklima
eklima16 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Finse_weather_2016_ny.xlsx") %>% 
  rename(dato = Date) %>% 
  select(-Nedbor) %>% 
  mutate(Date = as.Date(dato)) %>% 
  rename(TempS16 = Temp_station) %>% 
  select(-dato) %>% 
  mutate(DOY = yday(Date)) %>% 
  select(-Date)

eklima17 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2017/Finse_weather.xlsx") %>% 
  rename(TempS17 = temperature) %>% 
  mutate(Date = as.Date(date)) %>% 
  select(-precipitation, -date) %>% 
  mutate(DOY = yday(Date)) %>% 
  select(-Date)

Klima <- eklima17 %>% 
  left_join(eklima16, by = "DOY") %>% 
  na.omit() 
  

#adiabatic temp with meter above sea level per site
ATemp <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_adiabatic_rate.xlsx") %>% 
  rename(ID = siteID) %>% 
  rename(ATemp = Adiabatic.temp) %>% 
  mutate(MASLCorr = MASL - 1222)

# Define the adiabatic lapse rate (6.5°C per 1000 meters)
lapse_rate <- 6.5  # in °C/1000m

Klima <- Klima %>% 
  mutate(Temp16New = (TempS16 - (lapse_rate * 228/1000))) %>% 
  mutate(Temp17New = (TempS17 - (lapse_rate * 228/1000)))
  
  
  
  