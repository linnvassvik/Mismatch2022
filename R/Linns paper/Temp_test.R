#Temperature

library("readxl")
library("tidyverse")
library("dplyr")
library("lubridate")


####Compare temperature data from climate station and ibutton in 2016 (E = snowmelt stage 2, M = snowmelt stage 3, L = snowmelt stage 4)

#Load temperature dataset from both eklima and ibutton
#iButton 
iButtonFinse <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_iButton_2016.xlsx")

iButtonFinse <- iButtonFinse %>%
  separate(Date, into = c("Date", "Time"), sep = " ") %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, ID) %>%
  summarize(average_temperature = mean(Temperature)) %>%
  rename(Date = 1, ID = 2, average_temperature = 3)

#eklima
eKlima <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Finse_weather_2016_ny.xlsx")

eKlima <- eKlima %>% 
  select(-Nedbor) %>% 
  mutate(Date = as.Date(Date))

#adiabatic temp with meter above sea level per site
ATemp <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_adiabatic_rate.xlsx")

ATemp <- ATemp %>%
  rename(ID = 1, ATemp = 2, MASL = 3) %>%
  mutate(MASLCorr = MASL - 1222)


#merge eklima  and ibutton temperature data
merged_temp <- merge(iButtonFinse, eKlima, by = "Date") %>% 
  rename(Temperature_iButton = average_temperature, Temperature_Station = Temp_station) 

#add adiabatic temperature to station temperature
Temperature_Finse <- merge(merged_temp, ATemp, by = "ID")

# Define the adiabatic lapse rate (6.5°C per 1000 meters)
lapse_rate <- 6.5  # in °C/1000m

Temperature_Finse <- Temperature_Finse %>%
  mutate(Adiabatic_Temperature = Temperature_Station - (lapse_rate * (MASLCorr / 1000))) %>%
  select(-ATemp) %>%
  mutate(doy = yday(Date),
         Stage = substr(ID, 1, 1))

### Calculate difference in temperature between highest and lowest site
Temperature_Finse2 <- Temperature_Finse %>% 
  select(-Temperature_Station, -MASLCorr, -Adiabatic_Temperature) %>% 
  filter(MASL == 1489 | MASL == 1411) %>% 
  filter(doy > 195)

avg_temp <- Temperature_Finse2 %>%
  group_by(Stage, doy) %>%
  summarise(avg_temperature = mean(Temperature_iButton))

# Pivot the data to have separate columns for E and L stages
pivoted_temp <- pivot_wider(avg_temp, names_from = Stage, values_from = avg_temperature)

# Calculate the temperature difference (E - L)
pivoted_temp <- pivoted_temp %>%
  mutate(Temp_Diff = l-e)

print(pivoted_temp)

#test difference 
model <- lme(Temperature_iButton ~ Adiabatic_Temperature, random = ~ 1 | ID, data = Temperature_Finse)
summary(model)



#############################################################
########Compare temperature data in 2016 and 2017 from climate station correlated for the adiabatic lapse rate

###Stage and meter above sea level (MASL) file
Site_MASL <- read_excel("Data_plant_pollinator_Finse_2016_2017/Stage_MASL.xlsx")

Site_MASL <- Site_MASL %>%
  select(-siteID) %>%
  mutate(MASLCorr = MASL - 1222)

#Import data 2017
Temperature_2017 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2017/Finse_weather.xlsx")

Temperature_2017 <- Temperature_2017 %>%
  mutate(Date = as.Date(date)) %>%
  rename(Temp_2017 = temperature) %>%
  filter(Date >= as.Date(date)) %>%
  mutate(doy = yday(Date)) %>% 
  select(-date)


#ALR = adiabatic lapse rate
Temp_2017 <- Temperature_2017 %>%
  filter(as.Date(Date) >= "2017-05-01") %>%
  mutate(Date = as.Date(Date),
         doy = yday(Date)) %>% 
  select(-Date, -precipitation)


##Temperature 2016
Temperature_2016 <- eKlima %>% 
  rename(Temp_2016 = Temp_station) %>% 
  filter(Date >= as.Date("2016-05-01")) %>% 
  mutate(doy = yday(Date)) 


Temp_2016 <- Temperature_2016 %>% 
  select(-Date)

#Merge Temperature files
Temperature_all <- merge(Temp_2016, Temp_2017, by = c("doy"))

Temperature_all <- Temperature_all %>%
  filter(doy > 150)
