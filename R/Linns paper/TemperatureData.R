library("readxl")
library("dplyr")
library("tidyr")

#Load temperature dataset from both eklima and ibutton
#iButton
iButtonFinse <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_iButton_2016.xlsx")

iButtonFinse <- iButtonFinse %>% 
  separate(Date, into = c("Date", "Time"), sep = " ") %>%
  mutate(Date = as.Date(Date))

#average temp per day
iButtonFinse <- aggregate(iButtonFinse$Temperature, by = list(iButtonFinse$Date, iButtonFinse$ID), FUN = mean)

colnames(iButtonFinse) <- c("Date", "ID", "average_temperature")

#eklima
eKlima <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Finse_weather_2016.xlsx")

eKlima <- eKlima %>% 
  select(-Nedbør)

#adiabatic temp with meter above sea level per site
ATemp <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_adiabatic_rate.xlsx")


#merge eklima with adiabatic time lapse 
merged_temp <- merge(iButtonFinse, eKlima, by = "Date")



#...and create new temperature column

#create new column with ID, only use dates where stage was snow free

#compare average temp per day per site from ibutton to eklima