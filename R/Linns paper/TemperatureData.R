library("readxl")
library("dplyr")
library("tidyr")
library("lubridate")
library("lme4")
library("ggplot2")

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
  select(-Nedbor) 

eKlima$Date <- as.Date(eKlima$Date)

#adiabatic temp with meter above sea level per site
ATemp <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Weather_adiabatic_rate.xlsx")

colnames(ATemp) <- c("ID", "ATemp", "MASL")

#Corrigate for meters above sea level (MASL) for study sites and MASL for climate station (climate station at 1222 MASL)
ATemp$MASLCorr <- ATemp$MASL - 1222


#merge eklima 
merged_temp <- merge(iButtonFinse, eKlima, by = "Date")


#...and create new temperature column
colnames(merged_temp) <- c("Date", "ID", "Temperature_iButton", "Temperature_Station")


#add adiabatic temperature to station temperature
Temperature_Finse <- merge(merged_temp, ATemp, by = "ID")

# Define the adiabatic lapse rate (6.5°C per 1000 meters)
lapse_rate <- 6.5  # in °C/1000m

# Calculate the adiabatic temperature
Temperature_Finse$Adiabatic_Temperature <- Temperature_Finse$Temperature_Station - (lapse_rate * (Temperature_Finse$MASLCorr / 1000))

Temperature_Finse <- Temperature_Finse %>% 
  select(-ATemp)

Temperature_Finse <- Temperature_Finse %>% 
  mutate(doy = yday(Date))

Temperature_Finse$Stage <- substr(Temperature_Finse$ID, 1, 1)

#compare average temp per day per site from ibutton to eklima
TemperatureFinsePlot <- ggplot(Temperature_Finse, aes(x = doy)) +
  geom_smooth(aes(y = Temperature_iButton, color = "Temperature iButton")) +
  geom_smooth(aes(y = Adiabatic_Temperature, color = "Temperature Climate Station")) +
  labs(x = "Day of the year", y="Average daily temperature (°C)", color = "") +
  scale_color_manual(values = c("Temperature iButton" = "#FF6666", "Temperature Climate Station" = "#99CCCC")) +
  theme(legend.position="bottom", panel.background = element_blank(), text = element_text(size = 8))
ggsave(TemperatureFinsePlot, filename = "Figures/TemperatureFinsePlot.jpeg", height = 6, width = 8)

