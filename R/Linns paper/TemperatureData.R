library("readxl")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("cowplot")
library("ggpubr")

####Compare temperature data from climate station and ibutton in 2016

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
eKlima <- read_excel("Data_plant_pollinator_Finse_2016_2017/2016/Finse_weather_2016_ny.xlsx")

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
  theme(legend.position="bottom", panel.background = element_blank(), text = element_text(size = 8)) #+
  facet_grid(~Stage)
ggsave(TemperatureFinsePlot, filename = "Figures/TemperatureFinsePlot.jpeg", height = 6, width = 8)





########Compare temperature data in 2016 and 2017 from climate station correlated for the adiabatic lapse rate

###Stage and meter above sea level (MASL) file
Site_MASL <- read_excel("Data_plant_pollinator_Finse_2016_2017/Stage_MASL.xlsx")

Site_MASL <- Site_MASL %>% 
  select(-siteID)

Site_MASL$MASLCorr <- Site_MASL$MASL - 1222

#Import data 2017
Temperature_2017 <- read_excel("Data_plant_pollinator_Finse_2016_2017/2017/Finse_weather.xlsx")

Temperature_2017$Date <- as.Date(Temperature_2017$date)

Temperature_2017 <- Temperature_2017 %>%
  rename(Temp_2017 = temperature) %>% 
  filter(Date >= as.Date("2017-06-06")) %>% 
  mutate(doy = yday(Date)) 

#Load stages and dates file
Stages2017 <- read_excel("Data_plant_pollinator_Finse_2016_2017/Dates_Stages_2017.xlsx") 

#create dataframe for the three stages
Temperature_2017_Site <- left_join(Temperature_2017, Stages2017, by = "Date")


Temperature_2017_Site %>% 
  filter(Date >= as.Date("2017-06-06")) %>% 
  mutate(doy = yday(Date))

Temp_2017_ALR <-Temperature_2017_Site %>% 
  select(-date, -Date, -precipitation)

#Add heightdata for each site
Temp_MASL_2017 <- merge(Temp_2017_ALR, Site_MASL, by = "Stage")

#add adiabatic time lapse to data and corrigate temperature according to m asl.
Temp_MASL_2017$Temp_2017_ALR <- Temp_MASL_2017$Temp_2017 - (lapse_rate * (Temp_MASL_2017$MASLCorr / 1000))


##Temperature 2016
Temperature_2016 <- eKlima %>% 
  rename(Temp_2016 = Temp_station) %>% 
  filter(Date >= as.Date("2016-06-17")) %>% 
  mutate(doy = yday(Date)) 

#Load stages and dates file
Stages2016 <- read_excel("Data_plant_pollinator_Finse_2016_2017/Dates_Stages_2016.xlsx") 

#create dataframe for the three stages
Temperature_2016_Site <- left_join(Temperature_2016, Stages2016, by = "Date")


Temperature_2016_Site %>% 
  filter(Date >= as.Date("2017-06-06")) %>% 
  mutate(doy = yday(Date))

Temp_2016_ALR <- Temperature_2016_Site %>% 
  select(-Date)

#Add heightdata for each site
Temp_MASL_2016 <- merge(Temp_2016_ALR, Site_MASL, by = "Stage")

#add adiabatic time lapse to data and corrigate temperature according to m asl.
Temp_MASL_2016$Temp_2016_ALR <- Temp_MASL_2016$Temp_2016 - (lapse_rate * (Temp_MASL_2016$MASLCorr / 1000))

#Merge Temperature files
Temperature_all <- merge(Temp_MASL_2016, Temp_MASL_2017, by = c("doy", "Stage"), all = TRUE)


#compare average temp per day per site from climate station data in 2016 and 2017, with snowmeltstages marked out with colored bands. Taken earliest flowering and latest peak from 2017 and 2016 to mark start and stop.
#2016
TemperatureFinseComb16 <- ggplot(Temperature_all, aes(x = doy)) +
  geom_smooth(aes(y = Temp_2016, color = "Temperature 2016"), se = FALSE) +
  geom_rect(aes(xmin=169, xmax=197, ymin=0, ymax=1, fill = "mid"), alpha = 0.5) +
  geom_rect(aes(xmin=186, xmax=205, ymin=1, ymax=2, fill = "late"), alpha = 0.5) +
  geom_rect(aes(xmin=197, xmax=209, ymin=2, ymax=3, fill = "very late"), alpha = 0.5) +
  labs(x = "Day of the year", y="Average daily temperature (°C)", color = "") +
  scale_color_manual(values = c("#FF6666"), guide = "none") +
  scale_fill_manual("Stage", values = c(mid = "#FFCC33", late = "#FF9966", `very late` = "#FF6600"),
                    labels = c("Mid", "Late", "Very Late")) +
  ggtitle("a) 2016") +
  theme(legend.position="bottom", panel.background = element_blank(), text = element_text(size = 8))
ggsave(TemperatureFinseComb16, filename = "Figures/TemperatureFinseComb16.jpeg", height = 6, width = 8)

#2017
TemperatureFinseComb17 <- ggplot(Temperature_all, aes(x = doy)) +
  geom_smooth(aes(y = Temp_2017, color = "Temperature 2017"), se = FALSE) +
  geom_rect(aes(xmin=162, xmax=197, ymin=0, ymax=1, fill = "early"), alpha=0.5) +
  geom_rect(aes(xmin=175, xmax=206, ymin=1, ymax=2, fill = "mid"), alpha = 0.5) +
  geom_rect(aes(xmin=184, xmax=207, ymin=2, ymax=3, fill = "late"), alpha = 0.5) +
  labs(x = "Day of the year", y="Average daily temperature (°C)", color = "") +
  scale_color_manual(values = c("#FF6666"), guide = "none") +
  scale_fill_manual("Stage", values = c(early = "#FFFF99", mid = "#FFCC33", late = "#FF9966"),
                    labels = c("Early", "Mid", "Late")) +
  ggtitle("b) 2017") +
  theme(legend.position="bottom", panel.background = element_blank(), text = element_text(size = 8))
ggsave(TemperatureFinseComb17, filename = "Figures/TemperatureFinseComb17.jpeg", height = 6, width = 8)

#Combine
TemperatureSnowmeltStagesCombines <- ggarrange(TemperatureFinseComb16, TemperatureFinseComb17, ncol = 1, heights = c(1, 1, 0.2), top = "Temperature, June-August", legend = "bottom")
ggsave(TemperatureSnowmeltStagesCombines, filename = "Figures/TemperatureSnowmeltStagesCombines.jpeg", height = 6, width = 8)




TemperatureFinse_comb <- ggplot(Temperature_all, aes(x = doy)) +
  geom_smooth(aes(y = Temp_2016_ALR, color = "2016")) +
  geom_smooth(aes(y = Temp_2017_ALR, color = "2017")) +
  labs(x = "Day of the year", y="Average daily temperature (°C)", color = "") +
  scale_color_manual(values = c("2016" = "#FF6666", "2017" = "#99CCCC")) +
  theme(legend.position="bottom", panel.background = element_blank(), text = element_text(size = 8)) #+
facet_grid(~Stage)
ggsave(TemperatureFinse_comb, filename = "Figures/TemperatureFinse_comb.jpeg", height = 6, width = 8)
