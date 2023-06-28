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
  ylim(0,10) +
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
  ylim(0,10) +
  theme(legend.position="bottom", panel.background = element_blank(), text = element_text(size = 8))
ggsave(TemperatureFinseComb17, filename = "Figures/TemperatureFinseComb17.jpeg", height = 6, width = 8)

#Combine
TemperatureSnowmeltStagesCombines <- ggarrange(TemperatureFinseComb16 + ylim(0, 10), TemperatureFinseComb17 + ylim(0, 10), ncol = 1, heights = c(1, 1, 0.2), top = "Temperature, June-August", legend = "bottom")
ggsave(TemperatureSnowmeltStagesCombines, filename = "Figures/TemperatureSnowmeltStagesCombines.jpeg", height = 6, width = 8)


###NEW PLOT


marked_doy <- data.frame(doy = c(145, 169, 163, 186, 170, 197),
                         color = c("#FFFF99", "#FFCC33", "#FFCC99", "#FF9966", "#FF6600", "#CC3300"),
                         shape = c("Diamond", "Circle", "Diamond", "Circle", "Diamond", "Circle"))


#Change to geom_rect above, one line per snowmelt stage

TemperatureFinse_comb <- ggplot() +
  geom_smooth(data = Temperature_all, aes(x = doy, y = Temp_2016_ALR, color = "2016"), alpha = 0) +
  geom_smooth(data = Temperature_all, aes(x = doy, y = Temp_2017_ALR, color = "2017"), alpha = 0) +
  labs(x = "Day of the year", y = "Average daily temperature (°C)", color = "") +
  scale_color_manual(values = c("2016" = "#FF6666", "2017" = "#990000")) +
  theme(legend.position = c(0.02, 0.9), legend.justification = c(0, 1),
        panel.background = element_blank(), text = element_text(size = 14),
        plot.title = element_text(hjust = 0, vjust = 1, margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  geom_rect(aes(xmin=195, xmax=219, ymin=0, ymax=0.2, fill = "1"), alpha=0.5) + 
  geom_rect(aes(xmin=191, xmax=223, ymin=0.2, ymax=0.4, fill = "2a"), alpha=0.5) + #but two flowers were hand pollinated already 2nd of july, doy= 183
  geom_rect(aes(xmin=201, xmax=228, ymin=0.4, ymax=0.6, fill = "2b"), alpha = 0.5) +
  geom_rect(aes(xmin=196, xmax=228, ymin=0.6, ymax=0.8, fill = "3a"), alpha = 0.5) +
  geom_rect(aes(xmin=204, xmax=232, ymin=0.8, ymax=1, fill = "3b"), alpha = 0.5) +
  geom_rect(aes(xmin=228, xmax=247, ymin=1, ymax=1.2, fill = "4"), alpha = 0.5) +
  geom_text(aes(x = 207, y = 0.1, label = "1: 2017"), color = "brown", size = 3) +
  geom_text(aes(x = 203, y = 0.3, label = "2: 2016"), color = "brown", size = 3) +
  geom_text(aes(x = 214.5, y = 0.5, label = "2: 2017"), color = "brown", size = 3) +
  geom_text(aes(x = 214, y = 0.7, label = "3: 2016"), color = "brown", size = 3) +
  geom_text(aes(x = 218, y = 0.9, label = "3: 2017"), color = "brown", size = 3) +
  geom_text(aes(x = 237.5, y = 1.1, label = "4: 2016"), color = "brown", size = 3) +
  geom_point(aes(x = 145, y = 0.1, label = "none"), color = "brown", fill = "#FFFF00", shape = 21, size = 2, stroke = 0.2) +
  geom_point(aes(x = 169, y = 0.3, label = "none"), color = "brown", fill = "#FFCC33", shape = 24, size = 2, stroke = 0.2) +
  geom_point(aes(x = 163, y = 0.5, label = "none"), color = "brown", fill = "#FFCC99", shape = 21, size = 2, stroke = 0.2) +
  geom_point(aes(x = 186, y = 0.7, label = "none"), color = "brown", fill = "#FF9966", shape = 24, size = 2, stroke = 0.2) +
  geom_point(aes(x = 170, y = 0.9, label = "none"), color = "brown", fill = "#FF6600", shape = 21, size = 2, stroke = 0.2) +
  geom_point(aes(x = 197, y = 1.1, label = "none"), color = "brown", fill = "#CC3300", shape = 24, size = 2, stroke = 0.2) +
  geom_point(aes(x = 197, y = 0.1, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 197, y = 0.3, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 206, y = 0.5, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 205, y = 0.7, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 207, y = 0.9, label = "none"), shape = 8, size = 2) +
  geom_point(aes(x = 209, y = 1.1, label = "none"), shape = 8, size = 2) +
  scale_fill_manual("Stage", values = c("1" = "#FFFF33", "2a" = "#FFCC33", "2b" = "#FFCC99", "3a" = "#FF9966", "3b" = "#FF6600", "4" = "#CC3300")) +
  guides(color = guide_legend(title = "Year"), fill = "none")
ggsave(TemperatureFinse_comb, filename = "Figures/TemperatureFinse_comb.jpeg", height = 6, width = 8)


#Snowmelt stages calculated from first handpollination (except for 2a, four flowers where hand pollinated 2nd of july, but makes the season weirdly long) to first seed collection
#Snowmelt in 2016 was the same for all stages, for 2017 i calculated an average for all sites
#download new temperature data?
#star indicates peak flowering, something odd with stage 4, probably because sheep ate the flowers before hp AND we selected flowers outside of plot






