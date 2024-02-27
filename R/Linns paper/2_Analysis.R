source("R/Linns paper/1_Import_Data.R")
source("R/Linns paper/MyFunctions.R")

library(lme4)
library(nlme)
library(broom.mixed)
library(performance)
library(patchwork)
library(readxl)
library(ggplot2) #remove
library(ggpubr)

### Seed mass 2016
dat16 <- dat_DOY |> 
  filter(Year == 2016)

### Seed mass 2017
dat17 <- dat_DOY |> 
  filter(Year == 2017)


#Seed mass
#2016

hist(dat16$PeakFlower_doy, breaks = 10, fill = "lightgray")
qqnorm(dat16$PeakFlower_doy)
qqline(dat16$PeakFlower_doy)


#What determines when plants flower (reaches peak flowering, number of flowers at peak or doy?)

sm_model_16 <- glmer(PeakFlower_doy ~ Snowmelt_doy.cen * CumTemp_before.cen + (1|siteID), family = poisson, data = dat16)


sm_model_17 <- glmer(PeakFlower_doy ~ Snowmelt_doy.cen * CumTemp_before.cen + (1|siteID), family = poisson, data = dat17)

#plot peak flower x snowmelt doy and mean(temp), endre temperatur til snowmelt->peakdoy

summary(sm_model_16)

check_model(sm_model_17)

#Snowmelt doy important for peak flowering doy


#What determines plants seed production (temp vs snowmelt    and pollen limitation?)
sm_model_16_2 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen, random =  ~ 1|siteID, data = dat16)

sm_model_17_2 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen, random =  ~ 1|siteID, data = dat17)

summary(sm_model_17_2)


dat16 %>% 
  ggplot(aes(x = Snowmelt_doy, y = log(Seed_mass))) +
  geom_point() +
  geom_line(data = Fitted16, aes(x = Snowmelt_doy, y = .fitted))


#Doy on x
Newdata16 <- expand.grid(Snowmelt_doy = c(160, 165, 170, 175, 180, 185, 190, 195, 200), Temp_total.cen = c(-2,2), Seed_mass = 0)

Newdata16$pred <- predict(sm_model_16_2, Newdata16, level = 0)

Designmat <- model.matrix(formula(sm_model_16_2)[-2], Newdata16)
predvar <- diag(Designmat %*% vcov(sm_model_16_2) %*% t(Designmat)) 
Newdata16$SE <- sqrt(predvar) 


cmult <- 2
temp_16_plot <- ggplot(dat16,aes(x= Snowmelt_doy , y = log(Seed_mass)))+ 
  geom_point(alpha = 0.5) +
  geom_line(Newdata16, mapping = aes(x = Snowmelt_doy, y = pred, color = as.factor(Temp_total.cen))) +
  scale_color_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  geom_ribbon(Newdata16, mapping = aes(y=pred,ymin = pred-cmult*SE, ymax=pred+cmult*SE, fill = as.factor(Temp_total.cen)), alpha = 0.5) +
  scale_fill_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  labs(x = "Snowmelt doy", y = "log(Seed mass in g)", title = "2016") +
  theme_minimal()

#Temp on x
Newdata16b <- expand.grid(Snowmelt_doy = c(160, 195), Temp_total.cen = c(-2,-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2), Seed_mass = 0)

Newdata16b$pred <- predict(sm_model_16_2, Newdata16b, level = 0)

Designmat_b <- model.matrix(formula(sm_model_16_2)[-2], Newdata16b)
predvar_b <- diag(Designmat %*% vcov(sm_model_16_2) %*% t(Designmat)) 
Newdata16b$SE <- sqrt(predvar_b) 

cmult <- 2
doy_16_plot <- ggplot(dat16,aes(x= Temp_total.cen , y = log(Seed_mass)))+ 
  geom_point(alpha = 0.5) +
  geom_line(Newdata16b, mapping = aes(x = Temp_total.cen, y = pred, color = as.factor(Snowmelt_doy))) +
  scale_color_manual(values = c("#336633", "#E69F00"), name = "Doy") +
  geom_ribbon(Newdata16b, mapping = aes(y=pred,ymin = pred-cmult*SE, ymax=pred+cmult*SE, fill = as.factor(Snowmelt_doy)), alpha = 0.5) +
  scale_fill_manual(values = c("#336633", "#E69F00"), name = "Doy") +
  labs(x = "Temperature", y = "log(Seed mass in g)", title = "2016") +
  theme_minimal()

#2017
Newdata17 <- expand.grid(Snowmelt_doy = c(140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190), Temp_total.cen = c(-2,2), Seed_mass = 0)

Newdata17$pred <- predict(sm_model_17_2, Newdata17, level = 0)

Designmat <- model.matrix(formula(sm_model_17_2)[-2], Newdata17)
predvar2 <- diag(Designmat %*% vcov(sm_model_17_2) %*% t(Designmat)) 
Newdata17$SE <- sqrt(predvar2) 

temp_17_plot <- ggplot(dat17,aes(x= Snowmelt_doy , y = log(Seed_mass)))+ 
  geom_point(alpha = 0.5) +
  geom_line(Newdata17, mapping = aes(x = Snowmelt_doy, y = pred, color = as.factor(Temp_total.cen))) +
  scale_color_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  geom_ribbon(Newdata17, mapping = aes(y=pred,ymin = pred-cmult*SE, ymax=pred+cmult*SE, fill = as.factor(Temp_total.cen)), alpha = 0.5) +
  scale_fill_manual(values = c("#6666FF", "#FC4E07"), name = "Temperature") +
  labs(x = "Snowmelt doy", y = "", title = "2017") +
  theme_minimal()

#Temp on x
Newdata17_temp <- expand.grid(Snowmelt_doy = c(140, 180), Temp_total.cen = c(-2,-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2), Seed_mass = 0)

Newdata17_temp$pred <- predict(sm_model_17_2, Newdata17_temp, level = 0)

Designmat <- model.matrix(formula(sm_model_17_2)[-2], Newdata17_temp)
predvar_temp <- diag(Designmat %*% vcov(sm_model_17_2) %*% t(Designmat)) 
Newdata17_temp$SE <- sqrt(predvar_temp) 

doy_17_plot <- ggplot(dat17,aes(x= Temp_total.cen , y = log(Seed_mass)))+ 
  geom_point(alpha = 0.5) +
  geom_line(Newdata17_temp, mapping = aes(x = Temp_total.cen, y = pred, color = as.factor(Snowmelt_doy))) +
  scale_color_manual(values = c("#336633", "#E69F00"), name = "doy") +
  geom_ribbon(Newdata17_temp, mapping = aes(y=pred,ymin = pred-cmult*SE, ymax=pred+cmult*SE, fill = as.factor(Snowmelt_doy)), alpha = 0.5) +
  scale_fill_manual(values = c("#336633", "#E69F00"), name = "doy") +
  labs(x = "Temperature", y = "", title = "2017") +
  theme_minimal()

summary(sm_model_16_2)

Temp_snowmelt <- ggarrange(temp_16_plot, temp_17_plot, common.legend = TRUE) 
ggsave(Temp_snowmelt, filename = "Figures/Temp_snowmelt.jpeg", height = 6, width = 8)

Temp_snowmelt_2 <- ggarrange(doy_16_plot, doy_17_plot, common.legend = TRUE) 
ggsave(Temp_snowmelt_2, filename = "Figures/Temp_snowmelt_2.jpeg", height = 6, width = 8)





#Which biotic factors affect reproductive output, and how do they change along the snowmelt gradient
sm_model_16_10 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * Treatment, random =  ~ 1|siteID, data = dat16)
sm_model_16_11 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * MeanFlower.cen, random =  ~ 1|siteID, data = dat16)
dat16a <- na.omit(dat16)
sm_model_16_12 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * MeanVisit.cen, random =  ~ 1|siteID, data = dat16a) #centrere mean visit som meanflower
#Visitation rate

summary(sm_model_17_10)

sm_model_17_10 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * Treatment, random =  ~ 1|siteID, data = dat17)
sm_model_17_11 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * MeanFlower.cen, random =  ~ 1|siteID, data = dat17)
sm_model_17_12 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * MeanVisit.cen, random =  ~ 1|siteID, data = dat17)
sm_model_17_13 <- lme(log(Seed_mass) ~ Snowmelt_doy.cen * Temp_total.cen * MeanFlower.cen * MeanVisit.cen, random =  ~ 1|siteID, data = dat17)

dat17 %>% 
  ggplot(aes(x = MeanFlower.cen, y = MeanVisit.cen)) +
  geom_point() +
  geom_smooth()

summary(sm_model_17_13)

#VIsit and Flower
Newdata17_VisitFlower <- expand.grid(Snowmelt_doy.cen = mean(dat17$Snowmelt_doy.cen), Temp_total.cen = mean(dat17$Temp_total.cen), MeanVisit.cen = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2), Seed_mass = 0, MeanFlower.cen = c(-0.5, 2))

VisitFlower17 <- make_prediction(Newdata17_VisitFlower, sm_model_17_13)

VisitFlower17_plot <- make_prettyplot(dat = dat17, 
                                 Newdata = VisitFlower17, 
                                 xaxis = MeanVisit.cen, 
                                 yaxis = log(Seed_mass), 
                                 prediction = pred, 
                                 ColorVariable = as.factor(MeanFlower.cen),
                                 SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "Visitation rate") +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "Visitation rate") +
  labs(x = "Mean flower abundance", y = "log(Seed mass in g)", title = "")
ggsave(VisitFlower17_plot, filename = "Figures/VisitFlower17_plot.jpeg", height = 6, width = 8)

Newdata17_Flower <- expand.grid(Snowmelt_doy.doy = mean(dat17$Snowmelt_doy), Temp_total.cen = mean(dat17$Temp_total.cen), MeanFlower.cen = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4), Seed_mass = 0)

Flower17 <- make_prediction(Newdata17_Flower, sm_model_17_11)

Flower17_plot <- make_prettyplot(dat = dat17, 
                                    Newdata = Flower17, 
                                    xaxis = MeanFlower.cen, 
                                    yaxis = log(Seed_mass), 
                                    prediction = pred, 
                                    ColorVariable = "#336666",
                                    SE = SE) +
  scale_color_manual(values = "#336666") +
  scale_fill_manual(values = "#336666") +
  labs(x = "Mean flower abundance", y = "", title = "Flower abundance") +
  theme(legend.position = "none")

FlowerVisitPlot <- ggarrange(Visit17_plot, Flower17_plot) 
ggsave(FlowerVisitPlot, filename = "Figures/FlowerVisitPlot17.jpeg", height = 6, width = 8)

Newdata17_Flower <- expand.grid(Snowmelt_doy = mean(dat17$Snowmelt_doy), Temp_total.cen = mean(dat17$Temp_total.cen), MeanFlower.cen = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4), Seed_mass = 0)

#Visit x Flower
Newdata17_FlowerVisit <- expand.grid(Snowmelt_doy.cen = mean(dat17$Snowmelt_doy), Temp_total.cen = mean(dat17$Temp_total.cen), MeanFlower.cen = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4), MeanVisit.cen = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2))

FlowerVisit17 <- make_prediction(Newdata17_FlowerVisit, sm_model_17_11)

FlowerVisit17_plot <- make_prettyplot(dat = dat17, 
                                 Newdata = Flower17, 
                                 xaxis = MeanFlower.cen, 
                                 yaxis = log(Seed_mass), 
                                 prediction = pred, 
                                 ColorVariable = "#336666",
                                 SE = SE) +
  scale_color_manual(values = "#336666") +
  scale_fill_manual(values = "#336666") +
  labs(x = "Mean flower abundance", y = "", title = "Flower abundance") +
  theme(legend.position = "none")



#Temp on x
Newdata17_treatment <- expand.grid(Snowmelt_doy = mean(dat17$Snowmelt_doy), Temp_total.cen = c(-2,-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2), Seed_mass = 0, Treatment = c("Control", "Pollinated"))

Treatment17 <- make_prediction(Newdata17_treatment, sm_model_17_10)

Treatment17_plot <- make_prettyplot(dat = dat17, 
                Newdata = Treatment17, 
                xaxis = Temp_total.cen, 
                yaxis = log(Seed_mass), 
                prediction = pred, 
                ColorVariable = Treatment,
                SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "doy") +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "doy") +
  labs(x = "Temperature", y = "", title = "2017")

#2016
Newdata16_treatment <- expand.grid(Snowmelt_doy = mean(dat16$Snowmelt_doy), Temp_total.cen = c(-2,-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2), Seed_mass = 0, Treatment = c("Control", "Pollinated"))

Treatment16 <- make_prediction(Newdata16_treatment, sm_model_16_10)

Treatment16_plot <- make_prettyplot(dat = dat16, 
                Newdata = Treatment16, 
                xaxis = Temp_total.cen, 
                yaxis = log(Seed_mass), 
                prediction = pred, 
                ColorVariable = Treatment,
                SE = SE) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "doy") +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "doy") +
  labs(x = "Temperature", y = "log(Seed mass in g)", title = "2016")

TreatmentPlot <- ggarrange(Treatment16_plot, Treatment17_plot, common.legend = TRUE) 
ggsave(TreatmentPlot, filename = "Figures/TreatmentPlot.jpeg", height = 6, width = 8)

summary(sm_model_16_10)

dat16 %>% 
  group_by(Treatment) %>% 
  summarise(mean(Seed_mass))

dat17 %>% 
  group_by(Treatment) %>% 
  summarise(mean(Seed_mass))




sm_model_16_6 <- lme(log(Seed_mass) ~ Snowmelt_doy * (MeanFlower.cen + Treatment), random =  ~ 1|siteID, data = dat16)
sm_model_17_6 <- lme(log(Seed_mass) ~ Snowmelt_doy * (MeanFlower.cen + Treatment), random =  ~ 1|siteID, data = dat17)
summary(sm_model_16_6)
summary(sm_model_17_6)


#Snowmelt alone is important for seed mass, with negative value? meaning that with early snowmelt you have lower seed mass? or with late snowmelt you have lower seed mass?
#Temp * treatment is important for seed mass, meaning a higher temp results in a better effect from hand pollination which again affects seed mass?






### Snumber
dat16_2 <- dat16_2 %>% 
  group_by(BlockID, Year, Plant) %>% 
  mutate(Number_seedovule = (Seed_number + Ovule_number)) %>% 
  ungroup()


sp_model <- glmer(Seed_number ~ Snowmelt_doy.cen * Temp_total.cen + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_2)

sp_model <- lme(Seed_number ~ Snowmelt_doy.cen * Temp_total.cen, random = ~ 1|siteID, data = dat16_2)

glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy.cen * Temp_total.cen + (1|siteID), family = binomial(link = "logit"), data = dat16_2)


summary(sp_model) 

Newdata16_SN <- expand.grid(Snowmelt_doy.cen = c(-1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5), Temp_total.cen = c(-2,2), Seed_number = 0, Number_seedovule = mean(dat16_2$Number_seedovule), siteID = "E 01")

Newdata = Newdata16_SN
model = sp_model

Newdata$pred <- predict(model,Newdata, re.form=NA)
mm <- model.matrix(terms(model),Newdata)
pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
tvar1 <- pvar1+VarCorr(model)$siteID[1] 
cmult <- 1.96 ## make CI
Newdata <- data.frame(
  Newdata
  , plo = Newdata$pred-cmult*sqrt(pvar1)
  , phi = Newdata$pred+cmult*sqrt(pvar1)
  , tlo = Newdata$pred-cmult*sqrt(tvar1)
  , thi = Newdata$pred+cmult*sqrt(tvar1)
)  



TempSnow_16plot <- ggplot(dat16, aes(x = Snowmelt_doy.cen , y = Seed_number))+ 
  geom_point(alpha = 0.5) +
  geom_line(Newdata, mapping = aes(x = Snowmelt_doy.cen, y = pred, color = as.factor(Temp_total.cen))) +
  geom_ribbon(Newdata, mapping = aes(y= pred,ymin = pred - plo, ymax = pred + phi, fill = as.factor(Temp_total.cen)), alpha = 0.5) +
  theme_minimal()




VisitFlower17_plot <- make_prettyplot(dat = dat16, 
                                      Newdata = TempSnow16_SN, 
                                      xaxis = Snowmelt_doy.cen, 
                                      yaxis = log(Seed_mass), 
                                      prediction = pred, 
                                      ColorVariable = as.factor(Temp_total.cen),
                                      SE = sqrt(pvar1)) +
  scale_color_manual(values = c("#FFCC99", "#336666"), name = "Temperature") +
  scale_fill_manual(values = c("#FFCC99", "#336666"), name = "Temperature") +
  labs(x = "Snowmelt doy", y = "log(Seed mass in g)", title = "")

ggsave(VisitFlower17_plot, filename = "Figures/VisitFlower17_plot.jpeg", height = 6, width = 8)

#
sp_model5 <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy * Temp_total.cen * MeanFlower.cen + (1|siteID), family = binomial(link = "logit"), data = dat16_2)
sp_model6 <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy * Temp_total.cen * Treatment + (1|siteID), family = binomial(link = "logit"), data = dat16_2)
dat16_2a <- na.omit(dat16_2)
sp_model7 <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy * Temp_total.cen * MeanVisit.cen + (1|siteID), family = binomial(link = "logit"), data = dat16_2a)

summary(sp_model6)


sp_model8 <- glmer(cbind(Seed_number, Ovule_number) ~ Snowmelt_doy * (MeanFlower.cen + Treatment) + (1|siteID), family = binomial(link = "logit"), data = dat16_2)




sp_model6 <- glmer(cbind(Seed_number, Ovule_number) ~ Temp_total.cen * (MeanFlower.cen + Treatment) + (1|siteID), family = binomial(link = "logit"), data = dat16_2)

summary(sp_model5)
summary(sp_model6)


#



















#When removing biomass 2016 ends up with stage as sifnificant (more than last time), and 2017 with flower abundance (more than before) and temp (same importance)

#2016 seperate per stage and year
dat16_S2 <- dat16 |> 
  filter(Stage2 == 2)

sm_model_16_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S2)
summary(sm_model_16_S2)

dat16_S3 <- dat16 |> 
  filter(Stage2 == 3)

sm_model_16_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S3)
summary(sm_model_16_S3)

dat16_S4 <- dat16 |> 
  filter(Stage2 == 4)

sm_model_16_S4 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat16_S4)
summary(sm_model_16_S4)

#2017 seperate per stage and year
dat17_S1 <- dat17 |> 
  filter(Stage2 == 1)

sm_model_17_S1 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S1)
summary(sm_model_17_S1)

dat17_S2 <- dat17 |> 
  filter(Stage2 == 2)

sm_model_17_S2 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S2)
summary(sm_model_17_S2)

dat17_S3 <- dat17 |> 
  filter(Stage2 == 3)

sm_model_17_S3 <- lme(log(Seed_mass) ~ MeanFlower.cen + CumTemp_after.cen + Treatment, random =  ~ 1|siteID, data = dat17_S3)
summary(sm_model_17_S3)


###############
###############
dat16 <- dat16 %>% 
  group_by(BlockID, Year, Plant) %>% 
  mutate(Number_seedovule = (Seed_number + Ovule_number)) %>% 
  ungroup()

### Snumber
sp_model <- glmer(Seed_number ~ Stage2 * (MeanFlower.cen + CumTemp_after.cen + Treatment) + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16)
summary(sp_model)



#2016 seperate per stage
dat16_S2 <- dat16 |> 
  filter(Stage2 == 2)

sp_model_S2 <- glmer(Seed_number ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_S2)
summary(sp_model_S2)


dat16_S3 <- dat16 |> 
  filter(Stage2 == 3)

sp_model_S3 <- glmer(Seed_number ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_S3)
summary(sp_model_S3)


dat16_S4 <- dat16 |> 
  filter(Stage2 == 4)

sp_model_S4 <- glmer(Seed_number ~ Stage2 + MeanFlower.cen + CumTemp_after.cen + Treatment + offset(log(Number_seedovule)) + (1|siteID), family = poisson, data = dat16_S4)
summary(sp_model_S4)



##########################################################################
### Correlation between tested factors


library(reshape2)
library(ggcorrplot)

dat4 <- dat %>% 
  filter(Year != '2017')

# Create a subset of your data with the relevant variables
subset_data <- dat[c("CumTemp_after.cen", "Stage2", "MeanFlower.cen")]

# Calculate the Pearson correlation matrix
cor_matrix <- cor(subset_data, method = "pearson")

# Convert correlation matrix to long format
cor_matrix_long <- melt(cor_matrix)
# Print the correlation matrix
print(cor_matrix)


ggcorrplot::ggcorrplot(cor_matrix, type = "lower", lab = TRUE)

ggplot(cor_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()

subset_data %>% 
  ggplot(aes(y = Biomass, x = CumTemp_after.cen)) +
  geom_point() +
  geom_smooth(method = lm)




############################################################################################################

##seed mass
dat %>%
  group_by(Year, Stage) %>%
  summarize(total_Seedmass = mean(Seed_mass))

# Independent samples t-test
SeedMassDiff <- t.test(Seed_mass ~ Year, data = dat)

# Print the result
print(SeedMassDiff)


###number of flowers
phenology %>%
  group_by(stage, year) %>%
  summarize(total_flowers = sum(flower.sum))

phenology %>%
  group_by(year) %>%
  summarize(total_flowers = sum(flower.sum))

# Independent samples t-test
phenoDiff <- t.test(flower.sum ~ year, data = phenology)

# Print the result
print(phenoDiff)


###Length of stages
dat_onlyHP <- dat %>% 
  filter(Plant %in% c("HP1", "HP2"))

dat_onlyHP <- dat_onlyHP %>% 
  mutate(DaysDifference = Collected - Date1)

result_stagesYear <- dat_onlyHP %>% 
  group_by(Year, Stage) %>% 
  summarize(AverageDays = mean(DaysDifference, na.rm = TRUE))

result_year <- dat_onlyHP %>% 
  group_by(Year) %>% 
  summarize(AverageDays = mean(DaysDifference, na.rm = TRUE))

result_Stages <- dat_onlyHP %>% 
  group_by(Stage2) %>% 
  summarize(AverageDays = mean(DaysDifference, na.rm = TRUE))

# Assuming you have two groups, one for each year, in your dat_onlyHP dataframe
group_2016 <- dat_onlyHP$DaysDifference[dat_onlyHP$Year == 2016]
group_2017 <- dat_onlyHP$DaysDifference[dat_onlyHP$Year == 2017]

# Perform a t-test
t_test_result <- t.test(group_2016, group_2017)

# Print the results
t_test_result



######### Calculate average seedmass

# Calculate the average seedmass per year
average_seedmass <- aggregate(Seed_mass ~ Year, dat, FUN = mean)

# Calculate the standard deviation of the biomass per year
standard_deviation <- aggregate(Seed_mass ~ Year, dat, FUN = sd)

# Merge the average seedmass and standard deviation into a single data frame
resultSeed <- merge(average_seedmass, standard_deviation, by = "Year")

# Rename the columns
colnames(resultSeed) <- c("Year", "Average_Seedmass", "Standard_Deviation")

# Print the results
print(resultSeed)


# Independent samples t-test
SeedmassDiff <- t.test(Seed_mass ~ Year, data = dat)

# Print the result
print(SeedmassDiff)

####### TEST SIGNIFICANCE

# Select the seedmass for the two years you want to compare
year1_seedmass <- dat$Seed_mass[dat$Year == '2016']
year2_seedmass <- dat$Seed_mass[dat$Year == '2017']

# Perform the independent samples t-test
t_test_result <- t.test(year1_seedmass, year2_seedmass)

# Print the t-test result
print(t_test_result)

###Difference in temperature
# Independent samples t-test
TempDiff <- t.test(Temp_after ~ Year, data = dat3)

# Print the result
print(TempDiff)

# Select the temperature for the two years you want to compare
#year1_temp <- dat$CumTemp_after.cen[dat$Year == '2016']
#year2_temp <- dat$CumTemp_after.cen[dat$Year == '2017']

# Perform the independent samples t-test
#t_test_result_temp <- t.test(year1_temp, year2_temp)

# Print the t-test result
#print(t_test_result_temp)


######## Number of seeds
average_seednumber <- aggregate(Seed_number ~ Year, dat, FUN = mean)
standard_deviation_Seednumber <- aggregate(Seed_number ~ Year, dat, FUN = sd)
resultSeedNumber <- merge(average_seednumber, standard_deviation_Seednumber, by = "Year" )
colnames(resultSeedNumber) <- c("Year", "Average_Seednumber", "Standard_Deviation_SeedNumber")

# Print the results
print(resultSeedNumber)

average_ovulenumber <- aggregate(Ovule_number ~ Year, dat, FUN = mean)
standard_deviation_ovulenumber <- aggregate(Ovule_number ~ Year, dat, FUN = sd)
resultOvuleNumber <- merge(average_ovulenumber, standard_deviation_ovulenumber, by = "Year")
colnames(resultOvuleNumber) <- c("Year", "Average_Ovulenumber", "Standard_Deviation_OvuleNumber")

# Print the results
print(resultOvuleNumber)


average_SeedPotential <- aggregate(Seed_potential ~ Treatment, dat, FUN = mean)
standard_deviation_SeedPotential <- aggregate(Seed_potential ~ Treatment, dat, FUN = sd)
resultSeedPotential <- merge(average_SeedPotential, standard_deviation_SeedPotential, by = "Treatment")
colnames(resultSeedPotential) <- c("Treatment", "Average_SeedPotential", "Standard_Deviation_SeedPotential")

# Print the results
print(resultSeedPotential)

SeedPotentialDiff <- t.test(Seed_potential ~ Treatment, data = dat)
# Print the result
print(SeedPotentialDiff)

######### REMOVE??? #########


###### Find missing temperature at Finse in 2017 by predicting it from temperature from Midtstova
TempFinseMidtstova <- read_excel("Data_plant_pollinator_Finse_2016_2017/TempFinseMidtstova.xlsx") %>% 
  select(name = Navn, date = Tid, temperature = Temp) %>% 
  mutate(date = dmy(date), temperature = as.numeric(temperature))

FinseMidt <- TempFinseMidtstova %>% 
  group_by(date) %>% 
  pivot_wider(names_from = name, values_from = temperature) %>% 
  ungroup()

tPred <- lm(Finsevatn ~ Midtstova, data = FinseMidt)
summary(tPred)

#a = 1.078, R^2 = 0.9392, intercept = -0.79

#TFinse = A * tMId + Intercept

FinseMidtNew <- FinseMidt %>% 
  group_by(date) %>% 
  mutate(FinsevatnNy = (1.07811 * Midtstova + (-0.79)))
