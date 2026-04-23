library(ggplot2)
library(dplyr)
library(nlme)
library(emmeans)

##make sure dplyr is active rather than rstatix
detach("package:ggpubr", unload = TRUE)
detach("package:rstatix", unload = TRUE)
##ugh whatever fine it doesn't seem to work

setwd("~/Documents/Science/Culture Nutrients")

nflux <- read.csv("master_flux.csv", na.strings = "NA")
nflux %>% head

culture <- nflux %>% filter(nflux$Project.Name == "SG Oyster Culture")
culture %>% head
str(culture)
culture$SOM <- as.numeric(culture$SOM)
culture$DNF_eff <- as.numeric(culture$DNF_eff)

n2flux_2021 <- culture %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Treatment, N.Trial) %>% 
  dplyr::summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
n2flux_2021 %>% head

library(tidyverse)
DNF_eff_val <- subset(culture,!is.na(DNF_eff))
dnfeff_2021 <- DNF_eff_val %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Treatment, N.Trial) %>% 
  dplyr::summarise(mean = mean(DNF_eff), 
                   se = sd(DNF_eff) / sqrt(n()))
dnfeff_2021 %>% head

nhflux_2021 <- culture %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Treatment, N.Trial) %>% 
  dplyr::summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
nhflux_2021 %>% head

ambient <- culture %>% filter(N.Trial == "Ambient")
ambient %>% head

ncols <- n2flux_2021 %>% c("Ambient" = "steelblue3", "Enriched" = "violetred4")
treatcols <- ambient %>% c("Flat" = "orange", 
                           "Bottom Cage" = "skyblue", 
                           "Floating Bags" = "blue",
                           "Floating Bags Edge" = "darkgreen",
                           "Loose on Bottom" = "firebrick2",
                           "Natural Reef" = "purple1")

shapegear <- ambient %>% c("Flat" = "1", 
                           "Bottom Cage" = "12", 
                           "Floating Bags" = "7",
                           "Floating Bags Edge" = "0",
                           "Loose on Bottom" = "10",
                           "Natural Reef" = "8")

treatcolsest <- ambient %>% c("Core Sound" = "orange",
                              "Newport R" = "deepskyblue2",
                              "Stump Sound" = "purple3")

treatcolarea <- ambient %>% c("Core Sound" = "orange",
                              "Newport River" = "deepskyblue2",
                              "Stump Sound" = "purple3")

#set error bars
n2flux_2021 <- n2flux_2021 %>% mutate(ymax = mean + se, 
                            ymin = mean - se)

#set error bar positions
dodge <- position_dodge(width = 0.9)


##N2 flux by culture type##

ggplot(n2flux_2021, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 150)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags Edge", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom",
                              "Natural Reef"))

ggsave("n2flux_2021.png", width = 6, height = 5, dpi = 300)

##errors as average of estuaries rather than individual trials
n2flux_2021all <- culture %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Treatment, N.Trial, Location) %>% 
  dplyr::summarise(flux = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
n2flux_2021all %>% head

ncols <- n2flux_2021all %>% c("Ambient" = "steelblue3", "Enriched" = "violetred4")
treatcols <- ambient %>% c("Flat" = "orange", 
                           "Bottom Cage" = "skyblue", 
                           "Floating Bags" = "blue",
                           "Floating Bags Edge" = "darkgreen",
                           "Loose on Bottom" = "firebrick2",
                           "Natural Reef" = "purple1")

#set error bars
n2flux_2021allS <- n2flux_2021all %>% 
  dplyr::group_by(N.Trial, Treatment) %>% 
  dplyr::summarise(mean = mean(flux), 
            se = sd(flux) / sqrt(n()))
n2flux_2021allS %>% head

n2flux_2021allS <- n2flux_2021allS %>% mutate(ymax = mean + se, 
                                      ymin = mean - se)

#set error bar positions
dodge <- position_dodge(width = 0.9)


##N2 flux by culture type##

ggplot(n2flux_2021allS, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-20, 200)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags Edge", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom",
                              "Natural Reef"))

ggsave("n2flux_2021all.png", width = 6, height = 5, dpi = 300)

##Ammonium
nhflux_2021all <- culture %>% 
  filter(Year == 2021) %>% 
  group_by(Treatment, N.Trial, Location) %>% 
  summarise(flux = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
nhflux_2021all %>% head

ncols <- nhflux_2021all %>% c("Ambient" = "steelblue3", "Enriched" = "violetred4")
treatcols <- ambient %>% c("Flat" = "orange", 
                           "Bottom Cage" = "skyblue", 
                           "Floating Bags" = "blue",
                           "Floating Bags Edge" = "darkgreen",
                           "Loose on Bottom" = "firebrick2",
                           "Natural Reef" = "purple1")

#set error bars
nhflux_2021allS <- nhflux_2021all %>% 
  group_by(N.Trial, Treatment) %>% 
  summarise(mean = mean(flux), 
            se = sd(flux) / sqrt(n()))
nhflux_2021allS %>% head

nhflux_2021allS <- nhflux_2021allS %>% mutate(ymax = mean + se, 
                                              ymin = mean - se)

#set error bar positions
dodge <- position_dodge(width = 0.9)


##NH4 flux by culture type##

ggplot(nhflux_2021allS, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-100, 200)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags Edge", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom",
                              "Natural Reef"))

ggsave("nhflux_2021all.png", width = 6, height = 5, dpi = 300)

##culture types grouped
n2flux_2021g <- culture %>% 
  filter(Year == 2021) %>% 
  group_by(Habitat, N.Trial, Location) %>% 
  summarise(flux = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
n2flux_2021g %>% head

n2flux_2021allG <- n2flux_2021g %>% 
  group_by(N.Trial, Habitat) %>% 
  summarise(mean = mean(flux), 
            se = sd(flux) / sqrt(n()))
n2flux_2021allG %>% head

n2flux_2021allG <- n2flux_2021allG %>% mutate(ymax = mean + se, 
                                              ymin = mean - se)

ggplot(n2flux_2021allG, aes(x = Habitat, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_discrete(limits = c("FLAT", 
                              "CULTURE", 
                              "REEF"),
                   labels = c("Flat", "Culture", "Reef"))

ggsave("n2flux_2021allgroup.png", width = 6, height = 5, dpi = 300)

culturemod <- aov(flux ~ Habitat, data = n2flux_2021g)
summary(culturemod)
TukeyHSD(culturemod)

##model of just ambient
n2flux_2021_amb <- n2flux_2021g %>% filter(N.Trial == "Ambient")
culturemod1 <- aov(flux ~ Habitat, data = n2flux_2021_amb)
summary(culturemod1)
TukeyHSD(culturemod1)

##SOD, 2021
sodflux_2021all <- culture %>% 
  filter(Year == 2021) %>% 
  group_by(Treatment, N.Trial, Location) %>% 
  summarise(flux = mean(SOD), 
            se = sd(SOD) / sqrt(n()))
sodflux_2021all %>% head

##SOM, 2021
##clean for NAs
library(tidyverse)
somflux <- subset(culture,!is.na(SOM))

somflux_2021all <- somflux %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Treatment, N.Trial, Location) %>% 
  dplyr::summarise(flux = mean(SOM), 
            se = sd(SOM) / sqrt(n()))
somflux_2021all %>% head

culture$SOM <- as.numeric(culture$SOM)
somflux_2021all <- culture %>% 
  dplyr::filter(Year == 2021) %>% 
  dplyr::group_by(Treatment, N.Trial, Location) %>% 
  dplyr::summarise(flux = mean(SOM, na.rm =T), 
            se = sd(SOM, na.rm = T) / sqrt(n()))
somflux_2021all %>% head


##with site as a facet wrap

n2fluxsite <- culture %>% 
  dplyr::filter(Year == 2021) %>%
  dplyr::group_by(Treatment, N.Trial, Location) %>% 
  dplyr::summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
n2fluxsite %>% head

n2fluxsite <- n2fluxsite %>% mutate(ymax = mean + se, 
                            ymin = mean - se)

ggplot(n2fluxsite, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "Culture Type", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  facet_wrap(vars(Location)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-80, 400)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags Edge", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom",
                              "Natural Reef"))

ggsave("n2fluxbysite.jpeg", width = 10, height = 8, dpi = 300)

##just ambient
n2fluxsiteA <- n2fluxsite %>% filter(N.Trial == "Ambient")
ggplot(n2fluxsiteA, aes(x = Treatment, y = mean))+
  geom_col(position = "dodge", color = "steelblue3", fill = "gray77") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "Culture Type", 
       y = expression(paste("Average net N"[2]~"-N flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  facet_wrap(vars(Location)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-80, 400)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags Edge", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom",
                              "Natural Reef"))

ggsave("n2fluxbysiteAmb.jpeg", width = 10, height = 8, dpi = 300)

##DNF efficiency across estuaries, ambient only
DNF_eff_val_amb <- DNF_eff_val %>% filter(N.Trial == "Ambient")

dnf_eff_site <- DNF_eff_val_amb %>% 
  dplyr::filter(Year == 2021) %>%
  dplyr::group_by(Treatment, Location) %>% 
  dplyr::summarise(mean = mean(DNF_eff), 
                   se = sd(DNF_eff) / sqrt(n()))
dnf_eff_site %>% head

dnf_eff_site <- dnf_eff_site %>% mutate(ymax = mean + se, 
                                    ymin = mean - se)


ggplot(dnf_eff_site, aes(x = Treatment, y = mean))+
  geom_col(position = "dodge", color = "steelblue3", fill = "gray77") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "Culture Type", 
       y = expression(paste("Average DNF Efficiency (%)")),
       fill = NULL) +
  facet_wrap(vars(Location)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags Edge", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom",
                              "Natural Reef"))


##Ammonium accross estuaries

nhfluxsite <- culture %>% 
  filter(Year == 2021) %>%
  group_by(Treatment, N.Trial, Location) %>% 
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
nhfluxsite %>% head

nhfluxsite <- nhfluxsite %>% mutate(ymax = mean + se, 
                                    ymin = mean - se)

ggplot(nhfluxsite, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "Culture Type", 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  facet_wrap(vars(Location)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-150, 500)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags Edge", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom",
                              "Natural Reef"))

ggsave("nhfluxbysite.jpeg", width = 10, height = 8, dpi = 300)

##quick boxplot##

ggplot(culture, aes(x = Treatment, y = N2.Flux, fill = N.Trial)) +
  geom_boxplot()

##sod and n2 comparison (ambient, including relay)
amball <- nflux %>% filter(N.Trial == "Ambient")

ggplot(amball, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

##by habitat
ggplot(amball, aes(x = SOD, y = N2.Flux, color = Habitat)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

##by area
ggplot(amball, aes(x = SOD, y = N2.Flux, color = Area)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

##SOD and N2 comparison (ambient)

ambient <- culture %>% filter(N.Trial == "Ambient")
ambient %>% head

ggplot(ambient, aes(x = SOD, y = N2.Flux, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE)

##just 2021
ambient_2021 <- ambient %>% filter(Year == 2021)

ggplot(ambient_2021, aes(x = SOD, y = N2.Flux, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                          "Floating Bags Edge", 
                                          "Floating Bags",
                                          "Bottom Cage",
                                          "Loose on Bottom",
                                          "Natural Reef")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

##add geom_stat_func to see r2 values

ggsave("n2SOD.jpeg", width = 12, height = 8)

##sort out by estuary instead of culture type
ggplot(ambient_2021, aes(x = SOD, y = N2.Flux, color = Location)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcolsest, limits = c("Core Sound", 
                                                    "Newport R", 
                                                    "Stump Sound")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)


treatcolsest <- ambient %>% c("Core Sound" = "orange",
                              "Newport R" = "deepskyblue2",
                              "Stump Sound" = "purple3")

##sort out by both estuary and culture type, all trials
ggplot(ambient_2021, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 4, aes(color = Location, 
                             shape = Treatment, 
                             group = interaction(Location,Treatment))) +
  scale_color_manual(values = treatcolsest, limits = c("Core Sound", 
                                                       "Newport R", 
                                                       "Stump Sound")) +
  scale_shape_manual(values = shapegear, limits = c("Flat", 
                                "Floating Bags Edge", 
                                "Floating Bags",
                                "Bottom Cage",
                                "Loose on Bottom",
                                "Natural Reef")) +
  labs(x = expression(paste("Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Net N"[2]~"-N flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  geom_abline(intercept = -25.8, slope = 0.107, linetype = "dotted", color = "deepskyblue2", linewidth = 1)+
  geom_abline(intercept = -14.8, slope = 0.072, linetype = "dotted", color = "orange", linewidth = 1)+
  geom_abline(intercept = -14.7, slope = 0.036, linetype = "dotted", color = "purple3", linewidth = 1)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

ggsave("n2SOD_gear_est2021.jpeg", width = 8, height = 5.5, dpi = 300)

##ditch geom_ablines if you just want the main trend

##HOT DAMNNNNN


##both years, all estuaries

ggplot(ambient, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 2.5, aes(color = Area, 
                             shape = Treatment, 
                             group = interaction(Location,Treatment))) +
  scale_color_manual(values = treatcolarea, limits = c("Core Sound", 
                                                       "Newport River", 
                                                       "Stump Sound")) +
  scale_shape_manual(values = shapegear, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

sodn2 <- lm(N2.Flux ~ SOD, data = ambient)
summary(sodn2)

sodn2all <- lm(N2.Flux ~ SOD*Area, data = ambient)
summary(sodn2all)


##both years, npr only
nprambientbothyears <- ambient %>% dplyr::filter(Location != "Core Sound")
nprambientbothyears <- nprambientbothyears %>% dplyr::filter(Location != "Stump Sound")

ggplot(nprambientbothyears, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 2.5, aes(shape = Treatment)) +
  scale_shape_manual(values = shapegear, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

ggsave("n2SOD_NPR.jpeg", width = 8, height = 5.5, dpi = 300)

##check model of culture type in NPR alone
sodn2modNPR <- lm(N2.Flux ~ SOD*Treatment, data = nprambientbothyears)
summary(sodn2modNPR)
anova(sodn2modNPR)
qqnorm(resid(sodn2modNPR))
qqline(resid(sodn2modNPR))
##not a great fit at those upper points, predictably

##model that compares year to year??
nprbetweenyears <- lm(N2.Flux ~ SOD * Year, data = nprambientbothyears)
summary(nprbetweenyears)
anova(nprbetweenyears)
mod.list <- lstrends(nprbetweenyears, ~ Year, var = "SOD")
mod.list
pairs(mod.list)


##NPR just 2022
ambient_2022 <- ambient %>% dplyr::filter(Year == 2022)

ggplot(ambient_2022, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 2.5, aes(shape = Treatment)) +
  scale_shape_manual(values = shapegear, limits = c("Flat", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

ggsave("n2SOD_NPR2022.jpeg", width = 6.5, height = 5, dpi = 300)

##check model of culture type in NPR, 2022
sodn2modNPR22 <- lm(N2.Flux ~ SOD*Treatment, data = ambient_2022)
summary(sodn2modNPR22)
anova(sodn2modNPR22)
##only effect of sod, not treatment or interaction of the two
qqnorm(resid(sodn2modNPR22))
qqline(resid(sodn2modNPR22))
##pretty good fit
mod.list <- lstrends(sodn2modNPR22, ~ Treatment, var = "SOD")
mod.list
pairs(mod.list)

sodn2modNPR22all <- lm(N2.Flux ~ SOD, data = ambient_2022)
summary(sodn2modNPR22all)

##estuarine position and sod/n2 flux
nprgradient <- ambient_2022 %>% filter(Date == "7/11/22")
Psodn2modNPR22 <- lm(N2.Flux ~ SOD*Location, data = nprgradient)
summary(Psodn2modNPR22)
anova(Psodn2modNPR22)
##effect of sod, location, and interaction of the two
qqnorm(resid(Psodn2modNPR22))
qqline(resid(Psodn2modNPR22))
mod.list <- lstrends(Psodn2modNPR22, ~ Location, var = "SOD")
mod.list
pairs(mod.list)


sodn2npr22 <- lm(N2.Flux ~ SOD, data = ambient_2022)
summary(sodn2npr22)


##NPR, just 2021
npr2021 <- nprambientbothyears %>% dplyr::filter(Year == 2021)

ggplot(npr2021, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 2.5, aes(shape = Treatment)) +
  scale_shape_manual(values = shapegear, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

ggsave("n2SOD_NPR2021.jpeg", width = 6.5, height = 5, dpi = 300)

##check model of culture type in NPR, 2021
sodn2modNPR21 <- lm(N2.Flux ~ SOD*Treatment, data = npr2021)
summary(sodn2modNPR21)
anova(sodn2modNPR21)
##only effect of sod, not treatment or interaction of the two
qqnorm(resid(sodn2modNPR21))
qqline(resid(sodn2modNPR21))
##terrible model

##just fit SOD
SODn2modNPR21 <- lm(N2.Flux ~ SOD, data = npr2021)
summary(SODn2modNPR21)
anova(SODn2modNPR21)
##large effect
qqnorm(resid(SODn2modNPR21))
qqline(resid(SODn2modNPR21))
##still sucks as a model

##compare NPR 2021 and 2022
anova(SODn2modNPR21, sodn2npr22)

##2022, just culture type up-estuary
ggplot(upculamb, aes(x = SOD, y = N2.Flux, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

##estuarine position
ggplot(nprgradient, aes(x = SOD, y = N2.Flux, color = Location)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)


##linear models, 2021
sodn2mod <- lm(N2.Flux ~ SOD, data = ambient_2021)
summary(sodn2mod)
##adj r2 0.446

sodn2modestuary <- lm(N2.Flux ~ SOD*Location, data = ambient_2021)
summary(sodn2modestuary)
anova(sodn2modestuary)
##adj r2 0.682 (best interactive model)
library("lsmeans")
mod.list <- lstrends(sodn2modestuary, ~ Location, var = "SOD")
mod.list
pairs(mod.list)

sodn2modgear <- lm(N2.Flux ~ SOD*Treatment, data = ambient_2021)
summary(sodn2modgear)
anova(sodn2modgear)
##adj r2 0.663
mod.list <- lstrends(sodn2modgear, ~ Treatment, var = "SOD")
mod.list
pairs(mod.list)

sodn2modgearest <- lm(N2.Flux ~SOD*Treatment*Location, data = ambient_2021)
summary(sodn2modgearest)
anova(sodn2modgearest)
qqnorm(resid(sodn2modgearest))
qqline(resid(sodn2modgearest))
##adj r2 0.662
mod.list <- lstrends(sodn2modgearest, ~ Treatment*Location, var = "SOD")
mod.list
pairs(mod.list)
##not enough power to do this


sodn2modgearestAdd <- lm(N2.Flux ~SOD*Treatment+Location, data = ambient_2021)
summary(sodn2modgearestAdd)
anova(sodn2modgearestAdd)
qqnorm(resid(sodn2modgearestAdd))
qqline(resid(sodn2modgearestAdd))
##adj r2 0.693 (good add model)

alladd <- lm(N2.Flux ~ SOD + Treatment + Location, data = ambient_2021)
summary(alladd)
anova(alladd)
qqnorm(resid(alladd))
qqline(resid(alladd))
##mult r2 0.655 (not as good)

addtreat <- lm(N2.Flux ~SOD*Location+Treatment, data = ambient_2021)
summary(addtreat)
anova(addtreat)
qqnorm(resid(addtreat))
qqline(resid(addtreat))
##adj r2 0.693 (best add model)



sodn2lme <- lme4::lmer(N2.Flux ~ SOD*Location + (1|Location:Treatment), data = ambient_2021)
summary(sodn2lme)
anova(sodn2lme)
library(car)
Anova(sodn2lme)
qqnorm(resid(sodn2lme))
qqline(resid(sodn2lme))
##not a great model at upper and lower bounds

##linear model, all
sodn2modALL <- lm(N2.Flux ~ SOD, data = ambient)
summary(sodn2modALL)

##barebones figure of all types together 2021
ggplot(ambient_2021, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

ggsave("n2SODgeneral.jpeg", width = 12, height = 8)

##sod vs som
##make sure SOM is ok
str(ambient_2021)

ggplot(ambient_2021, aes(x = SOM, y = SOD)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment Organic Matter (%)")), 
       y = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

##som vs sod, separated by culture type
ggplot(ambient_2021, aes(x = SOM, y = SOD, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment Organic Matter")), 
       y = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

##SOM vs sod by estuary + culture type
ggplot(ambient_2021, aes(x = SOM, y = SOD)) +
  geom_point(size = 4, aes(color = Location, 
                             shape = Treatment, 
                             group = interaction(Location,Treatment))) +
  scale_color_manual(values = treatcolsest, limits = c("Core Sound", 
                                                       "Newport R", 
                                                       "Stump Sound")) +
  scale_shape_manual(values = shapegear, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Sediment Organic Matter (%)")), 
       y = expression(paste("Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

ggsave("somsod2021.jpeg", width = 8, height = 5.5, dpi = 300)

##model of SOD vs. SOM
sodSOMmod <- lm(SOD ~ SOM, data = ambient_2021)
summary(sodSOMmod)

sodSOMmodCul <- lm(SOD ~ SOM*Treatment, data = ambient_2021)
summary(sodSOMmodCul)
anova(sodSOMmodCul)
mod.list <- lstrends(sodSOMmodCul, ~ Treatment, var = "SOM")
mod.list
pairs(mod.list)


ggplot(ambient_2021, aes(x = SOM, y = SOD, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment Organic Matter")), 
       y = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)


sodSOMmodloc <- lm(SOD ~ SOM*Location, data = ambient_2021)
summary(sodSOMmodloc)
anova(sodSOMmodloc)
mod.list <- lstrends(sodSOMmodloc, ~ Location, var = "SOM")
mod.list
pairs(mod.list)

##just the interaction--not sure this is effective
somxlocation <- lm(SOD ~ SOM:Location, data = ambient_2021)
summary(somxlocation)
anova(somxlocation)
mod.list <- lstrends(somxlocation, ~ Location, var = "SOM")
mod.list
pairs(mod.list)

intsodmod <- lm(SOD ~ SOM*Location*Treatment, data = ambient_2021)
summary(intsodmod)

addsodmod <- lm(SOD ~ SOM+Location+Treatment, data = ambient_2021)
summary(addsodmod)

intaddmod <- lm(SOD ~ SOM*Location+Treatment, data = ambient_2021)
summary(intaddmod)

intaddmod2 <- lm(SOD ~ SOM*Treatment+Location, data = ambient_2021)
summary(intaddmod2)

##sod - som 2022
sodSOMmod22 <- lm(SOD ~ SOM, data = ambient_2022)
summary(sodSOMmod22)

sodSOMmodpos <- lm(SOD ~ SOM*Location, data = amb2022)
summary(sodSOMmodpos)
anova(sodSOMmodpos)

sodSOMmodcul22 <- lm(SOD ~ SOM*Treatment, data = culamb2022)
summary(sodSOMmodcul22)
anova(sodSOMmodcul22)
mod.list <- lstrends(sodSOMmodcul22, ~ Treatment, var = "SOM")
mod.list
pairs(mod.list)

##both years
ggplot(ambient, aes(x = SOD, y = N2.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)



##both years, NH4. weaker relationship
ggplot(ambient, aes(x = SOD, y = NH4.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

##linear model, NH4 and SOD
sodnh4modALL <- lm(NH4.Flux ~ SOD, data = ambient)
summary(sodnh4modALL)

## NH4 annd SOD 2021, by type
ggplot(ambient_2021, aes(x = SOD, y = NH4.Flux, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

##NH4 and SOD 2021, grouped
ggplot(ambient_2021, aes(x = SOD, y = NH4.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment O"[2]~ "Demand ("*mu*"mol O"[2]~"m"^-2*"h"^-1*")")), 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

sodnh4modALL <- lm(NH4.Flux ~ SOD, data = ambient_2021)
summary(sodnh4modALL)

##SOM and N2, all sites 2021

ambient_2021$SOM <- as.numeric(ambient_2021$SOM)

ggplot(ambient_2021, aes(x = SOM, y = N2.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment Organic Matter (%)")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

ggsave("n2SOMgeneral.jpeg", width = 12, height = 8)


##quick grouped linear model
somn2mod <- lm(N2.Flux ~ SOM, data = ambient_2021)
summary(somn2mod)

##dnf efficiency
somdnfemod <- lm(DNF_eff ~ SOM, data = ambient_2021)
summary(somdnfemod)

##dnf eff and sod
soddnfemod <- lm(DNF_eff ~ SOD, data = ambient_2021)
summary(soddnfemod)

##SOM by culture type, 2021
ggplot(ambient_2021, aes(x = SOM, y = N2.Flux, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment Organic Matter (%)")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)


ggsave("n2SOMtypes.jpeg", width = 12, height = 8, dpi = 300)


##SOM by estuary and culture type
ggplot(ambient_2021, aes(x = SOM, y = N2.Flux)) +
  geom_point(size = 4, aes(color = Location, 
                             shape = Treatment, 
                             group = interaction(Location,Treatment))) +
  scale_color_manual(values = treatcolsest, limits = c("Core Sound", 
                                                       "Newport R", 
                                                       "Stump Sound")) +
  scale_shape_manual(values = shapegear, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Sediment Organic Matter (%)")), 
       y = expression(paste("Net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = TRUE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)

ggsave("somn2flux2021.jpeg", width = 8, height = 5.5, dpi = 300)

##model of SOM vs. N2 flux
n2SOMmod <- lm(N2.Flux ~ SOM, data = ambient_2021)
summary(n2SOMmod)

n2SOMmodCul <- lm(N2.Flux ~ SOM*Treatment, data = ambient_2021)
summary(n2SOMmodCul)
anova(n2SOMmodCul)
mod.list <- lstrends(n2SOMmodCul, ~ Treatment, var = "SOM")
mod.list
pairs(mod.list)

n2SOMmodloc <- lm(N2.Flux ~ SOM*Location, data = ambient_2021)
summary(n2SOMmodloc)
anova(n2SOMmodloc)
mod.list <- lstrends(n2SOMmodloc, ~ Location, var = "SOM")
mod.list
pairs(mod.list)

intn2mod <- lm(N2.Flux ~ SOM*Location*Treatment, data = ambient_2021)
summary(intn2mod)

addn2mod <- lm(N2.Flux ~ SOM+Location+Treatment, data = ambient_2021)
summary(addn2mod)

intaddmodn2 <- lm(N2.Flux ~ SOM*Location+Treatment, data = ambient_2021)
summary(intaddmodn2)

intaddmod2n2 <- lm(N2.Flux ~ SOM*Treatment+Location, data = ambient_2021)
summary(intaddmod2n2)

##N2 model predicted by SOM and SOD

DNFmod <- lm(N2.Flux ~ SOD*SOM, data = ambient_2021)
summary(DNFmod)
qqnorm(resid(DNFmod))
qqline(resid(DNFmod))
##only a good fit in the mid range values

DNFmodintadd <- lm(N2.Flux ~ SOD*SOM+Location+Treatment, data = ambient_2021)
summary(DNFmodintadd)
qqnorm(resid(DNFmodintadd))
qqline(resid(DNFmodintadd))
##helps a bit

##NH4 and SOM
ambient_2021$SOM <- as.numeric(ambient_2021$SOM)

ggplot(ambient_2021, aes(x = SOM, y = NH4.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Average Sediment Organic Matter (%)")), 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

##quick grouped linear model
somnh4mod <- lm(NH4.Flux ~ SOM, data = ambient_2021)
summary(somnh4mod)


##SOM by culture type NH4, 2021
ggplot(ambient_2021, aes(x = SOM, y = NH4.Flux, color = Treatment)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = treatcols, limits = c("Flat", 
                                                    "Floating Bags Edge", 
                                                    "Floating Bags",
                                                    "Bottom Cage",
                                                    "Loose on Bottom",
                                                    "Natural Reef")) +
  labs(x = expression(paste("Average Sediment Organic Matter (%)")), 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)


##estuarine position 2022, floating bags (position is created later down in the code)
summer2 <- culture %>% filter(Year == 2022)
position <- summer2 %>% filter(Date == "7/11/22")
culture2022 <- summer2 %>% filter(Date == "7/25/22")
pos <- position %>% group_by(N.Trial, Location, Treatment) %>%
  dplyr::summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
pos

pos <- pos %>% mutate(ymax = mean + se, 
                                    ymin = mean - se)

pos$Location <- factor(pos$Location,
                      levels = c("Frey UP", "Boyd Mid", "Bayer DWN"))

site_labeller <- c(
  "Frey UP" = "Up Estuary",
  'Boyd Mid' = "Mid Estuary",
  'Bayer DWN' = "Down Estuary")

ggplot(pos, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  facet_wrap(vars(Location), labeller = as_labeller(site_labeller)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-10, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags"))

ggsave("2022position.png", width = 6, height = 4.5)

posamb <- pos %>% filter(N.Trial == "Ambient")

ecols <- posamb %>% c("Frey UP" = "steelblue4", "Boyd Mid" = "mediumorchid4", "Bayer DWN" = "firebrick")

install.packages("viridis")
library(viridis)
escols <- pos %>% c("Frey UP" = "#440154FF", "Boyd Mid" = "#21908CFF", "Bayer DWN" = "#FDE725FF")

ggplot(pos, aes(x = Treatment, y = mean, fill = Location))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Upper", "Middle", "Lower"), 
                    values = escols) +
  facet_wrap(vars(N.Trial)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-10, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags"))

ggsave("estgradient.png", width = 6, height = 5, dpi = 300)

##estuarine gradient just ambient n2 fluxes
ggplot(posamb, aes(x = Treatment, y = mean))+
  geom_col(position = "dodge", color = "steelblue3", fill = "gray77") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"-N flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  facet_wrap(vars(Location), labeller = as_labeller(site_labeller)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-10, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags"))

ggsave("2022positionAmb.png", width = 6, height = 4.5)

##DNF efficiency, estuarine position
posDNFe <- position %>% 
  dplyr::group_by(N.Trial, Location, Treatment) %>%
  dplyr::summarise(mean = mean(DNF_eff), 
            se = sd(DNF_eff) / sqrt(n()))
posDNFe
posDNFeAmb <- posDNFe %>% filter(N.Trial == "Ambient")

posDNFeAmb <- posDNFeAmb %>% mutate(ymax = mean + se, 
                            ymin = mean - se)

posDNFeAmb$Location <- factor(posDNFeAmb$Location,
                          levels = c("Frey UP", "Boyd Mid", "Bayer DWN"))

site_labeller <- c(
  "Frey UP" = "Up Estuary",
  'Boyd Mid' = "Mid Estuary",
  'Bayer DWN' = "Down Estuary")

ggplot(posDNFeAmb, aes(x = Treatment, y = mean))+
  geom_col(position = "dodge", color = "steelblue3", fill = "gray77") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average DNF Efficiency")),
       fill = NULL) +
  facet_wrap(vars(Location), labeller = as_labeller(site_labeller)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags"))



##NH4 estuarine position 2022, floating bags
posnh4 <- position %>% group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
posnh4

posnh4 <- posnh4 %>% mutate(ymax = mean + se, 
                      ymin = mean - se)

posnh4$Location <- factor(posnh4$Location,
                       levels = c("Frey UP", "Boyd Mid", "Bayer DWN"))

site_labeller <- c(
  "Frey UP" = "Up Estuary",
  'Boyd Mid' = "Mid Estuary",
  'Bayer DWN' = "Down Estuary")

ggplot(posnh4, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  facet_wrap(vars(Location), labeller = as_labeller(site_labeller)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-50, 150)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags"))

ggsave("2022positionNH4.png", width = 6, height = 4.5)

posambnh4 <- posnh4 %>% filter(N.Trial == "Ambient")

ecols <- posambnh4 %>% c("Frey UP" = "steelblue4", "Boyd Mid" = "mediumorchid4", "Bayer DWN" = "firebrick")

install.packages("viridis")
library(viridis)
escols <- posnh4 %>% c("Frey UP" = "#440154FF", "Boyd Mid" = "#21908CFF", "Bayer DWN" = "#FDE725FF")

ggplot(posnh4, aes(x = Treatment, y = mean, fill = Location))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Upper", "Middle", "Lower"), 
                    values = escols) +
  facet_wrap(vars(N.Trial)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-25, 150)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags"))

ggsave("estgradientnh4.png", width = 6, height = 5, dpi = 300)

##culture 2022 figure, up-estuary only
cul2022 <- culture2022 %>% 
  dplyr::filter(Location == "NPR UP") %>%
  dplyr::group_by(N.Trial, Location, Treatment) %>%
  dplyr::summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
cul2022

cul2022 <- cul2022 %>% mutate(ymax = mean + se, 
                      ymin = mean - se)

ggplot(cul2022, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 150)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom"))

ggsave("n2flux_2022.png", width = 6, height = 5)

##culture, NPR up-estuary 2022, ambient only
cul2022A <- cul2022 %>% filter(N.Trial == "Ambient")
ggplot(cul2022A, aes(x = Treatment, y = mean))+
  geom_col(position = "dodge", color = "steelblue3", fill = "gray77") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"-N flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom"))

ggsave("n2flux_2022amb.png", width = 6, height = 5)


nprambcul2022 <- aov(N2.Flux ~ Treatment, data = upculamb)
summary(nprambcul2022)
TukeyHSD(nprambcul2022)

##DNF efficiency up -estuary culture type
cul2022dnf <- culture2022 %>% 
  dplyr::filter(Location == "NPR UP") %>%
  dplyr::group_by(N.Trial, Location, Treatment) %>%
  dplyr::summarise(mean = mean(DNF_eff), 
                   se = sd(DNF_eff) / sqrt(n()))
cul2022dnf

cul2022dnf <- cul2022dnf %>% mutate(ymax = mean + se, 
                              ymin = mean - se)

cul2022Adnf <- cul2022dnf %>% filter(N.Trial == "Ambient")

ggplot(cul2022Adnf, aes(x = Treatment, y = mean))+
  geom_col(position = "dodge", color = "steelblue3", fill = "gray77") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average DNF Efficiency")),
       fill = NULL) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom"))

##effect of culture type on DNF efficiency
nprambcul2022dnf <- aov(DNF_eff ~ Treatment, data = upculamb)
summary(nprambcul2022dnf)
TukeyHSD(nprambcul2022dnf)
multcompLetters4(nprambcul2022dnf,  TukeyHSD(aov(nprambcul2022dnf)), reversed = FALSE)


##NH4 by treatment, 2022, up-estuary NPR
cul2022NH4 <- culture2022 %>% filter(Location == "NPR UP") %>%
  group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
cul2022NH4

cul2022NH4 <- cul2022NH4 %>% mutate(ymax = mean + se, 
                              ymin = mean - se)

ggplot(cul2022NH4, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net NH"[4]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-25, 100)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom"))

ggsave("nh4flux_2022.png", width = 6, height = 5)

##culture 2022 figure, all
culture_site2022 <- culture2022 %>% group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
culture_site2022

culture_site2022 <- culture_site2022 %>% mutate(ymax = mean + se, 
                              ymin = mean - se)

ggplot(culture_site2022, aes(x = Treatment, y = mean, fill = N.Trial))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  scale_fill_manual(labels = c("Ambient Seawater", "Nitrate-enriched"), 
                    values = ncols) +
  facet_wrap(vars(Location), labeller = as_labeller(site_labeller)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(-2, 150)) +
  scale_x_discrete(limits = c("Flat", 
                              "Floating Bags",
                              "Bottom Cage",
                              "Loose on Bottom"))

ggsave("n2fluxcultureposition_2022.png", width = 6, height = 5)


##relay nutrient data

relaynflux <- read.csv("relay_n_r2.csv", na.strings = "NA")
relaynflux %>% head

relayflux <- relaynflux %>% 
  group_by(Treatment) %>% 
  summarise(mean = mean(N2.Flux.Ambient), 
            se = sd(N2.Flux.Ambient) / sqrt(n()))
relayflux %>% head

siteflux <- relaynflux %>% 
  group_by(Treatment, Site) %>%
  summarise(mean = mean(N2.Flux.Ambient), 
            se = sd(N2.Flux.Ambient) / sqrt(n()))
siteflux %>% head

relaycols <- relaynflux %>% c("Relay" = "steelblue3", "No Relay" = "darkgoldenrod2")

##simple relay N flux
relayflux <- relayflux %>% mutate(ymax = mean + se, 
                                    ymin = mean - se)

ggplot(relayflux, aes(x = Treatment, y = mean))+
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = dodge, width = 0.25)+
  labs(x = "", 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")),
       fill = NULL) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
  scale_x_discrete(limits = c("No Relay", "Relay"))

ggsave("relaynflux.jpeg", height = 10, width = 12)

##oyster density and N flux
ggplot(relaynflux, aes(x = oysters_m2, y = N2.Flux.Ambient, color = Treatment)) +
  geom_point(size = 3.5) +
  scale_color_manual(values = relaycols, limits = c("Relay", 
                                                    "No Relay")) +
  labs(x = expression(paste("Oysters per m"^2)), 
       y = expression(paste("Net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black"))

ggsave("relaynfluxoysters.jpeg", width = 12, height = 8)

##legal oyster density and N flux
ggplot(relaynflux, aes(x = legal_m2, y = N2.Flux.Ambient, color = Treatment)) +
  geom_point(size = 3.5) +
  scale_color_manual(values = relaycols, limits = c("Relay", 
                                                    "No Relay")) +
  labs(x = expression(paste("Legal oysters (>75mm) per m"^2)), 
       y = expression(paste("Net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")")))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black"))

ggsave("relaynfluxlegal.jpeg", width = 12, height = 8)

##tests

##2021
##culture type on N flux, ambient
nflux_culture_type <- lme(N2.Flux ~ Treatment, random = ~1|Location, method = "REML", na.action = na.exclude, data = ambient_2021)
summary(nflux_culture_type)
anova(nflux_culture_type, test = "F")
emmeans(nflux_culture_type, pairwise ~ Treatment)
qqnorm(resid(nflux_culture_type))
qqline(resid(nflux_culture_type))

##culture vs. natural on N flux, ambient
nflux_culture <- lme(N2.Flux ~ Habitat, random = ~1|Location, method = "REML", na.action = na.exclude, data = ambient_2021)
summary(nflux_culture)
anova(nflux_culture, test = "F")
emmeans(nflux_culture, pairwise ~ Habitat)
qqnorm(resid(nflux_culture))
qqline(resid(nflux_culture))
TukeyHSD(nflux_culture)

nflux_culture_habloc <- lm(N2.Flux ~ Habitat * Location, data = ambient_2021)
summary(nflux_culture_habloc)
anova(nflux_culture_habloc)
qqnorm(resid(nflux_culture_habloc))
qqline(resid(nflux_culture_habloc))
emmeans(nflux_culture_habloc, pairwise ~ Habitat)

##culture type on NH4 flux, ambient
nhflux_culture_type <- lme(NH4.Flux ~ Treatment, random = ~1|Location, method = "REML", na.action = na.exclude, data = ambient_2021)
summary(nhflux_culture_type)
anova(nhflux_culture_type, test = "F")
emmeans(nhflux_culture_type, pairwise ~ Treatment)
qqnorm(resid(nhflux_culture_type))
qqline(resid(nhflux_culture_type))

##culture type on DNF efficiency, ambient
dnfflux_culture_type <- lme(DNF_eff ~ Treatment, random = ~1|Location, method = "REML", na.action = na.exclude, data = ambient_2021)
summary(dnfflux_culture_type)
anova(dnfflux_culture_type, test = "F")
emmeans(dnfflux_culture_type, pairwise ~ Treatment)
qqnorm(resid(dnfflux_culture_type))
qqline(resid(dnfflux_culture_type))

##interaction of culture type and location
nflux_culture_est <- lm(N2.Flux ~ Treatment * Location, data = ambient_2021)
summary(nflux_culture_est)
anova(nflux_culture_est)

fluxestamb <- n2flux_2021all %>% filter(N.Trial == "Ambient")

nflux_culture_est <- lm(flux ~ Treatment, data = fluxestamb)
summary(nflux_culture_est)
anova(nflux_culture_est)

##culture type on N flux, spike
spike <- culture %>% filter(N.Trial == "Enriched")
spike %>% head
spike_2021 <- spike %>% filter(Year == 2021)
spikenflux_culture_type <- lme(N2.Flux ~ Treatment, random = ~1|Location, method = "REML", na.action = na.exclude, data = spike_2021)
summary(spikenflux_culture_type)
anova(spikenflux_culture_type, test = "F")
emmeans(spikenflux_culture_type, pairwise ~ Treatment)
qqnorm(resid(spikenflux_culture_type))
qqline(resid(spikenflux_culture_type))

##culture grouped
spikenflux_culture <- lme(N2.Flux ~ Habitat, random = ~1|Location, method = "REML", na.action = na.exclude, data = spike_2021)
summary(spikenflux_culture)
anova(spikenflux_culture, test = "F")
emmeans(spikenflux_culture, pairwise ~ Habitat)
qqnorm(resid(spikenflux_culture))
qqline(resid(spikenflux_culture))

##Spiked for NH4
spike <- culture %>% filter(N.Trial == "Enriched")
spike %>% head
spike_2021 <- spike %>% filter(Year == 2021)
spikenhflux_culture_type <- lme(NH4.Flux ~ Treatment, random = ~1|Location, method = "REML", na.action = na.exclude, data = spike_2021)
summary(spikenhflux_culture_type)
anova(spikenhflux_culture_type, test = "F")
emmeans(spikenhflux_culture_type, pairwise ~ Treatment)
qqnorm(resid(spikenhflux_culture_type))
qqline(resid(spikenhflux_culture_type))


##estuary specific N flux ANOVAs
##NPR
library(multcomp)
install.packages("multcompView")
library(multcompView)

npr <- culture %>% filter(Location == "Newport R", Year == 2021)
npramb <- npr %>% filter(N.Trial == "Ambient")
nprspike <- npr %>% filter(N.Trial == "Enriched")

nprambaov <- aov(N2.Flux ~ Treatment, data = npramb)
summary(nprambaov)
TukeyHSD(nprambaov)
multcompLetters4(nprambaov,  TukeyHSD(aov(nprambaov)), reversed = FALSE)

nprspikeaov <- aov(N2.Flux ~ Treatment, data = nprspike)
summary(nprspikeaov)
TukeyHSD(nprspikeaov)
multcompLetters4(nprspikeaov,  TukeyHSD(aov(nprspikeaov)), reversed = FALSE)

##Stump Sound
ss <- culture %>% filter(Location == "Stump Sound", Year == 2021)
ssamb <- ss %>% filter(N.Trial == "Ambient")
ssspike <- ss %>% filter(N.Trial == "Enriched")

ssambaov <- aov(N2.Flux ~ Treatment, data = ssamb)
summary(ssambaov)
TukeyHSD(ssambaov)
multcompLetters4(ssambaov,  TukeyHSD(aov(ssambaov)), reversed = FALSE)

ssspikeaov <- aov(N2.Flux ~ Treatment, data = ssspike)
summary(ssspikeaov)
TukeyHSD(ssspikeaov)
multcompLetters4(ssspikeaov,  TukeyHSD(aov(ssspikeaov)), reversed = FALSE)


##Core Sound
cs <- culture %>% filter(Location == "Core Sound", Year == 2021)
csamb <- cs %>% filter(N.Trial == "Ambient")
csspike <- cs %>% filter(N.Trial == "Enriched")

csambaov <- aov(N2.Flux ~ Treatment, data = csamb)
summary(csambaov)
TukeyHSD(csambaov)
multcompLetters4(csambaov,  TukeyHSD(aov(csambaov)), reversed = FALSE)

csspikeaov <- aov(N2.Flux ~ Treatment, data = csspike)
summary(csspikeaov)
TukeyHSD(csspikeaov)
multcompLetters4(csspikeaov,  TukeyHSD(aov(csspikeaov)), reversed = FALSE)


##estuary specific NH4 flux ANOVAs
##NPR
library(multcomp)
install.packages("multcompView")
library(multcompView)

npr <- culture %>% filter(Location == "Newport R", Year == 2021)
npramb <- npr %>% filter(N.Trial == "Ambient")
nprspike <- npr %>% filter(N.Trial == "Enriched")

nprambaovNH4 <- aov(NH4.Flux ~ Treatment, data = npramb)
summary(nprambaovNH4)
TukeyHSD(nprambaovNH4)
multcompLetters4(nprambaovNH4,  TukeyHSD(aov(nprambaovNH4)), reversed = FALSE)

nprspikeaovNH4 <- aov(NH4.Flux ~ Treatment, data = nprspike)
summary(nprspikeaovNH4)
TukeyHSD(nprspikeaovNH4)
multcompLetters4(nprspikeaovNH4,  TukeyHSD(aov(nprspikeaovNH4)), reversed = FALSE)

nprspikenh4 <- nprspike %>% group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
nprspikenh4

##Stump Sound
ss <- culture %>% filter(Location == "Stump Sound", Year == 2021)
ssamb <- ss %>% filter(N.Trial == "Ambient")
ssspike <- ss %>% filter(N.Trial == "Enriched")

ssambaovNH4 <- aov(NH4.Flux ~ Treatment, data = ssamb)
summary(ssambaovNH4)
TukeyHSD(ssambaovNH4)
multcompLetters4(ssambaovNH4,  TukeyHSD(aov(ssambaovNH4)), reversed = FALSE)

ssspikeaovNH4 <- aov(NH4.Flux ~ Treatment, data = ssspike)
summary(ssspikeaovNH4)
TukeyHSD(ssspikeaovNH4)
multcompLetters4(ssspikeaovNH4,  TukeyHSD(aov(ssspikeaovNH4)), reversed = FALSE)


##Core Sound
cs <- culture %>% filter(Location == "Core Sound", Year == 2021)
csamb <- cs %>% filter(N.Trial == "Ambient")
csspike <- cs %>% filter(N.Trial == "Enriched")

csambaovNH4 <- aov(NH4.Flux ~ Treatment, data = csamb)
summary(csambaovNH4)
TukeyHSD(csambaovNH4)
multcompLetters4(csambaovNH4,  TukeyHSD(aov(csambaovNH4)), reversed = FALSE)

csspikeaovNH4 <- aov(NH4.Flux ~ Treatment, data = csspike)
summary(csspikeaovNH4)
TukeyHSD(csspikeaovNH4)
multcompLetters4(csspikeaovNH4,  TukeyHSD(aov(csspikeaovNH4)), reversed = FALSE)

csspikenh4 <- csspike %>% group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
csspikenh4

##DNF efficiency
##NPR

npr <- culture %>% filter(Location == "Newport R", Year == 2021)
npramb <- npr %>% filter(N.Trial == "Ambient")

nprambaovDNFe <- aov(DNF_eff ~ Treatment, data = npramb)
summary(nprambaovDNFe)
TukeyHSD(nprambaovDNFe)
multcompLetters4(nprambaovDNFe,  TukeyHSD(aov(nprambaovDNFe)), reversed = FALSE)


##Stump Sound
ss <- culture %>% filter(Location == "Stump Sound", Year == 2021)
ssamb <- ss %>% filter(N.Trial == "Ambient")

ssambaovDNFe <- aov(DNF_eff ~ Treatment, data = ssamb)
summary(ssambaovDNFe)
TukeyHSD(ssambaovDNFe)
multcompLetters4(ssambaovDNFe,  TukeyHSD(aov(ssambaovDNFe)), reversed = FALSE)


##Core Sound
cs <- culture %>% filter(Location == "Core Sound", Year == 2021)
csamb <- cs %>% filter(N.Trial == "Ambient")

csambaovDNFe <- aov(DNF_eff ~ Treatment, data = csamb)
summary(csambaovDNFe)
TukeyHSD(csambaovDNFe)
multcompLetters4(csambaovDNFe,  TukeyHSD(aov(csambaovDNFe)), reversed = FALSE)



##2022
summer2 <- culture %>% filter(Year == 2022)
position <- summer2 %>% filter(Date == "7/11/22")
culture2022 <- summer2 %>% filter(Date == "7/25/22")

##SOD 2022, position
sodflux_2022P <- position %>% 
  group_by(Treatment, N.Trial, Location) %>% 
  summarise(flux = mean(SOD), 
            se = sd(SOD) / sqrt(n()))
sodflux_2022P %>% head

sodPosMod <- aov(SOD ~ Location, data = amb2022)
summary(sodPosMod)
TukeyHSD(sodPosMod)

##SOM 2022, position
somPosMod <- aov(SOM ~ Location, data = amb2022)
summary(somPosMod)
TukeyHSD(somPosMod)

##som position and oysters
somPosModOy <- aov(SOM ~ Location*Treatment, data = amb2022)
summary(somPosModOy)
anova(somPosModOy)
TukeyHSD(somPosModOy)


##SOD 2022, culture
sodflux_2022C <- culture2022 %>% 
  group_by(Treatment, N.Trial, Location) %>% 
  summarise(flux = mean(SOD), 
            se = sd(SOD) / sqrt(n()))
sodflux_2022C %>% head

sodCulMod <- aov(SOD ~ Treatment, data = culamb2022)
summary(sodCulMod)
TukeyHSD(sodCulMod)

##only up estuary (remove mid)
upsodCulMod <- aov(SOD ~ Treatment, data = upculamb)
summary(upsodCulMod)
TukeyHSD(upsodCulMod)

##Position
##ambient
##quick graph
ggplot(amb2022) + 
  aes(x = Treatment, y = N2.Flux, fill = Location) +
  geom_boxplot()

amb2022 <- position %>% filter(N.Trial == "Ambient")
ambaov2022 <- aov(N2.Flux ~ Treatment*Location, data = amb2022)
summary(ambaov2022)
TukeyHSD(ambaov2022)
multcompLetters4(ambaov2022,TukeyHSD(aov(ambaov2022)), reversed = FALSE)
qqnorm(resid(ambaov2022))
qqline(resid(ambaov2022))

##NH4 by position
amb2022nh4 <- position %>% filter(N.Trial == "Ambient")
ambaov2022nh4 <- aov(NH4.Flux ~ Treatment*Location, data = amb2022)
summary(ambaov2022nh4)
TukeyHSD(ambaov2022nh4)
multcompLetters4(ambaov2022nh4,TukeyHSD(aov(ambaov2022nh4)), reversed = FALSE)

##remove mid
ambupdown <- amb2022 %>% filter(Location != "Boyd Mid")
ambaovupdown <- aov(N2.Flux ~ Treatment*Location, data = ambupdown)
summary(ambaovupdown)
TukeyHSD(ambaovupdown)
multcompLetters4(ambaovupdown,TukeyHSD(aov(ambaovupdown)), reversed = FALSE)
qqnorm(resid(ambaovupdown))
qqline(resid(ambaovupdown))


frey <- amb2022 %>% filter(Location == "Frey UP")
freyaov <- aov(N2.Flux ~ Treatment, data = frey)
summary(freyaov)
upavg <- frey %>% group_by(Treatment) %>%
  summarise(mean = mean(N2.Flux), 
          se = sd(N2.Flux) / sqrt(n()))
upavg

boyd <- amb2022 %>% filter(Location == "Boyd Mid")
boydaov <- aov(N2.Flux ~ Treatment, data = boyd)
summary(boydaov)
midavg <- boyd %>% group_by(Treatment) %>%
  summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
midavg

bayer <- amb2022 %>% filter(Location == "Bayer DWN")
bayeraov <- aov(N2.Flux ~ Treatment, data = bayer)
summary(bayeraov)
downavg <- bayer %>% group_by(Treatment) %>%
  summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
downavg

##nh4 ambient by position
frey <- amb2022 %>% filter(Location == "Frey UP")
freyaovnh4 <- aov(NH4.Flux ~ Treatment, data = frey)
summary(freyaovnh4)
upavgnh4 <- frey %>% group_by(Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
upavgnh4

boyd <- amb2022 %>% filter(Location == "Boyd Mid")
boydaovnh4 <- aov(NH4.Flux ~ Treatment, data = boyd)
summary(boydaovnh4)
midavgnh4 <- boyd %>% group_by(Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
midavgnh4

bayer <- amb2022 %>% filter(Location == "Bayer DWN")
bayeraovnh4 <- aov(NH4.Flux ~ Treatment, data = bayer)
summary(bayeraovnh4)
downavgnh4 <- bayer %>% group_by(Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
downavgnh4

##spiked
spike2022 <- position %>% filter(N.Trial == "Enriched")
spikeaov2022 <- aov(N2.Flux ~ Treatment*Location, data = spike2022)
summary(spikeaov2022)
TukeyHSD(spikeaov2022)

##nh4 spike individual
upspike <- spike2022 %>% filter(Location == "Frey UP")
upspikeaovnh4 <- aov(NH4.Flux ~ Treatment, data = upspike)
summary(upspikeaovnh4)

midspike <- spike2022 %>% filter(Location == "Boyd Mid")
midspikeaovnh4 <- aov(NH4.Flux ~ Treatment, data = midspike)
summary(midspikeaovnh4)

downspike <- spike2022 %>% filter(Location == "Bayer DWN")
downspikeaovnh4 <- aov(NH4.Flux ~ Treatment, data = downspike)
summary(downspikeaovnh4)

##spiked NH4 total
spike2022 <- position %>% filter(N.Trial == "Enriched")
spikeaov2022nh4 <- aov(NH4.Flux ~ Treatment*Location, data = spike2022)
summary(spikeaov2022nh4)
TukeyHSD(spikeaov2022nh4)

##n2 by location
upspike <- spike2022 %>% filter(Location == "Frey UP")
upspikeaov <- aov(N2.Flux ~ Treatment, data = upspike)
summary(upspikeaov)

midspike <- spike2022 %>% filter(Location == "Boyd Mid")
midspikeaov <- aov(N2.Flux ~ Treatment, data = midspike)
summary(midspikeaov)

downspike <- spike2022 %>% filter(Location == "Bayer DWN")
downspikeaov <- aov(N2.Flux ~ Treatment, data = downspike)
summary(downspikeaov)

##DNF efficiency
amb2022 <- position %>% filter(N.Trial == "Ambient")
ambaov2022dnfe <- aov(DNF_eff ~ Treatment*Location, data = amb2022)
summary(ambaov2022dnfe)
TukeyHSD(ambaov2022dnfe)
multcompLetters4(ambaov2022dnfe,TukeyHSD(aov(ambaov2022dnfe)), reversed = FALSE)
qqnorm(resid(ambaov2022dnfe))
qqline(resid(ambaov2022dnfe))

##remove mid
ambupdown <- amb2022 %>% filter(Location != "Boyd Mid")
ambaovupdowndnfe <- aov(DNF_eff ~ Treatment*Location, data = ambupdown)
summary(ambaovupdowndnfe)
TukeyHSD(ambaovupdowndnfe)
multcompLetters4(ambaovupdowndnfe,TukeyHSD(aov(ambaovupdowndnfe)), reversed = FALSE)
qqnorm(resid(ambaovupdowndnfe))
qqline(resid(ambaovupdowndnfe))

##each position test
freyaovdnfe <- aov(DNF_eff ~ Treatment, data = frey)
summary(freyaovdnfe)
upavgdnfe <- frey %>% group_by(Treatment) %>%
  summarise(mean = mean(DNF_eff), 
            se = sd(DNF_eff) / sqrt(n()))
upavgdnfe

boydaovdnfe <- aov(DNF_eff ~ Treatment, data = boyd)
summary(boydaovdnfe)
midavgdnfe <- boyd %>% group_by(Treatment) %>%
  summarise(mean = mean(DNF_eff), 
            se = sd(DNF_eff) / sqrt(n()))
midavgdnfe

bayeraovdnfe <- aov(DNF_eff ~ Treatment, data = bayer)
summary(bayeraovdnfe)
downavgdnfe <- bayer %>% group_by(Treatment) %>%
  summarise(mean = mean(DNF_eff), 
            se = sd(DNF_eff) / sqrt(n()))
downavgdnfe



##Culture Type
##ambient
culamb2022 <- culture2022 %>% filter(N.Trial == "Ambient")
culambaov2022 <- aov(N2.Flux ~ Treatment*Location, data = culamb2022)
summary(culambaov2022)
TukeyHSD(culambaov2022)
qqnorm(resid(culambaov2022))
qqline(resid(culambaov2022))

##up only
upculamb <- culamb2022 %>% filter(Location == "NPR UP")
upculaov <- aov(N2.Flux ~ Treatment, data = upculamb)
summary(upculaov)
TukeyHSD(upculaov)
qqnorm(resid(upculaov))
qqline(resid(upculaov))
multcompLetters4(upculaov,  TukeyHSD(aov(upculaov)), reversed = FALSE)

upculambn2 <- upculamb %>% group_by(N.Trial, Location, Treatment) %>%
  dplyr::summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))
upculambn2

##mid only
midculamb <- culamb2022 %>% filter(Location == "NPR MID")
midculaov <- aov(N2.Flux ~ Treatment, data = midculamb)
summary(midculaov)

##spiked
culspike2022 <- culture2022 %>% filter(N.Trial == "Enriched")
culspikeaov2022 <- aov(N2.Flux ~ Treatment*Location, data = culspike2022)
summary(culspikeaov2022)
TukeyHSD(culspikeaov2022)

upculspike <- culspike2022 %>% filter(Location == "NPR UP")
upculspikeaov2022 <- aov(N2.Flux ~ Treatment, data = upculspike)
summary(upculspikeaov2022)
TukeyHSD(upculspikeaov2022)
multcompLetters4(upculspikeaov2022,  TukeyHSD(aov(upculspikeaov2022)), reversed = FALSE)

midculspike <- culspike2022 %>% filter(Location == "NPR MID")

midculspiken2 <- midculspike %>% group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(N2.Flux), 
            se = sd(N2.Flux) / sqrt(n()))




##Culture Type, NH4, 2022
##up NPR
upculamb <- culamb2022 %>% filter(Location == "NPR UP")
upculaovNH4 <- aov(NH4.Flux ~ Treatment, data = upculamb)
summary(upculaovNH4)
TukeyHSD(upculaovNH4)

upculambnh4 <- upculamb %>% group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
upculambnh4

##spiked
culspike2022 <- culture2022 %>% filter(N.Trial == "Enriched")

culspike <- culspike2022 %>% group_by(N.Trial, Location, Treatment) %>%
  summarise(mean = mean(NH4.Flux), 
            se = sd(NH4.Flux) / sqrt(n()))
culspike


upculspike <- culspike2022 %>% filter(Location == "NPR UP")
upculspikeaov2022NH4 <- aov(NH4.Flux ~ Treatment, data = upculspike)
summary(upculspikeaov2022NH4)
TukeyHSD(upculspikeaov2022NH4)
multcompLetters4(upculspikeaov2022NH4,  TukeyHSD(aov(upculspikeaov2022NH4)), reversed = FALSE)

##SOM 2022
amb2022$SOM <- as.numeric(amb2022$SOM)


ggplot(amb2022, aes(x = SOM, y = N2.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Sediment Organic Matter (%)")), 
       y = expression(paste("Net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

ggsave("n2SOMgeneral2022.jpeg", width = 12, height = 8)

##quick grouped linear model
somn2modPOS <- lm(N2.Flux ~ SOM, data = amb2022)
summary(somn2modPOS)

##SOM by position, 2022
ggplot(amb2022, aes(x = SOM, y = N2.Flux, color = Location)) +
  geom_point(size = 2.5) +
  scale_color_manual(labels = c("Lower", "Middle", "Upper"), 
                     values = escols) +
  labs(x = expression(paste("Sediment Organic Matter (%)")), 
       y = expression(paste("Net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

ggsave("n2SOMgeneral2022pos.jpeg", width = 7, height = 5)

somn2modPOS1 <- lm(N2.Flux ~ SOM*Location, data = amb2022)
summary(somn2modPOS1)
anova(somn2modPOS1)
mod.list <- lstrends(somn2modPOS1, ~ Location, var = "SOM")
mod.list
pairs(mod.list)

somn2modPOS2 <- lm(N2.Flux ~ SOM*Location*Treatment, data = amb2022)
summary(somn2modPOS2)
anova(somn2modPOS2)
mod.list <- lstrends(somn2modPOS2, ~ Location*Treatment, var = "SOM")
mod.list
pairs(mod.list)
## not enough statistical power

somn2modPOS3 <- lm(N2.Flux ~ SOM*Treatment, data = culamb2022)
summary(somn2modPOS3)
anova(somn2modPOS3)
mod.list <- lstrends(somn2modPOS3, ~ Treatment, var = "SOM")
mod.list
pairs(mod.list)

##just up estuary
somupcul <- lm(N2.Flux ~ SOM*Treatment, data = upculamb)
summary(somupcul)
anova(somupcul)
mod.list <- lstrends(somupcul, ~ Treatment, var = "SOM")
mod.list
pairs(mod.list)


##Using both dates, 2022
amb2022all <- ambient %>% filter(Year==2022)

amb2022all$SOM <- as.numeric(amb2022all$SOM)

ggplot(amb2022all, aes(x = SOM, y = N2.Flux)) +
  geom_point(size = 2.5) +
  labs(x = expression(paste("Sediment Organic Matter (%)")), 
       y = expression(paste("Net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method = lm, se = FALSE)

ggsave("n2SOMgeneral.jpeg", width = 12, height = 8)

##quick grouped linear model
somn2modPOSall <- lm(N2.Flux ~ SOM, data = amb2022all)
summary(somn2modPOSall)

##SOM by position, 2022

amb2022all <- replace(amb2022all$Location, amb2022all$Location=="NPR UP", "Frey UP")

ggplot(amb2022, aes(x = SOM, y = N2.Flux, color = Location)) +
  geom_point(size = 2.5) +
  scale_color_manual(labels = c("Lower", "Middle", "Upper"), 
                     values = escols) +
  labs(x = expression(paste("Average Sediment Organic Matter (%)")), 
       y = expression(paste("Average net N"[2]~"flux ("*mu*"mol N m"^-2*"h"^-1*")"))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 17, margin = margin(t = 12)),
        axis.title.y = element_text(size = 15, margin = margin(r = 12)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_smooth(method = lm, se = FALSE) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)




