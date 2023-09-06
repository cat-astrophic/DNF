# This script performs the econometrics analysis for the DNF paper

# Loading libraries

library(AER)
library(stargazer)
library(sandwich)
library(jtools)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(modelsummary)

# Directory info

direc <- 'D:/DNF/'

# Reading in the data

data <- read.csv(paste(direc, 'data/data.csv', sep = ''))

# Subsetting to remove observations too far from stations

data <- data %>% filter(Event_Station_Distance_Miles < 10)

# Subsetting for the DNF analysis (all data is valid for the DNS analysis)

dnf <- data %>% filter(DNS == 0)

# Getting lists of runners who have dnf'd and dns'd at some point

dnfx <- dnf %>% filter(Prior_DNF == 1)
dnsx <- data %>% filter(Prior_DNS == 1)

dnfx_ids <- unique(dnfx$Runner_ID)
dnsx_ids <- unique(dnsx$Runner_ID)

dnfx <- dnf %>% filter(Runner_ID %in% dnfx_ids)
dnsx <- data %>% filter(Runner_ID %in% dnsx_ids)

# Running regressions for DNF

mod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.co <- lm(DNF ~ log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)

xmod.pm <- coeftest(mod.pm, vcov = vcovCL, cluster = ~Event)
xmod.pm10 <- coeftest(mod.pm10, vcov = vcovCL, cluster = ~Event)
xmod.o3 <- coeftest(mod.o3, vcov = vcovCL, cluster = ~Event)
xmod.no2 <- coeftest(mod.no2, vcov = vcovCL, cluster = ~Event)
xmod.co <- coeftest(mod.co, vcov = vcovCL, cluster = ~Event)
xmod.pb <- coeftest(mod.pb, vcov = vcovCL, cluster = ~Event)
xmod.nopm <- coeftest(mod.nopm, vcov = vcovCL, cluster = ~Event)
xmod.nopm10 <- coeftest(mod.nopm10, vcov = vcovCL, cluster = ~Event)
xmod.nopb <- coeftest(mod.nopb, vcov = vcovCL, cluster = ~Event)
xmod.all <- coeftest(mod.all, vcov = vcovCL, cluster = ~Event)

stargazer(xmod.pm, xmod.pm10, xmod.o3, xmod.no2, xmod.co, xmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(xmod.nopm, xmod.nopm10, xmod.nopb, xmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating with gender - pollution interactions

gmod.pm <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.pm10 <- lm(DNF ~ log(PM10+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.o3 <- lm(DNF ~ log(O3+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.no2 <- lm(DNF ~ log(NO2+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.co <- lm(DNF ~ log(CO+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.pb <- lm(DNF ~ log(Pb+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.nopm <- lm(DNF ~ log(PM10+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.nopm10 <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.nopb <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.all <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + log(Pb+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)

gxmod.pm <- coeftest(gmod.pm, vcov = vcovCL, cluster = ~Event)
gxmod.pm10 <- coeftest(gmod.pm10, vcov = vcovCL, cluster = ~Event)
gxmod.o3 <- coeftest(gmod.o3, vcov = vcovCL, cluster = ~Event)
gxmod.no2 <- coeftest(gmod.no2, vcov = vcovCL, cluster = ~Event)
gxmod.co <- coeftest(gmod.co, vcov = vcovCL, cluster = ~Event)
gxmod.pb <- coeftest(gmod.pb, vcov = vcovCL, cluster = ~Event)
gxmod.nopm <- coeftest(gmod.nopm, vcov = vcovCL, cluster = ~Event)
gxmod.nopm10 <- coeftest(gmod.nopm10, vcov = vcovCL, cluster = ~Event)
gxmod.nopb <- coeftest(gmod.nopb, vcov = vcovCL, cluster = ~Event)
gxmod.all <- coeftest(gmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(gxmod.pm, gxmod.pm10, gxmod.o3, gxmod.no2, gxmod.co, gxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(gxmod.nopm, gxmod.nopm10, gxmod.nopb, gxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeat for only those who have DNF'd at some point

qmod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.co <- lm(DNF ~ log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)

qxmod.pm <- coeftest(qmod.pm, vcov = vcovCL, cluster = ~Event)
qxmod.pm10 <- coeftest(qmod.pm10, vcov = vcovCL, cluster = ~Event)
qxmod.o3 <- coeftest(qmod.o3, vcov = vcovCL, cluster = ~Event)
qxmod.no2 <- coeftest(qmod.no2, vcov = vcovCL, cluster = ~Event)
qxmod.co <- coeftest(qmod.co, vcov = vcovCL, cluster = ~Event)
qxmod.pb <- coeftest(qmod.pb, vcov = vcovCL, cluster = ~Event)
qxmod.nopm <- coeftest(qmod.nopm, vcov = vcovCL, cluster = ~Event)
qxmod.nopm10 <- coeftest(qmod.nopm10, vcov = vcovCL, cluster = ~Event)
qxmod.nopb <- coeftest(qmod.nopb, vcov = vcovCL, cluster = ~Event)
qxmod.all <- coeftest(qmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(qxmod.pm, qxmod.pm10, qxmod.o3, qxmod.no2, qxmod.co, qxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(qxmod.nopm, qxmod.nopm10, qxmod.nopb, qxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating with gender - pollution interactions

qgmod.pm <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.pm10 <- lm(DNF ~ log(PM10+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.o3 <- lm(DNF ~ log(O3+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.no2 <- lm(DNF ~ log(NO2+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.co <- lm(DNF ~ log(CO+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.pb <- lm(DNF ~ log(Pb+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.nopm <- lm(DNF ~ log(PM10+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.nopm10 <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                   + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.nopb <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.all <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + log(Pb+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)

qgxmod.pm <- coeftest(qgmod.pm, vcov = vcovCL, cluster = ~Event)
qgxmod.pm10 <- coeftest(qgmod.pm10, vcov = vcovCL, cluster = ~Event)
qgxmod.o3 <- coeftest(qgmod.o3, vcov = vcovCL, cluster = ~Event)
qgxmod.no2 <- coeftest(qgmod.no2, vcov = vcovCL, cluster = ~Event)
qgxmod.co <- coeftest(qgmod.co, vcov = vcovCL, cluster = ~Event)
qgxmod.pb <- coeftest(qgmod.pb, vcov = vcovCL, cluster = ~Event)
qgxmod.nopm <- coeftest(qgmod.nopm, vcov = vcovCL, cluster = ~Event)
qgxmod.nopm10 <- coeftest(qgmod.nopm10, vcov = vcovCL, cluster = ~Event)
qgxmod.nopb <- coeftest(qgmod.nopb, vcov = vcovCL, cluster = ~Event)
qgxmod.all <- coeftest(qgmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(qgxmod.pm, qgxmod.pm10, qgxmod.o3, qgxmod.no2, qgxmod.co, qgxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(qgxmod.nopm, qgxmod.nopm10, qgxmod.nopb, qgxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating the main analyses with home pollution levels

qqmod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180, data = dnf)
qqmod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + PM10_Runner_180, data = dnf)
qqmod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + O3_Runner_180, data = dnf)
qqmod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + NO2_Runner_180, data = dnf)
qqmod.co <- lm(DNF ~ log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + CO_Runner_180, data = dnf)
qqmod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + Pb_Runner_180, data = dnf)
qqmod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + PM10_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180, data = dnf)
qqmod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                   + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180, data = dnf)
qqmod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180 + PM10_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180, data = dnf)
qqmod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180 + PM10_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180 + Pb_Runner_180, data = dnf)

qqxmod.pm <- coeftest(qqmod.pm, vcov = vcovCL, cluster = ~Event)
qqxmod.pm10 <- coeftest(qqmod.pm10, vcov = vcovCL, cluster = ~Event)
qqxmod.o3 <- coeftest(qqmod.o3, vcov = vcovCL, cluster = ~Event)
qqxmod.no2 <- coeftest(qqmod.no2, vcov = vcovCL, cluster = ~Event)
qqxmod.co <- coeftest(qqmod.co, vcov = vcovCL, cluster = ~Event)
qqxmod.pb <- coeftest(qqmod.pb, vcov = vcovCL, cluster = ~Event)
qqxmod.nopm <- coeftest(qqmod.nopm, vcov = vcovCL, cluster = ~Event)
qqxmod.nopm10 <- coeftest(qqmod.nopm10, vcov = vcovCL, cluster = ~Event)
qqxmod.nopb <- coeftest(qqmod.nopb, vcov = vcovCL, cluster = ~Event)
qqxmod.all <- coeftest(qqmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(qqxmod.pm, qqxmod.pm10, qqxmod.o3, qqxmod.no2, qqxmod.co, qqxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(qqxmod.nopm, qqxmod.nopm10, qqxmod.nopb, qqxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating the main analyses with home pollution levels and gender*pollution interactions

gqqmod.pm <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180, data = dnf)
gqqmod.pm10 <- lm(DNF ~ log(PM10+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + PM10_Runner_180, data = dnf)
gqqmod.o3 <- lm(DNF ~ log(O3+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + O3_Runner_180, data = dnf)
gqqmod.no2 <- lm(DNF ~ log(NO2+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + NO2_Runner_180, data = dnf)
gqqmod.co <- lm(DNF ~ log(CO+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + CO_Runner_180, data = dnf)
gqqmod.pb <- lm(DNF ~ log(Pb+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + Pb_Runner_180, data = dnf)
gqqmod.nopm <- lm(DNF ~ log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + PM10_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180, data = dnf)
gqqmod.nopm10 <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                   + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180, data = dnf)
gqqmod.nopb <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180 + PM10_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180, data = dnf)
gqqmod.all <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + log(Pb+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + PM_Runner_180 + PM10_Runner_180 + O3_Runner_180 + NO2_Runner_180 + CO_Runner_180 + Pb_Runner_180, data = dnf)

gqqxmod.pm <- coeftest(gqqmod.pm, vcov = vcovCL, cluster = ~Event)
gqqxmod.pm10 <- coeftest(gqqmod.pm10, vcov = vcovCL, cluster = ~Event)
gqqxmod.o3 <- coeftest(gqqmod.o3, vcov = vcovCL, cluster = ~Event)
gqqxmod.no2 <- coeftest(gqqmod.no2, vcov = vcovCL, cluster = ~Event)
gqqxmod.co <- coeftest(gqqmod.co, vcov = vcovCL, cluster = ~Event)
gqqxmod.pb <- coeftest(gqqmod.pb, vcov = vcovCL, cluster = ~Event)
gqqxmod.nopm <- coeftest(gqqmod.nopm, vcov = vcovCL, cluster = ~Event)
gqqxmod.nopm10 <- coeftest(gqqmod.nopm10, vcov = vcovCL, cluster = ~Event)
gqqxmod.nopb <- coeftest(gqqmod.nopb, vcov = vcovCL, cluster = ~Event)
gqqxmod.all <- coeftest(gqqmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(gqqxmod.pm, gqqxmod.pm10, gqqxmod.o3, gqqxmod.no2, gqqxmod.co, gqqxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(gqqxmod.nopm, gqqxmod.nopm10, gqqxmod.nopb, gqqxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Finally, those who have DNF'd with a runner fixed effect

vqmod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.co <- lm(DNF ~ log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                   + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqmod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)

vqxmod.pm <- coeftest(vqmod.pm, vcov = vcovCL, cluster = ~Event)
vqxmod.pm10 <- coeftest(vqmod.pm10, vcov = vcovCL, cluster = ~Event)
vqxmod.o3 <- coeftest(vqmod.o3, vcov = vcovCL, cluster = ~Event)
vqxmod.no2 <- coeftest(vqmod.no2, vcov = vcovCL, cluster = ~Event)
vqxmod.co <- coeftest(vqmod.co, vcov = vcovCL, cluster = ~Event)
vqxmod.pb <- coeftest(vqmod.pb, vcov = vcovCL, cluster = ~Event)
vqxmod.nopm <- coeftest(vqmod.nopm, vcov = vcovCL, cluster = ~Event)
vqxmod.nopm10 <- coeftest(vqmod.nopm10, vcov = vcovCL, cluster = ~Event)
vqxmod.nopb <- coeftest(vqmod.nopb, vcov = vcovCL, cluster = ~Event)
vqxmod.all <- coeftest(vqmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(vqxmod.pm, vqxmod.pm10, vqxmod.o3, vqxmod.no2, vqxmod.co, vqxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type', 'Runner_ID'))
stargazer(vqxmod.nopm, vqxmod.nopm10, vqxmod.nopb, vqxmod.all, type = 'text', omit = c('Year', 'Event', 'Type', 'Runner_ID'))

# Repeating with gender - pollution interactions

vqgmod.pm <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.pm10 <- lm(DNF ~ log(PM10+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.o3 <- lm(DNF ~ log(O3+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.no2 <- lm(DNF ~ log(NO2+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.co <- lm(DNF ~ log(CO+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.pb <- lm(DNF ~ log(Pb+.001)*factor(Gender) + factor(Gender)*Prior_DNF + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.nopm <- lm(DNF ~ log(PM10+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.nopm10 <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                    + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.nopb <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)
vqgmod.all <- lm(DNF ~ log(PM+.001)*factor(Gender) + factor(Gender)*Prior_DNF + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + log(Pb+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type) + factor(Runner_ID), data = dnfx)

vqgxmod.pm <- coeftest(vqgmod.pm, vcov = vcovCL, cluster = ~Event)
vqgxmod.pm10 <- coeftest(vqgmod.pm10, vcov = vcovCL, cluster = ~Event)
vqgxmod.o3 <- coeftest(vqgmod.o3, vcov = vcovCL, cluster = ~Event)
vqgxmod.no2 <- coeftest(vqgmod.no2, vcov = vcovCL, cluster = ~Event)
vqgxmod.co <- coeftest(vqgmod.co, vcov = vcovCL, cluster = ~Event)
vqgxmod.pb <- coeftest(vqgmod.pb, vcov = vcovCL, cluster = ~Event)
vqgxmod.nopm <- coeftest(vqgmod.nopm, vcov = vcovCL, cluster = ~Event)
vqgxmod.nopm10 <- coeftest(vqgmod.nopm10, vcov = vcovCL, cluster = ~Event)
vqgxmod.nopb <- coeftest(vqgmod.nopb, vcov = vcovCL, cluster = ~Event)
vqgxmod.all <- coeftest(vqgmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(vqgxmod.pm, vqgxmod.pm10, vqgxmod.o3, vqgxmod.no2, vqgxmod.co, vqgxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type', 'Runner_ID'))
stargazer(vqgxmod.nopm, vqgxmod.nopm10, vqgxmod.nopb, vqgxmod.all, type = 'text', omit = c('Year', 'Event', 'Type', 'Runner_ID'))









" can i drop the weather data? "
" is it the limiting factor here? "
" if so, the results shouldn't change... try this! "

" re: osmnx project: take road mean ebc for road value?? "





# Running regressions for DNS

mmod.pm <- lm(DNS ~ log(PM_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.pm10 <- lm(DNS ~ log(PM10_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.o3 <- lm(DNS ~ log(O3_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.no2 <- lm(DNS ~ log(NO2_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.co <- lm(DNS ~ log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.pb <- lm(DNS ~ log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.nopm <- lm(DNS ~ log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.nopm10 <- lm(DNS ~ log(PM_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.nopb <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
mmod.all <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)

xxmod.pm <- coeftest(mmod.pm, vcov = vcovCL, cluster = ~Event)
xxmod.pm10 <- coeftest(mmod.pm10, vcov = vcovCL, cluster = ~Event)
xxmod.o3 <- coeftest(mmod.o3, vcov = vcovCL, cluster = ~Event)
xxmod.no2 <- coeftest(mmod.no2, vcov = vcovCL, cluster = ~Event)
xxmod.co <- coeftest(mmod.co, vcov = vcovCL, cluster = ~Event)
xxmod.pb <- coeftest(mmod.pb, vcov = vcovCL, cluster = ~Event)
xxmod.nopm <- coeftest(mmod.nopm, vcov = vcovCL, cluster = ~Event)
xxmod.nopm10 <- coeftest(mmod.nopm10, vcov = vcovCL, cluster = ~Event)
xxmod.nopb <- coeftest(mmod.nopb, vcov = vcovCL, cluster = ~Event)
xxmod.all <- coeftest(mmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(xxmod.pm, xxmod.pm10, xxmod.o3, xxmod.no2, xxmod.co, xxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(xxmod.nopm, xxmod.nopm10, xxmod.nopb, xxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating with gender - pollution interactions

gmmod.pm <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.pm10 <- lm(DNS ~ log(PM10_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.o3 <- lm(DNS ~ log(O3_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.no2 <- lm(DNS ~ log(NO2_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.co <- lm(DNS ~ log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.pb <- lm(DNS ~ log(Pb_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.nopm <- lm(DNS ~ log(PM10_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.nopm10 <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.nopb <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + log(PM10_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)
gmmod.all <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + log(PM10_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + log(Pb_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = data)

gxxmod.pm <- coeftest(gmmod.pm, vcov = vcovCL, cluster = ~Event)
gxxmod.pm10 <- coeftest(gmmod.pm10, vcov = vcovCL, cluster = ~Event)
gxxmod.o3 <- coeftest(gmmod.o3, vcov = vcovCL, cluster = ~Event)
gxxmod.no2 <- coeftest(gmmod.no2, vcov = vcovCL, cluster = ~Event)
gxxmod.co <- coeftest(gmmod.co, vcov = vcovCL, cluster = ~Event)
gxxmod.pb <- coeftest(gmmod.pb, vcov = vcovCL, cluster = ~Event)
gxxmod.nopm <- coeftest(gmmod.nopm, vcov = vcovCL, cluster = ~Event)
gxxmod.nopm10 <- coeftest(gmmod.nopm10, vcov = vcovCL, cluster = ~Event)
gxxmod.nopb <- coeftest(gmmod.nopb, vcov = vcovCL, cluster = ~Event)
gxxmod.all <- coeftest(gmmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(gxxmod.pm, gxxmod.pm10, gxxmod.o3, gxxmod.no2, gxxmod.co, gxxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(gxxmod.nopm, gxxmod.nopm10, gxxmod.nopb, gxxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeat for only those who have DNS'd at some point

mmod.pm <- lm(DNS ~ log(PM_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.pm10 <- lm(DNS ~ log(PM10_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.o3 <- lm(DNS ~ log(O3_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.no2 <- lm(DNS ~ log(NO2_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.co <- lm(DNS ~ log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.pb <- lm(DNS ~ log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.nopm <- lm(DNS ~ log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.nopm10 <- lm(DNS ~ log(PM_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.nopb <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
mmod.all <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)

xxmod.pm <- coeftest(mmod.pm, vcov = vcovCL, cluster = ~Event)
xxmod.pm10 <- coeftest(mmod.pm10, vcov = vcovCL, cluster = ~Event)
xxmod.o3 <- coeftest(mmod.o3, vcov = vcovCL, cluster = ~Event)
xxmod.no2 <- coeftest(mmod.no2, vcov = vcovCL, cluster = ~Event)
xxmod.co <- coeftest(mmod.co, vcov = vcovCL, cluster = ~Event)
xxmod.pb <- coeftest(mmod.pb, vcov = vcovCL, cluster = ~Event)
xxmod.nopm <- coeftest(mmod.nopm, vcov = vcovCL, cluster = ~Event)
xxmod.nopm10 <- coeftest(mmod.nopm10, vcov = vcovCL, cluster = ~Event)
xxmod.nopb <- coeftest(mmod.nopb, vcov = vcovCL, cluster = ~Event)
xxmod.all <- coeftest(mmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(xxmod.pm, xxmod.pm10, xxmod.o3, xxmod.no2, xxmod.co, xxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(xxmod.nopm, xxmod.nopm10, xxmod.nopb, xxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating with gender - pollution interactions

gmmod.pm <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.pm10 <- lm(DNS ~ log(PM10_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.o3 <- lm(DNS ~ log(O3_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.no2 <- lm(DNS ~ log(NO2_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.co <- lm(DNS ~ log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.pb <- lm(DNS ~ log(Pb_Runner_180+.001)*factor(Gender) + Age + Travel_Distance + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.nopm <- lm(DNS ~ log(PM10_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.nopm10 <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
                   + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.nopb <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + log(PM10_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)
gmmod.all <- lm(DNS ~ log(PM_Runner_180+.001)*factor(Gender) + log(PM10_Runner_180+.001)*factor(Gender) + log(O3_Runner_180+.001)*factor(Gender) + log(NO2_Runner_180+.001)*factor(Gender) + log(CO_Runner_180+.001)*factor(Gender) + log(Pb_Runner_180+.001)*factor(Gender) + Age + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNS + factor(Year) + factor(Event) + factor(Type), data = dnsx)

gxxmod.pm <- coeftest(gmmod.pm, vcov = vcovCL, cluster = ~Event)
gxxmod.pm10 <- coeftest(gmmod.pm10, vcov = vcovCL, cluster = ~Event)
gxxmod.o3 <- coeftest(gmmod.o3, vcov = vcovCL, cluster = ~Event)
gxxmod.no2 <- coeftest(gmmod.no2, vcov = vcovCL, cluster = ~Event)
gxxmod.co <- coeftest(gmmod.co, vcov = vcovCL, cluster = ~Event)
gxxmod.pb <- coeftest(gmmod.pb, vcov = vcovCL, cluster = ~Event)
gxxmod.nopm <- coeftest(gmmod.nopm, vcov = vcovCL, cluster = ~Event)
gxxmod.nopm10 <- coeftest(gmmod.nopm10, vcov = vcovCL, cluster = ~Event)
gxxmod.nopb <- coeftest(gmmod.nopb, vcov = vcovCL, cluster = ~Event)
gxxmod.all <- coeftest(gmmod.all, vcov = vcovCL, cluster = ~Event)

stargazer(gxxmod.pm, gxxmod.pm10, gxxmod.o3, gxxmod.no2, gxxmod.co, gxxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(gxxmod.nopm, gxxmod.nopm10, gxxmod.nopb, gxxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))










#######
# additional regressions include:
# -------------------------------
# (1) add the different home-pollution-windows to dnf regressions?
# (2) the dns regressions for each window size (baseline + fully specified)
#######



