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

data <- data %>% filter(Event_Station_Distance_Miles < 20)

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

mod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.co <- lm(DNF ~ log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)
mod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)

xmod.pm <- coeftest(mod.pm, vcov = vcovCL, cluster = ~Runner_ID)
xmod.pm10 <- coeftest(mod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
xmod.o3 <- coeftest(mod.o3, vcov = vcovCL, cluster = ~Runner_ID)
xmod.no2 <- coeftest(mod.no2, vcov = vcovCL, cluster = ~Runner_ID)
xmod.co <- coeftest(mod.co, vcov = vcovCL, cluster = ~Runner_ID)
xmod.pb <- coeftest(mod.pb, vcov = vcovCL, cluster = ~Runner_ID)
xmod.nopm <- coeftest(mod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
xmod.nopm10 <- coeftest(mod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
xmod.nopb <- coeftest(mod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
xmod.all <- coeftest(mod.all, vcov = vcovCL, cluster = ~Runner_ID)

stargazer(xmod.pm, xmod.pm10, xmod.o3, xmod.no2, xmod.co, xmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(xmod.nopm, xmod.nopm10, xmod.nopb, xmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating with gender - pollution interactions

gmod.pm <- lm(DNF ~ log(PM+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.pm10 <- lm(DNF ~ log(PM10+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.o3 <- lm(DNF ~ log(O3+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.no2 <- lm(DNF ~ log(NO2+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.co <- lm(DNF ~ log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.pb <- lm(DNF ~ log(Pb+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
             + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.nopm <- lm(DNF ~ log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.nopm10 <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.nopb <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)
gmod.all <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + log(Pb+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Prior_DNF + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnf)

gxmod.pm <- coeftest(gmod.pm, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.pm10 <- coeftest(gmod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.o3 <- coeftest(gmod.o3, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.no2 <- coeftest(gmod.no2, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.co <- coeftest(gmod.co, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.pb <- coeftest(gmod.pb, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.nopm <- coeftest(gmod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.nopm10 <- coeftest(gmod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.nopb <- coeftest(gmod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
gxmod.all <- coeftest(gmod.all, vcov = vcovCL, cluster = ~Runner_ID)

stargazer(gxmod.pm, gxmod.pm10, gxmod.o3, gxmod.no2, gxmod.co, gxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(gxmod.nopm, gxmod.nopm10, gxmod.nopb, gxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeat for only those who have DNF'd at some point

qmod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.co <- lm(DNF ~ log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                  + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qmod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)

qxmod.pm <- coeftest(qmod.pm, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.pm10 <- coeftest(qmod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.o3 <- coeftest(qmod.o3, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.no2 <- coeftest(qmod.no2, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.co <- coeftest(qmod.co, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.pb <- coeftest(qmod.pb, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.nopm <- coeftest(qmod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.nopm10 <- coeftest(qmod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.nopb <- coeftest(qmod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
qxmod.all <- coeftest(qmod.all, vcov = vcovCL, cluster = ~Runner_ID)

stargazer(qxmod.pm, qxmod.pm10, qxmod.o3, qxmod.no2, qxmod.co, qxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(qxmod.nopm, qxmod.nopm10, qxmod.nopb, qxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

# Repeating with gender - pollution interactions

qgmod.pm <- lm(DNF ~ log(PM+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.pm10 <- lm(DNF ~ log(PM10+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.o3 <- lm(DNF ~ log(O3+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.no2 <- lm(DNF ~ log(NO2+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.co <- lm(DNF ~ log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.pb <- lm(DNF ~ log(Pb+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + Ability + Time_Since + I(Time_Since^2) + Previous_Races + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.nopm <- lm(DNF ~ log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.nopm10 <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                   + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.nopb <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                 + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)
qgmod.all <- lm(DNF ~ log(PM+.001)*factor(Gender) + log(PM10+.001)*factor(Gender) + log(O3+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(CO+.001)*factor(Gender) + log(Pb+.001)*factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Ability + Time_Since + I(Time_Since^2) + Previous_Races + Travel_Distance  + factor(Year) + factor(Event) + factor(Type), data = dnfx)

qgxmod.pm <- coeftest(qgmod.pm, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.pm10 <- coeftest(qgmod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.o3 <- coeftest(qgmod.o3, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.no2 <- coeftest(qgmod.no2, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.co <- coeftest(qgmod.co, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.pb <- coeftest(qgmod.pb, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.nopm <- coeftest(qgmod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.nopm10 <- coeftest(qgmod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.nopb <- coeftest(qgmod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
qgxmod.all <- coeftest(qgmod.all, vcov = vcovCL, cluster = ~Runner_ID)

stargazer(qgxmod.pm, qgxmod.pm10, qgxmod.o3, qgxmod.no2, qgxmod.co, qgxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(qgxmod.nopm, qgxmod.nopm10, qgxmod.nopb, qgxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))

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

xxmod.pm <- coeftest(mmod.pm, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.pm10 <- coeftest(mmod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.o3 <- coeftest(mmod.o3, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.no2 <- coeftest(mmod.no2, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.co <- coeftest(mmod.co, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.pb <- coeftest(mmod.pb, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.nopm <- coeftest(mmod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.nopm10 <- coeftest(mmod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.nopb <- coeftest(mmod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.all <- coeftest(mmod.all, vcov = vcovCL, cluster = ~Runner_ID)

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

gxxmod.pm <- coeftest(gmmod.pm, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.pm10 <- coeftest(gmmod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.o3 <- coeftest(gmmod.o3, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.no2 <- coeftest(gmmod.no2, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.co <- coeftest(gmmod.co, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.pb <- coeftest(gmmod.pb, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.nopm <- coeftest(gmmod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.nopm10 <- coeftest(gmmod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.nopb <- coeftest(gmmod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.all <- coeftest(gmmod.all, vcov = vcovCL, cluster = ~Runner_ID)

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

xxmod.pm <- coeftest(mmod.pm, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.pm10 <- coeftest(mmod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.o3 <- coeftest(mmod.o3, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.no2 <- coeftest(mmod.no2, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.co <- coeftest(mmod.co, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.pb <- coeftest(mmod.pb, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.nopm <- coeftest(mmod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.nopm10 <- coeftest(mmod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.nopb <- coeftest(mmod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
xxmod.all <- coeftest(mmod.all, vcov = vcovCL, cluster = ~Runner_ID)

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

gxxmod.pm <- coeftest(gmmod.pm, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.pm10 <- coeftest(gmmod.pm10, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.o3 <- coeftest(gmmod.o3, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.no2 <- coeftest(gmmod.no2, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.co <- coeftest(gmmod.co, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.pb <- coeftest(gmmod.pb, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.nopm <- coeftest(gmmod.nopm, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.nopm10 <- coeftest(gmmod.nopm10, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.nopb <- coeftest(gmmod.nopb, vcov = vcovCL, cluster = ~Runner_ID)
gxxmod.all <- coeftest(gmmod.all, vcov = vcovCL, cluster = ~Runner_ID)

stargazer(gxxmod.pm, gxxmod.pm10, gxxmod.o3, gxxmod.no2, gxxmod.co, gxxmod.pb, type = 'text', omit = c('Year', 'Event', 'Type'))
stargazer(gxxmod.nopm, gxxmod.nopm10, gxxmod.nopb, gxxmod.all, type = 'text', omit = c('Year', 'Event', 'Type'))










#######
# additional regressions include:
# -------------------------------
# (1) add the different home-pollution-windows to dnf regressions?
# (2) the dns regressions for each window size (baseline + fully specified)
#######



