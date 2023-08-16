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

# Running baseline regressions for DNF

mod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.co <- lm(DNF ~ log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)
mod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance, data = dnf)

xmod.pm <- coeftest(mod.pm, vcov = vcovCL, cluster = ~Event_Type)
xmod.pm10 <- coeftest(mod.pm10, vcov = vcovCL, cluster = ~Event_Type)
xmod.o3 <- coeftest(mod.o3, vcov = vcovCL, cluster = ~Event_Type)
xmod.no2 <- coeftest(mod.no2, vcov = vcovCL, cluster = ~Event_Type)
xmod.co <- coeftest(mod.co, vcov = vcovCL, cluster = ~Event_Type)
xmod.pb <- coeftest(mod.pb, vcov = vcovCL, cluster = ~Event_Type)
xmod.nopm <- coeftest(mod.nopm, vcov = vcovCL, cluster = ~Event_Type)
xmod.nopm10 <- coeftest(mod.nopm10, vcov = vcovCL, cluster = ~Event_Type)
xmod.nopb <- coeftest(mod.nopb, vcov = vcovCL, cluster = ~Event_Type)
xmod.all <- coeftest(mod.all, vcov = vcovCL, cluster = ~Event_Type)

stargazer(xmod.pm, xmod.pm10, xmod.o3, xmod.no2, xmod.co, xmod.pb, type = 'text')
stargazer(xmod.nopm, xmod.nopm10, xmod.nopb, xmod.all, type = 'text')

# Running fully specified regressions for DNF

mmod.pm <- lm(DNF ~ log(PM+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.pm10 <- lm(DNF ~ log(PM10+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
                + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.o3 <- lm(DNF ~ log(O3+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.no2 <- lm(DNF ~ log(NO2+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
               + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.co <- lm(DNF ~ log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.pb <- lm(DNF ~ log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint + Travel_Distance
              + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.nopm <- lm(DNF ~ log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.nopm10 <- lm(DNF ~ log(PM+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                  + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.nopb <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
                + Travel_Distance  + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)
mmod.all <- lm(DNF ~ log(PM+.001) + log(PM10+.001) + log(O3+.001) + log(NO2+.001) + log(CO+.001) + log(Pb+.001) + factor(Gender) + Age + Event_Temperature + Event_Precipitation + Event_Windspeed + Event_Dewpoint
               + Travel_Distance  + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = dnf)

xxmod.pm <- coeftest(mmod.pm, vcov = vcovCL, cluster = ~Event_Type)
xxmod.pm10 <- coeftest(mmod.pm10, vcov = vcovCL, cluster = ~Event_Type)
xxmod.o3 <- coeftest(mmod.o3, vcov = vcovCL, cluster = ~Event_Type)
xxmod.no2 <- coeftest(mmod.no2, vcov = vcovCL, cluster = ~Event_Type)
xxmod.co <- coeftest(mmod.co, vcov = vcovCL, cluster = ~Event_Type)
xxmod.pb <- coeftest(mmod.pb, vcov = vcovCL, cluster = ~Event_Type)
xxmod.nopm <- coeftest(mmod.nopm, vcov = vcovCL, cluster = ~Event_Type)
xxmod.nopm10 <- coeftest(mmod.nopm10, vcov = vcovCL, cluster = ~Event_Type)
xxmod.nopb <- coeftest(mmod.nopb, vcov = vcovCL, cluster = ~Event_Type)
xxmod.all <- coeftest(mmod.all, vcov = vcovCL, cluster = ~Event_Type)

stargazer(xxmod.pm, xxmod.pm10, xxmod.o3, xxmod.no2, xxmod.co, xxmod.pb, type = 'text')
stargazer(xxmod.nopm, xxmod.nopm10, xxmod.nopb, xxmod.all, type = 'text')

# Running baseline regressions for DNS

mod.pm <- lm(DNS ~ log(PM_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.pm10 <- lm(DNS ~ log(PM10_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.o3 <- lm(DNS ~ log(O3_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.no2 <- lm(DNS ~ log(NO2_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.co <- lm(DNS ~ log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.pb <- lm(DNS ~ log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.nopm <- lm(DNS ~ log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.nopm10 <- lm(DNS ~ log(PM_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.nopb <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)
mod.all <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance, data = data)

xmod.pm <- coeftest(mod.pm, vcov = vcovCL, cluster = ~Event_Type)
xmod.pm10 <- coeftest(mod.pm10, vcov = vcovCL, cluster = ~Event_Type)
xmod.o3 <- coeftest(mod.o3, vcov = vcovCL, cluster = ~Event_Type)
xmod.no2 <- coeftest(mod.no2, vcov = vcovCL, cluster = ~Event_Type)
xmod.co <- coeftest(mod.co, vcov = vcovCL, cluster = ~Event_Type)
xmod.pb <- coeftest(mod.pb, vcov = vcovCL, cluster = ~Event_Type)
xmod.nopm <- coeftest(mod.nopm, vcov = vcovCL, cluster = ~Event_Type)
xmod.nopm10 <- coeftest(mod.nopm10, vcov = vcovCL, cluster = ~Event_Type)
xmod.nopb <- coeftest(mod.nopb, vcov = vcovCL, cluster = ~Event_Type)
xmod.all <- coeftest(mod.all, vcov = vcovCL, cluster = ~Event_Type)

stargazer(xmod.pm, xmod.pm10, xmod.o3, xmod.no2, xmod.co, xmod.pb, type = 'text')
stargazer(xmod.nopm, xmod.nopm10, xmod.nopb, xmod.all, type = 'text')

# Running fully specified regressions for DNS

mmod.pm <- lm(DNS ~ log(PM_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.pm10 <- lm(DNS ~ log(PM10_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.o3 <- lm(DNS ~ log(O3_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.no2 <- lm(DNS ~ log(NO2_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.co <- lm(DNS ~ log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.pb <- lm(DNS ~ log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.nopm <- lm(DNS ~ log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.nopm10 <- lm(DNS ~ log(PM_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                  + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.nopb <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
                + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)
mmod.all <- lm(DNS ~ log(PM_Runner_180+.001) + log(PM10_Runner_180+.001) + log(O3_Runner_180+.001) + log(NO2_Runner_180+.001) + log(CO_Runner_180+.001) + log(Pb_Runner_180+.001) + factor(Gender) + Age + Travel_Distance
               + factor(Runner_ID) + factor(Year) + factor(Event_FIPS) + factor(Event) + factor(Type), data = data)

xxmod.pm <- coeftest(mod.pm, vcov = vcovCL, cluster = ~Event_Type)
xxmod.pm10 <- coeftest(mod.pm10, vcov = vcovCL, cluster = ~Event_Type)
xxmod.o3 <- coeftest(mod.o3, vcov = vcovCL, cluster = ~Event_Type)
xxmod.no2 <- coeftest(mod.no2, vcov = vcovCL, cluster = ~Event_Type)
xxmod.co <- coeftest(mod.co, vcov = vcovCL, cluster = ~Event_Type)
xxmod.pb <- coeftest(mod.pb, vcov = vcovCL, cluster = ~Event_Type)
xxmod.nopm <- coeftest(mod.nopm, vcov = vcovCL, cluster = ~Event_Type)
xxmod.nopm10 <- coeftest(mod.nopm10, vcov = vcovCL, cluster = ~Event_Type)
xxmod.nopb <- coeftest(mod.nopb, vcov = vcovCL, cluster = ~Event_Type)
xxmod.all <- coeftest(mod.all, vcov = vcovCL, cluster = ~Event_Type)

stargazer(xxmod.pm, xxmod.pm10, xxmod.o3, xxmod.no2, xxmod.co, xxmod.pb, type = 'text')
stargazer(xxmod.nopm, xxmod.nopm10, xxmod.nopb, xxmod.all, type = 'text')



"""
additional regressions include:

(1) add the different home-pollution-windows to dnf regressions?
(2) the dns regressions for each window size (baseline + fully specified)

"""












