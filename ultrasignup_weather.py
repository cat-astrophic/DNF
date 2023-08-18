# This script adds weather data to the ultrasignup data set

# Importing required modules

import pandas as pd
from glob import glob
from geopy.distance import distance
from geopy.geocoders import Nominatim

# Setting up the geolocator

email = '' # enter your email address here
geolocator = Nominatim(user_agent = email)

# Project directory info

dnf_direc = 'D:/DNF/data/'
noaa_direc = 'D:/NOAA/us_data/'

# Reading in the ultrasignup data

data = pd.read_csv(dnf_direc + 'scraped_data.csv')

# Dropping observations prior to 2000

data = data[data.Year >= 2000].reset_index(drop = True)

# Dropping observations for races that are still 'upcoming'

data = data[data.Upcoming == 0].reset_index(drop = True)

# Reading in the weather data

noaa_files = glob(noaa_direc + '*')

noaa_df = pd.DataFrame()

for file in noaa_files:
    
    print(file[21:25])
    
    if int(file[21:25]) in range(2010,2020):
        
        tmp = pd.read_csv(file)
        noaa_df = pd.concat([noaa_df, tmp], axis = 0).reset_index(drop = True)

# Employing an end of 2019 cutoff to the data

data = data[data.Year < 2020].reset_index(drop = True)

nyears = [int(d[:4]) for d in noaa_df.DATE]
noaa_df = pd.concat([noaa_df, pd.Series(nyears, name = 'YEAR')], axis = 1)

# Getting coordinates for runner and race locations

rnr_locs = list(data.Runner_City.unique())
race_locs = list(data.Event_Location.unique())

rnr_coords = []
race_coords = []

for loc in rnr_locs: # 18.842
    
    print(rnr_locs.index(loc))
    
    try:
        
        addy = geolocator.geocode(loc)
        rnr_coords.append((addy.latitude, addy.longitude))
        
    except:
        
        rnr_coords.append(None)

for loc in race_locs: # 3.416
   
    print(race_locs.index(loc))
   
    try:
        
        addy = geolocator.geocode(loc)
        race_coords.append((addy.latitude, addy.longitude))
        
    except:
        
        race_coords.append(None)

# Create a clean column of coordinate tuples in noaa_df

sc_list = [(noaa_df.LATITUDE[i],noaa_df.LONGITUDE[i]) for i in range(len(noaa_df))]
noaa_df = pd.concat([noaa_df, pd.Series(sc_list, name = 'STATION_COORDS')], axis = 1)

# Adding both sets of coordinates to data as tuples

full_rnr_coords = []
full_race_coords = []

for i in range(len(data)):
    
    print(str(1+i) + ' of ' + str(len(data)) + '.......')
    
    rnr_idx = rnr_locs.index(data.Runner_City[i])
    race_idx = race_locs.index(data.Event_Location[i])
    full_rnr_coords.append(rnr_coords[rnr_idx])
    full_race_coords.append(race_coords[race_idx])

full_rnr_coords = pd.Series(full_rnr_coords, name = 'Runner_Coordinates')
full_race_coords = pd.Series(full_race_coords, name = 'Event_Coordinates')
data = pd.concat([data, full_rnr_coords, full_race_coords], axis = 1)

# Get nearest station for each runner and race

stats = list(noaa_df.STATION.unique())
scoords = []

for s in stats:
    
    print(stats.index(s))
    
    tmp = noaa_df[noaa_df.STATION == s].reset_index(drop = True)
    scoords.append(tmp.STATION_COORDS[0])

# Removing extra columns from noaa_df to save time

noaa_df = noaa_df[['STATION', 'DATE', 'TEMP', 'DEWP', 'WDSP', 'MAX', 'MIN', 'PRCP', 'YEAR', 'STATION_COORDS']]

# Reformatting the date data in data

day_fx = lambda x: '0' + x if len(x) < 2 else x

def date_fx(d0):
    
    mos = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    nos = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12']
    dic = dict(zip(mos,nos))
    l = d0.replace(',','').split(' ')
    m = dic[l[0]]
    day = day_fx(l[1])
    d1 = l[2] + '-' + m + '-' + day
    
    return d1

ds = [date_fx(date) for date in data.Date]
data = pd.concat([data, pd.Series(ds, name = 'DATE')], axis = 1)

# Restrict the sample to 2010 - 2019

data = data[data.Year.isin([i for i in range(2010,2020)])].reset_index(drop = True)
noaa_df = noaa_df[noaa_df.YEAR.isin([i for i in range(2010,2020)])].reset_index(drop = True)

# Adding race day weather to data

e_temp = []
e_max = []
e_min = []
e_precip = []
e_wdsp = []
e_dewp = []
e_dist = []
e_stat = []

max_fx = lambda x: x if x < 9999.9 else None
min_fx = lambda x: x if x < 9999.9 else None
prcp_fx = lambda x: x if x < 99.99 else None
wdsp_fx = lambda x: x if x < 999.9 else None
dewp_fx = lambda x: x if x < 9999.9 else None

for i in range(len(data)):
    
    print(str(1+i) + ' of ' + str(len(data)) + '.......')
    
    d = data.DATE[i]
    etmp = noaa_df[noaa_df.DATE == d].reset_index(drop = True)
    l = [distance(data.Event_Coordinates[i],x).mi for x in etmp.STATION_COORDS]
    idx = l.index(min(l))
    
    e_temp.append(etmp.TEMP[idx])
    e_max.append(max_fx(etmp.MAX[idx]))
    e_min.append(min_fx(etmp.MIN[idx]))
    e_precip.append(prcp_fx(etmp.PRCP[idx]))
    e_wdsp.append(wdsp_fx(etmp.WDSP[idx]))
    e_dewp.append(dewp_fx(etmp.DEWP[idx]))
    e_dist.append(l[idx])
    e_stat.append(etmp.STATION[idx])

def sauron_is_my_lord_and_savior(inp):
    
    try:
        
        if inp > 0:
            
            shit = 1
            
        else:
            
            shit = 0
            
    except:
        
        shit = 0
    
    return shit

e_precip_bin = [sauron_is_my_lord_and_savior(p) for p in e_precip]

e_temp = pd.Series(e_temp, name = 'Event_Temperature')
e_max = pd.Series(e_max, name = 'Event_Max_Temp')
e_min = pd.Series(e_min, name = 'Event_Min_Temp')
e_precip = pd.Series(e_precip, name = 'Event_Precipitation')
e_wdsp = pd.Series(e_wdsp, name = 'Event_Windspeed')
e_dewp = pd.Series(e_dewp, name = 'Event_Dewpoint')
e_dist = pd.Series(e_dist, name = 'Event_Station_Distance_Miles')
e_stat = pd.Series(e_stat, name = 'Event_Station')
e_precip_bin = pd.Series(e_precip_bin, name = 'Event_Precip_Binary')

data = pd.concat([data, e_stat, e_dist, e_temp, e_max, e_min, e_precip, e_precip_bin, e_wdsp, e_dewp], axis = 1)

# Save the data

data.to_csv(dnf_direc + 'temp_data.csv', index = False)











"""

I NEED TO RULE OUT DISTANCE BASED EVENTS FOR DNF ANALYSIS

I ALSO NEED TO FIGURE OUT HOW TO THINK ABOUT DNS OUTCOMES - 
 - DO I DROP THEM ENTIRELY?
 - ARE THEY ONLY DROPPED FOR THE DNF ANALYSIS?

"""

"""

interact pollution and weather !!

in the study, can i predict which runners were more likely to DNF bc of pollution vs weather
and then look for differences in these groups to see why!

"""




# create a plot of the points???
# how far to use as a cutoff for distance to stations?
# can run several regs more restrictive, but need to cut off here

