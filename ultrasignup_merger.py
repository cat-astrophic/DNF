# This script adds pollution data to the ultrasignup data set

# Importing required modules

import pandas as pd
import numpy as np
import addfips
from datetime import datetime, timedelta
import haversine as hs   
from haversine import Unit
from ast import literal_eval as le

# Project directory info

direc = 'D:/DNF/data/'
epa_direc = 'D:/EPA/'

# Reading in the data

data = pd.read_csv(direc + 'temp_data.csv')
pm = pd.read_csv(epa_direc + 'epa_aqs_data_pm.csv')
pm10 = pd.read_csv(epa_direc + 'epa_aqs_data_pm10.csv')
o3 = pd.read_csv(epa_direc + 'epa_aqs_data_ozone.csv')
no2 = pd.read_csv(epa_direc + 'epa_aqs_data_no2.csv')
co = pd.read_csv(epa_direc + 'epa_aqs_data_co.csv')
pb = pd.read_csv(epa_direc + 'epa_aqs_data_pb.csv')

# Creata a FIPS code column for pollution

def fips_fx(dframe,x):
    
    try:
        
        if dframe.State[x] < 10:
            
            f = '0' + str(int(dframe.State[x]))
            
        else:
            
            f = str(dframe.State[x])
            
        if dframe.County[x] < 10:
            
            f = f + '00' + str(int(dframe.County[x]))
            
        elif dframe.County[x] < 100:
            
            f = f + '0' + str(int(dframe.County[x]))
        
        else:
            
            f = f + str(int(dframe.County[x]))
            
    except:
        
        f = None
        
    return f

pm_fips = [fips_fx(pm,i) for i in range(len(pm))]
pm10_fips = [fips_fx(pm10,i) for i in range(len(pm10))]
o3_fips = [fips_fx(o3,i) for i in range(len(o3))]
no2_fips = [fips_fx(no2,i) for i in range(len(no2))]
co_fips = [fips_fx(co,i) for i in range(len(co))]
pb_fips = [fips_fx(pb,i) for i in range(len(pb))]

pm = pd.concat([pm, pd.Series(pm_fips, name = 'FIPS')], axis = 1)
pm10 = pd.concat([pm10, pd.Series(pm10_fips, name = 'FIPS')], axis = 1)
o3 = pd.concat([o3, pd.Series(o3_fips, name = 'FIPS')], axis = 1)
no2 = pd.concat([no2, pd.Series(no2_fips, name = 'FIPS')], axis = 1)
co = pd.concat([co, pd.Series(co_fips, name = 'FIPS')], axis = 1)
pb = pd.concat([pb, pd.Series(pb_fips, name = 'FIPS')], axis = 1)

# Creating county-day pollution level data

good_pm = pm.groupby(['FIPS', 'Date']).mean().reset_index()
good_pm10 = pm10.groupby(['FIPS', 'Date']).mean().reset_index()
good_o3 = o3.groupby(['FIPS', 'Date']).mean().reset_index()
good_no2 = no2.groupby(['FIPS', 'Date']).mean().reset_index()
good_co = co.groupby(['FIPS', 'Date']).mean().reset_index()
good_pb = pb.groupby(['FIPS', 'Date']).mean().reset_index()

# Mapping cities to counties in data

latlong = pd.read_csv(direc + 'latlong.csv') # Contains lattitude and longitude
ccmap = pd.read_csv(direc + 'ccmap.csv', sep = '|') # The city to county map from before
ccmap = ccmap.replace(to_replace = 'Washington, D.C.', value = 'District of Columbia') # Update DC naming convention
ccmap.City = ccmap.City.str.lower()
ccmap.County = ccmap.County.str.lower()
latlong.Admin2 = latlong.Admin2.str.lower()

# Some quick data cleaning for mapping runners and events to counties

rlens = [len(x.split(', ')) for x in data.Runner_City]
elens = [len(x.split(', ')) for x in data.Event_Location]

rkeeps = [x for x in range(len(rlens)) if rlens[x] == 2]
ekeeps = [x for x in range(len(elens)) if elens[x] == 2]

data = data[data.index.isin(rkeeps)]
data = data[data.index.isin(ekeeps)].reset_index(drop = True)

# The functions for mapping city to county for runners and races

c_list = ['virginia beach city', 'alexandria city', 'norfolk city', 'fredericksburg city', 'chesapeake city', 'lexington city', 'falls church city',
          'staunton city', 'suffolk city', 'newport news city', 'hampton city', 'manassas city', 'harrisonburg city', 'prince georges',
          'la salle', 'saint marys', 'lynchburg city', 'portsmouth city', 'poquoson city', 'queen annes', 'matanuska susitna', 'st joseph', 'de kalb',
          'waynesboro city', 'winchester city', 'martinsville city', 'danville city', 'bristol city', 'de witt', 'galax city', 'colonial heights city']

b_list = ['virginia beach', 'alexandria', 'norfolk', 'fredericksburg', 'chesapeake', 'lexington', 'falls church', 'staunton', 'laporte', 'suffolk',
          'newport news', 'hampton', 'manassas', 'harrisonburg', "prince george's", 'lasalle', "st. mary's", 'lynchburg', 'portsmouth', 'poquoson',
          "queen anne's", 'matanuska-susitna', 'st. joseph', 'dekalb', 'waynesboro', 'winchester', 'martinsville', 'danville', 'bristol', 'dewitt',
          'galax', 'colonial heights']

def city_to_county(inp, rnr):
    
    if rnr == True:
        
        loc = inp.Runner_City.split(', ')
        city = loc[0].lower()
        state = loc[1].replace('"','').upper()
        
    else:
        
        loc = inp.Event_Location.split(', ')
        city = loc[0].lower()
        state = loc[1].replace('"','').upper()
        
    try:
        
        sx = list(ccmap['State short']).index(state)
        st = ccmap['State full'][sx]
        cc = ccmap[ccmap['City'] == city]
        cc = cc[cc['State short'] == state]
        county = cc.iloc[0]['County']
        
    except:
        
        county = 'NOPE'
        st = 'NOPE'
        
    if len(loc) > 2:
        
        county = 'NOPE'
        st = 'NOPE'
        
    if county != 'NOPE':
        
        if (county[0:5] == 'saint') and (county != 'saint marys'):
            
            back = county[5:]
            county = 'st.' + back
        
        elif county in c_list:
            
            county = b_list[c_list.index(county)]
            
    tmp = latlong[latlong.Province_State == st]
    tmp = tmp[tmp.Admin2 == county]
    
    if len(tmp) > 0:
        
        lat = tmp.iloc[0]['Lat']
        long = tmp.iloc[0]['Long_']
        coord = [lat,long]
        rco = county
        rst = st
        
    else:
        
        coord = [None,None]
        rco = None
        rst = None
    
    return [coord, rco, rst]

# Use the functions to get the counties for runner and race locations

r_counties = [city_to_county(data.iloc[i], rnr = True)[1] for i in range(len(data))]
e_counties = [city_to_county(data.iloc[i], rnr = False)[1] for i in range(len(data))]

# Get the states for the runners and counties

r_states = [data.Runner_City[i].split(', ')[1].replace('"', '').upper() for i in range(len(data))]
e_states = [data.Event_Location[i].split(', ')[1].replace('"', '').upper() for i in range(len(data))]

# Add these to data

r_counties = pd.Series(r_counties, name = 'Runner_County')
r_states = pd.Series(r_states, name = 'Runner_State')
e_counties = pd.Series(e_counties, name = 'Event_County')
e_states = pd.Series(e_states, name = 'Event_State')

data = pd.concat([data, r_counties, r_states, e_counties, e_states], axis = 1)

# Adding FIPS to data

def fips_r(x):
    
    if x.Runner_County != None:
        
        f = af.get_county_fips(x.Runner_County, state = x.Runner_State)
        
    else:
        
        f = None
    
    return f

def fips_e(x):
    
    if x.Event_County != None:
        
        f = af.get_county_fips(x.Event_County, state = x.Event_State)
        
    else:
        
        f = None
    
    return f

af = addfips.AddFIPS()

rfips = [fips_r(data.iloc[i]) for i in range(len(data))]
efips = [fips_e(data.iloc[i]) for i in range(len(data))]

data = pd.concat([data, pd.Series(rfips, name = 'Runner_FIPS'), pd.Series(efips, name = 'Event_FIPS')], axis = 1)

# Making a(nother) date column in data to match with the pollution data

def date_fixer(x):
    
    try:
        
        z = datetime.strptime(x, '%m/%d/%Y')
        
    except:
        
        z = datetime.strptime(x, '%Y-%m-%d')
    
    return z

dd = [date_fixer(data.DATE[i]) for i in range(len(data))]
data = pd.concat([data, pd.Series(dd, name = 'Race_Date')], axis = 1)

def date_fixer_2(x):
    
    z = datetime.strptime(str(x), '%Y%m%d')
    
    return z

dd_pm = [date_fixer_2(good_pm.Date[i]) for i in range(len(good_pm))]
dd_pm10 = [date_fixer_2(good_pm10.Date[i]) for i in range(len(good_pm10))]
dd_o3 = [date_fixer_2(good_o3.Date[i]) for i in range(len(good_o3))]
dd_no2 = [date_fixer_2(good_no2.Date[i]) for i in range(len(good_no2))]
dd_co = [date_fixer_2(good_co.Date[i]) for i in range(len(good_co))]
dd_pb = [date_fixer_2(good_pb.Date[i]) for i in range(len(good_pb))]

good_pm = pd.concat([good_pm, pd.Series(dd_pm, name = 'PM_Date')], axis = 1)
good_pm10 = pd.concat([good_pm10, pd.Series(dd_pm10, name = 'PM10_Date')], axis = 1)
good_o3 = pd.concat([good_o3, pd.Series(dd_o3, name = 'O3_Date')], axis = 1)
good_no2 = pd.concat([good_no2, pd.Series(dd_no2, name = 'NO2_Date')], axis = 1)
good_co = pd.concat([good_co, pd.Series(dd_co, name = 'CO_Date')], axis = 1)
good_pb = pd.concat([good_pb, pd.Series(dd_pb, name = 'Pb_Date')], axis = 1)

# Cleaning pollution FIPS

pm_fips = [f if len(f) == 5 else '0' + f for f in good_pm.FIPS]
pm10_fips = [f if len(f) == 5 else '0' + f for f in good_pm10.FIPS]
o3_fips = [f if len(f) == 5 else '0' + f for f in good_o3.FIPS]
no2_fips = [f if len(f) == 5 else '0' + f for f in good_no2.FIPS]
co_fips = [f if len(f) == 5 else '0' + f for f in good_co.FIPS]
pb_fips = [f if len(f) == 5 else '0' + f for f in good_pb.FIPS]

good_pm = pd.concat([good_pm, pd.Series(pm_fips, name = 'PM_FIPS')], axis = 1)
good_pm10 = pd.concat([good_pm10, pd.Series(pm10_fips, name = 'PM10_FIPS')], axis = 1)
good_o3 = pd.concat([good_o3, pd.Series(o3_fips, name = 'O3_FIPS')], axis = 1)
good_no2 = pd.concat([good_no2, pd.Series(no2_fips, name = 'NO2_FIPS')], axis = 1)
good_co = pd.concat([good_co, pd.Series(co_fips, name = 'CO_FIPS')], axis = 1)
good_pb = pd.concat([good_pb, pd.Series(pb_fips, name = 'Pb_FIPS')], axis = 1)

# Creating the event location pollution data

pm_data = []
pm10_data = []
o3_data = []
no2_data = []
co_data = []
pb_data = []

for i in range(len(data)):
    
    print(i)
    
    tmp_pm = good_pm[good_pm.PM_FIPS == data.Event_FIPS[i]]
    tmp_pm10 = good_pm10[good_pm10.PM10_FIPS == data.Event_FIPS[i]]
    tmp_o3 = good_o3[good_o3.O3_FIPS == data.Event_FIPS[i]]
    tmp_no2 = good_no2[good_no2.NO2_FIPS == data.Event_FIPS[i]]
    tmp_co = good_co[good_co.CO_FIPS == data.Event_FIPS[i]]
    tmp_pb = good_pb[good_pb.Pb_FIPS == data.Event_FIPS[i]]
    
    tmp_pm = tmp_pm[tmp_pm.PM_Date == data.Race_Date[i]].reset_index(drop = True)
    tmp_pm10 = tmp_pm10[tmp_pm10.PM10_Date == data.Race_Date[i]].reset_index(drop = True)
    tmp_o3 = tmp_o3[tmp_o3.O3_Date == data.Race_Date[i]].reset_index(drop = True)
    tmp_no2 = tmp_no2[tmp_no2.NO2_Date == data.Race_Date[i]].reset_index(drop = True)
    tmp_co = tmp_co[tmp_co.CO_Date == data.Race_Date[i]].reset_index(drop = True)
    tmp_pb = tmp_pb[tmp_pb.Pb_Date == data.Race_Date[i]].reset_index(drop = True)
    
    if len(tmp_pm) > 0:
        
        pm_data.append(tmp_pm.Value[0])
        
    else:
        
        pm_data.append(None)
        
    if len(tmp_pm10) > 0:
        
        pm10_data.append(tmp_pm10.Value[0])
        
    else:
        
        pm10_data.append(None)
        
    if len(tmp_o3) > 0:
        
        o3_data.append(tmp_o3.Value[0])
        
    else:
        
        o3_data.append(None)
        
    if len(tmp_no2) > 0:
        
        no2_data.append(tmp_no2.Value[0])
        
    else:
        
        no2_data.append(None)
        
    if len(tmp_co) > 0:
        
        co_data.append(tmp_co.Value[0])
        
    else:
        
        co_data.append(None)
        
    if len(tmp_pb) > 0:
        
        pb_data.append(tmp_pb.Value[0])
        
    else:
        
        pb_data.append(None)

pm_data = pd.Series(pm_data, name = 'PM')
pm10_data = pd.Series(pm10_data, name = 'PM10')
o3_data = pd.Series(o3_data, name = 'O3')
no2_data = pd.Series(no2_data, name = 'NO2')
co_data = pd.Series(co_data, name = 'CO')
pb_data = pd.Series(pb_data, name = 'Pb')

data = pd.concat([data, pm_data, pm10_data, o3_data, no2_data, co_data, pb_data], axis = 1)

# Creating the runner location pollution data

rnr_pm_data_30 = []
rnr_pm10_data_30 = []
rnr_o3_data_30 = []
rnr_no2_data_30 = []
rnr_co_data_30 = []
rnr_pb_data_30 = []

rnr_pm_data_60 = []
rnr_pm10_data_60 = []
rnr_o3_data_60 = []
rnr_no2_data_60 = []
rnr_co_data_60 = []
rnr_pb_data_60 = []

rnr_pm_data_90 = []
rnr_pm10_data_90 = []
rnr_o3_data_90 = []
rnr_no2_data_90 = []
rnr_co_data_90 = []
rnr_pb_data_90 = []

rnr_pm_data_180 = []
rnr_pm10_data_180 = []
rnr_o3_data_180 = []
rnr_no2_data_180 = []
rnr_co_data_180 = []
rnr_pb_data_180 = []

for i in range(len(data)):
    
    print(i) # 1127666
    
    pm_tmp = good_pm[good_pm.FIPS == data.Event_FIPS[i]]
    pm10_tmp = good_pm10[good_pm10.FIPS == data.Event_FIPS[i]]
    o3_tmp = good_o3[good_o3.FIPS == data.Event_FIPS[i]]
    no2_tmp = good_no2[good_no2.FIPS == data.Event_FIPS[i]]
    co_tmp = good_co[good_co.FIPS == data.Event_FIPS[i]]
    pb_tmp = good_pb[good_pb.FIPS == data.Event_FIPS[i]]
    
    pm_tmp = pm_tmp[pm_tmp.PM_Date < data.Race_Date[i]]
    pm10_tmp = pm10_tmp[pm10_tmp.PM10_Date < data.Race_Date[i]]
    o3_tmp = o3_tmp[o3_tmp.O3_Date < data.Race_Date[i]]
    no2_tmp = no2_tmp[no2_tmp.NO2_Date < data.Race_Date[i]]
    co_tmp = co_tmp[co_tmp.CO_Date < data.Race_Date[i]]
    pb_tmp = pb_tmp[pb_tmp.Pb_Date < data.Race_Date[i]]
    
    
    pm_tmp = pm_tmp[pm_tmp.PM_Date >= data.Race_Date[i] - timedelta(180)]
    pm10_tmp = pm10_tmp[pm10_tmp.PM10_Date >= data.Race_Date[i] - timedelta(180)]
    o3_tmp = o3_tmp[o3_tmp.O3_Date >= data.Race_Date[i] - timedelta(180)]
    no2_tmp = no2_tmp[no2_tmp.NO2_Date >= data.Race_Date[i] - timedelta(180)]
    co_tmp = co_tmp[co_tmp.CO_Date >= data.Race_Date[i] - timedelta(180)]
    pb_tmp = pb_tmp[pb_tmp.Pb_Date >= data.Race_Date[i] - timedelta(180)]
    
    pm_tmp2 = pm_tmp[pm_tmp.PM_Date >= data.Race_Date[i] - timedelta(90)]
    pm10_tmp2 = pm10_tmp[pm10_tmp.PM10_Date >= data.Race_Date[i] - timedelta(90)]
    o3_tmp2 = o3_tmp[o3_tmp.O3_Date >= data.Race_Date[i] - timedelta(90)]
    no2_tmp2 = no2_tmp[no2_tmp.NO2_Date >= data.Race_Date[i] - timedelta(90)]
    co_tmp2 = co_tmp[co_tmp.CO_Date >= data.Race_Date[i] - timedelta(90)]
    pb_tmp2 = pb_tmp[pb_tmp.Pb_Date >= data.Race_Date[i] - timedelta(90)]
    
    pm_tmp3 = pm_tmp[pm_tmp.PM_Date >= data.Race_Date[i] - timedelta(60)]
    pm10_tmp3 = pm10_tmp[pm10_tmp.PM10_Date >= data.Race_Date[i] - timedelta(60)]
    o3_tmp3 = o3_tmp[o3_tmp.O3_Date >= data.Race_Date[i] - timedelta(60)]
    no2_tmp3 = no2_tmp[no2_tmp.NO2_Date >= data.Race_Date[i] - timedelta(60)]
    co_tmp3 = co_tmp[co_tmp.CO_Date >= data.Race_Date[i] - timedelta(60)]
    pb_tmp3 = pb_tmp[pb_tmp.Pb_Date >= data.Race_Date[i] - timedelta(60)]
    
    pm_tmp4 = pm_tmp[pm_tmp.PM_Date >= data.Race_Date[i] - timedelta(30)]
    pm10_tmp4 = pm10_tmp[pm10_tmp.PM10_Date >= data.Race_Date[i] - timedelta(30)]
    o3_tmp4 = o3_tmp[o3_tmp.O3_Date >= data.Race_Date[i] - timedelta(30)]
    no2_tmp4 = no2_tmp[no2_tmp.NO2_Date >= data.Race_Date[i] - timedelta(30)]
    co_tmp4 = co_tmp[co_tmp.CO_Date >= data.Race_Date[i] - timedelta(30)]
    pb_tmp4 = pb_tmp[pb_tmp.Pb_Date >= data.Race_Date[i] - timedelta(30)]
    
    rnr_pm_data_30.append(np.mean(pm_tmp4.Value))
    rnr_pm_data_60.append(np.mean(pm_tmp3.Value))
    rnr_pm_data_90.append(np.mean(pm_tmp2.Value))
    rnr_pm_data_180.append(np.mean(pm_tmp.Value))
    
    rnr_pm10_data_30.append(np.mean(pm10_tmp4.Value))
    rnr_pm10_data_60.append(np.mean(pm10_tmp3.Value))
    rnr_pm10_data_90.append(np.mean(pm10_tmp2.Value))
    rnr_pm10_data_180.append(np.mean(pm10_tmp.Value))
    
    rnr_o3_data_30.append(np.mean(o3_tmp4.Value))
    rnr_o3_data_60.append(np.mean(o3_tmp3.Value))
    rnr_o3_data_90.append(np.mean(o3_tmp2.Value))
    rnr_o3_data_180.append(np.mean(o3_tmp.Value))
    
    rnr_no2_data_30.append(np.mean(no2_tmp4.Value))
    rnr_no2_data_60.append(np.mean(no2_tmp3.Value))
    rnr_no2_data_90.append(np.mean(no2_tmp2.Value))
    rnr_no2_data_180.append(np.mean(no2_tmp.Value))
    
    rnr_co_data_30.append(np.mean(co_tmp4.Value))
    rnr_co_data_60.append(np.mean(co_tmp3.Value))
    rnr_co_data_90.append(np.mean(co_tmp2.Value))
    rnr_co_data_180.append(np.mean(co_tmp.Value))
        
    rnr_pb_data_30.append(np.mean(pb_tmp4.Value))
    rnr_pb_data_60.append(np.mean(pb_tmp3.Value))
    rnr_pb_data_90.append(np.mean(pb_tmp2.Value))
    rnr_pb_data_180.append(np.mean(pb_tmp.Value))

rnr_pm_data_30 = pd.Series(rnr_pm_data_30, name = 'PM_Runner_30')
rnr_pm10_data_30 = pd.Series(rnr_pm10_data_30, name = 'PM10_Runner_30')
rnr_o3_data_30 = pd.Series(rnr_o3_data_30, name = 'O3_Runner_30')
rnr_no2_data_30 = pd.Series(rnr_no2_data_30, name = 'NO2_Runner_30')
rnr_co_data_30 = pd.Series(rnr_co_data_30, name = 'CO_Runner_30')
rnr_pb_data_30 = pd.Series(rnr_pb_data_30, name = 'Pb_Runner_30')

rnr_pm_data_60 = pd.Series(rnr_pm_data_60, name = 'PM_Runner_60')
rnr_pm10_data_60 = pd.Series(rnr_pm10_data_60, name = 'PM10_Runner_60')
rnr_o3_data_60 = pd.Series(rnr_o3_data_60, name = 'O3_Runner_60')
rnr_no2_data_60 = pd.Series(rnr_no2_data_60, name = 'NO2_Runner_60')
rnr_co_data_60 = pd.Series(rnr_co_data_60, name = 'CO_Runner_60')
rnr_pb_data_60 = pd.Series(rnr_pb_data_60, name = 'Pb_Runner_60')

rnr_pm_data_90 = pd.Series(rnr_pm_data_90, name = 'PM_Runner_90')
rnr_pm10_data_90 = pd.Series(rnr_pm10_data_90, name = 'PM10_Runner_90')
rnr_o3_data_90 = pd.Series(rnr_o3_data_90, name = 'O3_Runner_90')
rnr_no2_data_90 = pd.Series(rnr_no2_data_90, name = 'NO2_Runner_90')
rnr_co_data_90 = pd.Series(rnr_co_data_90, name = 'CO_Runner_90')
rnr_pb_data_90 = pd.Series(rnr_pb_data_90, name = 'Pb_Runner_90')

rnr_pm_data_180 = pd.Series(rnr_pm_data_180, name = 'PM_Runner_180')
rnr_pm10_data_180 = pd.Series(rnr_pm10_data_180, name = 'PM10_Runner_180')
rnr_o3_data_180 = pd.Series(rnr_o3_data_180, name = 'O3_Runner_180')
rnr_no2_data_180 = pd.Series(rnr_no2_data_180, name = 'NO2_Runner_180')
rnr_co_data_180 = pd.Series(rnr_co_data_180, name = 'CO_Runner_180')
rnr_pb_data_180 = pd.Series(rnr_pb_data_180, name = 'Pb_Runner_180')

data = pd.concat([data, rnr_pm_data_30, rnr_pm10_data_30, rnr_o3_data_30, rnr_no2_data_30, rnr_co_data_30, rnr_pb_data_30,
                  rnr_pm_data_60, rnr_pm10_data_60, rnr_o3_data_60, rnr_no2_data_60, rnr_co_data_60, rnr_pb_data_60,
                  rnr_pm_data_90, rnr_pm10_data_90, rnr_o3_data_90, rnr_no2_data_90, rnr_co_data_90, rnr_pb_data_90,
                  rnr_pm_data_180, rnr_pm10_data_180, rnr_o3_data_180, rnr_no2_data_180, rnr_co_data_180, rnr_pb_data_180,], axis = 1)

# Reading in a file I made with event types indicating which ones to keep

et = pd.read_csv(direc + 'event_types.csv')

# Using this file to remove non-ultra events

et = et[et.Keep == 1].reset_index(drop = True)
data = data[data.Event_Type.isin(list(et.Type))].reset_index(drop = True)

# Updating event type

new_types = [et.Type2[list(et.Type).index(e)] for e in data.Event_Type]
data = pd.concat([data, pd.Series(new_types, name = 'Type')], axis = 1)

# Calculating runner distance travelled to event

def le_fx(le_fx_in):
    
    try:
        
        le_fx_out = le(le_fx_in)
        
    except:
        
        le_fx_out = None
    
    return le_fx_out

rlocs = [le_fx(c) for c in data.Runner_Coordinates]
elocs = [le_fx(c) for c in data.Event_Coordinates]

dists = []

for i in range(len(data)):
    
    print(i)
    
    try:
        
        hdist = hs.haversine(rlocs[i], elocs[i], unit = Unit.KILOMETERS)
        dists.append(hdist)
        
    except:
        
        dists.append(None)

data = pd.concat([data, pd.Series(dists, name = 'Travel_Distance')], axis = 1)

# Save the data

data.to_csv(direc + 'data.csv', index = False)

