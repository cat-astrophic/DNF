# This script creates a runner level variables to avoid having almost 130k fixed effets in the regression :)

# Importing required modules

import pandas as pd
from datetime import datetime

# Project directory

direc = 'D:/DNF/'

# Reading in the data

data = pd.read_csv(direc + 'data/data.csv')
raw = pd.read_csv('D:/ultradata/raw_results_data.csv') # in a different directory bc its a chonky boy

# Creating gender counts for each race in raw

e = []
m = []
f = []

for rid in raw.RACE_ID.unique():
    
    tmp = raw[raw.RACE_ID == rid]
    mtmp = tmp[tmp.Gender == 'M']
    ftmp = tmp[tmp.Gender == 'F']
    e.append(rid)
    
    try:
        
        m.append(max(mtmp.Gender_Place))
        
    except:
        
        m.append(0)
        
    try:
        
        f.append(max(ftmp.Gender_Place))
        
    except:
        
        f.append(0)

mc = [m[e.index(rid)] for rid in raw.RACE_ID]
fc = [f[e.index(rid)] for rid in raw.RACE_ID]
raw = pd.concat([raw, pd.Series(mc, name = 'M_Count'), pd.Series(fc, name = 'F_Count')], axis = 1)

# Make a relative gender place column in raw

rp = []

for i in range(len(raw)):
    
    g = raw.Gender[i]
    
    if g == 'F':
        
        rp.append(1 - ((raw.Gender_Place[i] - 1) / raw.F_Count[i]))
        
    else:
        
        rp.append(1 - ((raw.Gender_Place[i] - 1) / raw.M_Count[i]))

raw = pd.concat([raw, pd.Series(rp, name = 'Relative_Place')], axis = 1)

# Mean runner relative place

rpdf = raw.groupby('Runner_ID').mean()

vals = []

for r in data.Runner_ID:
    
    try:
        
        vals.append(rpdf.Relative_Place[r])
        
    except:
        
        vals.append(None)

data = pd.concat([data, pd.Series(vals, name = 'Ability')], axis = 1)

# Date cleaner function for data

def data_dc(ugh):
    
    if '/' in ugh.DATE:
        
        d = ugh.DATE[-4:]
        
        if ugh.DATE[1] == '/':
            
            spot = 1
            d = d + '0' + ugh.DATE[0]
            
        else:
            
            spot = 2
            d = d + ugh.DATE[:2]
            
        if ugh.DATE[spot+2] == '/':
            
            d = d + '0' + ugh.DATE[spot+1:spot+2]
            
        else:
            
            d = d + ugh.DATE[spot+1:spot+3]
            
    elif '-' in ugh.DATE:
        
        d = ugh.DATE[:4] + ugh.DATE[5:7] + ugh.DATE[-2:]
        
    return d

# Date cleaner function for raw

def raw_dc(ugh):
    
    m1 = ['Aug', 'Jul', 'Jun', 'May', 'Apr', 'Mar', 'Feb', 'Jan', 'Dec', 'Nov', 'Oct', 'Sep']
    m2 = [8, 7, 6, 5, 4, 3, 2, 1, 12, 11, 10,9]
    
    d = str(ugh.RACE_Year)
    d = d + str(m2[m1.index(ugh.RACE_Month)])
    
    if ugh.RACE_Date < 10:
        
        d = d + '0' + str(ugh.RACE_Date)
        
    else:
        
        d = d + str(ugh.RACE_Date)
    
    return d

# Making clean dates

data_dates = [data_dc(data.iloc[i]) for i in range(len(data))]
raw_dates = [raw_dc(raw.iloc[i]) for i in range(len(raw))]

data = pd.concat([data, pd.Series(data_dates, name = 'D')], axis = 1)
raw = pd.concat([raw, pd.Series(raw_dates, name = 'D')], axis = 1)

# Time since last race for runner r

prev = []

for i in range(len(data)):
    
    tmp = raw[raw.Runner_ID == data.Runner_ID[i]].sort_values('D')
    tmp = tmp[tmp.D < data.D[i]]
    
    try:
        
        prev.append(max(tmp.D))
        
    except:
        
        prev.append(None)

time_since = []

for i in range(len(data)):
    
    try:
        
        time_since.append((datetime.strptime(data.D[i], '%Y%m%d') - datetime.strptime(prev[i], '%Y%m%d')).days)
        
    except:
        
        time_since.append(None)

data = pd.concat([data, pd.Series(time_since, name = 'Time_Since')], axis = 1)

# Total races completed by date d for runner r

past = []

for i in range(len(data)):
    
    
    print(i)
    
    tmp = raw[raw.Runner_ID == data.Runner_ID[i]]
    tmp = tmp[tmp.D < data.D[i]]
    past.append(len(tmp))

data = pd.concat([data, pd.Series(past, name = 'Previous_Races')], axis = 1)

# Save the data

data.to_csv(direc + 'data/data.csv')

