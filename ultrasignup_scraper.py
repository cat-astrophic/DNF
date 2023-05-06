# This script scrapes data from ultrasignup.com

# Importing required modules

import pandas as pd
from bs4 import BeautifulSoup as bs
import sys
from PyQt5.QtWebEngineWidgets import QWebEnginePage
from PyQt5.QtWidgets import QApplication
from PyQt5.QtCore import QUrl

# Project directory info

direc = 'F:/DNF/'

# Where the original ultradata was stored (too big to have multiple copies lying around....like i don't already...)

data_path = 'F:/ultradata/raw_results_data.csv'

# Reading in the raw ultramarathon data

raw_data = pd.read_csv(data_path)

# Subset for only American runners

raw_data = raw_data[raw_data.Country == 'USA'].reset_index(drop = True)

# Getting a list of all unique runners and relevant runner data

def get_name(i, df):
    
    try:
        
        tmp = df[df.Runner_ID == i].reset_index(drop = True)
        name = tmp['Name'][0]
        names = name.split(' ')
        
    except:
        
        names = None
    
    return names

def get_age(i, df):
    
    try:
        
        tmp = df[df.Runner_ID == i].reset_index(drop = True)
        a = tmp['Age'][0] + (2023 - tmp['RACE_Year'][0])
        
    except:
        
        a = None
    
    return a

def get_city(i, df):
    
    try:
        
        tmp = df[df.Runner_ID == i].reset_index(drop = True)
        c = tmp['City'][0]
        
    except:
        
        c = None
    
    return c

def get_state(i, df):
    
    try:
        
        tmp = df[df.Runner_ID == i].reset_index(drop = True)
        s = tmp['State'][0]
        
    except:
        
        s = None
    
    return s

unique_ids = list(raw_data.Runner_ID.unique())
fnames = []
lnames = []
ages = []
cities = []
states = []
icky = []

for rid in unique_ids:
    
    n = get_name(rid, raw_data)
    
    if n != None:
        
        fnames.append(n[0])
        lnames.append(n[-1])
        ages.append(get_age(rid, raw_data))
        cities.append(get_city(rid, raw_data))
        states.append(get_state(rid, raw_data))
        
    else:
        
        icky.append(rid)
        
unique_ids = [rid for rid in unique_ids if rid not in icky]

# Defining a Page class for calling dynamic webpages

class Page(QWebEnginePage):

    def __init__(self, url):
        
        self.app = QApplication(sys.argv)
        QWebEnginePage.__init__(self)
        self.html = ''
        self.loadFinished.connect(self._on_load_finished)
        self.load(QUrl(url))
        self.app.exec_()

    def _on_load_finished(self):
        
        self.html = self.toHtml(self.Callable)
        print('Load finished')

    def Callable(self, html_str):
        
        self.html = html_str
        self.app.quit()

# Main loop

runners = []
rcities = []
rstates = []
dates = []
years = []
events = []
overall = []
gp = []
results = []
rage = []
upcoming = []
competed = []
dnf = []
dns = []
check_rage = []

for i in range(len(fnames)):
    
    print(i)
    url = 'https://ultrasignup.com/results_participant.aspx?fname=' + fnames[i] + '&lname=' + lnames[i]
    page = Page(url)
    soup = bs(page.html, 'html.parser')
    athletes = soup.findAll('div', {'class': 'groupheader panel row'})
    events_data = soup.findAll('dd', {'class': 'accordion-content'})
    
    # Figure out which container is correct via age
    
    event_idx = None
    
    for athlete in athletes:
        
        idx = str(athlete).find('Gender + Age :')
        check_age = str(athlete)[idx+20:idx+22]
        
        try:
            
            check_age = int(check_age)
            
            if abs(ages[i] - int(check_age)) <= 1:
                
                event_idx = athletes.index(athlete)
                
        except:
            
            pass
            
    # Extract and store relevant data
    
    if event_idx != None:
        
        event_data = events_data[event_idx]
        rows = event_data.findAll('div', {'class': 'row'})
        rows = [r for r in rows if str(r) != '<div class="row">\n<div class="pull-left noMargin hidelink">\n</div>\n<div class="pull-right noMargin hidelink">\n</div>\n</div>']
        ref_list = []
        
        for row in rows:
            
            try:
                
                val = int('.toString("MMM d, yyyy")' in str(row.findAll('div')[1]))
                ref_list.append(val)
                
            except:
                
                ref_list.append(0)
        
        ref_list = ref_list + [1] # signifies end of list
        
        for row in rows:
            
            rr = row.findAll('div')
            weird = -1
            
            try:
                
                idx1 = str(rr[1]).find('.toString("MMM d, yyyy")')
                
            except:
                
                idx1 = -1
                
            try:
                
                idx2 = str(rr[1]).find('text: time')
                
            except:
                
                if len(rr) == 1:
                    
                    try:
                        
                        idx2 = str(rr[0]).find('text: time')
                        weird = 42069
                        
                    except:
                        
                        idx2 = -1
                else:
                    
                    idx2 = -1
            
            if idx1 > 0:
                
                date = str(rr[1])[idx1+26:idx1+38].replace('<','') # event date
                y = int(date[-4:]) # event year
                runners.append(unique_ids[i])
                rcities.append(cities[i])
                rstates.append(states[i])
                dates.append(date)
                years.append(y)
                eidx = + str(rr[0]).find('state">')
                eve = str(rr[0])[eidx+7:eidx+str(rr[0]).find('</strong>')-eidx]
                events.append(eve)
                
                if ref_list[rows.index(row)+1] == 1:
                    
                    overall.append(None)
                    gp.append(None)
                    results.append(None)
                    tmpdat = raw_data[raw_data.Runner_ID == rid].reset_index(drop = True)
                    competed.append(0)
                    
                    try:
                        
                        if rcities[-2] == rcities[-1]:
                            
                            rage.append(rage[-1] + y - years[-2])
                            
                        else:
                            
                            check_rage.append(len(rage))
                            rage.append(None)
                            
                    except:
                        
                        check_rage.append(len(rage))
                        rage.append(None)
                        
                    if y >= 2023:
                        
                        upcoming.append(1)
                        dnf.append(0)
                        dns.append(0)
                        
                    else:
                        
                        upcoming.append(0)
                        dnf.append(1-int('- DNS' in str(rr[0])))
                        dns.append(int('- DNS' in str(rr[0])))
                        
                else:
                    
                    competed.append(1)
                    upcoming.append(0)
                    dnf.append(0)
                    dns.append(0)
                    
            elif idx2 > 0:
                
                if weird > 0:
                    
                    overall.append(None)
                    gp.append(None)
                    res = str(rr[0])[idx2+12:str(rr[0]).find('</strong>')]
                    results.append(res)
                    
                else:
                    
                    try:
                        
                        res = str(rr[1])[idx2+12:str(rr[1]).find('</strong>')]
                        results.append(res)
                        oidx = str(rr[0]).find('gender_place ">')
                        otext = str(rr[0])[oidx+15:-6].split(' ')
                        overall.append(int(otext[0][8:]))
                        gp.append(int(otext[1][3:]))
                        
                    except:
                        
                        results.append(None)
                        overall.append(None)
                        gp.append(None)
                    
            else:
                
                rx = str(rr[0])[str(rr[0]).find('>Age: ')+6:-6]
                
                try:
                    
                    if int(rx) > 0:
                        
                        rage.append(int(rx))
                        
                    elif runners[-2] == runners[-1]:
                        
                        try:
                            
                            rage.append(rage[-1] + y - years[-1])
                            
                        except:
                            
                            check_rage.append(len(rage))
                            rage.append(None)
                            
                    else:
                        
                        check_rage.append(len(rage))
                        rage.append(None)
                        
                except:
                    
                    if runners[-2] == runners[-1]:
                        
                        try:
                            
                            rage.append(rage[-1] + y - years[-2])
                            
                        except:
                            
                            check_rage.append(len(rage))
                            rage.append(None)
                            
                    else:
                        
                        check_rage.append(len(rage))
                        rage.append(None)
                        
# Updating ages via check_rage

for number_increasing in range(10): # sometimes there are consecutive missing ages
    
    for c in check_rage:
        
        try:
            
            if rcities[c] == rcities[c+1]:
                
                new_rage = rage[c+1] + years[c] - years[c+1]
                rage[c] = new_rage
                check_rage.remove(c)
                
        except:
            
            continue

# Clean up events with event - distance - location

def event_cleaner(xxx):
    
    l = xxx.split(' - ')
    
    return l

event_names = []
event_types = []
event_locs = []

for event in events:
    
    tmp = event_cleaner(event)
    event_names.append(tmp[0])
    event_types.append(tmp[1])
    event_locs.append(tmp[2])

# Create a dataframe

runners = pd.Series(runners, name = 'Runner_ID')
rlocs = pd.Series([rcities[i] + ', ' + rstates[i] for i in range(len(rcities))], name = 'Runner_City')
dates = pd.Series(dates, name = 'Date')
years = pd.Series(years, name = 'Year')
overall = pd.Series(overall, name = 'Overall_Place')
gp = pd.Series(gp, name = 'Gender_Place')
results = pd.Series(results, name = 'Result')
rage = pd.Series(rage, name = 'Age')
event_names = pd.Series(event_names, name = 'Event')
event_types = pd.Series(event_types, name = 'Event_Type')
event_locs = pd.Series(event_locs, name = 'Event_Location')
upcoming = pd.Series(upcoming, name = 'Upcoming')
competed = pd.Series(competed, name = 'Competed')
dnf = pd.Series(dnf, name = 'DNF')
dns = pd.Series(dns, name = 'DNS')

out = pd.concat([runners, rlocs, dates, years, overall, gp, results, rage, event_names, event_types, event_locs, upcoming, competed, dnf, dns], axis = 1)

# Add runner gender to out

def get_gender(i):
    
    tmp = raw_data[raw_data.Runner_ID == i].reset_index(drop = True)
    g = tmp.Gender[0]
    
    return g

out_ids = list(out.Runner_ID.unique())

genders = [get_gender(out_id) for out_id in out_ids]
gender_list = [genders[out_ids.index(r)] for r in out.Runner_ID]

out = pd.concat([out, pd.Series(gender_list, name = 'Gender')], axis = 1)

# Save it

out.to_csv(direc + 'data/scraped_data.csv', index = False)

