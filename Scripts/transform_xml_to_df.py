# -*- coding: utf-8 -*-
"""
Created on Tue Apr 26 21:53:24 2016

@author: jiun

Data out of xml and into a Postgres db.  Largely based on composer_frequency.py
"""

#import modules
from __future__ import division
# from sys import argv
import re
# from collections import Counter
# from sets import Set
# import matplotlib.pyplot as plt
import xml.etree.ElementTree as ET
import os
import os.path
import pandas as pd

os.chdir('/Users/jiun/Documents/analytics/NY_Philarchive_performanceHistory/Programs')

#create xml collection of "docs" (i.e., programs in NYPhil's definition)
tree = ET.parse('complete.xml')
root = tree.getroot()
concerts = root.findall('program')

#convert season listing (e.g., 1842-43) to a single leading year (1842)
def simplify_date(hyphenated_season):
    simple_season = re.sub(r'(\d{4})-\d{2}',r'\1',hyphenated_season)
    return simple_season

def clean_date(date_string):
    tmp = re.search('^\d{4}-\d{2}-\d{2}', date_string)
    return date_string[tmp.start():tmp.end()]

#get the composer's last name only from the worksComposerTitle elements
def get_name(work):
    composer_name = re.sub(r'(.*?)(,| \[).*',r'\1',work)
    composer_name = re.sub(r"(.*)'(.*)",r"\1\\'\2",composer_name)
    return composer_name

def extract_text(obj):
    if obj is None:
        return 'NA'
    else:
        return obj.text

#gather info from XML file
all_seasons = []
composers = []
current_season = '1842'
total_works = 0

# create a long table with the following columns:
# programID, date,Time, eventType, Location, Venue, composerName, workTitle, conductorName

for c in concerts:
    # season = simplify_date(c.find('season').text)
    
    programID = c.find('programID').text

    # concert info
    concertInfo = c.find('concertInfo')
    date = clean_date(concertInfo.find('Date').text)
    time = concertInfo.find('Time').text
    eventType = concertInfo.find('eventType').text
    location = concertInfo.find('Location').text
    venue = concertInfo.find('Venue').text

    # work info
    # stopped here: need to handle the case where there are 
    workInfo = c.find('worksInfo')
    for w in workInfo.findall('work'):
        composer = extract_text(w.find('composerName'))
        title = extract_text(w.find('workTitle'))
        conductor = extract_text(w.find('conductorName'))
        
        row = [programID, date, time, eventType, location, venue, composer, title] #, conductor]
        all_seasons.append(row)
    
# convert to a data frame
output_df = pd.DataFrame(all_seasons, 
                         columns = ['programID', 
                                    'date',
                                    'Time', 
                                    'eventType', 
                                    'Location', 
                                    'Venue', 
                                    'composerName', 
                                    'workTitle'])

output_df.to_csv('NY_Philharmonic_df.csv', sep = '|', encoding = 'utf-8')