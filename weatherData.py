# author: Justin Davis
# Collect daily weather forecasts from World Weather Online.
#
# Note: Although I randomly get 70% of the NJ zip codes, I only use 10% of them in the project
# because of the time it takes to retrieve data for the specified zip over the last ten years.

from wwo_hist import retrieve_hist_data
import urllib.request
import requests
import os
from pyzipcode import ZipCodeDatabase
import math
import random
import csv

# change directory
dir = r"C:\Users\jadtr\Desktop\School\Spring 2020\Data Mining I\Project"
os.chdir(dir)

# function to get number of lines in the file
def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1

# getting all csv files of zip code weather data
# 1) add edit to the file name and 2) remove the repeat of march 2020 data
for file in os.listdir(dir):
    if file.endswith(".csv"):
        file_length = file_len(file)
        with open(file, 'r') as inp, open(file.replace('.csv','_edit.csv'), 'w', newline='') as out:
            writer = csv.writer(out)
            curr_line = 0
            for row in csv.reader(inp):
                if curr_line < file_length-31:
                    curr_line += 1
                    writer.writerow(row)

# getting NJ zip codes
zc = ZipCodeDatabase()
zipped_codes_nj = zc.find_zip(state='NJ')
uz_codes_nj = [z.zip for z in zipped_codes_nj]
# number of zip codes
num_zips = len(uz_codes_nj)
print(num_zips)
# want to use random zip codes to determine trends for the whole state - roughly want 70%
num_rand = math.floor(num_zips*.7)
# shuffle the zip codes
random.seed(10)
random.shuffle(uz_codes_nj)
# extract the first 70% zips -- these are now the random zip codes we will collect info on
good_zips = uz_codes_nj[0:num_rand]
# these zip codes gave problems when trying to get data - remove them
good_zips.remove('08017')
good_zips.remove('08370')
good_zips.remove('07427')
good_zips.remove('07841')
good_zips.remove('08227')
good_zips.remove('07184')
good_zips.remove('07114')
good_zips.remove('07189')
good_zips.remove('07842')
good_zips.remove('07607')
good_zips.remove('08878')

# find out where we left off when switching api keys
files = 0
file_list = list()
for file in os.listdir(dir):
    if file.endswith(".csv"):
        files += 1
        file_list.append(file.replace('.csv', ''))

# print to screen to see
print("# of files: " + str(files))

# getting forecasted weather data for all the zip codes
# gets daily weather forecasts
frequency = 24
start_date = '1-JAN-2009'
end_date = '31-MAR-2020'
api_key = 'cbb45e1ebda64725a11200528201004'
location_list = good_zips
hist_weather_data = retrieve_hist_data(api_key,
                                location_list,
                                start_date,
                                end_date,
                                frequency,
                                location_label = False,
                                export_csv = True,
                                store_df = True)