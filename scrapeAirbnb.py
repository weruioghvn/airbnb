# Script to scrape Airbnb page.
# Example:
#     python scrapeAirbnb.py --city "Austin TX"
#     Rscript show_metrics.R 'Austin TX'

import mechanize
import os
import cookielib
import datetime
from lxml import html
import csv
import cStringIO
import codecs
from random import randint
from time import sleep
from lxml.etree import tostring
import bs4
import json
import random
import psycopg2
import re
import collections
import calendar
import pandas
import urllib
import copy
import sets
import argparse
from airbnb_utils import *
DEBUG = True

# Constants
kApiKey = 'd306zoyjsyarp7ifhu67rjxn52tv0t20'
kRoomTypes = ["Entire home/apt", "Private room", "Shared room"]

def addMonths(sourcedate, months):
    month = sourcedate.month - 1 + months
    year = int(sourcedate.year + month / 12 )
    month = month % 12 + 1
    day = min(sourcedate.day,calendar.monthrange(year,month)[1])
    return datetime.date(year,month,day)

def getDailyInformation(ListingID, date = None):
    # Function to get daily rate and availability information for specified
    # listing. 
    if date is None:
        currentDate = datetime.date.today()
        year = addMonths(currentDate, -1).year
        month = addMonths(currentDate, -1).month
    else:
        year = date.year
        month = date.month
#
    url = "https://www.airbnb.com/api/v2/calendar_months?key=" + kApiKey + \
          "&currency=USD&locale=en&listing_id=" + str(ListingID) + "&month=" + \
          str(month) + "&year=" + str(year) + "&count=3&_format=with_conditions"
#
    page = getPage(url)
#
    datesJson = json.loads(page.text)
    dailyInfo = []
#
    if "error_code" in datesJson:
        print "Do not have permission to access the data."
        return None
#
    datesMonth = datesJson['calendar_months']
    currentDates = []
    for i in range(len(datesMonth)):
        datesDay = datesMonth[i]['days']
        for j in range(len(datesDay)):
            if datesDay[j]['date'] not in currentDates:
                row = {}
                row['date'] = datesDay[j]['date']
                row['rate'] = datesDay[j]['price']['native_price']
                row['availability'] = datesDay[j]['available']
                row['listing_id'] = ListingID
                currentDates.append(row['date'])
                dailyInfo.append(row)
#
    return dailyInfo

def scrapeDailyStats(listingIds, date = None):
    result = [] 
    for i, listingId in enumerate(listingIds):
        result += getDailyInformation(listingId, date = date)
        print "Scrape %s page out of %s pages" % (i + 1, len(listingIds))
    return pandas.DataFrame(result)
        
def getSearchStats(page):
    tree = html.fromstring(page.content)
    text = tree.xpath('//div[@class = "map-search"]/@data-bootstrap-data')[0]
    text = re.sub("&quot;", '"', text)
    js = json.loads(text)
    with open("debug/prettified_json.txt", "wb") as f:
        f.write(json.dumps(js, indent = 4))
    searchResults = js["results_json"]["search_results"]
    numResults = len(searchResults)
    listings = []
    currentListings = []
    counter = 0
    for i in searchResults:
        counter = counter + 1
        if i["listing"]["id"] not in currentListings:
            listing = {}
            listing["rate"] = i["pricing_quote"]["rate"]["amount"]
            listing["guests"] = i["pricing_quote"]["guests"]
            listing["can_instant_book"] = i["pricing_quote"]["can_instant_book"]
            listing["reviews_count"] = i["listing"]["reviews_count"]
            listing["host_id"] = i["listing"]["primary_host"]["id"]
            listing["longitude"] = i["listing"]["lng"]
            listing["latitude"] = i["listing"]["lat"]
            listing["listing_id"] = i["listing"]["id"]
            listing["star_rating"] = i["listing"]["star_rating"]
            listing["room_type"] = i["listing"]["room_type"]
            listing["bedrooms"] = i["listing"]["bedrooms"]
            listing["counter"] = counter
            currentListings.append(listing["listing_id"])
            listings.append(listing)
    return listings

def getSearchUrl(city, roomType = 0, guests = 4, \
                  checkin = None, checkout = None, page = 1):
    params = {'checkin': checkin, 'checkout': checkout, 'guests': guests, \
              'room_types': kRoomTypes[roomType], 'page': page}
    paramsCopy = copy.deepcopy(params)
    for k, v in paramsCopy.iteritems():
        if v is None:
            params.pop(k, None)
#
    return kBaseUrlPrefix + city + '?' + urllib.urlencode(params)

def scrapeSearchStats(city, roomType = 0, guests = 4, \
                      checkin = None, checkout = None, page = 10):
    result = []
    for i in range(1, page + 1):
        url = getSearchUrl(city, roomType = roomType, guests = guests, \
                 checkin = checkin, checkout = checkout, page = i)
        pg = getPage(url)
        for row in getSearchStats(pg):
            row.update({"page": i})
            result.append(row)
        print "Scrape %s page out of %s pages" % (i, page)
    return pandas.DataFrame(result)

def saveToCsv(city, guests = 4, checkin = "", checkout = "", page = 10):
    filename = '../data/' + city + '_' + str(page) + '_pages'
    searchData = scrapeSearchStats(city, checkin = checkin, checkout = checkout, \
                                   guests = guests, page = page)
    listingIds = list(set(searchData['listing_id']))
    dailyData = scrapeDailyStats(listingIds)
    searchData.to_csv(filename + "_search.csv", index = False)
    dailyData.to_csv(filename + "_daily.csv", index = False)
    
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--city', type = str, required = True,
                        help = "City Name with State Code")
    args = vars(parser.parse_args())
    # city = "San-Francisco-CA"
    saveToCsv(args['city'].replace(' ', '-'), page = 80)

if __name__ == '__main__':
    main()
