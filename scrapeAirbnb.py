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

DEBUG = True

def monkeypatch_mechanize():
    """Work-around for a mechanize 0.2.5 bug. See: https://github.com/jjlee/mechanize/pull/58"""
    import mechanize
    if mechanize.__version__ < (0, 2, 6):
        from mechanize._form import SubmitControl, ScalarControl

        def __init__(self, type, name, attrs, index=None):
            ScalarControl.__init__(self, type, name, attrs, index)
            # IE5 defaults SUBMIT value to "Submit Query"; Firebird 0.6 leaves it
            # blank, Konqueror 3.1 defaults to "Submit".  HTML spec. doesn't seem
            # to define this.
            if self.value is None:
                if self.disabled:
                    self.disabled = False
                    self.value = ""
                    self.disabled = True
                else:
                    self.value = ""
            self.readonly = True

        SubmitControl.__init__ = __init__

def add_months(sourcedate,months):
    month = sourcedate.month - 1 + months
    year = int(sourcedate.year + month / 12 )
    month = month % 12 + 1
    day = min(sourcedate.day,calendar.monthrange(year,month)[1])
    return datetime.date(year,month,day)

def getDailyInformation(ListingID, month = 3, year = 2016):
    apiKey = 'd306zoyjsyarp7ifhu67rjxn52tv0t20'
    url = "https://www.airbnb.com/api/v2/calendar_months?key=" + apiKey + \
          "&currency=USD&locale=en&listing_id=" + str(ListingID) + "&month=" + str(month) + "&year=" + \
          str(year) + "&count=3&_format=with_conditions"
    for i in range(10):
        try:
            proxy = random.choice(proxyList)
            print proxy
            br.set_proxies({"http": proxy})
            r = br.open(url)
            break
        except:
            print "Unable to open the URL"
    datesJson = json.loads(r.read())

    priceDict = {}
    availabilityDict = {}

    datesMonth = datesJson['calendar_months']
    for i in range(len(datesMonth)):
        datesDay = datesMonth[i]['days']
        for j in range(len(datesDay)):
            d = datesDay[j]['date']
            p = datesDay[j]['price']['native_price']
            a = datesDay[j]['available']
            priceDict[d] = p
            availabilityDict[d] = a

    return priceDict, availabilityDict

def scrapeSearchPage(city, guests = 4, checkin = "", checkout = "", page = 1):
    url = "https://www.airbnb.com/"
    br.open(url)
    br.select_form(nr = 0)
    br["location"] = city
    br["checkin"] = checkin
    br["checkout"] = checkout
    br["guests"] = [str(guests)]
    response = br.submit()
    sleep(3)

    if DEBUG:
        print "Filled the forms and Submit"
    urlPrefix = br.geturl()

    listingTable = []
    scrapeTable = []
    currentTime = str(datetime.datetime.now())[:19]

    scrapeTable.append(("DEFAULT",
                        city,
                        "NULL" if checkin == '' else "\'" + checkin + "\'",
                        "NULL" if checkout == '' else "\'" + checkout + "\'",
                        page,
                        guests,
                        currentTime))

    sql = "SELECT max(scrape_id) FROM scrape_plan;"
    r = redshiftConnection()
    isChar = [False, True, False, False, False, False, True]
    r.sendQuery(scrapeTable, "scrape_plan", isChar, 1)
    scrapeId = r.runQuery(sql)[0][0]

    for i in range(1, page + 1):
        pageUrl = "&page=" + str(i)
        url = urlPrefix + pageUrl
        if DEBUG:
            print "search page number =", i
        try:
            br.open(url)
        except:
            continue
        linkList = list(br.links())
        listDetail = collections.defaultdict(dict)
        for l in linkList:
            m = re.search(r"/(\d+)\?", l.url)
            if m:
                id = int(m.group(1))
                m1 = re.search(r"^\$(\d+)$", l.text)
                if m1:
                    listDetail[id]["Price"] = int(l.text[1:])
                    continue

                if l.text.endswith("review") or \
                   l.text.endswith("reviews"):
                    m1 = re.search("\xc2\xb7 (\d+) review", l.text)
                    listDetail[id]["Reviews"] = int(m1.group(1))
                    continue

        counter = 0
        for l in linkList:
            if l.text is not None and l.text != "" and l.text[0] == "$":
                counter += 1
                m = re.search(r"/(\d+)\?", l.url)
                id = int(m.group(1))
                listingTable.append((scrapeId,
                                     id,
                                     i,
                                     counter,
                                     listDetail[id]["Price"],
                                     0 if "Reviews" not in listDetail[id].keys() \
                                         else listDetail[id]["Reviews"]))

    r = redshiftConnection()
    isChar = [False, False, False, False, False, False]
    r.sendQuery(listingTable, "search_page", isChar, 1)

    return scrapeId, listingTable


def scrapeDetailPage(scrapeId, listingTable):
    finalResults = []
    counter = 0
    baseURL = 'https://www.airbnb.com/rooms/'
    listingId = [row[1] for row in listingTable]
    month = add_months(datetime.date.today(), -2).month
    year = add_months(datetime.date.today(), -2).year
    r = redshiftConnection()

    for listing in listingId:
        finalResults = []
        counter += 1
        if DEBUG:
            print 'Processing Listing %s out of %s' % (str(counter), str(len(listingId)))
        priceDict, availabilityDict = getDailyInformation(listing, month, year)

        for key in priceDict.keys():
            finalResults.append((scrapeId,
                                 listing,
                                 key,
                                 int(priceDict[key]),
                                 availabilityDict[key]))

        isChar = [False, False, True, False, False]
        r.sendQuery(finalResults, "detail_page", isChar, 1)

def main():
        # Browser
    br = mechanize.Browser()

    # learned necessary configuration from
    # http://stockrt.github.io/p/emulating-a-browser-in-python-with-mechanize/

    # Allow cookies
    cj = cookielib.LWPCookieJar()
    br.set_cookiejar(cj)

    # Browser options
    br.set_handle_equiv(True)
    br.set_handle_gzip(True)
    br.set_handle_redirect(True)
    br.set_handle_referer(True)
    br.set_handle_robots(False)

    proxyList = ["167.114.104.163:3128",
                 "52.11.192.42:8083",
                 "52.11.154.179:8083",
                 "205.202.51.8:80",
                 "52.53.214.11:8083",
                 "31.168.236.236:8080",
                 "193.232.57.60:8090",
                 "52.53.236.138:8083",
                 "78.46.150.154:3128",
                 "180.250.96.197:8080",
                 "51.255.194.125:80",
                 "94.23.17.157:80",
                 "190.12.92.51:80",
                 "167.198.80.83:3128",
                 "183.91.33.51:80",
                 "183.91.33.43:80",
                 "212.126.102.238:8080",
                 "49.231.137.196:8080",
                 "183.91.33.77:80",
                 "212.38.166.231:443",
                 "138.36.169.64:80"]

    proxy = random.choice(proxyList)
    print proxy
    br.set_proxies({"http": proxy})
    # Follows refresh 0 but not hangs on refresh > 0
    br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)
    # specify browser to emulate
    br.addheaders = [('User-agent',
                      'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]

    url = "https://www.airbnb.com/s/Las-Vegas--NV--United-States?guests=4"
    response = br.open(url)
    response.read()

    form = br.select_form(nr=1)
    br["checkin"] = "2016-06-01"
    br["checkout"] = "2016-06-02"
    br["guests"] = ["1"]
    response = br.submit()
    print response.read()

    for link in br.links():
        print link.text, link.url

    scrapeId, listingTable = scrapeSearchPage("Las Vegas", page = 20)
    scrapeDetailPage(scrapeId, listingTable)

if __name__ == '__main__':
    main()

