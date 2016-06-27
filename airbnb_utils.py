# Utility functions for Airbnb investment project.

from lxml import html
import re
import requests

# Constants
kRoomUrlPrefix = "https://www.airbnb.com/rooms/"

class redshiftConnection:
    def __init__(self):
        self.host = "airbnbrdb.cvuvrunlpgtc.us-west-2.rds.amazonaws.com"
        self.user = "sean"
        self.port = 5432
        self.password = "waterice1"
        self.dbname = "airbnbdb"
        self.conn = psycopg2.connect(host=self.host, \
                                     user=self.user, \
                                     port=self.port, \
                                     password=self.password, \
                                     dbname=self.dbname)

    def runQuery(self, query, returnData = True):
        if returnData:
            cur = self.conn.cursor()
            cur.execute(query)
            dat = cur.fetchall()
            cur.close()
            return dat
        else:
            cur = self.conn.cursor()
            cur.execute(query)
            self.conn.commit()
            cur.close()

    def sendQuery(self, data, tableName, isChar = True, nLoop = 5):
        ## data should be array
        nrow = len(data)
        ncol = len(data[0])
        if isChar == True:
            isChar = [True] * ncol
        formatter = "(" + ",".join(["'%s'" if i else "%s" for i in isChar]) + "),"
        ## redshift could only take query with size 16M, break the query down to send to insights
        for k in range(nLoop):
            query = "insert into " + tableName + " values "
            for i in range(k * nrow / nLoop, (k + 1) * nrow / nLoop):
                insert = formatter % tuple(
                    re.sub("'", "''", str(q)) if p else q for p, q in zip(isChar, data[i]))
                query += insert
            query = query[:-1] + ';'
            self.runQuery(query, returnData = False)

def getListingUrl(listingId):
    return kRoomUrlPrefix + str(listingId)

def getPage(url):
    page = requests.get(url)
    return page

def getCoordinates(page):
    floatPattern = "\s*(-?\d*.\d+)"

    def getCoordinate(shorthand):
        pattern = "offset_" + shorthand + "&quot;:" + floatPattern
        return float(re.search(pattern, page.text).group(1))
    
    latitude = getCoordinate("lat")
    longitude = getCoordinate("lng")
    return (latitude, longitude)

def main():
    listingId = 8541471
    url = getListingUrl(listingId)
    page = getPage(url)
    print getCoordinates(page)

if __name__ == "__main__":
    main()
