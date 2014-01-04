import urllib2
from bs4 import BeautifulSoup
import itertools
import csv
import os, errno
import os.path
import string
import pg
from collections import OrderedDict


_dir = os.path.dirname(os.path.abspath(__file__))
cur = pg.connect(host="127.0.0.1")


def scrape_twitaholic(url):
    soup = BeautifulSoup(urllib2.urlopen(url).read())
    res = []
    for tr in soup.findAll('tr', {'style': 'border-top:1px solid black;'}):
        temp = tr.find('td', {'class': 'statcol_name'})
        res.append(temp.a['title'].split('(')[1][4:-1])
    return res

file = 'top1000.csv'
def generateTopUsersCSV():
    res = []
    for i in range(10):
        i = i + 1
        url = 'http://twitaholic.com/top' + str(i) + '00/followers/'
        res.append(scrape_twitaholic(url))
    res = itertools.chain(*res)
    res = list(OrderedDict.fromkeys(res))
    res = filter(None, res)
    with open(file, 'wb') as csvfile:
        csvWriter = csv.writer(csvfile, quoting=csv.QUOTE_ALL)
        rank = 0
        for item in res:
            rank = rank + 1
            csvWriter.writerow([rank, item])


def storeTopUsers(dir, file):
    cmd = "copy topUsers (rank, userName) from '${dir}/${file}' delimiters ',' csv"
    cmd = string.Template(cmd).substitute(locals())
    cur.query(cmd)

generateTopUsersCSV()
storeTopUsers(_dir, file)

