import urllib2
from bs4 import BeautifulSoup
import itertools


def scrape_twitaholic(url):
    soup = BeautifulSoup(urllib2.urlopen(url).read())
    res = []
    for tr in soup.findAll('tr', {'style': 'border-top:1px solid black;'}):
        temp = tr.find('td', {'class': 'statcol_name'})
        res.append(temp.a['title'].split('(')[1][4:-1])
    return res

res = []
for i in range(10):
    i = i + 1
    url = 'http://twitaholic.com/top' + str(i) + '00/followers/'
    res.append(scrape_twitaholic(url))
    res

for item in itertools.chain(*res):
    print item
