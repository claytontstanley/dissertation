import time
import sys
import argparse
import urllib2
from bs4 import BeautifulSoup
import itertools
import csv
import os, errno
import os.path
import string
import pg
from collections import OrderedDict
import tweepy
import codecs
from pylama.main import parse_options

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
            csvWriter.writerow([item, rank])


def storeTopUsers(dir, file):
    cmd = "copy topUsers (user_screen_name, rank) from '${dir}/${file}' delimiters ',' csv"
    cmd = string.Template(cmd).substitute(locals())
    cur.query(cmd)

#generateTopUsersCSV()
#storeTopUsers(_dir, file)


consumer_key="vKbz24SqytZnYO33FNkR7w"
consumer_secret="jjobro8Chy9aKMzo8szYMz9tHftONLRkjNnrxk0"
access_key = "363361813-FKSdmwSbzuUzHWg326fTGJM7Bu2hTviqEetjMgu8"
access_secret = "VKgzDnTvDUWR1csliUR3BiMOI2oqO9NzocNKX1jPd4"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)

# ref: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
# When set to false, the timeline will strip any native retweets (though they
# will still count toward both the maximal length of the timeline and the slice
# selected by the count parameter).
includeRetweets = True

def getRemainingHits():
    return api.rate_limit_status()['resources']['statuses']['/statuses/user_timeline']['remaining']

def getAllTweets(screen_name):
    def getTweetsBetween(greaterThanID, lessThanID):
        alltweets = []
        while True:
            print "at rate limit %s: getting tweets later than %s but before %s" % (getRemainingHits(), greaterThanID, lessThanID)
            newTweets = api.user_timeline(screen_name = screen_name,count=200,max_id=lessThanID-1, since_id=greaterThanID+1, include_rts=includeRetweets)
            if len(newTweets) == 0:
                break
            alltweets.extend(newTweets)
            lessThanID = alltweets[-1].id
            print "...%s tweets downloaded so far" % (len(alltweets))
        return alltweets
    print "getting tweets for %s" % (screen_name)
    alltweets = []
    userID = api.get_user(screen_name).id
    lessThanID = api.user_timeline(screen_name=screen_name, count=1, include_rts=includeRetweets)[-1].id + 1
    cmd = string.Template("select id from tweets where user_id = '${userID}' order by id desc limit 1").substitute(locals())
    res = cur.query(cmd).getresult()
    if len(res) == 0:
        newestGrabbed = 0
    else:
        newestGrabbed = int(res[0][0])
    res = getTweetsBetween(newestGrabbed, lessThanID)
    alltweets.extend(res)
    cmd = string.Template("select id from tweets where user_id = '${userID}' order by id asc limit 1").substitute(locals())
    res = cur.query(cmd).getresult()
    if len(res) == 0:
        lessThanID = 0
    else:
        lessThanID = int(res[0][0])
    alltweets.extend(getTweetsBetween(0, lessThanID))
    outTweets = [[tweet.id_str, tweet.user.id, tweet.user.screen_name, tweet.created_at, tweet.retweeted, tweet.in_reply_to_status_id_str, tweet.text.encode("utf-8")] for tweet in alltweets]
    #print(outTweets)
    file = '/tmp/%s_tweets.csv' % screen_name
    print "writing %s results to temp file: %s" % (len(outTweets), file)
    with open(file, 'wb') as f:
        writer = csv.writer(f, quoting=csv.QUOTE_ALL)
        writer.writerows(outTweets)
    print "updating database with results in temp file"
    cmd = "copy tweets (id, user_id, user_screen_name, created_at, retweeted, in_reply_to_status_id, text) from '${file}' delimiters ',' csv"
    cmd = string.Template(cmd).substitute(locals())
    cur.query(cmd)

def userAlreadyCollected (user_screen_name):
    res = cur.query(string.Template("select * from tweets where user_screen_name='${user_screen_name}' limit 1").substitute(locals())).getresult()
    return len(res) > 0

def getAllTweetsForTopUsers ():
    res = cur.query('select (user_screen_name) from topUsers limit 100').getresult()
    screenNames = [[user[0]] for user in res]
    screenNames = itertools.chain(*screenNames)
    for screenName in screenNames:
        if userAlreadyCollected(screenName):
            print "already collected tweets for %s; moving to next user" % (screenName)
            continue
        if getRemainingHits() > 10:
            getAllTweets(screenName)
        else:
            print "only %s remaining hits; waiting until greater than 10" % (getRemainingHits())
            time.sleep(60)

#getAllTweets('claytonstanley1')
getAllTweetsForTopUsers()


