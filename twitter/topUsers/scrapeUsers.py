import itertools
from itertools import izip_longest
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
_cur = pg.connect(host="127.0.0.1")

def scrape_twitaholic(url):
    soup = BeautifulSoup(urllib2.urlopen(url).read())
    res = []
    for tr in soup.findAll('tr', {'style': 'border-top:1px solid black;'}):
        temp = tr.find('td', {'class': 'statcol_name'})
        res.append(temp.a['title'].split('(')[1][4:-1])
    return res

def scrape_socialbakers(url):
    soup = BeautifulSoup(urllib2.urlopen(url).read())
    res = []
    for div in soup.findAll('div', {'id': 'snippet-bookmarkToggle-bookmarkToggle'}):
        res.append(div.findAll('div')[0]['id'].split('-')[-1])
    print "grabbed %s results from url %s" % (len(res), url)
    return res

def generateTopUsersTwitaholic ():
    res = []
    for i in range(10):
        i = i + 1
        url = 'http://twitaholic.com/top' + str(i) + '00/followers/'
        res.append(scrape_twitaholic(url))
    return res

def generateTopUsersSocialBakers (numUsers=10000):
    res = []
    for i in range(numUsers/50):
        url = 'http://socialbakers.com/twitter/page-' + str(i+1) + '/'
        res.append(scrape_socialbakers(url))
    return res

_topUsersDir = "%s/dissertationData/topRankedUsers" % (_dir)

def generateTopUsersCSV (scrapeFun, topUsersFile):
    res = scrapeFun()
    res = list(itertools.chain(*res))
    res = [x.lower() for x in res]
    res = OrderedDict.fromkeys(res)
    res = filter(None, res)
    with open("%s/%s" % (_topUsersDir, topUsersFile), 'wb') as csvfile:
        csvWriter = csv.writer(csvfile, quoting=csv.QUOTE_ALL)
        rank = 0
        for item in res:
            rank = rank + 1
            csvWriter.writerow([item, rank])

def storeTopUsers(topUsersFile):
    topUsersDir = _topUsersDir
    cmd = "copy topUsers (user_screen_name, rank) from '${topUsersDir}/${topUsersFile}' delimiters ',' csv"
    cmd = string.Template(cmd).substitute(locals())
    _cur.query(cmd)

def generateTopUsers (scrapeFun=generateTopUsersTwitaholic, topUsersFile='top1000Twitaholic.csv'):
    generateTopUsersCSV(scrapeFun=scrapeFun, topUsersFile=topUsersFile)
    storeTopUsers(topUsersFile=topUsersFile)

def backupTables ():
    tableNames = ['topUsers', 'twitter_users']
    for tableName in tableNames:
        file = "%s/dissertationData/tables/%s.csv" % (_dir, tableName)
        cmd = string.Template("copy ${tableName} to '${file}' delimiter ',' csv header").substitute(locals())
        _cur.query(cmd)

def getTweepyAPI ():
    consumer_key="vKbz24SqytZnYO33FNkR7w"
    consumer_secret="jjobro8Chy9aKMzo8szYMz9tHftONLRkjNnrxk0"
    access_key = "363361813-FKSdmwSbzuUzHWg326fTGJM7Bu2hTviqEetjMgu8"
    access_secret = "VKgzDnTvDUWR1csliUR3BiMOI2oqO9NzocNKX1jPd4"
    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_key, access_secret)
    return tweepy.API(auth)

_api = getTweepyAPI()

def getRemainingHitsUserTimeline():
    stat = _api.rate_limit_status()
    return stat['resources']['statuses']['/statuses/user_timeline']['remaining']

def getRemainingHitsGetUser():
    stat = _api.rate_limit_status()
    return stat['resources']['users']['/users/lookup']['remaining']

def getTweets(screen_name, **kwargs):
    # w.r.t include_rts: ref: https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline
    # When set to false, the timeline will strip any native retweets (though they
    # will still count toward both the maximal length of the timeline and the slice
    # selected by the count parameter).
    return _api.user_timeline(screen_name=screen_name,include_rts=True,**kwargs)

def isRetweet(tweet):
    return hasattr(tweet, 'retweeted_status')

def write2csv(res, file):
    with open(file, 'wb') as f:
        writer = csv.writer(f, quoting=csv.QUOTE_ALL)
        writer.writerows(res)

def getInfoForUser(screenNames):
    users = _api.lookup_users(screen_names=screenNames)
    res = [[user.id, user.created_at, user.description.encode('utf-8'), user.followers_count, user.friends_count,
            user.lang, user.location.encode('utf-8'), user.name.encode('utf-8'), user.screen_name.lower(), user.verified] for user in users]
    file = '/tmp/%s..%s_user.csv' % (screenNames[0], screenNames[-1])
    print "writing %s results to file: %s" % (len(res), file)
    write2csv(res, file)
    print "updating database with results in temp file"
    _cur.query("copy twitter_users (id,created_at,description,followers_count,friends_count,lang,location,name,user_screen_name,verified) from '%s' delimiters ',' csv" % (file))

def getAllTweets(screenNames):
    def getTweetsBetween(greaterThanID, lessThanID):
        alltweets = []
        while True:
            print "getting tweets from %s that are later than %s but before %s" % (screen_name, greaterThanID, lessThanID)
            newTweets = getTweets(screen_name, count=200, max_id=lessThanID-1, since_id=greaterThanID+1)
            if len(newTweets) == 0:
                break
            alltweets.extend(newTweets)
            lessThanID = alltweets[-1].id
            print "...%s tweets downloaded so far" % (len(alltweets))
        return alltweets
    assert len(screenNames) == 1, "Passed more than one screen name into function"
    screen_name = screenNames[0]
    print "getting tweets for %s" % (screen_name)
    alltweets = []
    userID = _api.get_user(screen_name).id
    lessThanID = getTweets(screen_name, count=1)[-1].id+1
    cmd = string.Template("select id from tweets where user_id = '${userID}' order by id desc limit 1").substitute(locals())
    res = _cur.query(cmd).getresult()
    if len(res) == 0:
        newestGrabbed = 0
    else:
        newestGrabbed = int(res[0][0])
    res = getTweetsBetween(newestGrabbed, lessThanID)
    alltweets.extend(res)
    cmd = string.Template("select id from tweets where user_id = '${userID}' order by id asc limit 1").substitute(locals())
    res = _cur.query(cmd).getresult()
    if len(res) == 0:
        lessThanID = 0
    else:
        lessThanID = int(res[0][0])
    alltweets.extend(getTweetsBetween(0, lessThanID))
    outTweets = [[tweet.id_str, tweet.user.id, tweet.user.screen_name.lower(), tweet.created_at, isRetweet(tweet), tweet.in_reply_to_status_id_str,
                  tweet.lang, tweet.truncated, tweet.text.encode("utf-8")] for tweet in alltweets]
    file = '/tmp/%s_tweets.csv' % screen_name
    print "writing %s results to file: %s" % (len(outTweets), file)
    write2csv(outTweets, file)
    print "updating database with results in temp file"
    _cur.query("copy tweets (id, user_id, user_screen_name, created_at, retweeted, in_reply_to_status_id, lang, truncated,text) from '%s' delimiters ',' csv" % (file))

def userAlreadyCollected (user_screen_name):
    res = _cur.query(string.Template("select * from tweets where user_screen_name='${user_screen_name}' limit 1").substitute(locals())).getresult()
    return len(res) > 0

def userInfoAlreadyCollected (user_screen_name):
    res = _cur.query(string.Template("select * from twitter_users where user_screen_name='${user_screen_name}' limit 1").substitute(locals())).getresult()
    return len(res) > 0

# ref: http://stackoverflow.com/questions/434287/what-is-the-most-pythonic-way-to-iterate-over-a-list-in-chunks/434411#434411
def chunker(seq, size):
    return (seq[pos:pos + size] for pos in xrange(0, len(seq), size))

def getForTopUsers (alreadyCollectedFun, getForUserFun, getRemainingHitsFun, hitsAlwaysGreaterThan, groupFun=lambda x: chunker(x,1)):
    res = _cur.query('select (user_screen_name) from topUsers order by rank asc limit 10000').getresult()
    screenNames = [[user[0]] for user in res]
    screenNames = list(itertools.chain(*screenNames))
    screenNameGroups = groupFun(screenNames)
    for screenNameGroup in screenNameGroups:
        newScreenNames = []
        for screenName in screenNameGroup:
            if alreadyCollectedFun(screenName):
                print "already collected tweets for %s; moving to next user" % (screenName)
                continue
            newScreenNames.append(screenName)
        if len(newScreenNames) == 0:
            continue
        try:
            while True:
                remainingHits = getRemainingHitsFun()
                if remainingHits > hitsAlwaysGreaterThan:
                    break
                print "only %s remaining hits; waiting until greater than %s" % (remainingHits, hitsAlwaysGreaterThan)
                time.sleep(60)
            print "calling %s with %s at %s remaining hits" % (getForUserFun, newScreenNames, remainingHits)
            getForUserFun(newScreenNames)
        except Exception as e:
            print "couldn't do it for %s: %s" % (newScreenNames, e)
            time.sleep(1)
            pass

def getAllTweetsForTopUsers ():
    getForTopUsers(alreadyCollectedFun=userAlreadyCollected, getForUserFun=getAllTweets, getRemainingHitsFun=getRemainingHitsUserTimeline, hitsAlwaysGreaterThan=30)

def getUserInfoForTopUsers ():
    getForTopUsers(alreadyCollectedFun=userInfoAlreadyCollected, getForUserFun=getInfoForUser, getRemainingHitsFun=getRemainingHitsGetUser, hitsAlwaysGreaterThan=30, groupFun=lambda x: chunker(x, 100))

#generateTopUsers(scrapeFun=generateTopUsersSocialBakers, topUsersFile='top10000SocialBakers.csv')
#generateTopUsers(scrapeFun=generateTopUsersTwitaholic, topUsersFile='top1000Twitaholic.csv')
#generateTopUsers(scrapeFun=lambda : generateTopUsersSocialBakers(numUsers=100000), topUsersFile='top100000SocialBakers.csv')
#storeTopUsers(topUsersFile='top100000SocialBakers.csv')
#getAllTweets('claytonstanley1')
#getAllTweetsForTopUsers()
#getUserInfoForTopUsers()
backupTables()
