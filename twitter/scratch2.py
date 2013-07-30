import sys
import tweepy
from bson import json_util
import json
import datetime
import codecs
import os

consumer_key="vKbz24SqytZnYO33FNkR7w"
consumer_secret="jjobro8Chy9aKMzo8szYMz9tHftONLRkjNnrxk0"
access_key = "363361813-FKSdmwSbzuUzHWg326fTGJM7Bu2hTviqEetjMgu8"
access_secret = "VKgzDnTvDUWR1csliUR3BiMOI2oqO9NzocNKX1jPd4" 

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)

@classmethod
def parse(cls, api, raw):
	status = cls.first_parse(api, raw)
	setattr(status, 'json', json.dumps(raw))
	return status

tweepy.models.Status.first_parse = tweepy.models.Status.parse
tweepy.models.Status.parse = parse

_dir = os.path.dirname(os.path.abspath(__file__))

class CustomStreamListener(tweepy.StreamListener):
	def on_status(self, status):
		print status.user.screen_name
		with codecs.open(os.path.join(_dir, 'tweets.json'), "a", 'utf-8') as textFile:
			textFile.write(status.json)
			textFile.write('\n')

	def on_error(self, status_code):
		print >> sys.stderr, 'Encountered error with status code:', status_code
		return True # Don't kill the stream

	def on_timeout(self):
		print >> sys.stderr, 'Timeout...'
		return True # Don't kill the stream

class TweetEncoder(json.JSONEncoder):
	def default(self, obj):
		if isinstance(obj, tweepy.models.User):
			return obj.__getstate__() 
		elif isinstance(obj, datetime.datetime):
			return str(obj)
		elif isinstance(obj, tweepy.models.Status):
			return obj.__getstate__() 
		if isinstance(obj, tweepy.models.Place):
			return obj.__getstate__() 
		if isinstance(obj, tweepy.models.BoundingBox):
			return obj.__getstate__() 
		return json.JSONEncoder.default(self, obj)

sapi = tweepy.streaming.Stream(auth, CustomStreamListener())
# sapi.filter(track=['curiosity'])
# sapi.filter(track=['believetour'])
# sapi.filter(locations=[-122.75,36.8,-121.75,37.8])

filter = ["#dwts", "#glee", "#idol", "#xfactor", "#news", "#love", "#photography", "#fashion", "#health", "#fail", "#jobs", "#business", "#sales", "#economy", "#marketing", "#socialmedia", "#startup", "#android", "#androidgames""#iphone", "#iphonegames", "#ipad", "#ipadgames", "#app", "#edtech", "#education", "#lrnchat", "#teachers", "#climate", "#solar", "#globalwarming", "#drought", "#socialgood", "#cause", "#volunteer", "#4change", "#gemini", "#capricorn", "#libra", "#aries", "#scorpio", "#virgo", "#taurus", "#cancer", "#pisces", "#leo", "#sagittarius", "#aquarius"]

sapi.filter(track=filter)
