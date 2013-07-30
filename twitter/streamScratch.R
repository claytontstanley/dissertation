PATH = getPathToThisFile()

library(streamR)
library(RCurl)
library(ROAuth)
library(stringr)

library(twitteR)

load("~/twitteR_credentials")

registerTwitterOAuth(twitCred)

sampleStream(file="tweetsScratch.json", oauth=twitCred, timeout=60)

library(rjson)

fromJSON(file=str_c(PATH, '/jsonTmp.json'))

parseTweets(str_c(PATH, '/jsonTmp.json'))
