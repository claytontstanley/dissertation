library(RPostgreSQL)
library(stringr)
library(sqldf)
library(data.table)
library(reshape2)
library(assertthat)
library(tm)
library(tau)
library(XML)
PATH = getPathToThisFile()

options(sqldf.RPostgreSQL.user = 'claytonstanley',
	sqldf.RPostgreSQL.dbname = 'claytonstanley')
options("scipen"=100, "digits"=4)

sqlScratch <- function() {
	sqldf("select * from tweets where user_screen_name = 'claytonstanley1'")
	sqldf('select count(*) from tweets')
	sqldf('select count(*) from topUsers')
	sqldf('select * from topUsers')
	#data.table(sqldf('select t.user_screen_name,rank from tweets as t join topusers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name,u.rank order by rank'))
	data.table(sqldf('select * from topusers order by rank'))
	sqldf("select * from tweets where user_screen_name='jlo'")
	data.table(sqldf('select * from (select user_screen_name,count(*) from tweets group by user_screen_name) as t join topUsers as u on t.user_screen_name = u.user_screen_name order by rank'))
	sqldf('select lang,count(*) as count from tweets group by lang order by count desc')
	sqldf('select retweeted,count(*) as count from tweets group by retweeted order by count desc')
	sqldf('select truncated,count(*) as count from tweets group by truncated order by count desc')
	sqldf("select * from topUsers")[1:100,]
	data.table(sqldf("select * from twitter_users"))
	twitter_users = data.table(read.csv(str_c(PATH, "/dissertationData/tables/twitter_users.csv")))
	topUsers = data.table(read.csv(str_c(PATH, "/dissertationData/tables/topUsers.csv")))
	twitter_users[created_at=='2010-10-29 19:05:25',]
	#fread('col1,col2\n5,"4\n3"')
}

getTokenizedTbl <- function(tweetsTbl, from) {
	regex = '\\S+'
	matches = regmatches(tweetsTbl[[from]], gregexpr(regex, tweetsTbl[[from]], perl=T))
	wideTbl = data.table(id=tweetsTbl$id, matches=matches)
	extractMatches = function(m) list(chunk=m, pos=seq(from=1, by=1, length.out=length(m)))
	tokenizedTbl = wideTbl[, extractMatches(unlist(matches)), by=id]
	tokenizedTbl
}

getTokenizedTbl(data.table(id=c(1,2,3,4), text=c('kfkf idid!!','  ','ie #2', 'kdkd')), from='text')

addTokenText <- function(tweetsTbl) {
	stripDelimiters = function(text) gsub(pattern='(\t|\n|\r)', replacement=' ', x=text)
	rawTweetsFile = 'rawTweets.txt'
	tokenizedTweetsFile = 'tokenizedTweets.txt'
	with(tweetsTbl, writeLines(stripDelimiters(text), sprintf('/tmp/%s', rawTweetsFile), useBytes=T)) 
	cmd = sprintf('%s/bin/ark-tweet-nlp-0.3.2/runTagger.sh --no-confidence --just-tokenize --quiet /tmp/%s > /tmp/%s', PATH, rawTweetsFile, tokenizedTweetsFile)
	cmdOut = system(cmd)
	tokenizedTbl = data.table(read.delim(sprintf('/tmp/%s', tokenizedTweetsFile), sep='\t', quote="", header=F, stringsAsFactors=F))
	tweetsTbl[, tokenText := tokenizedTbl[[1]]]
}

getTweetsTbl <- function(sqlStr="select * from tweets limit 10000") {
	tweetsTbl = data.table(sqldf(sqlStr))
	addTokenText(tweetsTbl)
	setkey(tweetsTbl, id)
	getDiffTimeSinceFirst <- function(ts) {
		difftime(ts, ts[1], units='secs')
	}
	tweetsTbl[, dt := getDiffTimeSinceFirst(created_at), by=user_screen_name]
	tweetsTbl
}

getHashtagsTbl <- function(tweetsTbl, from='tokenText') {
	tokenizedTbl = getTokenizedTbl(tweetsTbl, from=from)
	tokenizedTbl[, chunk := tolower(chunk)]
	htOfTokenizedTbl = tokenizedTbl[grepl('^#', chunk),]
	setkey(htOfTokenizedTbl,id)
	hashtagsTbl = htOfTokenizedTbl[tweetsTbl, list(hashtag=chunk, pos=pos, created_at=created_at, dt=dt, user_id=user_id, user_screen_name=user_screen_name), nomatch=0]
	hashtagsTbl
}

getTusersTbl <- function() {
	tusersTbl = data.table(sqldf('select * from twitter_users'))
	tusersTbl[, rank := order(followers_count, decreasing=T)]
	setkey(tusersTbl, id)
	tusersTbl
}

getHashtagEntropy <- function(hashtagsTbl) {
	countTbl = hashtagsTbl[, as.data.table(table(hashtag)) , by=user_id]
	countTbl[, p := N/sum(N), by=user_id]
	HTbl = countTbl[, list(N=sum(N), NUnique=.N, H = - sum(p * log(p))), by=user_id]
	HTbl
}

compareHashtagTbls <- function() {
	hashtagTblText = getHashtagsTbl(tweetsTbl, from='text')[, as.data.table(table(hashtag)), by=user_id]
	hashtagTblTokenText = getHashtagsTbl(tweetsTbl, from='tokenText')[, as.data.table(table(hashtag)), by=user_id]
	setkeyv(hashtagTblText, c('user_id', 'hashtag'))
	setkeyv(hashtagTblTokenText, c('user_id', 'hashtag'))
	hashtagTblText
	hashtagTblText[hashtagTblTokenText]
}

curWS <- function() {
	tweetsTbl <<- getTweetsTbl()
	tweetsTbl
	hashtagsTbl <<- getHashtagsTbl(tweetsTbl, from='text')
	hashtagsTbl <<- getHashtagsTbl(tweetsTbl, from='tokenText')
	print(hashtagsTbl, topn=50)
	compareHashtagTbls()[N!=N.1]
	getHashtagEntropy(hashtagsTbl)
	tusersTbl <<- getTusersTbl()
	tusersTbl
	visHashtags(hashtagsTbl)
	print(hashtagsTbl[user_screen_name=='iamsrk'], nrow=500)
}

visHashtags <- function(hashtagsTbl) {
	setkey(hashtagsTbl, id)
	hashtagsTbl[, {dev.new(); plot(dt, main=user_screen_name)}, by=user_screen_name]
	hashtagsTbl[, {dev.new(); plot(factor(hashtag), dt)}, by=user_id]
}

curWS()

break()

sessionInfo()
tweetsTbl[, list(N=.N), by=retweeted]
tweetsTbl[, .N, by=lang]
tables()
.ls.objects()
gc()
tweetsTbl[, maxID := max(id), by=user_screen_name]
tweetsTbl[, minID := min(id), by=user_screen_name]

tweetsTbl[, containsHashtag := 0]
tweetsTbl[grepl('#', text), containsHashtag := 1]
tweetsTbl[, list(numWithHashtags=sum(containsHashtag), numOfTweets=.N), by=user_screen_name][, propWithHashtags := numWithHashtags/numOfTweets][,][,hist(propWithHashtags)]
tweetsTbl

#sqldf("select user_screen_name,id,created_at,text from tweets where user_screen_name = 'katyperry' order by id desc limit 10")
#sqldf("select user_screen_name,id,created_at,text from tweets where user_screen_name = 'BarackObama' order by id desc limit 10")
#sqldf("select rank, t.user_screen_name, count(*) from tweets as t join topUsers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name, rank order by rank desc")
#write.csv(tweetsTbl, file=str_c(PATH, "/currentTweets.csv"))
#tweetsTbl
#tweetsTbl[in_reply_to_status_id != "",]


