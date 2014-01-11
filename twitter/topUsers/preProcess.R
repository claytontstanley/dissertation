library(RPostgreSQL)
library(stringr)
library(sqldf)
library(data.table)
library(reshape2)
library(assertthat)
library(tm)
library(tau)
PATH = getPathToThisFile()

options(sqldf.RPostgreSQL.user = 'claytonstanley',
	sqldf.RPostgreSQL.dbname = 'claytonstanley')
options("scipen"=100, "digits"=4)

sqlScratch = function() {
	sqldf("select * from tweets where user_screen_name = 'claytonstanley1'")
	sqldf('select count(*) from tweets')
	sqldf('select count(*) from topUsers')
	sqldf('select * from topUsers')
	#data.table(sqldf('select t.user_screen_name,rank from tweets as t join topusers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name,u.rank order by rank'))
	data.table(sqldf('select * from topusers order by rank'))
	sqldf("select * from tweets where user_screen_name='jlo'")
	sqldf('select user_screen_name,count(*) from tweets group by user_screen_name')
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

getTokenizedTbl = function(tweetsTbl, from) {
	regex = '\\S+'
	matches = regmatches(tweetsTbl[[from]], gregexpr(regex, tweetsTbl[[from]], perl=T))
	wideTbl = data.table(id=tweetsTbl$id, matches=matches)
	extractMatches = function(m) list(chunk=m, pos=seq(from=1, by=1, length.out=length(m)))
	tokenizedTbl = wideTbl[, extractMatches(unlist(matches)), by=id]
	tokenizedTbl[, chunk := sub('[!.?,:”]+$', "", chunk)]
	tokenizedTbl
}

getTokenizedTbl(data.table(id=c(1,2,3,4), text=c('kfkf idid!!','  ','ie #2', 'kdkd')), from='text')



getTweetsTbl = function() {
	tweetsTbl = data.table(sqldf("select * from tweets limit 50000"))
	setkey(tweetsTbl, id)
	tweetsTbl[, text := gsub(pattern='(\t|\n|\r)', replacement=' ', x=text),]
	fileConn = file(str_c(PATH, '/res.json'))
	with(tweetsTbl, writeLines(text, fileConn))
	close(fileConn)
	foo = system(str_c('~/Downloads/ark-tweet-nlp-0.3.2/runTagger.sh --no-confidence --just-tokenize --quiet ', PATH, '/res.json  > ', PATH, '/out.json'))
	res2 = data.table(read.delim(str_c(PATH, '/out.json'), sep='\t', quote="", header=F))
	tweetsTbl[, tokenText := res2[[1]]]
	tweetsTbl
}

getHashtagsTbl = function(tweetsTbl, from='tokenText') {
	tokenizedTbl = getTokenizedTbl(tweetsTbl, from=from)
	tokenizedTbl[, chunk := tolower(chunk)]
	htOfTokenizedTbl= tokenizedTbl[grepl('^#', chunk),]
	htOfTokenizedTbl
	setkey(htOfTokenizedTbl,id)
	hashtagsTbl = htOfTokenizedTbl[tweetsTbl, list(hashtag=chunk, pos=pos, created_at=created_at, user_id=user_id, user_screen_name=user_screen_name), nomatch=0]
	hashtagsTbl
}

getTusersTbl = function() {
	tusersTbl = data.table(sqldf('select * from twitter_users'))
	tusersTbl[, rank := order(followers_count, decreasing=T)]
	setkey(tusersTbl, id)
	tusersTbl
}

curWS = function() {
	tweetsTbl <<- getTweetsTbl()
	tweetsTbl
	hashtagsTbl <<- getHashtagsTbl(tweetsTbl, from='tokenText')
	hashtagsTbl[,.N,by=user_screen_name]
	hashtagsTbl[user_screen_name=='katyperry',]
	tusersTbl <<- getTusersTbl()
	tusersTbl
	setkey(hashtagsTbl, user_id)
	hashtagsTbl[tusersTbl, list(user_screen_name=user_screen_name[1], totHashtags=.N, difHashtags=length(unique(hashtag))), nomatch=0]
	table(hashtagsTbl[user_screen_name=='barackobama',hashtag])
	hashtagsTbl
	tweetsTbl
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


