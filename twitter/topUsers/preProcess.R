library(RPostgreSQL)
library(stringr)
library(sqldf)
library(data.table)
library(reshape2)
library(assertthat)
library(tm)
library(tau)
library(XML)
library(Matrix)
library(utils)
library(testthat)
PATH = getPathToThisFile()


options(sqldf.RPostgreSQL.user = 'claytonstanley',
	sqldf.RPostgreSQL.dbname = 'claytonstanley')
options("scipen"=100, "digits"=4)

# Interface to retrieve chunkHash for chunk name
getHashes = function(vals, db) {
	ret = db[match(vals, names(db))]
	ret = ret[!is.na(ret)]
	#stopifnot(length(ret) > 0)
	myPrint(str_c(length(vals), "->", length(ret)))
	return(ret)
}

# And vice versa
getVals = function(hashes, db) {
	return(names(db[match(hashes, db)]))
}

makeDB <- function(vals) {
	db = seq(from = 1, by=1, length=length(vals))
	names(db) = vals
	db
}

debugP = F

myPrint <- function(str) {
	if (debugP == T) {
		print(substitute(str))
		print(str)
	}
}

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
		as.numeric(difftime(ts, ts[1], units='secs'))
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

computeActs <- function(hashtags, dt, d) {
	myPrint(hashtags)
	myPrint(dt)
	myPrint(d)
	cTime = dt[length(dt)]
	myPrint(cTime)
	dt = cTime - dt
	myPrint(dt)
	indeces = dt>0
	hashtagsSub = hashtags[indeces] 
	myPrint(hashtagsSub)
	dtSub = dt[indeces]
	myPrint(dtSub)
	list(hashtag=hashtagsSub, partialAct=dtSub^(-d), dt=rep(cTime, length(hashtagsSub)))
}

computeActsForUser <- function(hashtag, dt, d) {
	retIndeces = which(!duplicated(dt))[-1]
	if (length(retIndeces) == 0) {
		return(list(hashtag=c(), partialAct=c(), dt=c()))
	}
	partialRes = data.table(i=retIndeces)
	myPrint(partialRes)
	partialRes = partialRes[, computeActs(hashtag[1:i], dt[1:i], d=d), by=i]
	partialRes[, i := NULL]
	partialRes
}

computeActsByUser <- function(hashtagsTbl, d) {
	Rprof()
	hashtagsTbl
	partialRes = hashtagsTbl[, computeActsForUser(hashtag, dt, d), by=user_screen_name]
	partialRes
	res = partialRes[, list(N=.N, act=log(sum(partialAct))), by=list(dt, hashtag, user_screen_name)]
	Rprof(NULL)
	myPrint(summaryRprof())
	res
}

visHashtags <- function(hashtagsTbl, db) {
	hashtagsTbl[, {dev.new(); plot(dt, main=user_screen_name)}, by=user_screen_name]
	hashtagsTbl[, {dev.new(); plot(getHashes(hashtag, db), dt, main=user_screen_name)}, by=user_screen_name]
}

visCompare <- function(hashtagsTbl, modelHashtagsTbl, db) {
	expect_that(sort(unique(hashtagsTbl$user_screen_name)), is_equivalent_to(sort(unique(modelHashtagsTbl$user_screen_name))))
	plotFun <- function(hashtagsTbl, modelHashtagsTbl, userScreenName) {
		dev.new()
		with(hashtagsTbl, plot(getHashes(hashtag, db), dt, main=userScreenName))
		with(modelHashtagsTbl, lines(getHashes(hashtag, db), dt, col='red', typ='p', pch=4, cex=.1))

	}
	lapply(unique(hashtagsTbl$user_screen_name), function(usr) plotFun(hashtagsTbl[user_screen_name==usr], modelHashtagsTbl[user_screen_name==usr], usr))
}

testPriorActivations <- function() {
	testHashtagsTbl = data.table(user_screen_name=c(1,1,1,1), dt=c(0,2,3,4), hashtag=c('a', 'b', 'a', 'b'))
	expectedAct = data.table(dt=c(2,3,3,4,4), hashtag=c('a','a','b','a','b'), user_screen_name=c(1,1,1,1,1), N=c(1,1,1,2,1), act=c(log(2^(-.5)), log(3^(-.5)), log(1), log(4^(-.5)+1), log(2^(-.5))))
	act = computeActsByUser(testHashtagsTbl, d=.5)
	expect_that(act, is_equivalent_to(expectedAct))
	testHashtagsTbl = data.table(user_screen_name=c(1,1,2,2), dt=c(0,2,0,3), hashtag=c('a','b','b','b'))
	act = computeActsByUser(testHashtagsTbl, d=.5)
	expectedAct = data.table(dt=c(2,3), hashtag=c('a','b'), user_screen_name=c(1,2), N=c(1,1), act=c(log(2^(-.5)), log(3^(-.5))))
	expect_that(act, is_equivalent_to(expectedAct))
}

runTests <- function() {
	testPriorActivations()
}

curWS <- function() {
	debugP <<- F
	runTests()
	tweetsTbl <<- getTweetsTbl('select * from tweets limit 10000')
	tweetsTbl
	hashtagsTbl <<- getHashtagsTbl(tweetsTbl, from='text')
	hashtagsTbl <<- getHashtagsTbl(tweetsTbl, from='tokenText')
	lapply(tweetsTbl, class)
	print(hashtagsTbl, topn=50)
	compareHashtagTbls()[N!=N.1]
	getHashtagEntropy(hashtagsTbl)
	tusersTbl <<- getTusersTbl()
	tusersTbl
	db = makeDB(do.call(function(x) sample(x, length(x)), list(unique(hashtagsTbl$hashtag))))
	visHashtags(hashtagsTbl, db)
	act = computeActsByUser(hashtagsTbl, d=.00001)
	act
	modelHashtagsTbl = act[, list(hashtag=hashtag[act==max(act)]), by=list(dt, user_screen_name)]
	visHashtags(modelHashtagsTbl, db)
	visCompare(hashtagsTbl, modelHashtagsTbl, db)
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


