library(RPostgreSQL)
library(microbenchmark)
library(popbio)
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
library(ROCR)
PATH = getPathToThisFile()

options(sqldf.RPostgreSQL.user = 'claytonstanley',
	sqldf.RPostgreSQL.dbname = 'claytonstanley')
options("scipen"=100, "digits"=4)

# Interface to retrieve chunkHash for chunk name
getHashes <- function(vals, db) {
	ret = db[match(vals, names(db))]
	ret = ret[!is.na(ret)]
	#stopifnot(length(ret) > 0)
	myPrint(str_c(length(vals), "->", length(ret)))
	return(ret)
}

# And vice versa
getVals <- function(hashes, db) {
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

flushPrint <- function(str) {
	on.exit(flush.console())
	print(str)
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
}

getTokenizedTbl <- function(tweetsTbl, from) {
	regex = '\\S+'
	matches = regmatches(tweetsTbl[[from]], gregexpr(regex, tweetsTbl[[from]], perl=T))
	wideTbl = data.table(id=tweetsTbl$id, matches=matches)
	extractMatches = function(m) list(chunk=m, pos=seq(from=1, by=1, length.out=length(m)))
	tokenizedTbl = wideTbl[, extractMatches(unlist(matches)), by=id]
	tokenizedTbl
}

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

computeActs <- function(hashtags, dt, cTime, d) {
	myPrint(hashtags)
	myPrint(dt)
	myPrint(d)
	#cTime = dt[length(dt)]
	myPrint(cTime)
	dt = cTime - dt
	myPrint(dt)
	indeces = dt>0
	hashtagsSub = hashtags[indeces] 
	myPrint(hashtagsSub)
	cTimeSub = cTime[indeces]
	cTimeSubRep = rep(cTimeSub, times=length(d))
	dtSub = dt[indeces]
	dtSubRep = rep(dtSub, times=length(d))
	dRep = rep(d, each=length(dtSub))
	hashtagsSubRep = rep(hashtagsSub)
	myPrint(dtSubRep)
	myPrint(dRep)
	list(hashtag=hashtagsSubRep, partialAct=dtSubRep^(-dRep), dt=cTimeSubRep, d=dRep)
}

computeActsForUser <- function(hashtag, dt, ds, user_screen_name) {
	flushPrint(sprintf('computing partial activation for user %s', user_screen_name))
	retIndeces = which(!duplicated(dt))[-1]
	expect_true(length(retIndeces) > 0)
	partialRes = data.table(i=retIndeces)
	partialRes = partialRes[, list(hashtag=hashtag[1:i], dt=dt[1:i], cTime=dt[i]), by=i]
	partialRes = with(partialRes, as.data.table(computeActs(hashtag, dt, cTime, d=ds)))
	partialRes
}

computeActsByUser <- function(hashtagsTbl, ds) {
	partialRes = hashtagsTbl[, computeActsForUser(hashtag, dt, ds, user_screen_name), by=user_screen_name]
	myPrint(partialRes)
	Rprof()
	flushPrint('setting key for partial table')
	setkeyv(partialRes, c('user_screen_name','d','dt','hashtag'))
	flushPrint('computing activations across table')
	res = partialRes[, list(N=.N, act=log(sum(partialAct))), keyby=list(user_screen_name, d, dt, hashtag)]
	Rprof(NULL)
	flushPrint(summaryRprof())
	with(res, expect_that(any(is.infinite(act)), is_false()))
	res
}

visHashtags <- function(hashtagsTbl, db) {
	hashtagsTbl[, {dev.new(); plot(dt, main=user_screen_name)}, by=user_screen_name]
	hashtagsTbl[, {dev.new(); plot(getHashes(hashtag, db), dt, main=user_screen_name)}, by=user_screen_name]
}

visCompare <- function(hashtagsTbl, modelHashtagsTbl, db) {
	expect_that(sort(unique(hashtagsTbl$user_screen_name)), is_equivalent_to(sort(unique(modelHashtagsTbl$user_screen_name))))
	plotDFun <- function(hashtagsTbl, modelHashtagsTbl, userScreenName, d) {
		dev.new()
		with(hashtagsTbl, plot(getHashes(hashtag, db), dt, main=sprintf('%s, %f', userScreenName, d)))
		with(modelHashtagsTbl, lines(getHashes(hashtag, db), dt, col='red', typ='p', pch=4, cex=.1))
	}
	plotFun <- function(hashtagsTbl, modelHashtagsTbl, userScreenName) {
		modelHashtagsTbl[, plotDFun(hashtagsTbl, .SD, userScreenName, d), by=d]

	}
	lapply(unique(hashtagsTbl$user_screen_name), function(usr) plotFun(hashtagsTbl[user_screen_name==usr], modelHashtagsTbl[user_screen_name==usr], usr))
	return()
}

testPriorActivations <- function() {
	sortExpectedTbl <- function(tbl) {
		setcolorder(tbl, c('user_screen_name', 'd', 'dt', 'hashtag', 'N', 'act'))
		setkey(tbl, user_screen_name, d, dt, hashtag, N, act)
		tbl
	}
	testHashtagsTbl = data.table(user_screen_name=c(1,1,1,1), dt=c(0,2,3,4), hashtag=c('a', 'b', 'a', 'b'))
	expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3,3,4,4), hashtag=c('a','a','b','a','b'), d=c(.5,.5,.5,.5,.5),
						    user_screen_name=c(1,1,1,1,1), N=c(1,1,1,2,1), act=c(log(2^(-.5)), log(3^(-.5)), log(1), log(4^(-.5)+1), log(2^(-.5)))))
	actTbl = computeActsByUser(testHashtagsTbl, d=.5)
	expect_that(actTbl, is_equivalent_to(expectedActTbl))

	testHashtagsTbl = data.table(user_screen_name=c(1,1,2,2), dt=c(0,2,0,3), hashtag=c('a','b','b','b'))
	expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3), hashtag=c('a','b'), d=c(.5, .5), user_screen_name=c(1,2), N=c(1,1), act=c(log(2^(-.5)), log(3^(-.5)))))
	actTbl = computeActsByUser(testHashtagsTbl, d=.5)
	expect_that(actTbl, is_equivalent_to(expectedActTbl))

	testHashtagsTbl = data.table(user_screen_name=c(1,1), dt=c(0,2), hashtag=c('a','b'))
	expectedActTbl = sortExpectedTbl(data.table(dt=c(2,2,2,2), hashtag=c('a','a','a','a'), d=c(.2,.3,.4,.5),
						    user_screen_name=c(1,1,1,1), N=c(1,1,1,1), act=c(log(2^(-.2)), log(2^(-.3)), log(2^(-.4)), log(2^(-.5)))))
	actTbl = computeActsByUser(testHashtagsTbl, d=c(.2,.3,.4,.5))
	expect_that(actTbl, is_equivalent_to(expectedActTbl))

	testHashtagsTbl = data.table(user_screen_name=c(1,1,1), dt=c(0,2,3), hashtag=c('a','b','c'))
	expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3,3,2,3,3), hashtag=c('a','a','b','a','a','b'), d=c(.5,.5,.5,.4,.4,.4),
						    user_screen_name=c(1,1,1,1,1,1), N=c(1,1,1,1,1,1),
						    act=c(log(2^(-.5)), log(3^(-.5)), log(1^(-.5)),
							  log(2^(-.4)), log(3^(-.4)), log(1^(-.4)))))
	actTbl = computeActsByUser(testHashtagsTbl, d=c(.5,.4))
	expect_that(actTbl, is_equivalent_to(expectedActTbl))


	testHashtagsTbl = data.table(user_screen_name=c(1,1), dt=c(0,100000), hashtag=c('a','a'))
	expect_that(computeActsByUser(testHashtagsTbl, d=50000), throws_error())

	testHashtagsTbl = data.table(user_screen_name=c(1,2,2), dt=c(0,0,2), hashtag=c('a','a','a'))
	expectedActTbl = data.table(dt=2,hashtag='a',d=.5,user_screen_name=2,N=1,act=log(2^(-.5)))
	expect_that(computeActsByUser(testHashtagsTbl, d=.5), throws_error())
}

testGetTokenizedTbl <- function() {
	testTokenizedTbl = getTokenizedTbl(data.table(id=c(1,2,3,4), text=c('kfkf idid!!','  ','ie #2', 'kdkd')), from='text')
	expectedTokenizedTbl = data.table(id=c(1,1,3,3,4), chunk=c('kfkf','idid!!','ie','#2','kdkd'), pos=c(1,2,1,2,1))
	expect_equivalent(testTokenizedTbl, expectedTokenizedTbl)
}

runTests <- function() {
	testPriorActivations()
	testGetTokenizedTbl()
}

addMetrics <- function(hashtagsTbl, modelHashtagsTbl) {
	flushPrint('adding metrics for modelHashtagsTbl')
	modelHashtagsTbl[, topHashtag := act==max(act), by=list(d, dt, user_screen_name)]
	modelHashtagsTbl[, NTopHashtag := .N, by=list(d, dt, user_screen_name, topHashtag)]
	setkeyv(modelHashtagsTbl, c('user_screen_name', 'dt', 'hashtag'))
	setkeyv(hashtagsTbl, c('user_screen_name', 'dt', 'hashtag'))
	modelHashtagsTbl[, hashtagUsedP := F]
	modelHashtagsTbl[hashtagsTbl, hashtagUsedP := T]
	return()
}

visMetrics <- function(modelHashtagsTbl) {
	plotForUserAndD <- function(modelHashtagsTbl, user_screen_name, d) {
		pred = with(modelHashtagsTbl, prediction(act, hashtagUsedP))
		perf = performance(pred, 'ppv')
		return()
		#dev.new()
		#with(modelHashtagsTbl, logi.hist.plot(act, hashtagUsedP, boxp=T, type='hist', col='gray', las.h=0, xlabel='Activation'))
		#density_plot(pred)
		#modelHashtagsTbl[, {print(sprintf('%s, %f, %s', user_screen_name, d, hashtagUsedP)); print(summary(act))}, by=hashtagUsedP]
		
		#plot(perf, main=sprintf('%s, %f', user_screen_name, d))
	}
	modelHashtagsTbl[, .N, by=list(user_screen_name, topHashtag, hashtagUsedP, d)]
	#modelHashtagsTbl[, plotForUserAndD(.SD, user_screen_name, d), by=list(user_screen_name, d)]
}

summarizeExtremes <- function(modelHashtagsTbl) {
	frequencyTbl = modelHashtagsTbl[hashtagUsedP==T & d==min(d), .N, by=list(user_screen_name,d,hashtag)][, list(N=max(N), hashtag=hashtag[N==max(N)]), by=list(user_screen_name,d)]
	setkeyv(modelHashtagsTbl, c('dt', 'hashtagUsedP'))
	modelHashtagsTbl[, hashtagUsedRecentlyP := .SD[.SD[,list(dt=dt-.1)], roll=T]$hashtagUsedP, by=list(user_screen_name, hashtag)]
	recencyTbl = modelHashtagsTbl[hashtagUsedP==T & hashtagUsedRecentlyP==T, .N, by=list(user_screen_name,d,hashtagUsedRecentlyP,hashtagUsedP)][d==min(d)]
	list(recencyTbl=recencyTbl, frequencyTbl=frequencyTbl)
}

getModelVsPredTbl <- function(modelHashtagsTbl) {
	modelVsPredTbl = visMetrics(modelHashtagsTbl)
	setkeyv(modelVsPredTbl, c('user_screen_name', 'd', 'topHashtag', 'hashtagUsedP'))
	modelVsPredTbl[, maxNP := N==max(N), by=list(user_screen_name, topHashtag, hashtagUsedP)]
	modelVsPredTbl[maxNP==T, maxNP := abs(d-mean(d)) == min(abs(d-mean(d))), by=list(user_screen_name, topHashtag, hashtagUsedP)]
	myPrint(modelVsPredTbl[topHashtag==T & hashtagUsedP==T])
	modelVsPredTbl
}

genAggModelVsPredTbl <- function(hashtagsTbl, ds=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,5,20)) {
	genModelVsPredTbl <- function(hashtagsTbl, d, userScreenName) {
		flushPrint(sprintf('generating model predictions for user %s', userScreenName))
		modelHashtagsTbl = computeActsByUser(hashtagsTbl, d=d)
		addMetrics(hashtagsTbl, modelHashtagsTbl)
		modelVsPredTbl = getModelVsPredTbl(modelHashtagsTbl)	
		modelVsPredTbl
	}
	singleHashtagUsers = hashtagsTbl[, list(uniqueDTs=length(unique(dt)) <= 1), by=user_screen_name][uniqueDTs==T]$user_screen_name
	flushPrint(sprintf('not running users (%s) since they all have less than two dt hashtag observations', paste(singleHashtagUsers, sep=',', collapse=NULL)))
	users = data.table(cur_user_screen_name=Filter(function(v) !(v %in% singleHashtagUsers), unique(hashtagsTbl$user_screen_name)))
	users
	setkey(hashtagsTbl, user_screen_name)
	res = users[, genModelVsPredTbl(hashtagsTbl[cur_user_screen_name], ds, cur_user_screen_name), by=cur_user_screen_name]
	res[, cur_user_screen_name := NULL]
	res
}

curWS <- function() {
	debugP = F
	runTests()
	tweetsTbl = getTweetsTbl('select * from tweets limit 100000')
	tweetsTbl
	#hashtagsTbl = getHashtagsTbl(tweetsTbl, from='text')
	hashtagsTbl = getHashtagsTbl(tweetsTbl, from='tokenText')
	compareHashtagTbls()[N!=N.1]
	getHashtagEntropy(hashtagsTbl)
	tusersTbl = getTusersTbl()
	tusersTbl
	db = makeDB(do.call(function(x) sample(x, length(x)), list(unique(hashtagsTbl$hashtag))))
	visHashtags(hashtagsTbl, db)
	modelHashtagsTbl = computeActsByUser(hashtagsTbl[user_screen_name=='icarly'], d=c(.5,.7))
	addMetrics(hashtagsTbl, modelHashtagsTbl)
	visHashtags(modelHashtagsTbl[topHashtag==T,], db)
	unique(hashtagsTbl$user_screen_name)
	visCompare(hashtagsTbl[user_screen_name=='icarly'], modelHashtagsTbl[topHashtag==T & user_screen_name=='icarly',], db)
	summarizeExtremes(modelHashtagsTbl)
	modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl[user_screen_name=='icarly' | user_screen_name=='icarly'])
	modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl)
	modelVsPredTblBig = modelVsPredTbl 
	modelVsPredTbl
	modelVsPredTblBig[d < 10 & maxNP==T & topHashtag & hashtagUsedP][, plot(N, d)]
	modelVsPredTblBig[topHashtag & hashtagUsedP]
	tables()
}

#curWS()

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
