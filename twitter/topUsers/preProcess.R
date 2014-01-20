library(RPostgreSQL)
library(ggplot2)
library(memoise)
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
	setkey(hashtagsTbl, user_screen_name, dt, hashtag)
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
	setkeyv(partialRes, c('user_screen_name','dt','hashtag','d'))
	flushPrint('computing activations across table')
	res = partialRes[, list(N=.N, act=log(sum(partialAct))), keyby=list(user_screen_name, dt, hashtag, d)]
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
		cols = c('user_screen_name', 'dt', 'hashtag', 'd', 'N', 'act')
		setcolorder(tbl, cols) 
		setkeyv(tbl, cols) 
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

testOnlyFirstT <- function() {
	expect_equal(onlyFirstT(c(F,F,T,T,F,F)), c(F,F,T,F,F,F))
	expect_error(onlyFirstT(c(F,F)))
	expect_equal(onlyFirstT(c(T,T)), c(T,F))
	expect_equal(onlyFirstT(c(F,F,T,F,F)), c(F,F,T,F,F))
	expect_error(onlyFirstT(c()))
}

runTests <- function() {
	testPriorActivations()
	testGetTokenizedTbl()
	testOnlyFirstT()
}

addMetrics <- function(hashtagsTbl, modelHashtagsTbl) {
	tagCountTbl = hashtagsTbl[, list(tagCountN=.N), by=list(user_screen_name, dt)]
	modelHashtagsTbl[tagCountTbl, tagCount := tagCountN]
	modelHashtagsTbl[tagCountTbl[, list(tagCountUserN=sum(tagCountN)), keyby=user_screen_name], tagCountUser := tagCountUserN]
	isTopHashtag <- function(act, tagCount) {
		expect_true(all(tagCount[1] == tagCount))
		tagCount = tagCount[1]
		s = sort(act, index.return=T, decreasing=T)
		numPossible = length(act)
		res = rep(FALSE, numPossible) 
		res[s$i[1:min(numPossible, tagCount)]] = TRUE
		res
	}
	flushPrint('adding metrics for modelHashtagsTbl')
	modelHashtagsTbl[, topHashtagPost := isTopHashtag(act, tagCount), by=list(user_screen_name, dt, d)]
	modelHashtagsTbl[, topHashtagAct := isTopHashtag(act, tagCountUser), by=list(user_screen_name, d)]
	expect_that(key(modelHashtagsTbl), equals(c('user_screen_name', 'dt', 'hashtag', 'd')))
	expect_that(key(hashtagsTbl), equals(c('user_screen_name', 'dt', 'hashtag')))
	modelHashtagsTbl[, hashtagUsedP := F]
	modelHashtagsTbl[hashtagsTbl, hashtagUsedP := T]
	#wideTbl = hashtagsTbl[, list(usedHashtags=list(hashtag)), by=list(user_screen_name, dt)]
	#modelHashtagsTbl[wideTbl, usedHashtags := usedHashtags]
	return()
}

summarizeExtremes <- function(hashtagsTbl) {
	tagCountTbl = hashtagsTbl[, list(tagCount=.N), by=list(user_screen_name, dt)]
	countTbl = hashtagsTbl[, .N, keyby=list(user_screen_name, hashtag)]
	countTbl[, rank := {i = sort(N, decreasing=T, index.return=T)$i; r=1:length(i); r[i] = 1:length(i); r}, by=user_screen_name]
	setkey(countTbl, user_screen_name, rank)
	getTopNHashtags <- function(topN, userScreenName) {
		ret = countTbl[J(userScreenName,1:topN)]$hashtag
		ret
	}
	mGetTopNHashtags = memoise(getTopNHashtags)
	frequencyTbl = hashtagsTbl[, list(hashtagChosenP = hashtag %in% mGetTopNHashtags(length(hashtag), user_screen_name)), by=list(user_screen_name, dt)][, list(NFrequency=sum(hashtagChosenP)), keyby=user_screen_name]
	flatHashtagsTbl = hashtagsTbl[, list(hashtag=list(hashtag)), by=list(user_screen_name, dt)]
	joinTbl = hashtagsTbl[, list(userScreenName=user_screen_name, dt1=dt, hashtag=hashtag)]
	setkey(joinTbl, userScreenName, dt1)
	recencyTbl = hashtagsTbl[, list(hashtags=list(hashtag), prevHashtags=list(unique(rev(joinTbl[J(user_screen_name),][dt1 < dt,]$hashtag)))), by=list(user_screen_name, dt)]
	recencyTbl = recencyTbl[, list(hashtag=unlist(hashtags), prevHashtag=prevHashtags, hashtagChosenP = unlist(hashtags) %in% unlist(prevHashtags)[1:length(unlist(hashtags))]), by=list(user_screen_name, dt)]
	recencyTbl = recencyTbl[, list(NRecency=sum(hashtagChosenP)), by=list(user_screen_name)]
	res = frequencyTbl[recencyTbl]
	res
}

onlyFirstT <- function(bool) {
	expect_true(any(bool == T))
	ret = rep(F, length(bool))
	ret[which(bool)[1]] = T
	ret
}

getModelVsPredTbl <- function(modelHashtagsTbl) {
	tempTbl = modelHashtagsTbl[, list(NCell=.N, DVName='topHashtagPost'), by=list(user_screen_name, topHashtagPost, hashtagUsedP, d)]
	setnames(tempTbl, 'topHashtagPost', 'topHashtag')
	modelVsPredTbl = tempTbl
	tempTbl = modelHashtagsTbl[, list(NCell=.N, DVName='topHashtagAct'), by=list(user_screen_name, topHashtagAct, hashtagUsedP, d)]
	setnames(tempTbl, 'topHashtagAct', 'topHashtag')
	modelVsPredTbl = rbind(modelVsPredTbl, tempTbl, use.names=T)
	modelVsPredTbl[, maxNP := NCell==max(NCell), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	modelVsPredTbl[maxNP==T, maxNP := onlyFirstT(abs(d-mean(d)) == min(abs(d-mean(d)))), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	modelVsPredTbl
}

compareModelVsExtreme <-function(modelHashtagsTbl, extremesTbl) {
	print(modelHashtagsTbl[d==20][topHashtag==T], topn=20)
	tables()
	setkey(extremesTbl, user_screen_name, dt, hashtag)
	extremesTbl
	modelHashtagsTbl
	?data.table
	fooTbl = extremesTbl[modelHashtagsTbl[d==20], allow.cartesian=T, nomatch=0][, list(tagCount, user_screen_name, dt, hashtag, hashtagChosenP, topHashtag, lapply(prevHashtags, function(x) x[1:4]))]
	fooTbl
	extremesTbl
	print(fooTbl, topn=500)
	fooTbl[, sum(topHashtag)]
	fooTbl
}

genAggModelVsPredTbl <- function(hashtagsTbl, ds=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2,5,10,20), outFile='/tmp/modelVsPred.csv') {
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
	res = users[, genModelVsPredTbl(hashtagsTbl[cur_user_screen_name], ds, cur_user_screen_name), by=cur_user_screen_name]
	res[, cur_user_screen_name := NULL]
	setkey(res, user_screen_name, DVName, d)
	write.csv(res, row.names=F, file=outFile)
	res
}

visModelVsPredTbl <- function(modelVsPredTbl, hashtagsTbl) {
	modelVsPredTbl[hashtagUsedP==T, totN := length(hashtagsTbl[user_screen_name]$user_screen_name), by=user_screen_name]
	dev.new()
	modelVsPredTbl[d < 10 & maxNP==T & topHashtag & hashtagUsedP][, plot(NCell, d)]
	dev.new()
	print(ggplot(modelVsPredTbl[topHashtag & hashtagUsedP], aes(log(d),NCell/totN, colour=as.factor(user_screen_name)), group = as.factor(user_screen_name)) + geom_line())
	dev.new()
	print(ggplot(modelVsPredTbl[topHashtag & hashtagUsedP], aes(log(d),NCell, colour=as.factor(user_screen_name)), group = as.factor(user_screen_name)) + geom_line())
}

modelVsPredOutFile <- function(name) {
	sprintf('%s/dissertationData/modelVsPred/%s.csv', PATH, name)
}

curWS <- function() {
	debugP = F
	runTests()
	tweetsTbl = getTweetsTbl("select * from tweets limit 100000")
	tweetsTbl = getTweetsTbl("select * from tweets where user_screen_name='eddieizzard'")
	tweetsTbl
	#hashtagsTbl = getHashtagsTbl(tweetsTbl, from='text')
	hashtagsTbl = getHashtagsTbl(tweetsTbl, from='tokenText')
	compareHashtagTbls()[N!=N.1]
	getHashtagEntropy(hashtagsTbl)
	tusersTbl = getTusersTbl()
	tusersTbl
	db = makeDB(do.call(function(x) sample(x, length(x)), list(unique(hashtagsTbl$hashtag))))
	visHashtags(hashtagsTbl[user_screen_name=='chelseafc'], db)
	modelHashtagsTbl = computeActsByUser(hashtagsTbl[user_screen_name=='joelmchale'], d=c(10))
	addMetrics(hashtagsTbl, modelHashtagsTbl)
	setkey(hashtagsTbl, user_screen_name, dt, hashtag)	
	setkey(modelHashtagsTbl, user_screen_name, dt, d, act)
	setkey(modelHashtagsTbl, user_screen_name, dt)
	modelHashtagsTbl[topHashtag==T]
	modelHashtagsTbl
	print(modelHashtagsTbl, topn=40)
	print(hashtagsTbl[modelHashtagsTbl[topHashtag==T]], topn=40)
	tables()
	visHashtags(modelHashtagsTbl[topHashtag==T,], db)
	unique(hashtagsTbl$user_screen_name)
	visCompare(hashtagsTbl[user_screen_name=='joelmchale'], modelHashtagsTbl[topHashtag==T & user_screen_name=='joelmchale',], db)
	hashtagsTbl[user_screen_name=='joelmchale'][, .N, by=hashtag][, sum(N)]

	extremesTbl = summarizeExtremes(hashtagsTbl)
	extremesTbl
	extremesTbl
	Q

	summarizeExtremes(hashtagsTbl[user_screen_name=='eddieizzard'])
	modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl[user_screen_name %in% unique(user_screen_name)[1:25]])
	modelVsPredTblSmall = genAggModelVsPredTbl(hashtagsTbl[user_screen_name == 'ap'], outFile = modelVsPredOutFile('testing1'))
	modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl[user_screen_name == 'eddieizzard'], ds=40)
	modelVsPredTblBig = modelVsPredTbl
	modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl)
	visModelVsPredTbl(modelVsPredTblSmall[DVName=='topHashtag'], hashtagsTbl)
	visModelVsPredTbl(modelVsPredTbl, hashtagsTbl)
	setkey(modelVsPredTbl, user_screen_name)
	modelVsPredTbl[topHashtag ==T & hashtagUsedP]
	setkey(extremesTbl, user_screen_name)
	setkey(modelVsPredTbl, user_screen_name, d)
	print(modelVsPredTblBig[topHashtag & hashtagUsedP])
	print(modelVsPredTblBig[topHashtag & hashtagUsedP][extremesTbl, allow.cartesian=T][d==0 | d==20], topn=2000)
	foo1 = modelVsPredTbl[topHashtag ==T, sum(N), by=list(user_screen_name, d)][, list(mean(V1), sd(V1)), by=list(user_screen_name)]
	foo2 = hashtagsTbl[, .N, keyby=user_screen_name]
	tables()
	foo2[foo1][, N-V1]
	modelVsPredTbl[maxNP==T, sum(maxNP), by=list(user_screen_name, d)]
	modelVsPredTbl[user_screen_name=='veja']
	tables()
	modelVsPredTblSmall[topHashtag==T & hashtagUsedP]

	joinTbl = modelVsPredTblBig[topHashtag & hashtagUsedP][extremesTbl, allow.cartesian=T][maxNP==T]
	joinTbl[, list(user_screen_name, best=N/totN, r=NRecency/totN, f=NFrequency/totN)][, list(user_screen_name, best-r, best-f)][, lapply(list(V2, V3), mean),]
	joinTbl[, list(user_screen_name, best=N/totN, r=NRecency/totN, f=NFrequency/totN)][, list(user_screen_name, best-r, best-f)][, hist(V2)]
}

#curWS()

