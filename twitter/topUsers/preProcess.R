library(RPostgreSQL)
library(lavaan)
library(Rmisc)
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
FILE = getNameOfThisFile()

options(sqldf.RPostgreSQL.user = 'claytonstanley',
	sqldf.RPostgreSQL.dbname = 'claytonstanley')
options("scipen"=100, "digits"=4)

# Interface to retrieve chunkHash for chunk name
getHashes <- function(vals, db) {
	ret = db[match(vals, names(db))]
	ret = ret[!is.na(ret)]
	#stopifnot(length(ret) > 0)
	debugPrint(str_c(length(vals), "->", length(ret)))
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

logLevel = 1
debugLogLevel = 2

setLogLevel <- function(lvl) logLevel <<- lvl
getLogLevel <-function() logLevel

debugPrint <- function(str) {
	myLog(substitute(str), debugLogLevel)
	myLog(str, debugLogLevel)
}

myLog <- function(str, forLogLevel=1) {
	on.exit(flush.console())
	if (logLevel >= forLogLevel) print(str)
}

# ref: http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
html2txt <- function(str) {
	xpathApply(htmlParse(str, asText=TRUE),
		   "//body//text()", 
		   xmlValue)[[1]] 
}

withProf <- function(thunk) {
	.tempRprofOut = tempfile(pattern='RProf-', tmpdir='/tmp', fileext='.out')
	Rprof(.tempRprofOut)
	on.exit({
		Rprof(NULL)
		myLog(summaryRprof(.tempRprofOut))
	})
	thunk
}

myReadCSV <- function(file) {
	data.table(read.csv(file, stringsAsFactors=F))
}

# Computing CIs around a variance statistic

CIVar <- function(vals) {
	sSize = length(vals)	
	sVar = sd(vals)^2
	ci = .95
	df = sSize - 1
	halfalpha = (1 - ci) / 2
	ub = sVar * df / qchisq(halfalpha,df)
	ua = sVar * df / qchisq(1-halfalpha,df)
	c(upper=ub, mean=sVar, lower=ua)
}

CIVar2 <- function(vals) {
	df = data.frame(x=vals)
	model = 'x ~~ x'
	fit = sem(model, data=df, likelihood = "wishart" )
	res = parameterEstimates(fit)
	with(res, c(upper=ci.upper, mean=est, lower=ci.lower))
}

sqlScratch <- function() {
	install.packages('~/src/datatable/pkg', repos=NULL, type='source')
	install.packages('~/Desktop/data.table', repos=NULL, type='source')
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
	data.table(sqldf("select retweeted, count(*) from tweets group by retweeted"))[, min(count) / (max(count) + min(count))]
	sqldf("select * from tweets where 1=1 and user_screen_name='katyperry' limit 10")
	data.table(sqldf("select * from twitter_users"))
	twitter_users = myReadCSV(str_c(PATH, "/dissertationData/tables/twitter_users.csv"))
	topUsers = myReadCSV(str_c(PATH, "/dissertationData/tables/topUsers.csv"))
	twitter_users[created_at=='2010-10-29 19:05:25',]
}

getTokenizedTbl <- function(tweetsTbl, from, regex) {
	matches = regmatches(tweetsTbl[[from]], gregexpr(regex, tweetsTbl[[from]], perl=T))
	wideTbl = data.table(id=tweetsTbl$id, matches=matches)
	extractMatches = function(m) list(chunk=m, pos=seq(from=1, by=1, length.out=length(m)))
	tokenizedTbl = wideTbl[, extractMatches(unlist(matches)), by=id]
	tokenizedTbl[, chunk := tolower(chunk)]
	setkey(tokenizedTbl, id)
	tokenizedTbl
}

addTokenText <- function(tweetsTbl, from) {
	stripDelimiters = function(text) gsub(pattern='(\t|\n|\r)', replacement=' ', x=text)
	rawTweetsFile = tempfile(pattern='rawTweets-', tmpdir='/tmp', fileext='.txt')
	tokenizedTweetsFile = tempfile(pattern='tokenizedTweets-', tmpdir='/tmp', fileext='.txt')
	stderFile = tempfile(pattern='stderr-', tmpdir='/tmp', fileext='.txt')
	writeLines(stripDelimiters(tweetsTbl[[from]]), rawTweetsFile, useBytes=T)
	cmd = sprintf('%s/bin/ark-tweet-nlp-0.3.2/runTagger.sh', PATH)
	args = sprintf('--no-confidence --just-tokenize --quiet %s > %s 2>%s', rawTweetsFile, tokenizedTweetsFile, stderFile)
	myLog(sprintf('running tagger with in/out temp files: %s, %s', rawTweetsFile, tokenizedTweetsFile))
	cmdOut = system2(cmd, args=args)
	myLog(paste(readLines(stderFile), sep='\n'))
	tokenTextTbl = data.table(read.delim(tokenizedTweetsFile, sep='\t', quote="", header=F, stringsAsFactors=F))
	tweetsTbl[, tokenText := tokenTextTbl[[1]]]
}

getPostCntTbl <- function() {
	data.table(sqldf("select Post_Type_Id, N, Owner_User_Id, Display_Name, Reputation from
			 (select owner_user_id, Post_Type_Id, count(*) as N from Posts 
			  where Post_Type_Id = 1
			  group by Owner_User_Id,Post_Type_Id) as foo2
			 join Users on Users.Id = foo2.Owner_User_Id
			 order by Reputation desc"))
}

getDiffTimeSinceFirst <- function(ts) {
	as.numeric(difftime(ts, ts[1], units='secs'))
}

getTweetsTbl <- function(sqlStr="select * from tweets limit 10000") {
	tweetsTbl = data.table(sqldf(sqlStr))
	addTokenText(tweetsTbl, from='text')
	setkey(tweetsTbl, id)
	tweetsTbl[, dt := getDiffTimeSinceFirst(created_at), by=user_screen_name]
	tweetsTbl
}

getPostsTbl <- function(sqlStr) {
	postsTbl = data.table(sqldf(sqlStr))
	setkey(postsTbl, id)
	stopifnot(!duplicated(postsTbl$id))
	# TODO: this is not vectorized b/c html2txt isn't vectorized
	postsTbl[, tagsNoHtml := html2txt(tags), by=id]
	postsTbl[, dt := getDiffTimeSinceFirst(creation_date), by=owner_user_id]
	postsTbl
}

getHashtagsTbl <- function(tweetsTbl, from) {
	tokenizedTbl = getTokenizedTbl(tweetsTbl, from=from, regex='\\S+')
	htOfTokenizedTbl = tokenizedTbl[grepl('^#', chunk),]
	stopifnot(c('id') == key(htOfTokenizedTbl))
	hashtagsTbl = htOfTokenizedTbl[tweetsTbl, list(hashtag=chunk, pos=pos, created_at=created_at, dt=dt, user_id=user_id, user_screen_name=user_screen_name), nomatch=0]
	setkey(hashtagsTbl, user_screen_name, dt, hashtag)
	hashtagsTbl
}

getTagsTbl <- function(postsTbl) {
	tokenizedTbl = getTokenizedTbl(postsTbl, from='tagsNoHtml', regex='(?<=<)[^>]+(?=>)')
	tagsTbl = tokenizedTbl[postsTbl, list(hashtag=chunk, pos=pos, created_at=creation_date, dt=dt, user_id=owner_user_id, user_screen_name=as.character(owner_user_id)), nomatch=0]
	setkey(tagsTbl, user_screen_name, dt, hashtag)
	tagsTbl
}

getTusersTbl <- function() {
	tusersTbl = data.table(sqldf('select * from twitter_users'))
	tusersTbl[, rank := order(followers_count, decreasing=T)]
	setkey(tusersTbl, id)
	tusersTbl
}

getSOusersTbl <- function() {
	sousersTbl = data.table(sqldf('select * from users limit 1000'))
	sousersTbl
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
	debugPrint(hashtags)
	debugPrint(dt)
	debugPrint(d)
	#cTime = dt[length(dt)]
	debugPrint(cTime)
	dt = cTime - dt
	debugPrint(dt)
	indeces = dt>0
	hashtagsSub = hashtags[indeces] 
	debugPrint(hashtagsSub)
	cTimeSub = cTime[indeces]
	cTimeSubRep = rep(cTimeSub, times=length(d))
	dtSub = dt[indeces]
	dtSubRep = rep(dtSub, times=length(d))
	dRep = rep(d, each=length(dtSub))
	hashtagsSubRep = rep(hashtagsSub)
	debugPrint(dtSubRep)
	debugPrint(dRep)
	list(hashtag=hashtagsSubRep, partialAct=dtSubRep^(-dRep), dt=cTimeSubRep, d=dRep)
}

computeActsForUser <- function(hashtag, dt, ds, user_screen_name) {
	myLog(sprintf('computing partial activation for user %s', user_screen_name))
	retIndeces = which(!duplicated(dt))[-1]
	stopifnot(length(retIndeces) > 0)
	partialRes = data.table(i=retIndeces)
	partialRes = partialRes[, list(hashtag=hashtag[1:i], dt=dt[1:i], cTime=dt[i]), by=i]
	partialRes = with(partialRes, as.data.table(computeActs(hashtag, dt, cTime, d=ds)))
	partialRes
}

computeActsByUser <- function(hashtagsTbl, ds) {
	partialRes = hashtagsTbl[, computeActsForUser(hashtag, dt, ds, user_screen_name), by=user_screen_name]
	debugPrint(partialRes)
	myLog('setting key for partial table')
	setkeyv(partialRes, c('user_screen_name','dt','hashtag','d'))
	myLog('computing activations across table')
	res = partialRes[, list(N=.N, act=log(sum(partialAct))), keyby=list(user_screen_name, dt, hashtag, d)]
	with(res, stopifnot(!is.infinite(act)))
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

addMetrics <- function(hashtagsTbl, modelHashtagsTbl) {
	tagCountTbl = hashtagsTbl[, list(tagCountN=.N), by=list(user_screen_name, dt)]
	modelHashtagsTbl[tagCountTbl, tagCount := tagCountN]
	modelHashtagsTbl[tagCountTbl[, list(tagCountUserN=sum(tagCountN)), keyby=user_screen_name], tagCountUser := tagCountUserN]
	myLog('adding metrics for modelHashtagsTbl')
	modelHashtagsTbl[order(act, decreasing=T), topHashtagPost := 1:length(act) <= tagCount[1], by=list(user_screen_name, dt, d)]
	modelHashtagsTbl[order(act, decreasing=T), topHashtagAct := 1:length(act) <= tagCountUser[1], by=list(user_screen_name, d)]
	stopifnot(key(modelHashtagsTbl) == (c('user_screen_name', 'dt', 'hashtag', 'd')))
	stopifnot(key(hashtagsTbl) == (c('user_screen_name', 'dt', 'hashtag')))
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
	stopifnot(any(bool == T))
	ret = rep(F, length(bool))
	ret[which(bool)[1]] = T
	ret
}

modelVsPredForDV <- function(modelHashtagsTbl, DVName) {
	tempTbl = modelHashtagsTbl[, list(NCell=.N, DVName=DVName), by=c('user_screen_name', DVName, 'hashtagUsedP', 'd')]
	setnames(tempTbl, DVName, 'topHashtag')
	tempTbl
}

getModelVsPredTbl <- function(modelHashtagsTbl, hashtagsTbl) {
	modelVsPredTbl = rbind(modelVsPredForDV(modelHashtagsTbl, 'topHashtagPost'), 
			       modelVsPredForDV(modelHashtagsTbl, 'topHashtagAct'))
	modelVsPredTbl[, maxNP := NCell==max(NCell), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	# TODO: Doesn't using the maxNP closest to the center of all of the maxNP's create an artifact for low N when all ds are MaxNP's?
	modelVsPredTbl[maxNP==T, maxNP := onlyFirstT(abs(d-mean(d)) == min(abs(d-mean(d)))), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	modelVsPredTbl[, totN := NA_integer_] # Making sure that the totN column is added, even if model never generates an activation value for a hashtag that is used
	modelVsPredTbl[hashtagUsedP==T, totN := length(hashtagsTbl[user_screen_name]$user_screen_name), by=user_screen_name]
	modelVsPredTbl
}

compareModelVsExtreme <-function(modelHashtagsTbl, extremesTbl) {
	modelHashtagsTbl[d==20][topHashtag==T]
	tables()
	setkey(extremesTbl, user_screen_name, dt, hashtag)
	extremesTbl
	modelHashtagsTbl
	?data.table
	fooTbl = extremesTbl[modelHashtagsTbl[d==20], allow.cartesian=T, nomatch=0][, list(tagCount, user_screen_name, dt, hashtag, hashtagChosenP, topHashtag, lapply(prevHashtags, function(x) x[1:4]))]
	fooTbl
	extremesTbl
	fooTbl
	fooTbl[, sum(topHashtag)]
	fooTbl
}

genAggModelVsPredTbl <- function(hashtagsTbl, ds=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2,5,10,20), outFile='/tmp/modelVsPred.csv') {
	genModelVsPredTbl <- function(hashtagsTbl, d, userScreenName) {
		myLog(sprintf('generating model predictions for user %s', userScreenName))
		modelHashtagsTbl = computeActsByUser(hashtagsTbl, d=d)
		addMetrics(hashtagsTbl, modelHashtagsTbl)
		modelVsPredTbl = getModelVsPredTbl(modelHashtagsTbl, hashtagsTbl)	
		modelVsPredTbl
	}
	singleHashtagUsers = hashtagsTbl[, list(uniqueDTs=length(unique(dt)) <= 1), by=user_screen_name][uniqueDTs==T]$user_screen_name
	myLog(sprintf('not running users (%s) since they all have less than two dt hashtag observations', paste0(singleHashtagUsers, collapse=',')))
	users = data.table(cur_user_screen_name=Filter(function(v) !(v %in% singleHashtagUsers), unique(hashtagsTbl$user_screen_name)))
	res = users[, genModelVsPredTbl(hashtagsTbl[cur_user_screen_name], ds, cur_user_screen_name), by=cur_user_screen_name]
	res[, cur_user_screen_name := NULL]
	setkey(res, user_screen_name, DVName, d)
	write.csv(res, row.names=F, file=outFile)
	res
}

visModelVsPredTbl <- function(modelVsPredTbl) {
	myLog(ggplot(modelVsPredTbl[maxNP==T & topHashtag & hashtagUsedP], aes(totN, d)) +
		   geom_point() +
		   xlab('total number of hashtags for user'))
	modelVsPredTbl[topHashtag & hashtagUsedP, meanPC := mean(NCell/totN), by=user_screen_name]
	modelVsPredTbl[topHashtag & hashtagUsedP, relPC := NCell/totN - mean(NCell/totN), by=user_screen_name]
	modelVsPredTbl[topHashtag & hashtagUsedP, meanRelPC := mean(relPC), by=d]
	dev.new()
	myLog(ggplot(modelVsPredTbl[topHashtag & hashtagUsedP], aes(log(d),relPC)) +
		   geom_point() +
		   geom_line(aes(log(d), meanRelPC, group=user_screen_name[1])) +
		   xlab('log(d)') +
		   ylab('change in proportion correct from mean for user'))
	dev.new()
	myLog(ggplot(modelVsPredTbl[topHashtag & hashtagUsedP & user_screen_name %in% sample(unique(user_screen_name), size=min(20, length(unique(user_screen_name))))],
			  aes(log(d),NCell/totN, group=as.factor(user_screen_name))) + geom_line() +
		   ylab('proportion correct'))
	dev.new()
	myLog(ggplot(modelVsPredTbl[topHashtag & hashtagUsedP & maxNP], aes(d)) +
		   geom_histogram(aes(y = ..density..)) +
		   geom_density())
}

tableModelVsPredTbl <- function(modelVsPredTbl) {
	# Summary table of optimal d values and sample variance
	modelVsPredTbl[topHashtag & hashtagUsedP & maxNP][, list(mean=mean(d), median=median(d), NCell=mean(NCell), acc=mean(NCell/totN), sd=sd(d),
								 meanCI=CI(d), sdCI=sqrt(CIVar(d)), sdCI1=sqrt(CIVar2(d))), by=list(datasetName, DVName)]
}

modelVsPredOutFile <- function(name) {
	sprintf('%s/dissertationData/modelVsPred/%s.csv', PATH, name)
}

runPriorSO <- function(query, ...) {
	withProf({
		postsTbl = getPostsTbl(query)
		tagsTbl = getTagsTbl(postsTbl)
		modelVsPredTbl = genAggModelVsPredTbl(tagsTbl, ...)
		modelVsPredTbl
	})
}

runPrior <- function(query, ...) {
	withProf({
		tweetsTbl = getTweetsTbl(query)
		hashtagsTbl = getHashtagsTbl(tweetsTbl, from='tokenText')
		modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl, ...)
		modelVsPredTbl
	})
}

getQueryGT <- function(val, filters='1=1') {
	sprintf('select * from tweets as t where %s and user_screen_name in (select user_screen_name from twitter_users where followers_count > %d order by followers_count asc limit 100)', filters, val)
}

getQueryGTSO <- function(val) {
	sprintf('select id, owner_user_id, creation_date, title, tags from posts where post_type_id = 1 and owner_user_id in (select id from users where reputation > %d order by reputation asc limit 500)', val)
}

combineFilters <- function(f1, f2) {
	paste(f1, f2, sep=' and ')
}

getQueryGTNoRetweets <- function(val, filters='1=1') {
	getQueryGT(val, combineFilters("retweeted = 'False'", filters))
}

run1M <- function() runPrior(getQueryGT(1000000), outFile=modelVsPredOutFile('gt1M'))
run1Mr2 <- function() runPrior(getQueryGTNoRetweets(1000000), outFile=modelVsPredOutFile('gt1Mr2'))
run100k <- function() runPrior(getQueryGT(100000), outFile=modelVsPredOutFile('gt100k'))
run100kr2 <- function() runPrior(getQueryGTNoRetweets(100000), outFile=modelVsPredOutFile('gt100kr2'))
# so_pr user causes segfault w/ data.table 1.8.10
run10k <- function() runPrior(getQueryGT(10000, "user_screen_name != 'so_pr'"), outFile=modelVsPredOutFile('gt10k'))
run10kr2 <- function() runPrior(getQueryGTNoRetweets(10000, "user_screen_name != 'so_pr'"), outFile=modelVsPredOutFile('gt10kr2'))
run1k <- function() runPrior(getQueryGT(1000), outFile=modelVsPredOutFile('gt1k'))
run1kr2 <- function() runPrior(getQueryGTNoRetweets(1000), outFile=modelVsPredOutFile('gt1kr2'))
# tweet 12466832063  has a corrupt utf-8 encoded string
run10M <- function() runPrior(getQueryGT(10000000, 't.id != 12466832063'), outFile=modelVsPredOutFile('gt10M'))
run10Mr2 <- function() runPrior(getQueryGTNoRetweets(10000000, 't.id != 12466832063'), outFile=modelVsPredOutFile('gt10Mr2'))

makeSORun <- function(val, outFileName) {
	runFun = function() runPriorSO(getQueryGTSO(val), outFile=modelVsPredOutFile(outFileName))
	runFun
}

runSO100k <- function() makeSORun(100000, 'SOgt100k')()
runSO50k <- function() makeSORun(50000, 'SOgt50k')()
runSO10k <- function() makeSORun(10000, 'SOgt10k')()
runSO5k <- function() makeSORun(5000, 'SOgt5k')()
runSO1k <- function() makeSORun(1000, 'SOgt1k')()
runSO500 <- function() makeSORun(500, 'SOgt500')()

buildTables <- function(outFileNames) {
	buildTable <- function(outFileName) {
		tbl = myReadCSV(modelVsPredOutFile(outFileName))
		tbl[, datasetName := outFileName]
		tbl
	}
	rbindlist(lapply(outFileNames, buildTable))
}

curWS <- function() {
	test_dir(sprintf("%s/%s", PATH, 'tests'))
	tweetsTbl = getTweetsTbl("select * from tweets limit 100000")
	tweetsTbl = getTweetsTbl("select * from tweets where user_screen_name='eddieizzard'")
	tweetsTbl
	runPrior("select * from tweets where id=12466832063")
	runPrior("select * from tweets where user_id=50393960")
	runSO1k()
	# Checking that tweets for twitter users from each followers_count scale are being collected properly
	usersWithTweetsTbl = data.table(sqldf("select distinct on (user_id) t.user_screen_name,u.followers_count from tweets as t join twitter_users as u on t.user_screen_name = u.user_screen_name"))
	usersWithTweetsTbl[order(followers_count), plot(log10(followers_count))]
	usersWithTweetsTbl[order(followers_count),][followers_count > 10000000]
	tweetsTbl
	#hashtagsTbl = getHashtagsTbl(tweetsTbl, from='text')
	hashtagsTbl = getHashtagsTbl(tweetsTbl, from='tokenText')
	hashtagsTbl
	compareHashtagTbls()[N!=N.1]
	getHashtagEntropy(hashtagsTbl)
	tusersTbl = getTusersTbl()
	tusersTbl
	tusersTbl[order(rank, decreasing=T)][20000:30000][, plot(1:length(followers_count), followers_count)]
	runPrior("select * from tweets where user_screen_name = 'katyperry'")
	runPrior(getQueryGT(10000000))
	db = makeDB(do.call(function(x) sample(x, length(x)), list(unique(hashtagsTbl$hashtag))))
	visHashtags(hashtagsTbl[user_screen_name=='chelseafc'], db)
	visHashtags(modelHashtagsTbl[topHashtag==T,], db)
	visCompare(hashtagsTbl[user_screen_name=='joelmchale'], modelHashtagsTbl[topHashtag==T & user_screen_name=='joelmchale',], db)

	extremesTbl = summarizeExtremes(hashtagsTbl)
	extremesTbl
	Q

	summarizeExtremes(hashtagsTbl[user_screen_name=='eddieizzard'])
	modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl[user_screen_name %in% unique(user_screen_name)[1:25]])
	modelVsPredTblBig = modelVsPredTbl
	modelVsPredTbl = genAggModelVsPredTbl(hashtagsTbl)
	modelVsPredTbl
	write.csv(modelVsPredTbl, sprintf('%s/tmp2.csv', PATH))
	modelVsPredTbl = myReadCSV(modelVsPredOutFile('gt100k'))
	modelVsPredTbl = myReadCSV(modelVsPredOutFile('gt1M'))
	modelVsPredTbl = myReadCSV(modelVsPredOutFile('gt1Mr2'))
	modelVsPredTbl = myReadCSV(modelVsPredOutFile('gt10M'))
	modelVsPredTbl = myReadCSV(modelVsPredOutFile('SOgt10k'), stringsAsFactors=F)
	modelVsPredTbl = buildTables(c('gt100k', 'gt1M', 'gt1Mr2', 'gt10M'))
	modelVsPredTbl = buildTables(c('gt1k', 'gt1kr2', 'gt10k', 'gt10kr2', 'gt100k', 'gt100kr2', 'gt1M', 'gt1Mr2', 'gt10M', 'gt10Mr2', 'SOgt100k', 'SOgt10k', 'SOgt1k'))
	modelVsPredTbl = buildTables(c('gt1kr2', 'gt10kr2', 'gt100kr2', 'gt1Mr2', 'gt10Mr2', 'SOgt500', 'SOgt1k', 'SOgt5k', 'SOgt10k', 'SOgt50k', 'SOgt100k'))
	modelVsPredTbl
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagRank'][user_screen_name %in% sample(unique(user_screen_name), size=10)], hashtagsTbl)
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & !(user_screen_name %in% c('cokguzelhareket', 'pmoindia')),], hashtagsTbl)
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & !(user_screen_name %in% lowUsers)], hashtagsTbl)
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='SOgt100k'])


	tableModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost'])
	modelVsPredTbl[, user_screen_name, by=user_screen_name]
	modelVsPredTbl[, list(f=unique(user_screen_name), !(unique(user_screen_name) %in% unique(modelVsPredTbl[hashtagUsedP==T,user_screen_name])))]
	modelVsPredTbl[DVName=='topHashtagPost' & maxNP & topHashtag & hashtagUsedP][,hist(d)]
	modelVsPredTbl[topHashtag == T][, list(totN, sum(NCell)), by=list(user_screen_name,d)]
	# Check that totN calculated makes sense	
	modelVsPredTbl[topHashtag == T][, list(totN, sum(NCell)), by=list(user_screen_name,d, DVName)][d==0][!is.na(totN)][,totN-V2]
	setkey(modelVsPredTbl, user_screen_name)
	modelVsPredTbl[topHashtag ==T & hashtagUsedP]
	setkey(extremesTbl, user_screen_name)
	setkey(modelVsPredTbl, user_screen_name, d)
	modelVsPredTblBig[topHashtag & hashtagUsedP]
	modelVsPredTblBig[topHashtag & hashtagUsedP][extremesTbl, allow.cartesian=T][d==0 | d==20]
	tables()
	foo2[foo1][, N-V1]
	tables()
	modelVsPredTblSmall[topHashtag==T & hashtagUsedP]
	setkey(modelVsPredTbl, user_screen_name)

	joinTbl = modelVsPredTblBig[topHashtag & hashtagUsedP][extremesTbl, allow.cartesian=T][maxNP==T]
	joinTbl[, list(user_screen_name, best=N/totN, r=NRecency/totN, f=NFrequency/totN)][, list(user_screen_name, best-r, best-f)][, lapply(list(V2, V3), mean),]
	joinTbl[, list(user_screen_name, best=N/totN, r=NRecency/totN, f=NFrequency/totN)][, list(user_screen_name, best-r, best-f)][, hist(V2)]
}

#curWS()

