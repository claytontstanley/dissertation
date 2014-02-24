library(tools)
library(RPostgreSQL)
library(lavaan)
library(Rmisc)
library(ggplot2)
require(gridExtra)
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

withDBConnect <- function(var, thunk) {
	var = substitute(var)
	eval(bquote(assign(.(deparse(var)), dbConnect(PostgreSQL(), host="localhost", user= "claytonstanley", dbname="claytonstanley"))))
	on.exit(eval(bquote(dbDisconnect(.(var)))))
	eval(substitute(thunk))
}

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

savePlotsP = T

myPlotPrint <- function(fig, name) {
	dev.new()
	myLog(fig)
	if (savePlotsP) ggsave(filename=sprintf('%s/figures/%s.pdf', PATH, name), plot=fig)
	fig
}


# ref: http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
html2txt <- function(str) {
	xpathApply(htmlParse(str, asText=TRUE),
		   "//body//text()", 
		   xmlValue)[[1]] 
}

html2txt2 <- function(vect) {
	inFile = tempfile(pattern='htmlIn-', tmpdir='/tmp', fileext='.csv')
	outFile = tempfile(pattern='htmlOut-', tmpdir='/tmp', fileext='.csv')
	myWriteCSV(vect, file=inFile)
	cmd = '/opt/local/bin/python'
	args = sprintf('%s/lib/html2txt.py %s %s', PATH, inFile, outFile)
	myLog(sprintf('running html2txt with in/out temp files: %s, %s', inFile, outFile))
	cmdOut = system2(cmd, args=args)
	res = myReadCSV(outFile, header=F)
	stopifnot(res$V1 == vect)
	res$V2
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

withKey <- function(tbl, conKey, thunk) {
	.curKey = key(tbl)
	on.exit(setkeyv(tbl, .curKey))
	eval(bquote(setkey(.(substitute(tbl)), .(substitute(conKey)))), envir=parent.frame())
	thunk
}

myReadCSV <- function(file, ...) {
	data.table(read.csv(file, stringsAsFactors=F, ...))
}

myWriteCSV <- function(data, file, ...) {
	write.csv(data, file=file, row.names=F, ...)
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
	data.table(sqldf("select retweeted, count(*) from tweets group by retweeted"))
	sqldf("select * from tweets where 1=1 and user_screen_name='katyperry' limit 10")
	foo = data.table(sqldf("select * from twitter_users"))
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
	cmd = sprintf('%s/lib/ark-tweet-nlp-0.3.2/runTagger.sh', PATH)
	args = sprintf('--no-confidence --just-tokenize --quiet %s > %s 2>%s', rawTweetsFile, tokenizedTweetsFile, stderFile)
	myLog(sprintf('running tagger with in/out temp files: %s, %s', rawTweetsFile, tokenizedTweetsFile))
	cmdOut = system2(cmd, args=args)
	myLog(paste(readLines(stderFile), sep='\n'))
	tokenTextTbl = data.table(read.delim(tokenizedTweetsFile, sep='\t', quote="", header=F, stringsAsFactors=F))
	stopifnot(tokenTextTbl[[2]] == stripDelimiters(tweetsTbl[[from]]))
	tweetsTbl[, tokenText := tokenTextTbl[[1]]]
	return()
}

getDiffTimeSinceFirst <- function(ts) {
	as.numeric(difftime(ts, ts[1], units='secs'))
}

getTweetsTbl <- function(sqlStr="select * from tweets limit 10000", config) {
	tweetsTbl = data.table(sqldf(sqlStr))
	stopifnot(sort(unique(tweetsTbl$retweeted)) == c('False', 'True'))
	if (!config$includeRetweetsP) tweetsTbl = tweetsTbl[retweeted == 'False']
	addTokenText(tweetsTbl, from='text')
	setkey(tweetsTbl, id)
	tweetsTbl[, dt := getDiffTimeSinceFirst(created_at), by=user_screen_name]
	tweetsTbl
}

getTagSynonymsTbl <- function(sqlStr='select * from tag_synonyms') {
	tagSynonymsTbl = data.table(sqldf(sqlStr))
	setkey(tagSynonymsTbl, source_tag_name)
	tagSynonymsTbl
}

getPostsTbl <- function(sqlStr, config) {
	postsTbl = data.table(sqldf(sqlStr))
	setkey(postsTbl, id)
	stopifnot(!duplicated(postsTbl$id))
	# TODO: this is not vectorized b/c html2txt isn't vectorized
	# FIXME: Test this change
	#postsTbl[, tagsNoHtml := html2txt(tags), by=id]
	postsTbl[, tagsNoHtml := html2txt2(tags)]
	postsTbl[, dt := getDiffTimeSinceFirst(creation_date), by=owner_user_id]
	postsTbl
}

matchWhitespace = '\\S+'
matchTag = '(?<=<)[^>]+(?=>)'

getHashtagsTbl <- function(tweetsTbl, config) {
	from = config$from
	tokenizedTbl = getTokenizedTbl(tweetsTbl, from=from, regex=matchWhitespace)
	htOfTokenizedTbl = tokenizedTbl[grepl('^#', chunk),]
	stopifnot(c('id') == key(htOfTokenizedTbl))
	hashtagsTbl = htOfTokenizedTbl[tweetsTbl, list(hashtag=chunk, pos=pos, created_at=created_at, dt=dt, user_id=user_id, user_screen_name=user_screen_name), nomatch=0]
	setkey(hashtagsTbl, user_screen_name, dt, hashtag)
	hashtagsTbl
}

getTagsTbl <- function(postsTbl, config) {
	tokenizedTbl = getTokenizedTbl(postsTbl, from='tagsNoHtml', regex=matchTag)
	if (config$convertTagSynonymsP) {
		withKey(tokenizedTbl, chunk,
			{tokenizedTbl[getTagSynonymsTbl(), chunk := target_tag_name]
			})
	}
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

computeActs <- function(hashtags, dtP, cTime, d) {
	debugPrint(hashtags)
	debugPrint(d)
	debugPrint(cTime)
	dtP = cTime - dtP
	indeces = dtP>0
	hashtagsSub = hashtags[indeces] 
	debugPrint(hashtagsSub)
	cTimeSub = cTime[indeces]
	cTimeSubRep = rep(cTimeSub, times=length(d))
	dtPSub = dtP[indeces]
	dtPSubRep = rep(dtPSub, times=length(d))
	dRep = rep(d, each=length(dtPSub))
	hashtagsSubRep = rep(hashtagsSub)
	list(hashtag=hashtagsSubRep, partialAct=dtPSubRep^(-dRep), dt=cTimeSubRep, d=dRep, dtP=dtPSubRep)
}

computeActsForUser <- function(hashtag, dt, ds, user_screen_name) {
	#myLog(sprintf('computing partial activation for user %s', user_screen_name))
	retIndeces = which(!duplicated(dt))[-1]
	stopifnot(length(retIndeces) > 0)
	partialRes = data.table(i=retIndeces)
	partialRes = partialRes[, list(hashtag=hashtag[1:i], dtP=dt[1:i], cTime=dt[i]), by=i]
	partialRes = with(partialRes, as.data.table(computeActs(hashtag, dtP, cTime, d=ds)))
	partialRes
}

computeActsByUser <- function(hashtagsTbl, ds) {
	partialRes = hashtagsTbl[, computeActsForUser(hashtag, dt, ds, user_screen_name), by=user_screen_name]
	debugPrint(partialRes)
	#myLog('setting key for partial table')
	setkeyv(partialRes, c('user_screen_name','dt','hashtag','d'))
	#myLog('computing activations across table')
	res = partialRes[, list(N=.N,
				act=log(sum(partialAct)),
				actOL=if (d[1]>=1) NaN else log(.N/(1-d))-d*log(dt),
				actOL2=if (d[1]>=1) NaN else log(.N/(1-d))-d*log(max(dtP))), keyby=list(user_screen_name, dt, hashtag, d)]
	with(res, stopifnot(!is.infinite(act)))
	with(res, stopifnot(!is.infinite(actOL2)))
	res
}

visHashtags <- function(hashtagsTbl) {
	plots = hashtagsTbl[, list(resPlots=list(ggplot(.SD, aes(x=hashtag, y=dt)) + geom_point())), by=user_screen_name]
	plots
}

visCompare <- function(hashtagsTbl, modelHashtagsTbl, bestDTbl) {
	stopifnot(sort(unique(hashtagsTbl$user_screen_name)) == sort(unique(modelHashtagsTbl$user_screen_name)))
	plotBuildFun <- function(modelHashtagsTbl, userScreenName, d) {
		list(resPlots=list(ggplot(hashtagsTbl[user_screen_name==userScreenName], aes(x=hashtag, y=dt)) +
				   geom_point() +
				   ggtitle(sprintf('d=%s', d)) + 
				   geom_point(data=modelHashtagsTbl, aes(x=hashtag, y=dt), colour="red", size=.7)))
	}
	minMaxDs = modelHashtagsTbl[, c(min(d), max(d))]
	allDsTbl = bestDTbl[, list(allDs=c(minMaxDs[1],d,minMaxDs[2])), by=list(user_screen_name, d)]
	allDsTbl[, d := NULL]
	setkey(modelHashtagsTbl, user_screen_name, d)
	setkey(allDsTbl, user_screen_name, allDs)
	subsetModelHashtagsTbl = modelHashtagsTbl[allDsTbl, nomatch=0]
	modelPlots = subsetModelHashtagsTbl[, plotBuildFun(.SD, user_screen_name, d), by=list(user_screen_name, d)]
	hashtagPlots = visHashtags(hashtagsTbl)
	hashtagPlots[, resPlots := list(list(resPlots[[1]] + ggtitle('Observed'))), by=user_screen_name]
	setkey(hashtagPlots, user_screen_name)
	fullPlots = modelPlots[, list(resPlots=list(do.call(arrangeGrob, append(hashtagPlots[user_screen_name]$resPlots, resPlots)))), by=user_screen_name]
	fullPlots[, list(resPlots=list(myPlotPrint(resPlots[[1]], sprintf('HTMByTime-%s', .BY[1])))), by=list(user_screen_name)]
}

addMetrics <- function(hashtagsTbl, modelHashtagsTbl) {
	tagCountTbl = hashtagsTbl[, list(tagCountN=.N), by=list(user_screen_name, dt)]
	modelHashtagsTbl[tagCountTbl, tagCount := tagCountN]
	modelHashtagsTbl[tagCountTbl[, list(tagCountUserN=sum(tagCountN)), keyby=user_screen_name], tagCountUser := tagCountUserN]
	myLog('adding metrics for modelHashtagsTbl')
	addDVCols <- function(col, newDVPost, newDVAct) {
		col = substitute(col)
		newDVPost = substitute(newDVPost)
		newDVAct = substitute(newDVAct)
		expr = bquote(modelHashtagsTbl[order(.(col), decreasing=T), .(newDVPost) := 1:length(.(col)) <= tagCount[1], by=list(user_screen_name, dt, d)])
		eval(expr)
		expr = bquote(modelHashtagsTbl[order(.(col), decreasing=T), .(newDVAct) := 1:length(.(col)) <= tagCountUser[1], by=list(user_screen_name, d)])
		eval(expr)
	}
	addDVCols(act, topHashtagPost, topHashtagAct)
	addDVCols(actOL, topHashtagPostOL, topHashtagActOL)
	addDVCols(actOL2, topHashtagPostOL2, topHashtagActOL2)
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

guardAllEqualP <- function(vect) {
	stopifnot(length(unique(vect)) <= 1)
	vect
}

modelVsPredForDV <- function(modelHashtagsTbl, DVName) {
	tempTbl = modelHashtagsTbl[, list(NCell=.N, DVName=DVName), by=c('user_screen_name', DVName, 'hashtagUsedP', 'd')]
	setnames(tempTbl, DVName, 'topHashtag')
	tempTbl
}

getModelVsPredTbl <- function(modelHashtagsTbl, hashtagsTbl) {
	modelVsPredTbl = rbind(modelVsPredForDV(modelHashtagsTbl, 'topHashtagPost'), 
			       modelVsPredForDV(modelHashtagsTbl, 'topHashtagAct'),
			       modelVsPredForDV(modelHashtagsTbl, 'topHashtagPostOL'),
			       modelVsPredForDV(modelHashtagsTbl, 'topHashtagActOL'),
			       modelVsPredForDV(modelHashtagsTbl, 'topHashtagPostOL2'),
			       modelVsPredForDV(modelHashtagsTbl, 'topHashtagActOL2'))
	modelVsPredTbl[, maxNP := NCell==max(NCell), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	# TODO: Doesn't using the maxNP closest to the center of all of the maxNP's create an artifact for low N when all ds are MaxNP's?
	modelVsPredTbl[maxNP==T, maxNP := onlyFirstT(abs(d-mean(d)) == min(abs(d-mean(d)))), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	modelVsPredTbl[, totN := NA_integer_] # Making sure that the totN column is added, even if model never generates an activation value for a hashtag that is used
	modelVsPredTbl[hashtagUsedP==T, totN := length(hashtagsTbl[user_screen_name]$user_screen_name), by=user_screen_name]
	modelVsPredTbl
}

compareModelVsExtreme <-function(modelHashtagsTbl, extremesTbl) {
	modelHashtagsTbl[d==max(d)][topHashtag==T]
	setkey(extremesTbl, user_screen_name, dt, hashtag)
	extremesTbl
	modelHashtagsTbl
	?data.table
	fooTbl = extremesTbl[modelHashtagsTbl[d==max(d)], allow.cartesian=T, nomatch=0][, list(tagCount, user_screen_name, dt, hashtag, hashtagChosenP, topHashtag, lapply(prevHashtags, function(x) x[1:4]))]
	fooTbl
	extremesTbl
	fooTbl
	fooTbl[, sum(topHashtag)]
	fooTbl
}

genAggModelVsPredTbl <- function(hashtagsTbl, config) {
	outFile = config$modelVsPredOutFile
	ds = config$ds
	modelHashtagsTbls = data.table()
	genModelVsPredTbl <- function(hashtagsTbl, ds, userScreenName) {
		myLog(sprintf('generating model predictions for user %s', userScreenName))
		modelHashtagsTbl = rbindlist(lapply(ds, function(d) computeActsByUser(hashtagsTbl, d=d)))
		setkey(modelHashtagsTbl, user_screen_name, dt, hashtag, d)
		addMetrics(hashtagsTbl, modelHashtagsTbl)
		modelVsPredTbl = getModelVsPredTbl(modelHashtagsTbl, hashtagsTbl)	
		if (config$accumModelHashtagsTbl == T) modelHashtagsTbls <<- rbind(modelHashtagsTbls, modelHashtagsTbl)
		rm(modelHashtagsTbl)
		gc()
		modelVsPredTbl
	}
	singleHashtagUsers = hashtagsTbl[, list(uniqueDTs=length(unique(dt)) <= 1), by=user_screen_name][uniqueDTs==T]$user_screen_name
	myLog(sprintf('not running users (%s) since they all have less than two dt hashtag observations', paste0(singleHashtagUsers, collapse=',')))
	users = data.table(cur_user_screen_name=Filter(function(v) !(v %in% singleHashtagUsers), unique(hashtagsTbl$user_screen_name)))
	res = users[, genModelVsPredTbl(hashtagsTbl[cur_user_screen_name], ds, cur_user_screen_name), by=cur_user_screen_name]
	res[, cur_user_screen_name := NULL]
	setkey(res, user_screen_name, DVName, d)
	myWriteCSV(res, file=outFile)
	list(modelVsPredTbl=res, modelHashtagsTbl=modelHashtagsTbls)
}

visModelVsPredTbl <- function(modelVsPredTbl) {
	assign('p1', ggplot(modelVsPredTbl[predUsedBest == T], aes(totN, d)) +
	       geom_point() +
	       xlab('Total Number of Hashtags'))
	modelVsPredTbl[topHashtag & hashtagUsedP, meanPC := mean(acc), by=user_screen_name]
	modelVsPredTbl[topHashtag & hashtagUsedP, relPC := acc - mean(acc), by=user_screen_name]
	modelVsPredTbl[topHashtag & hashtagUsedP, meanRelPC := mean(relPC), by=d]
	assign('p2', ggplot(modelVsPredTbl[topHashtag & hashtagUsedP], aes(log(d),relPC)) +
	       geom_point() +
	       geom_line(aes(log(d), meanRelPC, group=user_screen_name[1])) +
	       xlab('log(d)') +
	       ylab('Normalized Mean'))
	assign('p3', ggplot(modelVsPredTbl[topHashtag & hashtagUsedP & user_screen_name %in% sample(unique(user_screen_name), size=min(20, length(unique(user_screen_name))))],
			    aes(log(d),acc, group=as.factor(user_screen_name))) + geom_line() +
	       ylab('Accuracy'))
	assign('p4', ggplot(modelVsPredTbl[predUsedBest == T], aes(d)) +
	       geom_histogram(aes(y = ..density..)) +
	       geom_density() +
	       ylab('Density'))
	ext = sprintf('%s-%s', guardAllEqualP(p1$data$datasetName)[1], guardAllEqualP(p1$data$DVName)[1])
	myPlotPrint(p1, sprintf('visDByN-%s', ext)) 
	myPlotPrint(p2, sprintf('visNormMean-%s', ext)) 
	myPlotPrint(p3, sprintf('visAcc-%s', ext)) 
	myPlotPrint(p4, sprintf('visHistD-%s', ext)) 
	return()
}

tableModelVsPredTbl <- function(modelVsPredTbl) {
	# Summary table of optimal d values and sample variance
	modelVsPredTbl[predUsedBest == T][, list(mean=mean(d), median=median(d), totN=mean(totN), NCell=mean(NCell), acc=mean(NCell/totN),
						 datasetName=guardAllEqualP(datasetName)[1],
						 datasetGroup=guardAllEqualP(datasetGroup)[1],
						 #sdCI=sqrt(CIVar(d)), sdCI1=sqrt(CIVar2(d)), meanCI=CI(d))
						 sd=sd(d)), by=list(datasetNameRoot, runNum, DVName)]
}

modelVsPredDir <- function() {
	sprintf('%s/dissertationData/modelVsPred', PATH)
}

getModelVsPredOutFile <- function(name) {
	sprintf('%s/%s.csv', modelVsPredDir(), name)
}

runPrior <- function(config) {
	stopifnot(!any(sapply(config,is.null)))
	withProf({
		postsTbl = config$getPostsFun(config$query, config=config)
		hashtagsTbl = config$getHashtagsFun(postsTbl, config=config)
		res = genAggModelVsPredTbl(hashtagsTbl, config=config)
		modelVsPredTbl = res$modelVsPredTbl
		modelHashtagsTbl = res$modelHashtagsTbl
		list(modelVsPredTbl=modelVsPredTbl, modelHashtagsTbl=modelHashtagsTbl, hashtagsTbl=hashtagsTbl)
	})
}

defaultBaseConfig = list(ds=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2,5,10,20),
			 modelVsPredOutFile='/tmp/modelVsPred.csv',
			 query=NULL)

defaultTConfig = append(defaultBaseConfig,
			list(from='tokenText',
			     accumModelHashtagsTbl=F,
			     getPostsFun=getTweetsTbl,
			     getHashtagsFun=getHashtagsTbl,
			     includeRetweetsP=F))

defaultSOConfig = append(defaultBaseConfig,
			 list(convertTagSynonymsP=T,
			      accumModelHashtagsTbl=F,
			      getPostsFun=getPostsTbl,
			      getHashtagsFun=getTagsTbl))

runPriorT <- function(config=defaultTConfig) {
	runPrior(config)
}

runPriorSO <- function(config=defaultSOConfig) {
	runPrior(config)
}

modConfig <- function(config, mods) {
	newConfig = config
	stopifnot(names(mods) %in% names(config))
	for(modName in names(mods)) {
		newConfig[[modName]] = mods[[modName]]
	}
	newConfig
}

getQueryGeneralT <- function(val, from, filters) {
	sprintf('select * from tweets as t where %s and user_screen_name in (select user_screen_name from twitter_users where %s > %d order by %s asc limit 100)', filters, from, val, from)
}

getQueryT <- function(val, filters='1=1') {
	getQueryGeneralT(val, 'followers_count', filters)
}

getQueryTStatuses <- function(val, filters='1=1') {
	getQueryGeneralT(val, 'statuses_count', filters)
}

getQuerySO <- function(val) {
	sprintf('select id, owner_user_id, creation_date, title, tags from posts where post_type_id = 1 and owner_user_id in (select id from users where reputation > %d order by reputation asc limit 500)', val)
}

getQuerySOQ <- function(val) {
	sprintf("select id, owner_user_id, creation_date, title, tags from posts where post_type_id = 1 and owner_user_id in 
		(select Owner_User_Id from
		 (select owner_user_id, Post_Type_Id, count(*) as N from Posts 
		  where Post_Type_Id = 1
		  group by Owner_User_Id,Post_Type_Id) as foo2
		 join Users on Users.Id = foo2.Owner_User_Id
		 where N > %d
		 order by N asc
		 limit 100)
		", val)
}

combineFilters <- function(f1, f2='1=1') {
	paste(f1, f2, sep=' and ')
}

makeTRun <- function(val, outFileName, config) {
	function() runPriorT(config=modConfig(config, list(query=config$query(val), modelVsPredOutFile=getModelVsPredOutFile(outFileName))))
}

makeTRunr1 <- function(val, outFileName, ...) {
	makeTRun(val, outFileName, config=modConfig(defaultTConfig, list(query=function(val) getQueryT(val, ...), includeRetweetsP=T)))
}

makeTRunr2 <- function(val, outFileName, ...) {
	makeTRun(val, outFileName, config=modConfig(defaultTConfig, list(query=function(val) getQueryT(val, ...), includeRetweetsP=F)))
}

makeTRunr3 <- function(val, outFileName, ...) {
	makeTRun(val, outFileName, config=modConfig(defaultTConfig, list(query=function(val) getQueryTStatuses(val, ...), includeRetweetsP=T)))
}

makeTRunr4 <- function(val, outFileName, ...) {
	makeTRun(val, outFileName, config=modConfig(defaultTConfig, list(query=function(val) getQueryTStatuses(val, ...), includeRetweetsP=F)))
}

runTFollow1k <- makeTRunr1(1000, 'TFollowgt1k')
runTFollow1kr2 <- makeTRunr2(1000, 'TFollowgt1kr2')
runTFollow5k <- makeTRunr1(5000, 'TFollowgt5k', filters="user_screen_name != 'g4scareers'")			# g4scareers causes segfault w/ data.table 1.8.10
runTFollow5kr2 <- makeTRunr2(5000, 'TFollowgt5kr2', filters="user_screen_name != 'g4scareers'")
runTFollow10k <- makeTRunr1(10000, 'TFollowgt10k', filters="user_screen_name != 'so_pr'") 			# so_pr user causes segfault w/ data.table 1.8.10
runTFollow10kr2 <- makeTRunr2(10000,'TFollowgt10kr2', filters="user_screen_name != 'so_pr'")
runTFollow100k <- makeTRunr1(100000, 'TFollowgt100k', filters="user_screen_name != 'hermosa_brisa'") 		# hermosa_brisa causes segfault w/ data.table 1.8.10
runTFollow100kr2 <- makeTRunr2(100000, 'TFollowgt100kr2', filters="user_screen_name != 'hermosa_brisa'")
runTFollow1M <- makeTRunr1(1000000, 'TFollowgt1M')
runTFollow1Mr2 <- makeTRunr2(1000000,'TFollowgt1Mr2')
runTFollow10M <- makeTRunr1(10000000, 'TFollowgt10M', filters='t.id != 12466832063')				# tweet 12466832063  has a corrupt utf-8 encoded string
runTFollow10Mr2 <- makeTRunr2(10000000, 'TFollowgt10Mr2', filters='t.id != 12466832063')
runTTweets1e2 <- makeTRunr3(100, 'TTweetsgt1e2')
runTTweets1e2r2 <- makeTRunr4(100, 'TTweetsgt1e2r2')
runTTweets5e2 <- makeTRunr3(500, 'TTweetsgt5e2')
runTTweets5e2r2 <- makeTRunr4(500, 'TTweetsgt5e2r2')
runTTweets1e3 <- makeTRunr3(1000, 'TTweetsgt1e3')
runTTweets1e3r2 <- makeTRunr4(1000, 'TTweetsgt1e3r2')
runTTweets5e3 <- makeTRunr3(5000, 'TTweetsgt5e3')
runTTweets5e3r2 <- makeTRunr4(5000, 'TTweetsgt5e3r2')
runTTweets1e4 <- makeTRunr3(10000, 'TTweetsgt1e4')
runTTweets1e4r2 <- makeTRunr4(10000, 'TTweetsgt1e4r2')
runTTweets5e4 <- makeTRunr3(50000, 'TTweetsgt5e4', filters="user_screen_name != 'stanhjerleid'")
runTTweets5e4r2 <- makeTRunr4(50000, 'TTweetsgt5e4r2', filters="user_screen_name != 'stanhjerleid'")

makeSORun <- function(val, outFileName, config) {
	runFun = function() runPriorSO(config=modConfig(config, list(query=config$query(val), modelVsPredOutFile=getModelVsPredOutFile(outFileName))))
	runFun
}

getSOr1Config <- function () modConfig(defaultSOConfig, list(convertTagSynonymsP=F, query=getQuerySO))
getSOr2Config <- function () modConfig(defaultSOConfig, list(convertTagSynonymsP=T, query=getQuerySO))
getSOr3Config <- function () modConfig(defaultSOConfig, list(convertTagSynonymsP=F, query=getQuerySOQ))
getSOr4Config <- function () modConfig(defaultSOConfig, list(convertTagSynonymsP=T, query=getQuerySOQ))
makeSORunr1 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr1Config())
makeSORunr2 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr2Config())
makeSORunr3 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr3Config())
makeSORunr4 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr4Config())

runSO100k <- makeSORunr1(100000, 'SOgt100k')
runSO50k <- makeSORunr1(50000, 'SOgt50k')
runSO10k <- makeSORunr1(10000, 'SOgt10k')
runSO5k <- makeSORunr1(5000, 'SOgt5k')
runSO1k <- makeSORunr1(1000, 'SOgt1k')
runSO500 <- makeSORunr1(500, 'SOgt500')
runSO100kr2 <- makeSORunr2(100000, 'SOgt100kr2')
runSO50kr2 <- makeSORunr2(50000, 'SOgt50kr2')
runSO10kr2 <- makeSORunr2(10000, 'SOgt10kr2')
runSO5kr2 <- makeSORunr2(5000, 'SOgt5kr2')
runSO1kr2 <- makeSORunr2(1000, 'SOgt1kr2')
runSO500r2 <- makeSORunr2(500, 'SOgt500r2')
runSOQgt500 <- makeSORunr3(500, 'SOQgt500')
runSOQgt400 <- makeSORunr3(400, 'SOQgt400')
runSOQgt300 <- makeSORunr3(300, 'SOQgt300')
runSOQgt200 <- makeSORunr3(200, 'SOQgt200')
runSOQgt100 <- makeSORunr3(100, 'SOQgt100')
runSOQgt050 <- makeSORunr3(050, 'SOQgt050')
runSOQgt500r2 <- makeSORunr4(500, 'SOQgt500r2')
runSOQgt400r2 <- makeSORunr4(400, 'SOQgt400r2')
runSOQgt300r2 <- makeSORunr4(300, 'SOQgt300r2')
runSOQgt200r2 <- makeSORunr4(200, 'SOQgt200r2')
runSOQgt100r2 <- makeSORunr4(100, 'SOQgt100r2')
runSOQgt050r2 <- makeSORunr4(050, 'SOQgt050r2')

buildTables <- function(outFileNames) {
	buildTable <- function(outFileName) {
		colClasses = c('character', 'logical', 'logical', 'numeric', 'integer', 'character', 'logical', 'integer')
		tbl = myReadCSV(getModelVsPredOutFile(outFileName), colClasses=colClasses)
		tbl[, datasetName := outFileName]
		tbl
	}
	addRunNum <- function(modelVsPredTbl) {
		modelVsPredTbl[, runNum := 1]
		modelVsPredTbl[grepl('r[0-9]$', datasetName), runNum := as.numeric(substr(datasetName, nchar(datasetName), nchar(datasetName)))]
	}
	addDatasetNameRoot <- function(modelVsPredTbl) {
		modelVsPredTbl[, datasetNameRoot := datasetName]
		modelVsPredTbl[grepl('r[0-9]$', datasetName), datasetNameRoot := substr(datasetName, 1, nchar(datasetName)-2)] 
	}
	addDatasetType <- function(modelVsPredTbl) {
		modelVsPredTbl[, datasetType := 'unknown']
		modelVsPredTbl[grepl('^SO', datasetName), datasetType := 'stackoverflow']
		modelVsPredTbl[grepl('^T', datasetName), datasetType := 'twitter']
	}
	addDatasetGroup <- function(modelVsPredTbl) {
		modelVsPredTbl[, datasetGroup := 'unknown']
		modelVsPredTbl[grepl('^SOQ', datasetName), datasetGroup := 'topQuestions']
		modelVsPredTbl[grepl('^SOg', datasetName), datasetGroup := 'topReputation']
		modelVsPredTbl[grepl('^TT', datasetName), datasetGroup := 'topTweets']
		modelVsPredTbl[grepl('^TF', datasetName), datasetGroup := 'topFollowers']
	}
	addMiscellaneous <- function(modelVsPredTbl) {
		modelVsPredTbl[, predUsedBest := F]
		modelVsPredTbl[topHashtag & hashtagUsedP & maxNP, predUsedBest := T]
		modelVsPredTbl[hashtagUsedP == T, acc := NCell/totN]
	}
	modelVsPredTbl = rbindlist(lapply(outFileNames, buildTable))
	addRunNum(modelVsPredTbl)
	addDatasetNameRoot(modelVsPredTbl)
	addDatasetType(modelVsPredTbl)
	addDatasetGroup(modelVsPredTbl)
	addMiscellaneous(modelVsPredTbl)
	modelVsPredTbl[datasetType != 'unknown']
}

withCI <- function(dat) {
	res = CI(dat)
	list(N=length(dat), meanVal=res[2], minCI=res[1], maxCI=res[3])
}

genComparisonTbl <- function(SD) {
	resTbl = copy(SD)
	resTbl[, DVDirection := sprintf('%s%s', direction, if (DVName != '') sprintf(' for %s', DVName) else ''), by=list(direction, DVName)]
	resTbl[!is.na(diff), withCI(diff), by=list(DVDirection, direction, DVName)]
}

compare2DVs <- function(modelVsPredTbl, DVs, sortedOrder=c(1,2)) {
	sumTbl = modelVsPredTbl[predUsedBest == T][DVName %in% DVs,]
	setkey(sumTbl, datasetName, user_screen_name, DVName)
	sumTbl[, list(diff=acc[sortedOrder[2]]-acc[sortedOrder[1]], direction=paste(DVName[sortedOrder[2]], '-', DVName[sortedOrder[1]])), by=list(datasetName, user_screen_name)][, DVName := ''][, genComparisonTbl(.SD)]
}

compare2Runs <- function(modelVsPredTbl, runNums) {
	sumTbl = modelVsPredTbl[predUsedBest == T][runNum %in% runNums]
	setkey(sumTbl, datasetNameRoot, DVName, user_screen_name, runNum)
	sumTbl[, list(diff=acc[2]-acc[1], direction=sprintf('run%s - run%s', runNums[2], runNums[1])), by=list(datasetNameRoot, DVName, user_screen_name)][, genComparisonTbl(.SD)]
}

compareDBestVsMin <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[topHashtag & hashtagUsedP & (maxNP | d == min(d))]
	setkey(sumTbl, datasetName, user_screen_name, DVName, maxNP) 
	sumTbl[, list(diff=acc[2]-acc[1], direction=paste('best d', '-', 'min d')), by=list(datasetName, user_screen_name, DVName)][, genComparisonTbl(.SD)]
}

compareDBestVsMax <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[topHashtag & hashtagUsedP & (maxNP | d == max(d))]
	setkey(sumTbl, datasetName, user_screen_name, DVName, maxNP) 
	sumTbl[, list(diff=acc[2]-acc[1], direction=paste('best d', '-', 'max d')), by=list(datasetName, user_screen_name, DVName)][, genComparisonTbl(.SD)]
}

plotBarSumTbl <- function(sumTbl, fillCol, figName, extras=NULL) {
	fillCol = substitute(fillCol)
	expr = bquote(ggplot(sumTbl, aes(x=factor(datasetGroup), y=meanVal, fill=.(fillCol))) +
		      geom_bar(position=position_dodge(), stat='identity') +
		      geom_errorbar(aes(ymin=minCI, ymax=maxCI), position=position_dodge(width=0.9), width=0.1, size=0.3) + 
		      scale_fill_grey())
	plot = eval(expr)
	lapply(extras, function(extra) plot <<- plot + extra)
	myPlotPrint(plot, figName)
	sumTbl
}

compareMeanDV <- function(modelVsPredTbl, DV, extras=NULL) {
	DV = substitute(DV)
	expr = bquote(tableModelVsPredTbl(modelVsPredTbl)[, withCI(.(DV)), keyby=list(DVName, datasetGroup)])
	sumTbl = eval(expr)
	plotBarSumTbl(sumTbl, DVName, sprintf('compareMeanDV-%s', deparse(DV)), extras=append(list(theme(axis.title.x=element_blank())), extras))
}

plotDatasetDescriptives <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[runNum==2 & predUsedBest == T & DVName == 'topHashtagPost'][, list(NUsers=.N, NHashtagObs=sum(totN)), by=list(datasetName,datasetType,datasetGroup)]
	plotBarSumTbl(sumTbl[, withCI(NUsers), by=list(datasetGroup, datasetName)],
		      datasetName, 'compareNumbers',
		      extras=list(theme(axis.title.x=element_blank()), ylab('Number of Users')))
	plotBarSumTbl(sumTbl[, withCI(NHashtagObs), by=list(datasetGroup, datasetName)],
		      datasetName, 'compareHashtagObs',
		      extras=list(theme(axis.title.x=element_blank()), ylab('Number of Hashtag Uses')))
	sumTbl
}

plotDVDiffs <- function(sumTbl) {
	plotBarSumTbl(sumTbl, DVDirection, sprintf('compareDVDiffs'), extras=list(theme(legend.position='top', legend.direction='vertical', axis.title.y=element_blank()),
										  #labs(x=element_blank()),
										  labs(y='Mean Difference in Accuracy'),
										  guides(fill=guide_legend(title="Difference Type")),
										  coord_flip()))
}

compareOptimalDs <- function(modelVsPredTbl) compareMeanDV(modelVsPredTbl, median, list(labs(y='Mean Optimal d')))
compareOptimalAcc <- function(modelVsPredTbl) compareMeanDV(modelVsPredTbl, acc, list(labs(y='Mean Accuracy')))

# TODO: Find a library that implements this
wrapQuotes <- function(charVect) {
	paste(paste(c("'"), charVect, sep='', collapse="',"), "'", sep="", collapse="")
}

plotTemporal <- function(runTbls) {
	bestDTbl = runTbls$modelVsPredTbl[topHashtag & hashtagUsedP & maxNP & DVName=='topHashtagPost']
	visCompare(runTbls$hashtagsTbl, runTbls$modelHashtagsTbl[topHashtagPost==T], bestDTbl)
}

analyzeTemporal <- function(modelVsPredTbl) {
	modelVsPredTbl[, unique(datasetName)]
	#screenTbl = modelVsPredTbl[datasetName=='SOQgt300r2'][, list(user_screen_name=wrapQuotes(sample(user_screen_name, 10))), by=list(datasetName, datasetType)]
	#user_screen_names = screenTbl[, paste(user_screen_name, sep='', collapse=','), by=datasetType]
	#user_screen_names = user_screen_names[, V1]
	user_screen_names = c("'rickeysmiley','fashionista_com','laurenpope','mtvindia','officialrcti'")
	user_screen_names = c("'fashionista_com'")
	runTbls = runPriorT(config=modConfig(defaultTConfig, list(accumModelHashtagsTbl=T,
								  query=sprintf("select * from tweets where user_screen_name in (%s)", user_screen_names))))
	plotTemporal(runTbls)
	user_screen_names = c("'520957','238260','413225','807325','521180'")
	user_screen_names = c("'520957','238260'")
	runTbls = runPriorSO(config=modConfig(defaultSOConfig, list(accumModelHashtagsTbl=T,
								    query=sprintf("select * from posts where post_type_id = 1 and owner_user_id in (%s)", user_screen_names))))
	plotTemporal(runTbls)
}

analyzeModelVsPredTbl <- function(modelVsPredTbl) {
	analyzeTemporal(modelVsPredTbl)
	plotDatasetDescriptives(modelVsPredTbl)[, list(meanNUsers=mean(NUsers),totNUsers=sum(NUsers),meanNHO=mean(NHashtagObs),totNHO=sum(NHashtagObs)), by=list(datasetType)]
	# Check that totN calculated makes sense. Result should be small.
	modelVsPredTbl[topHashtag == T & d==min(d)][, list(totN, sum(NCell)), by=list(user_screen_name,d,DVName, datasetName)][!is.na(totN)][,list(res=totN-V2)][, withCI(res)]
	# Check that the Ns for each dataset look right	
	modelVsPredTbl[, list(N=.N, names=list(unique(datasetName))), by=list(datasetType, datasetGroup, runNum,datasetNameRoot)]

	plotDVDiffs(rbind(modelVsPredTbl[runNum==2, compare2DVs(.SD, c('topHashtagPost', 'topHashtagPostOL2'), sortedOrder=c(2,1)), by=list(datasetType, datasetGroup), .SDcols=colnames(modelVsPredTbl)],
			  modelVsPredTbl[runNum==2, compare2DVs(.SD, c('topHashtagPost', 'topHashtagAct')), by=list(datasetType, datasetGroup), .SDcols=colnames(modelVsPredTbl)],
			  modelVsPredTbl[DVName %in% c('topHashtagPost', 'topHashtagPostOL2'), compare2Runs(.SD, c(1,2)), by=list(datasetType, datasetGroup), .SDcols=colnames(modelVsPredTbl)],
			  modelVsPredTbl[runNum==2 & DVName %in% c('topHashtagPost'), compareDBestVsMin(.SD), by=list(datasetType, datasetGroup)],
			  modelVsPredTbl[runNum==2 & DVName %in% c('topHashtagPost'), compareDBestVsMax(.SD), by=list(datasetType, datasetGroup)]))
	compareOptimalDs(modelVsPredTbl[DVName %in% c('topHashtagPost', 'topHashtagPostOL2', 'topHashtagAct') & runNum == 2])
	compareOptimalAcc(modelVsPredTbl[DVName %in% c('topHashtagPost', 'topHashtagPostOL2', 'topHashtagAct') & runNum == 2])

	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='TFollowgt10Mr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='TFollowgt1kr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='TTweetsgt5e4r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='TTweetsgt1e2r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='SOgt1kr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='SOgt100kr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='SOQgt050r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPost' & datasetName=='SOQgt500r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostOL2' & datasetName=='SOQgt500r2' & d < 1])
}

getNcoocTbl <- function(tokenizedTblChunks, tokenizedTblTags, chunks=10000) {
	setkey(tokenizedTblChunks, id)
	setkey(tokenizedTblTags, id)
	getNcoocTblSubset <- function(subset) {
		myLog('Getting Ncooc for subset')
		tokenizedTblChunksSubset = tokenizedTblChunks[J(subset)]
		tokenizedTblTagsSubset = tokenizedTblTags[J(subset)]
		addPosFromTag = function(tbl) tbl[, posFromTag := pos - pos.1][, list(id=id, chunk=chunk, tag=chunk.1, posFromTag=posFromTag)]
		coocTbl = tokenizedTblChunksSubset[tokenizedTblTagsSubset, allow.cartesian=T][, addPosFromTag(copy(.SD))]
		NcoocTbl = coocTbl[, list(partialN=.N), by=list(chunk, tag, posFromTag)]
		NcoocTbl[, posFromTag := as.character(posFromTag)]
		setkey(NcoocTbl, chunk, tag, posFromTag)
		rm(coocTbl)
		NcoocTbl
	}
	mergeNcoocTbls <- function(tbl1, subset2) {
		tbl2 = getNcoocTblSubset(subset2)
		tbl1[tbl2[, partialN1 := partialN][, partialN := NULL], partialN := partialN + partialN1]
		res = rbind(tbl1, tbl2[, partialN := partialN1][, partialN1 := NULL][!tbl1])
		setkey(res, chunk, tag, posFromTag)
		res
	}
	splitSequence <- function(d, n) {
		split(d, ceiling(seq_along(d)/n))
	}
	ids = unique(tokenizedTblChunks$id)
	groups = splitSequence(ids[2:length(ids)], chunks)
	NcoocTbl = Reduce(mergeNcoocTbls, append(list(getNcoocTblSubset(ids[1])), groups))
	NcoocTbl[, NChunkTag := sum(partialN), by=list(chunk, tag)]
	tables()
	NcoocTbl
}

genNcoocTblSO <- function(startId, endId, group_name='SOShuffledFull') {
	postsTbl = getPostsTbl(sprintf("select id, owner_user_id, creation_date, title, body, tags
				       from posts
				       where post_type_id = 1
				       and id in (select post_id 
						  from post_subsets
						  where id >= %s
						  and id <= %s
						  and group_name = '%s')",
				       startId, endId, group_name), defaultSOConfig)
	stopifnot(nrow(postsTbl) == endId - startId + 1)
	postsTbl[, tags := NULL][, bodyNoHtml := html2txt2(body)][, body := NULL]
	addTokenText(postsTbl, from='title')
	tokenizedTblTitle = getTokenizedTbl(postsTbl, from='tokenText', regex=matchWhitespace)[, type := 'title']
	addTokenText(postsTbl, from='bodyNoHtml')
	tokenizedTblBody = getTokenizedTbl(postsTbl, from='tokenText', regex=matchWhitespace)[, type := 'body']
	tokenizedTblTags = getTokenizedTbl(postsTbl, from='tagsNoHtml', regex=matchTag)[, type := 'tag'][, pos := NaN]
	NcoocTblTitle = getNcoocTbl(tokenizedTblTitle, tokenizedTblTags)
	NcoocTblBody = getNcoocTbl(tokenizedTblBody, tokenizedTblTags)
	outFile = sprintf('%s/dissertationData/cooc/NcoocTblBody-%s-thru-%s.csv', PATH, startId, endId)
	myWriteCSV(NcoocTblBody, file=outFile) 
	outFile = sprintf('%s/dissertationData/cooc/NcoocTblTitle%s-thru-%s.csv', PATH, startId, endId)
	myWriteCSV(NcoocTblTitle, file=outFile) 
}

runGenNcoocTblSO1thru100 <- function() genNcoocTblSO(1, 100)
runGenNcoocTblSO1thru1000 <- function() genNcoocTblSO(1, 1000)
runGenNcoocTblSO1thru10000 <- function() genNcoocTblSO(1, 10000)
runGenNcoocTblSO1thru100000 <- function() genNcoocTblSO(1, 100000)

addPostSubsets <- function() {
	postIdsTbl = data.table(sqldf('select id from posts where post_type_id = 1'))
	postIdsTbl[, post_id := id][, id := NULL]
	postIdsTbl[, id := sample(1:length(post_id), size=length(post_id))]
	postIdsTbl[, group_name := 'SOShuffledFull']
	setcolorder(postIdsTbl, c('post_id', 'id', 'group_name'))
	setkey(postIdsTbl, group_name, id)
	sqldf('select count(*) from post_subsets')
	withDBConnect(dbCon, dbWriteTable(dbCon, "post_subsets", postIdsTbl, append=T, row.names=0))
}

curWS <- function() {
	runGenNcoocTblSO1thru100000()
	test_dir(sprintf("%s/%s", PATH, 'tests'), reporter='summary')
	runTFollow1k()
	runSO1kr2()
	.ls.objects(order.by='Size')
	tweetsTbl = getTweetsTbl("select * from tweets limit 100000")
	tweetsTbl = getTweetsTbl("select * from tweets where user_screen_name='eddieizzard'")
	# Checking that tweets for twitter users from each followers_count,statuses_count scale are being collected properly
	usersWithTweetsTbl = data.table(sqldf("select distinct on (user_id) t.user_screen_name,u.followers_count,u.statuses_count from tweets as t join twitter_users as u on t.user_screen_name = u.user_screen_name"))
	usersWithTweetsTbl
	usersWithTweetsTbl[order(followers_count), plot(log10(followers_count))]
	usersWithTweetsTbl[order(statuses_count), plot(log10(statuses_count))]
	usersWithTweetsTbl[order(followers_count),][followers_count > 10000000]
	tweetsTbl
	hashtagsTbl
	compareHashtagTbls()[N!=N.1]
	getHashtagEntropy(hashtagsTbl)
	tusersTbl = getTusersTbl()
	tusersTbl
	tusersTbl[order(rank, decreasing=T)][20000:30000][, plot(1:length(followers_count), followers_count)]

	extremesTbl = summarizeExtremes(hashtagsTbl)
	extremesTbl
	summarizeExtremes(hashtagsTbl[user_screen_name=='eddieizzard'])

	modelVsPredTbl = buildTables(file_path_sans_ext(list.files(path=modelVsPredDir())))

	joinTbl = modelVsPredTblBig[topHashtag & hashtagUsedP][extremesTbl, allow.cartesian=T][maxNP==T]
	joinTbl[, list(user_screen_name, best=N/totN, r=NRecency/totN, f=NFrequency/totN)][, list(user_screen_name, best-r, best-f)][, lapply(list(V2, V3), mean),]
	joinTbl[, list(user_screen_name, best=N/totN, r=NRecency/totN, f=NFrequency/totN)][, list(user_screen_name, best-r, best-f)][, hist(V2)]
}

#curWS()

