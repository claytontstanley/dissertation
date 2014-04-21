library(tools)
library(RPostgreSQL)
library(lavaan)
library(Rmisc)
require(gridExtra)
library(memoise)
library(microbenchmark)
library(popbio)
library(stringr)
library(assertthat)
library(tm)
library(tau)
library(XML)
library(Matrix)
library(utils)
library(testthat)
library(ROCR)
library(popbio)
library(QuantPsyc)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(magrittr)
library(sqldf)
library(data.table)
library(reshape2)

PATH = getPathToThisFile()
FILE = getNameOfThisFile()

USER = Sys.getenv("USER")
options(sqldf.RPostgreSQL.user = USER,
	sqldf.RPostgreSQL.dbname = USER)
options("scipen"=100, "digits"=4)
options(error=traceback)

withDBConnect <- function(var, thunk) {
	var = substitute(var)
	eval(bquote(assign(.(deparse(var)), dbConnect(PostgreSQL(), host="localhost", user=USER, dbname=USER))), envir=parent.frame())
	on.exit(eval(bquote(dbDisconnect(.(var))), envir=parent.frame()))
	eval(substitute(thunk), envir=parent.frame())
}

# Interface to retrieve chunkHash for chunk name
getHashes <- function(vals, db) {
	ret = db[match(vals, names(db))]
	ret = ret[!is.na(ret)]
	#myStopifnot(length(ret) > 0)
	debugPrint(str_c(length(vals), "->", length(ret)))
	ret
}

# And vice versa
getVals <- function(hashes, db) {
	names(db[match(hashes, db)])
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

myGetGlobal <- function(...) {
	get(..., envir=globalenv())
}

myStopifnot <- function(...) {
	debugPrint(substitute(...))
	stopifnot(...)
}

savePlotsP = T

myPlotPrint <- function(fig, name) {
	dev.new()
	myLog(fig)
	if (savePlotsP) ggsave(filename=sprintf('%s/figures/%s.pdf', PATH, name), plot=fig)
	fig
}

html2txt2 <- function(vect) {
	inFile = tempfile(pattern='htmlIn-', tmpdir='/tmp', fileext='.csv')
	outFile = tempfile(pattern='htmlOut-', tmpdir='/tmp', fileext='.csv')
	myWriteCSV(vect, file=inFile)
	cmd = '/opt/local/bin/python'
	args = sprintf('%s/lib/html2txt.py %s %s', PATH, inFile, outFile)
	myLog(sprintf('running html2txt with in/out temp files: %s %s', inFile, outFile))
	cmdOut = system2(cmd, args=args)
	res = myReadCSV(outFile, header=F)
	myStopifnot(res$V1 == vect)
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
	myLog(sprintf('Reading results from file "%s"', file))
	setDT(read.csv(file, stringsAsFactors=F, ...))
}

myFread <- function(file, ...) {
	myLog(sprintf('Using fread to read results from file "%s"', file))
	fread(file, ...)
}

myWriteCSV <- function(data, file, ...) {
	myLog(sprintf('Writing results to "%s"', file))
	write.csv(data, file=file, row.names=F, ...)
}

sqldt <- function(...) {
	setDT(sqldf(...))
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
	cmd = sprintf('%s/dissertationData/lib/ark-tweet-nlp-0.3.2/runTagger.sh', PATH)
	args = sprintf('--no-confidence --just-tokenize --quiet %s > %s 2>%s', rawTweetsFile, tokenizedTweetsFile, stderFile)
	myLog(sprintf('running tagger with in/out temp files: %s %s', rawTweetsFile, tokenizedTweetsFile))
	cmdOut = system2(cmd, args=args)
	myLog(paste(readLines(stderFile), sep='\n'))
	tokenTextTbl = data.table(read.delim(tokenizedTweetsFile, sep='\t', quote="", header=F, stringsAsFactors=F))
	myStopifnot(tokenTextTbl[[2]] == stripDelimiters(tweetsTbl[[from]]))
	tweetsTbl[, tokenText := tokenTextTbl[[1]]]
	return()
}

addDtToTbl <- function(tbl) {
	tbl[, dt := creation_epoch - min(creation_epoch), by=user_screen_name]
}

setupTweetsTbl <- function(tweetsTbl, config) {
	addTokenText(tweetsTbl, from='text')
	setkey(tweetsTbl, id)
	addDtToTbl(tweetsTbl)
	tweetsTbl
}

getTweetsTbl <- function(sqlStr=sprintf("select %s from tweets limit 10000", defaultTCols), config) {
	allowedRetweetedVals = if (getConfig(config, "includeRetweetsP")) c('True', 'False') else c('False')
	tweetsTbl = sqldt(sqlStr)[retweeted %in% allowedRetweetedVals]
	myStopifnot(unique(tweetsTbl$retweeted) %in% allowedRetweetedVals)
	setupTweetsTbl(tweetsTbl, config)
	tweetsTbl
}

getTagSynonymsTbl <- function(sqlStr='select * from tag_synonyms') {
	tagSynonymsTbl = sqldt(sqlStr)
	setkey(tagSynonymsTbl, source_tag_name)
	tagSynonymsTbl
}

setupPostsTbl <- function(postsTbl, config) {
	setkey(postsTbl, id)
	myStopifnot(!duplicated(postsTbl$id))
	postsTbl[, tagsNoHtml := html2txt2(tags)]
	addDtToTbl(postsTbl)
	postsTbl
}

getPostsTbl <- function(sqlStr, config) {
	postsTbl = sqldt(sqlStr)
	setupPostsTbl(postsTbl, config)
	postsTbl
}

matchWhitespace = '\\S+'
matchHashtag = '^#'
matchTag = '(?<=<)[^>]+(?=>)'

getHashtagsTbl <- function(tweetsTbl, config) {
	from = getConfig(config, "from")
	tokenizedTbl = getTokenizedTbl(tweetsTbl, from=from, regex=matchWhitespace)
	htOfTokenizedTbl = tokenizedTbl[grepl(matchHashtag, chunk),]
	myStopifnot(c('id') == key(htOfTokenizedTbl))
	hashtagsTbl = htOfTokenizedTbl[tweetsTbl, nomatch=0][, list(id=id, hashtag=chunk, pos=pos, created_at=created_at, dt=dt, user_id=user_id, user_screen_name=user_screen_name)]
	setkey(hashtagsTbl, user_screen_name, dt, hashtag)
	hashtagsTbl
}

convertTagSynonyms <- function(tokenizedTbl) {
	withKey(tokenizedTbl, chunk,
		{tokenizedTbl[getTagSynonymsTbl(), chunk := target_tag_name]
		})
}

getTagsTbl <- function(postsTbl, config) {
	tokenizedTbl = getTokenizedTbl(postsTbl, from='tagsNoHtml', regex=matchTag)
	if (getConfig(config, "convertTagSynonymsP")) convertTagSynonyms(tokenizedTbl)
	tagsTbl = tokenizedTbl[postsTbl, nomatch=0][, list(id=id, hashtag=chunk, pos=pos, created_at=creation_date, dt=dt, user_id=owner_user_id, user_screen_name=user_screen_name)]
	setkey(tagsTbl, user_screen_name, dt, hashtag)
	tagsTbl
}

getTusersTbl <- function() {
	tusersTbl = sqldt('select * from twitter_users')
	tusersTbl[, rank := order(followers_count, decreasing=T)]
	setkey(tusersTbl, id)
	tusersTbl
}

getSOusersTbl <- function() {
	sousersTbl = sqldt('select * from users limit 1000')
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

computeActPrior <- function(hashtags, dtP, cTime, d) {
	myStopifnot(length(dtP) == length(hashtags))
	myStopifnot(length(dtP) == length(cTime))
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

computeActPriorForUser <- function(hashtag, dt, ds, user_screen_name) {
	#myLog(sprintf('computing partial activation for user %s', user_screen_name))
	retIndeces = which(!duplicated(dt))[-1]
	myStopifnot(length(retIndeces) > 0)
	partialRes = data.table(i=retIndeces)
	partialRes = partialRes[, list(hashtag=hashtag[1:i], dtP=dt[1:i], cTime=dt[i]), by=i]
	partialRes = with(partialRes, as.data.table(computeActPrior(hashtag, dtP, cTime, d=ds)))
	partialRes
}



getModelHashtagsTbl <- function(partialRes) {
	debugPrint(partialRes)
	#myLog('setting key for partial table')
	setkeyv(partialRes, c('user_screen_name','dt','hashtag','d'))
	#myLog('computing activations across table')
	res = partialRes[, list(N=.N,
				actPriorStd=log(sum(partialAct)),
				actPriorOL=if (is.na(d[1])) numeric(0) else if (d[1]>=1) NaN else log(.N/(1-d))-d*log(dt),
				actPriorOL2=if (is.na(d[1])) numeric(0) else if (d[1]>=1) NaN else log(.N/(1-d))-d*log(max(dtP))), keyby=list(user_screen_name, dt, hashtag, d)]
	with(res, myStopifnot(!is.infinite(actPriorStd)))
	with(res, myStopifnot(!is.infinite(actPriorOL2)))
	res
}

computeActPriorByUser <- function(hashtagsTbl, ds) {
	partialRes = hashtagsTbl[, computeActPriorForUser(hashtag, dt, ds, user_screen_name), by=user_screen_name]
	modelHashtagsTbl = getModelHashtagsTbl(partialRes)
	modelHashtagsTbl
}

getPriorAtEpoch <- function(priorTbl, cEpoch, d) {
	myLog(sprintf('computing prior at epoch %s', cEpoch))
	curUserPriorTbl = priorTbl[creation_epoch <= cEpoch]
	curUserPriorTbl
	if (nrow(curUserPriorTbl) > 0) {
		curUserPriorTbl[, cTime := cEpoch - min(creation_epoch)]
	} else {
		curUserPriorTbl[, cTime := numeric(0)]
	}
	partialRes = curUserPriorTbl[, computeActPrior(hashtag, dt, cTime, d), by=user_screen_name]
	modelHashtagsTbl = getModelHashtagsTbl(partialRes)
	modelHashtagsTbl
}

getPriorForUserAtEpoch <- function(priorTblUser, userScreenName, cEpoch, d) {
	getPriorAtEpoch(priorTblUser[J(userScreenName), nomatch=0], cEpoch, d)
}

visHashtags <- function(hashtagsTbl) {
	plots = hashtagsTbl[, list(resPlots=list(ggplot(.SD, aes(x=hashtag, y=dt)) +
						 geom_point() +
						 defaultGGPlotOpts + 
						 ylab('Time Since First Post (s)') +
						 xlab('Hashtag') + 
						 theme(axis.text.x = element_blank()))), by=user_screen_name]
	plots
}

visCompare <- function(hashtagsTbl, modelHashtagsTbl, bestDTbl) {
	myStopifnot(sort(unique(hashtagsTbl$user_screen_name)) == sort(unique(modelHashtagsTbl$user_screen_name)))
	plotBuildFun <- function(modelHashtagsTbl, userScreenName, d) {
		list(resPlots=list(ggplot(hashtagsTbl[user_screen_name==userScreenName], aes(x=hashtag, y=dt)) +
				   geom_point() +
				   ggtitle(sprintf('d=%s', d)) + 
				   geom_point(data=modelHashtagsTbl, aes(x=hashtag, y=dt), colour="red", size=.7) +
				   defaultGGPlotOpts + 
				   ylab('Time Since First Post (s)') +
				   xlab('Hashtag') + 
				   theme(axis.text.x = element_blank())
				   ))
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
	hashtagPlots[, list(resPlots=list(myPlotPrint(resPlots[[1]], sprintf('HTByTime-%s', .BY[1])))), by=list(user_screen_name)]
	fullPlots = modelPlots[, list(resPlots=list(do.call(arrangeGrob, c(hashtagPlots[user_screen_name]$resPlots, resPlots)))), by=user_screen_name]
	fullPlots[, list(resPlots=list(myPlotPrint(resPlots[[1]], sprintf('HTMByTime-%s', .BY[1])))), by=list(user_screen_name)]
}

topHashtagDVFromActDV <- function(actDV) {
	res = c(topHashtagPostDVFromActDV(actDV),
		topHashtagAcrossDVFromActDV(actDV))
	res
}

topHashtagAcrossDVFromActDV <- function(actDV) {
	myStopifnot(grepl(pattern='^act', x=actDV))
	actDV = gsub(pattern='_act', replacement='', x=actDV)
	res = gsub(pattern='^act', replacement='topHashtagAcross', x=actDV)
	res
}

topHashtagPostDVFromActDV <- function(actDV) {
	myStopifnot(grepl(pattern='^act', x=actDV))
	actDV = gsub(pattern='_act', replacement='', x=actDV)
	res = gsub(pattern='^act', replacement='topHashtagPost', x=actDV)
	res
}


addMetrics <- function(hashtagsTbl, modelHashtagsTbl, config) {
	actDVs = getConfig(config, 'actDVs')
	tagCountTbl = hashtagsTbl[, list(tagCountN=.N), by=list(user_screen_name, dt)]
	tagCountTbl
	modelHashtagsTbl
	modelHashtagsTbl[tagCountTbl, tagCount := tagCountN, nomatch=0]
	modelHashtagsTbl[tagCountTbl[, list(tagCountUserN=sum(tagCountN)), keyby=user_screen_name], tagCountUser := tagCountUserN]
	myLog('adding metrics for modelHashtagsTbl')
	addDVCols <- function(col, newDVPost, newDVAct) {
		col = as.symbol(col)
		newDVPost = as.symbol(newDVPost)
		newDVAct = as.symbol(newDVAct)
		expr = bquote(modelHashtagsTbl[order(.(col), decreasing=T), .(newDVPost) := 1:length(.(col)) <= tagCount[1], by=list(user_screen_name, dt, d)])
		eval(expr)
		expr = bquote(modelHashtagsTbl[order(.(col), decreasing=T), .(newDVAct) := 1:length(.(col)) <= tagCountUser[1], by=list(user_screen_name, d)])
		eval(expr)
	}
	for (actDV in actDVs) {
		actDV
		do.call(addDVCols, as.list(c(actDV, topHashtagDVFromActDV(actDV))))
	}
	myStopifnot(key(modelHashtagsTbl) == (c('user_screen_name', 'dt', 'hashtag', 'd')))
	myStopifnot(key(hashtagsTbl) == (c('user_screen_name', 'dt', 'hashtag')))
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
	myStopifnot(any(bool == T))
	ret = rep(F, length(bool))
	ret[which(bool)[1]] = T
	ret
}

guardAllEqualP <- function(vect) {
	myStopifnot(length(unique(vect)) <= 1)
	vect
}

modelVsPredForDV <- function(modelHashtagsTbl, DVName) {
	tempTbl = modelHashtagsTbl[, list(NCell=.N, DVName=DVName), by=c('user_screen_name', DVName, 'hashtagUsedP', 'd')]
	setnames(tempTbl, DVName, 'topHashtag')
	tempTbl
}

modelVsPredForDVs <- function(modelHashtagsTbl, dvs) {
	dvTbl = data.table(dv=dvs)
	resTbl = dvTbl[, modelVsPredForDV(modelHashtagsTbl, dv), by=dv][, dv := NULL]
}

getModelVsPredTbl <- function(modelHashtagsTbl, hashtagsTbl, config) {
	dvs = c(sapply(getConfig(config, 'actDVs'), topHashtagDVFromActDV))
	modelVsPredTbl = modelVsPredForDVs(modelHashtagsTbl, dvs)
	modelVsPredTbl[, maxNP := NCell==max(NCell), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	# TODO: Doesn't using the maxNP closest to the center of all of the maxNP's create an artifact for low N when all ds are MaxNP's?
	modelVsPredTbl[maxNP==T, maxNP := onlyFirstT(abs(d-mean(d)) == min(abs(d-mean(d)))), by=list(user_screen_name, topHashtag, hashtagUsedP, DVName)]
	modelVsPredTbl[, totN := NA_integer_] # Making sure that the totN column is added, even if model never generates an activation value for a hashtag that is used
	modelVsPredTbl[hashtagUsedP==T, totN := length(hashtagsTbl[user_screen_name]$user_screen_name), by=user_screen_name]
	modelVsPredTbl
}

writeModelVsPredTbl <- function(modelVsPredTbl, outFile) {
	setkey(modelVsPredTbl, user_screen_name, DVName, d, topHashtag, hashtagUsedP)
	myWriteCSV(modelVsPredTbl, file=outFile)
}

writeModelHashtagsTbl <- function(modelHashtagsTbl, outFile) {
	setkey(modelHashtagsTbl, user_screen_name, id, hashtag)
	myWriteCSV(modelHashtagsTbl, file=outFile)
}

writeLogregTbl <- function(logregTbl, outFile) {
	setkey(logregTbl, name, predName, d)
	myWriteCSV(logregTbl, file=outFile)
}

genAggModelVsPredTbl <- function(hashtagsTbl, config) {
	outFile = getConfig(config, "modelVsPredOutFile")
	ds = getConfig(config, "ds")
	modelHashtagsTbls = data.table()
	getModelVsPredTblFromHashtagsTbl <- function(hashtagsTbl, ds, userScreenName) {
		myLog(sprintf('generating model predictions for user %s', userScreenName))
		modelHashtagsTbl = rbindlist(lapply(ds, function(d) computeActPriorByUser(hashtagsTbl, d=d)))
		setkey(modelHashtagsTbl, user_screen_name, dt, hashtag, d)
		addMetrics(hashtagsTbl, modelHashtagsTbl, config)
		modelVsPredTbl = getModelVsPredTbl(modelHashtagsTbl, hashtagsTbl, config)	
		if (getConfig(config, "accumModelHashtagsTbl") == T) modelHashtagsTbls <<- rbind(modelHashtagsTbls, modelHashtagsTbl)
		rm(modelHashtagsTbl)
		gc()
		modelVsPredTbl
	}
	singleHashtagUsers = hashtagsTbl[, list(uniqueDTs=length(unique(dt)) <= 1), by=user_screen_name][uniqueDTs==T]$user_screen_name
	myLog(sprintf('not running users (%s) since they all have less than two dt hashtag observations', paste0(singleHashtagUsers, collapse=',')))
	users = data.table(cur_user_screen_name=Filter(function(v) !(v %in% singleHashtagUsers), unique(hashtagsTbl$user_screen_name)))
	res = users[, getModelVsPredTblFromHashtagsTbl(hashtagsTbl[cur_user_screen_name], ds, cur_user_screen_name), by=cur_user_screen_name]
	res[, cur_user_screen_name := NULL]
	writeModelVsPredTbl(res, outFile)
	list(modelVsPredTbl=res, modelHashtagsTbl=modelHashtagsTbls)
}

visModelVsPredTbl <- function(modelVsPredTbl) {
	assign('p1', ggplot(modelVsPredTbl[predUsedBest == T & d <= 2], aes(totN, d)) +
	       geom_point() +
	       defaultGGPlotOpts + 
	       xlab('Total Number of Hashtags'))
	modelVsPredTbl[topHashtag & hashtagUsedP, meanPC := mean(acc), by=user_screen_name]
	modelVsPredTbl[topHashtag & hashtagUsedP, relPC := acc - mean(acc), by=user_screen_name]
	modelVsPredTbl[topHashtag & hashtagUsedP, meanRelPC := mean(relPC), by=d]
	assign('p2', ggplot(modelVsPredTbl[topHashtag & hashtagUsedP], aes(log(d),relPC)) +
	       geom_point() +
	       geom_line(aes(log(d), meanRelPC, group=user_screen_name[1])) +
	       defaultGGPlotOpts + 
	       xlab('log(d)') +
	       ylab('Normalized Mean'))
	assign('p3', ggplot(modelVsPredTbl[topHashtag & hashtagUsedP & user_screen_name %in% sample(unique(user_screen_name), size=min(20, length(unique(user_screen_name))))],
			    aes(log(d),acc, group=as.factor(user_screen_name))) +
	       geom_line() +
	       defaultGGPlotOpts + 
	       ylab('Accuracy'))
	modelVsPredTbl[d <= .4]
	modelVsPredTbl[predUsedBest == T & d <= .4]
	assign('p4', ggplot(modelVsPredTbl[predUsedBest == T & d <= 2], aes(d)) +
	       geom_histogram(breaks=defaultBaseConfig$ds[defaultBaseConfig$ds <= min(2, max(modelVsPredTbl[,d+.1]))], aes(y = ..density..)) +
	       geom_density() +
	       defaultGGPlotOpts + 
	       xlab('Best Fit Decay Rate Parameter (d) by Subject') +
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
						 dsetGroup=guardAllEqualP(dsetGroup)[1],
						 #sdCI=sqrt(CIVar(d)), sdCI1=sqrt(CIVar2(d)), meanCI=CI(d))
						 sd=sd(d)), by=list(datasetNameRoot, runNum, DVName)]
}

getDirGen <- function(tblDir) {
	sprintf('%s/dissertationData/%s', PATH, tblDir)
}

getDirModelVsPred <- function() getDirGen('modelVsPred')

getDirHashtagsTbl <- function() getDirGen('hashtagsTbl')

getDirModelHashtags <- function() getDirGen('modelHashtagsTbl')

getDirLogreg <- function() getDirGen('logregTbl')

getDirCooc <- function() getDirGen('cooc')

getDirPrior <- function() getDirGen('prior') 

getOutFileGen <- function(dir, name) {
	sprintf('%s/%s.csv', dir, name)
}

getOutFileModelVsPred <- function(name) getOutFileGen(getDirModelVsPred(), name)

getOutFileHashtags <- function(name) getOutFileGen(getDirHashtagsTbl(), name)

getOutFileModelHashtags <- function(name) getOutFileGen(getDirModelHashtags(), name)

getLogregOutFile <- function(name) getOutFileGen(getDirLogreg(), name)

runPrior <- function(config) {
	myStopifnot(!any(sapply(config,is.null)))
	withProf({
		postsTbl = getConfig(config, "getPostsFun")(getConfig(config, "query"), config=config)
		hashtagsTbl = getConfig(config, "getHashtagsFun")(postsTbl, config=config)
		res = genAggModelVsPredTbl(hashtagsTbl, config=config)
		modelVsPredTbl = res$modelVsPredTbl
		modelHashtagsTbl = res$modelHashtagsTbl
		list(modelVsPredTbl=modelVsPredTbl, modelHashtagsTbl=modelHashtagsTbl, hashtagsTbl=hashtagsTbl)
	})
}

modConfig <- function(config, mods) {
	newConfig = config
	myStopifnot(names(mods) %in% names(config))
	for(modName in names(mods)) {
		newConfig[[modName]] = mods[[modName]]
	}
	newConfig
}

getConfig <- function(config, slot) {
	debugPrint(sprintf('Getting slot %s in config %s', slot, deparse(substitute(config))))
	myStopifnot(!duplicated(names(config)))
	myStopifnot(slot %in% names(config))
	config[[slot]]
}

defaultBaseConfig = list(ds=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2,5,10,20),
			 dStd=c(0.5,0.7),
			 modelVsPredOutFile='/tmp/modelVsPred.csv',
			 modelHashtagsOutFile='',
			 logregOutFile='',
			 actDVs = c('actPriorStd', 'actPriorOL', 'actPriorOL2'),
			 permNRows = 2048,
			 query=NULL)

defaultTConfig = c(defaultBaseConfig,
		   list(from='tokenText',
			accumModelHashtagsTbl=F,
			getPostsFun=getTweetsTbl,
			getHashtagsFun=getHashtagsTbl,
			makeChunkTblFun='make_chunk_table_Twitter',
			tokenizedChunkTypeTbl = 'top_hashtag_tokenized_chunk_types',
			tokenizedTypeTypeTbl = 'top_hashtag_tokenized_type_types',
			tokenizedTbl = 'top_hashtag_tokenized',
			postsTbl = 'top_hashtag_tweets',
			subsetsTbl = 'top_hashtag_subsets',
			tagTypeName = 'hashtag',
			groupName = '2014-02-27 17:13:30 initial',
			allGroupNames = c('2014-02-27 17:13:30 initial',
					  '2014-03-17 11:28:15 trendsmap',
					  '2014-03-24 13:06:19 trendsmap',
					  '2014-04-04 15:03:59 trendsmap'),
			priorTbl = 'priorTblGlobT',
			sjiTbl = 'sjiTblTOrderless',
			getTokenizedFromSubsetFun='getTokenizedFromSubsetT',
			includeRetweetsP=F))

defaultSOConfig = c(defaultBaseConfig,
			 list(convertTagSynonymsP=T,
			      accumModelHashtagsTbl=F,
			      getPostsFun=getPostsTbl,
			      getHashtagsFun=getTagsTbl,
			      tokenizedChunkTypeTbl = 'post_tokenized_chunk_types',
			      tokenizedTypeTypeTbl = 'post_tokenized_type_types',
			      tokenizedTbl = 'post_tokenized',
			      subsetsTbl = 'post_subsets',
			      postsTbl = 'posts',
			      tagTypeName = 'tag',
			      groupName = 'SOShuffledFull',
			      priorTbl = 'priorTblUserSO',
			      sjiTbl = 'sjiTblSOOrderless',
			      getTokenizedFromSubsetFun='getTokenizedFromSubsetSO',
			      makeChunkTblFun='make_chunk_table_SO'
			      ))

defaultPermConfig = list(permUseEntropyP='')

defaultSjiConfig = list(computeActFromContextTbl = 'computeActSjiFromContextTbl')

makeRunTbl <- function(runs) {
	makeTbl <- function(run) {
		name = paste(run, collapse='_', sep='')
		data.table(name=name, predName=run)
	}
	rbindlist(lapply(runs, makeTbl))
}

defaultTPermConfig = modConfig(c(defaultTConfig, defaultPermConfig,
				 list(runTbl=makeRunTbl(list(c('actPriorStd', 'actTweetOrder', 'actTweetOrderless'),
							     c('actTweetOrder', 'actTweetOrderless'),
							     c('actPriorStd', 'actTweetOrder'),
							     c('actPriorStd', 'actTweetOrderless'),
							     c('actPriorStd', 'actTweetOrderEntropy', 'actTweetOrderlessEntropy'),
							     c('actTweetOrderEntropy', 'actTweetOrderlessEntropy'))),
				      permEnvTbl='permEnvTblT',
				      permMemMatOrder='permMemMatTOrder',
				      permMemMatOrderless='permMemMatTOrderless',
				      computeActFromContextTbl = 'computeActPermTFromContextTbl')),
			       list(actDVs=c('actPriorStd_actTweetOrder_actTweetOrderless', 'actPriorStd',
					     'actTweetOrder', 'actTweetOrderless', 'actTweetOrder_actTweetOrderless',
					     'actPriorStd_actTweetOrder', 'actPriorStd_actTweetOrderless',
					     'actPriorStd_actTweetOrderEntropy_actTweetOrderlessEntropy',
					     'actTweetOrderlessEntropy', 'actTweetOrderEntropy', 'actTweetOrderEntropy_actTweetOrderlessEntropy')))

defaultSOPermConfig = modConfig(c(defaultSOConfig, defaultPermConfig,
				  list(runTbl=makeRunTbl(list(c('actPriorStd', 'actTitleOrderless', 'actBodyOrderless'),
							      c('actTitleOrderless', 'actBodyOrderless'),
							      c('actPriorStd', 'actTitleOrderless'),
							      c('actPriorStd', 'actBodyOrderless'),
							      c('actPriorStd', 'actTitleOrderlessEntropy', 'actBodyOrderlessEntropy'),
							      c('actTitleOrderlessEntropy', 'actBodyOrderlessEntropy'))),
				       permEnvTbl='permEnvTblSO',
				       permMemMatOrder='',
				       permMemMatOrderless='permMemMatSOOrderless',
				       computeActFromContextTbl = 'computeActPermSOFromContextTbl')),
				list(actDVs=c('actPriorStd_actTitleOrderless_actBodyOrderless', 'actPriorStd',
					      'actTitleOrderless', 'actBodyOrderless', 'actTitleOrderless_actBodyOrderless',
					      'actPriorStd_actTitleOrderless', 'actPriorStd_actBodyOrderless',
					      'actPriorStd_actTitleOrderlessEntropy_actBodyOrderlessEntropy',
					      'actTitleOrderlessEntropy_actBodyOrderlessEntropy',
					      'actTitleOrderlessEntropy', 'actBodyOrderlessEntropy')))

defaultTSjiConfig = modConfig(c(defaultTConfig, defaultSjiConfig,
				list(runTbl=makeRunTbl(list(c('actPriorStd', 'actTweet'))))),
			      list(actDVs=c('actPriorStd_actTweet', 'actPriorStd', 'actTweet')))

defaultSOSjiConfig = modConfig(c(defaultSOConfig, defaultSjiConfig,
				 list(runTbl=makeRunTbl(list(c('actPriorStd', 'actTitle', 'actBody'),
							     c('actTitle', 'actBody'))))),
			       list(actDVs=c('actPriorStd_actTitle_actBody', 'actPriorStd', 'actTitle', 'actBody', 'actTitle_actBody')))

defaultGGPlotOpts <- theme_bw() + theme_classic()

runPriorT <- function(config=defaultTConfig) {
	runPrior(config)
}

runPriorSO <- function(config=defaultSOConfig) {
	runPrior(config)
}

defaultTCols = "id::text, user_id, user_screen_name, created_at, retweeted, in_reply_to_status_id, lang, truncated, text, creation_epoch"

getQueryUsersSubset <- function(val, from) {
	sprintf('select user_screen_name from twitter_users where %s > %d order by %s asc limit 100', from, val, from)
}

getQueryGeneralT <- function(val, from, filters) {
	sprintf('select %s from tweets where %s and user_screen_name in (%s)', defaultTCols, filters, getQueryUsersSubset(val, from))
}

getQueryT <- function(val, filters='1=1') {
	getQueryGeneralT(val, 'followers_count', filters)
}

getQueryTStatuses <- function(val, filters='1=1') {
	getQueryGeneralT(val, 'statuses_count', filters)
}

defaultSOCols = sprintf('id, owner_user_id, user_screen_name, creation_date, creation_epoch, title, tags')

getQuerySO <- function(val) {
	sprintf('select %s from posts
		where post_type_id = 1 and owner_user_id in (select id from users where reputation > %d order by reputation asc limit 500)', defaultSOCols, val)
}

getQuerySOQ <- function(val) {
	sprintf("select %s from posts
		where post_type_id = 1 and owner_user_id in (select id from users where num_questions > %d order by num_questions asc limit 100)", defaultSOCols, val)
}

combineFilters <- function(f1, f2='1=1') {
	paste(f1, f2, sep=' and ')
}

makeTRun <- function(val, outFileName, config) {
	function() runPriorT(config=modConfig(config, list(query=getConfig(config, "query")(val), modelVsPredOutFile=getOutFileModelVsPred(outFileName))))
}

makeTRunr1 <- function(val, outFileName, query) {
	makeTRun(val, outFileName, config=modConfig(defaultTConfig, list(query=query, includeRetweetsP=T)))
}

makeTRunr2 <- function(val, outFileName, query) {
	makeTRun(val, outFileName, config=modConfig(defaultTConfig, list(query=query, includeRetweetsP=F)))
}

queryRunTFollow1k = getQueryT
queryRunTFollow5k = function(val) getQueryT(val, filters="user_screen_name != 'g4scareers'")
queryRunTFollow10k = function(val) getQueryT(val, filters="user_screen_name != 'so_pr'")
queryRunTFollow100k = function(val) getQueryT(val, filters="user_screen_name != 'hermosa_brisa'")
queryRunTFollow1M = getQueryT
queryRunTFollow10M = function(val) getQueryT(val, filters='tweets.id != 12466832063')

queryRunTTweets1e2 = getQueryTStatuses
queryRunTTweets5e2 = getQueryTStatuses
queryRunTTweets1e3 = getQueryTStatuses
queryRunTTweets5e3 = getQueryTStatuses
queryRunTTweets1e4 = getQueryTStatuses
queryRunTTweets5e4 = function(val) getQueryTStatuses(val, filters="user_screen_name != 'stanhjerleid'")

runTQueries = list(queryRunTFollow1k(1000),
		   queryRunTFollow5k(5000),
		   queryRunTFollow10k(10000),
		   queryRunTFollow100k(100000),
		   queryRunTFollow1M(1000000),
		   queryRunTFollow10M(10000000),
		   queryRunTTweets1e2(100),
		   queryRunTTweets5e2(500),
		   queryRunTTweets1e3(1000),
		   queryRunTTweets5e3(5000),
		   queryRunTTweets1e4(10000),
		   queryRunTTweets5e4(50000))

runSOQueries = list(getQuerySO(100000),
		    getQuerySO(50000),
		    getQuerySO(10000),
		    getQuerySO(5000),
		    getQuerySO(1000),
		    getQuerySO(500),
		    getQuerySOQ(500),
		    getQuerySOQ(400),
		    getQuerySOQ(300),
		    getQuerySOQ(200),
		    getQuerySOQ(100),
		    getQuerySOQ(050)
		    )


getTUsersFromRunQuery <- function(runQuery) {
	sqldt(sprintf("select distinct user_screen_name from (%s) as foo", runQuery))[, dataset := 'twitter']
}

getTPostsFromRunQuery <- function(runQuery) {
	sqldt(sprintf('select count(*) from (%s) as foo', runQuery))[, dataset := 'twitter']
}

getSOUsersFromRunQuery <- function(runQuery) {
	sqldt(sprintf("select distinct owner_user_id from (%s) as foo", runQuery))[, dataset := 'stackoverflow']
}

getSOPostsFromRunQuery <- function(runQuery) {
	sqldt(sprintf('select count(*) from (%s) as foo', runQuery))[, dataset := 'stackoverflow']
}

getSummaryStats <- function() {
	tUsersFromRunsTbl = rbindlist(lapply(runTQueries, getTUsersFromRunQuery))
	tPostsCntFromRunsTbl = rbindlist(lapply(runTQueries, getTPostsFromRunQuery))
	SOUsersFromRunsTbl = rbindlist(lapply(runSOQueries, getSOUsersFromRunQuery))
	SOPostsCntFromRunsTbl = rbindlist(lapply(runSOQueries, getSOPostsFromRunQuery))
	usersFromRunsTbl = rbind(tUsersFromRunsTbl, SOUsersFromRunsTbl, use.names=F)
	unique(usersFromRunsTbl)
	postsCntFromRunsTbl = rbind(tPostsCntFromRunsTbl, SOPostsCntFromRunsTbl)
	postsCntFromRunsTbl[, sum(count), by=dataset]
	modelVsPredTbl[predUsedBest == T][runNum == 2][, list(acc=mean(acc), d=median(d), N=.N), by=list(DVName, dsetType)][DVName %in% c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2')][, mean(acc), by=DVName]
	modelVsPredTbl[hashtagUsedP == T][topHashtag == T][DVName == 'topHashtagAcrossPriorStd'][, sum(totN), by=dsetType]
	modelVsPredTbl[, .N, by=d]
}


runTFollow1k <- makeTRunr1(1000, 'TFollowgt1k', queryRunTFollow1k)
runTFollow1kr2 <- makeTRunr2(1000, 'TFollowgt1kr2', queryRunTFollow1k)
runTFollow5k <- makeTRunr1(5000, 'TFollowgt5k', queryRunTFollow5k) 
runTFollow5kr2 <- makeTRunr2(5000, 'TFollowgt5kr2', queryRunTFollow5k) 
runTFollow10k <- makeTRunr1(10000, 'TFollowgt10k', queryRunTFollow10k) 
runTFollow10kr2 <- makeTRunr2(10000,'TFollowgt10kr2', queryRunTFollow10k) 
runTFollow100k <- makeTRunr1(100000, 'TFollowgt100k', queryRunTFollow100k)
runTFollow100kr2 <- makeTRunr2(100000, 'TFollowgt100kr2', queryRunTFollow100k) 
runTFollow1M <- makeTRunr1(1000000, 'TFollowgt1M', queryRunTFollow1M)
runTFollow1Mr2 <- makeTRunr2(1000000,'TFollowgt1Mr2', queryRunTFollow1M)
runTFollow10M <- makeTRunr1(10000000, 'TFollowgt10M', queryRunTFollow10M) 
runTFollow10Mr2 <- makeTRunr2(10000000, 'TFollowgt10Mr2', queryRunTFollow10M) 
runTTweets1e2 <- makeTRunr1(100, 'TTweetsgt1e2', queryRunTTweets1e2)
runTTweets1e2r2 <- makeTRunr2(100, 'TTweetsgt1e2r2', queryRunTTweets1e2)
runTTweets5e2 <- makeTRunr1(500, 'TTweetsgt5e2', queryRunTTweets5e2)
runTTweets5e2r2 <- makeTRunr2(500, 'TTweetsgt5e2r2', queryRunTTweets5e2)
runTTweets1e3 <- makeTRunr1(1000, 'TTweetsgt1e3', queryRunTTweets1e3)
runTTweets1e3r2 <- makeTRunr2(1000, 'TTweetsgt1e3r2', queryRunTTweets1e3)
runTTweets5e3 <- makeTRunr1(5000, 'TTweetsgt5e3', queryRunTTweets5e3)
runTTweets5e3r2 <- makeTRunr2(5000, 'TTweetsgt5e3r2', queryRunTTweets5e3)
runTTweets1e4 <- makeTRunr1(10000, 'TTweetsgt1e4', queryRunTTweets1e4)
runTTweets1e4r2 <- makeTRunr2(10000, 'TTweetsgt1e4r2', queryRunTTweets1e4)
runTTweets5e4 <- makeTRunr1(50000, 'TTweetsgt5e4', queryRunTTweets5e4)
runTTweets5e4r2 <- makeTRunr2(50000, 'TTweetsgt5e4r2', queryRunTTweets5e4)

makeSORun <- function(val, outFileName, config) {
	runFun = function() runPriorSO(config=modConfig(config, list(query=getConfig(config, "query")(val), modelVsPredOutFile=getOutFileModelVsPred(outFileName))))
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

addRunNum <- function(modelVsPredTbl) {
	modelVsPredTbl
	modelVsPredTbl[, runNum := 1]
	modelVsPredTbl[grepl('r[0-9]$', datasetName), runNum := as.numeric(substr(datasetName, nchar(datasetName), nchar(datasetName)))]
}

addDatasetNameRoot <- function(modelVsPredTbl) {
	modelVsPredTbl[, datasetNameRoot := datasetName]
	modelVsPredTbl[grepl('r[0-9]$', datasetName), datasetNameRoot := substr(datasetName, 1, nchar(datasetName)-2)] 
}

addDatasetType <- function(modelVsPredTbl) {
	modelVsPredTbl[, dsetType := 'unknown']
	modelVsPredTbl[grepl('^SO', datasetName), dsetType := 'stackoverflow']
	modelVsPredTbl[grepl('^T', datasetName), dsetType := 'twitter']
}

addDatasetGroup <- function(modelVsPredTbl) {
	modelVsPredTbl[, dsetGroup := 'unknown']
	modelVsPredTbl[grepl('^SOQ', datasetName), dsetGroup := 'topQuestions']
	modelVsPredTbl[grepl('^SOg', datasetName), dsetGroup := 'topReputation']
	modelVsPredTbl[grepl('^TT', datasetName), dsetGroup := 'topTweets']
	modelVsPredTbl[grepl('^TF', datasetName), dsetGroup := 'topFollowers']
	modelVsPredTbl[grepl('^TContext', datasetName), dsetGroup := 'topHashtags']
	modelVsPredTbl[grepl('^SOContext', datasetName), dsetGroup := 'sampleAcross']
}

addDatasetSize <- function(modelVsPredTbl) {
	modelVsPredTbl[, dsetSize := NA_integer_]
	modelVsPredTbl[grepl('-200', datasetName), dsetSize := 200]
	modelVsPredTbl[grepl('-20[^0]', datasetName), dsetSize := 20]
	modelVsPredTbl[grepl('-500', datasetName), dsetSize := 500]
}

addModelType <- function(modelVsPredTbl) {
	modelVsPredTbl[, modelType := 'unknown']
	modelVsPredTbl[grepl('ContextSji', datasetName), modelType := 'sji']
	modelVsPredTbl[grepl('ContextPerm', datasetName), modelType := 'perm']
}
buildModelHashtagsTables <- function(outFileNames) {
	buildTable <- function(outFileName) {
		tbl = myFread(getOutFileModelHashtags(outFileName))
		tbl[, datasetName := outFileName]
		tbl
	}
	modelHashtagsTbl = lapply(outFileNames, buildTable)
	for (tbl in modelHashtagsTbl) {
		addRunNum(tbl)
		addDatasetNameRoot(tbl)
		addDatasetType(tbl) 
		addDatasetGroup(tbl)
		addDatasetSize(tbl)
		addModelType(tbl)
	}
	names(modelHashtagsTbl) = outFileNames
	modelHashtagsTbl
}

isContextRun <- function(fname) {
	grepl(pattern = 'Context', x = fname) 
}

isPriorRun <- function(fname) {
	grepl('^(SOg)|(SOQ)|(TTweets)|(TFollow)', fname, perl = T)
}

getConfigFile <- function(fname) {
	stopifnot(isContextRun(fname))
	dataset=''
	model=''
	if (grepl(pattern='ContextSji', x=fname)) model='sji'
	if (grepl(pattern='ContextPerm', x=fname)) model='perm'
	if (grepl(pattern='^SO', x=fname)) dataset='SO'
	if (grepl(pattern='^T', x=fname)) dataset='T'
	stopifnot(dataset != '')
	stopifnot(model != '')
	if (dataset=='T' & model=='sji') res = defaultTSjiConfig
	if (dataset=='T' & model=='perm') res = defaultTPermConfig
	if (dataset=='SO' & model=='sji') res = defaultSOSjiConfig
	if (dataset=='SO' & model=='perm') res = defaultSOPermConfig
	res
}

buildTables <- function(outFileNames) {
	buildTable <- function(outFileName) {
		colClasses = c('character', 'logical', 'logical', 'numeric', 'integer', 'character', 'logical', 'integer')
		tbl = myReadCSV(getOutFileModelVsPred(outFileName), colClasses=colClasses)
		tbl[, datasetName := outFileName]
		tbl
	}
	addMiscellaneous <- function(modelVsPredTbl) {
		modelVsPredTbl[, predUsedBest := F]
		modelVsPredTbl[topHashtag & hashtagUsedP & maxNP, predUsedBest := T]
		modelVsPredTbl[hashtagUsedP == T, acc := NCell/totN]
	}
	modelVsPredTbl = rbindlist(lapply(outFileNames, buildTable))
	modelVsPredTbl
	addRunNum(modelVsPredTbl)
	addDatasetNameRoot(modelVsPredTbl)
	addDatasetType(modelVsPredTbl)
	addDatasetGroup(modelVsPredTbl)
	addDatasetSize(modelVsPredTbl)
	addMiscellaneous(modelVsPredTbl)
	addModelType(modelVsPredTbl)
	stopifnot(nrow(modelVsPredTbl[dsetType == 'unknown']) == 0)
	modelVsPredTbl
}

withCI <- function(dat) {
	res = CI(dat)
	list(N=length(dat), meanVal=res[2], minCI=res[1], maxCI=res[3])
}

getComparisonTbl <- function(SD) {
	resTbl = copy(SD)
	resTbl[, DVDirection := sprintf('%s%s', direction, if (DVName != '') sprintf(' for %s', DVName) else ''), by=list(direction, DVName)]
	resTbl[!is.na(diff), withCI(diff), by=list(DVDirection, direction, DVName)]
}

compare2DVs <- function(modelVsPredTbl, DVs, sortedOrder=c(1,2)) {
	sumTbl = modelVsPredTbl[predUsedBest == T][DVName %in% DVs,]
	setkey(sumTbl, datasetName, user_screen_name, DVName)
	sumTbl[, list(diff=acc[sortedOrder[2]]-acc[sortedOrder[1]], direction=paste(DVName[sortedOrder[2]], '-', DVName[sortedOrder[1]])), by=list(datasetName, user_screen_name)][, DVName := ''][, getComparisonTbl(.SD)]
}

compare2Runs <- function(modelVsPredTbl, runNums) {
	sumTbl = modelVsPredTbl[predUsedBest == T][runNum %in% runNums]
	setkey(sumTbl, datasetNameRoot, DVName, user_screen_name, runNum)
	sumTbl[, list(diff=acc[2]-acc[1], direction=sprintf('run%s - run%s', runNums[2], runNums[1])), by=list(datasetNameRoot, DVName, user_screen_name)][, getComparisonTbl(.SD)]
}

compareDBestVsMin <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[topHashtag & hashtagUsedP & (maxNP | d == min(d))]
	setkey(sumTbl, datasetName, user_screen_name, DVName, maxNP) 
	sumTbl[, list(diff=acc[2]-acc[1], direction=paste('best d', '-', 'min d')), by=list(datasetName, user_screen_name, DVName)][, getComparisonTbl(.SD)]
}

compareDBestVsMax <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[topHashtag & hashtagUsedP & (maxNP | d == max(d))]
	setkey(sumTbl, datasetName, user_screen_name, DVName, maxNP) 
	sumTbl[, list(diff=acc[2]-acc[1], direction=paste('best d', '-', 'max d')), by=list(datasetName, user_screen_name, DVName)][, getComparisonTbl(.SD)]
}

plotBarSumTbl <- function(sumTbl, fillCol, figName, extras=NULL) {
	fillCol = substitute(fillCol)
	renameColDatasetGroup(sumTbl)
	expr = bquote(ggplot(sumTbl, aes(x=factor(dsetGroup), y=meanVal, fill=.(fillCol))) +
		      geom_bar(width=0.7, position=position_dodge(), stat='identity') +
		      geom_errorbar(aes(ymin=minCI, ymax=maxCI), position=position_dodge(width=0.7), width=0.1, size=0.3) + 
		      defaultGGPlotOpts)
	plot = eval(expr)
	lapply(extras, function(extra) plot <<- plot + extra)
	myPlotPrint(plot, figName)
	sumTbl
}

compareMeanDV <- function(modelVsPredTbl, DV, extras=NULL, figName='') {
	DV = substitute(DV)
	modelVsPredTbl
	expr = bquote(tableModelVsPredTbl(modelVsPredTbl)[, withCI(.(DV)), keyby=list(DVName, dsetGroup)])
	expr
	sumTbl = eval(expr)
	sumTbl
	renameColDVName(sumTbl)
	plotBarSumTbl(sumTbl, DVName, sprintf('compareMeanDV-%s-%s', deparse(DV), figName),
		      extras=c(list(theme(legend.position='top', legend.direction='vertical', axis.title.y=element_blank()),
				    guides(fill=guide_legend(title='Model Name', reverse=T)),
				    coord_flip()
				    ), extras))
}

plotDatasetDescriptives <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[runNum==2 & predUsedBest == T & DVName == 'topHashtagPostPriorStd'][, list(NUsers=.N, NHashtagObs=sum(totN)), by=list(datasetName,dsetType,dsetGroup)]
	plotBarSumTbl(sumTbl[, withCI(NUsers), by=list(dsetGroup, datasetName)],
		      datasetName, 'compareNumbers',
		      extras=list(theme(axis.title.x=element_blank()),
				  scale_fill_discrete(guide = "none"),
				  ylab('Number of Users')))
	plotBarSumTbl(sumTbl[, withCI(NHashtagObs), by=list(dsetGroup, datasetName)],
		      datasetName, 'compareHashtagObs',
		      extras=list(theme(axis.title.x=element_blank()),
				  scale_fill_discrete(guide = "none"),
				  ylab('Number of Hashtag Occurrences')))
	sumTbl
}

renameColDatasetGroup <- function(tbl) {
	setkey(tbl, dsetGroup)
	mapTbl = data.table(dsetGroup = c('topReputation', 'topQuestions', 'topFollowers', 'topTweets'),
			    newName = c('SO Top Reputation', 'SO Top Questions', 'Twitter Top Followers', 'Twitter Top Tweets'))
	setkey(mapTbl, dsetGroup)
	tbl[mapTbl, dsetGroup := newName]
	tbl
}

renameDVDirection <- function(tbl) {
	setkey(tbl, DVDirection)
	mapTbl = data.table(DVDirection=c('topHashtagPostPriorStd - topHashtagPostPriorOL2', 'topHashtagPostPriorStd - topHashtagAcrossPriorStd',
					  'best d - min d for topHashtagPostPriorStd', 'best d - max d for topHashtagPostPriorStd',
					  'best d - min d for topHashtagPostPriorOL2', 'best d - max d for topHashtagPostPriorOL2'),
			    newName=c('Standard - Optimized Learning', 'Standard - Standard Relaxed Across Posts', 
				      'Best Fit - All Frequency for Standard Prior Model', 'Best Fit - All Recency for Standard Prior Model',
				      'Best Fit - All Frequency for Optimized Learning Model', 'Best Fit - All Recency for Optimized Learning Model'))
	setkey(mapTbl, DVDirection)
	tbl[mapTbl, DVDirection := newName]
	tbl
}

renameColDVName <- function(tbl) {
	setkey(tbl, DVName)
	mapTbl = data.table(DVName=c('topHashtagAcrossPriorStd', 'topHashtagPostPriorStd', 'topHashtagPostPriorOL2',
				     'topHashtagPostTitle', 'topHashtagPostBody', 'topHashtagPostTitleBody', 'topHashtagPostPriorStdTitleBody',
				     'topHashtagAcrossTitle', 'topHashtagAcrossBody', 'topHashtagAcrossTitleBody', 'topHashtagAcrossPriorStdTitleBody',
				     'topHashtagPostTitleOrderless', 'topHashtagPostBodyOrderless', 'topHashtagPostTitleOrderlessBodyOrderless', 'topHashtagPostPriorStdTitleOrderlessBodyOrderless',
				     'topHashtagAcrossTitleOrderless', 'topHashtagAcrossBodyOrderless', 'topHashtagAcrossTitleOrderlessBodyOrderless', 'topHashtagAcrossPriorStdTitleOrderlessBodyOrderless',
				     'topHashtagPostTweet', 'topHashtagPostPriorStdTweet',
				     'topHashtagAcrossTweet', 'topHashtagAcrossPriorStdTweet',
				     'topHashtagPostTweetOrderless', 'topHashtagPostTweetOrder', 'topHashtagPostTweetOrderTweetOrderless', 'topHashtagPostPriorStdTweetOrderTweetOrderless',
				     'topHashtagAcrossTweetOrderless', 'topHashtagAcrossTweetOrder', 'topHashtagAcrossTweetOrderTweetOrderless', 'topHashtagAcrossPriorStdTweetOrderTweetOrderless',
				     'topHashtagPostPriorStdTitleOrderless', 'topHashtagPostPriorStdBodyOrderless',
				     'topHashtagAcrossPriorStdTitleOrderless', 'topHashtagAcrossPriorStdBodyOrderless',
				     'topHashtagPostPriorStdTweetOrderless', 'topHashtagPostPriorStdTweetOrder',
				     'topHashtagAcrossPriorStdTweetOrderless', 'topHashtagAcrossPriorStdTweetOrder',
				     'topHashtagPostTweetOrderlessEntropy', 'topHashtagPostPriorStdTweetOrderEntropyTweetOrderlessEntropy',
				     'topHashtagPostTitleOrderlessEntropyBodyOrderlessEntropy', 'topHashtagPostPriorStdTitleOrderlessEntropyBodyOrderlessEntropy'),
			    newName=c('Standard Prior Model Relaxed Across Posts', 'Standard Prior Model', 'Optimized Learning Model',
				      'Bayes only title', 'Bayes only body', 'Bayes combined title and body', 'Bayes combined full',
				      'Bayes only title', 'Bayes only body', 'Bayes combined title and body', 'Bayes combined full',
				      'RP only title', 'RP only body', 'RP combined title and body', 'RP combined full',
				      'RP only title', 'RP only body', 'RP combined title and body', 'RP combined full',
				      'Bayes only context', 'Bayes combined full',
				      'Bayes only context', 'Bayes combined full',
				      'RP only orderless context', 'RP only order context', 'RP combined orderless and order', 'RP combined full',
				      'RP only orderless context', 'RP only order context', 'RP combined orderless and order', 'RP combined full',
				      'RP combined prior and title', 'RP combined prior and body',
				      'RP combined prior and title', 'RP combined prior and body',
				      'RP combined prior and orderless', 'RP combined prior and order',
				      'RP combined prior and orderless', 'RP combined prior and order',
				      'RP only orderless context w/ entropy', 'RP combined full w/ entropy',
				      'RP combined title and body w/ entropy', 'RP combined full w/ entropy'))
	setkey(mapTbl, DVName)
	tbl[mapTbl, DVName := newName]
	tbl
}


plotDVDiffs <- function(sumTbl) {
	renameDVDirection(sumTbl)
	plotBarSumTbl(sumTbl, DVDirection, sprintf('compareDVDiffs'), extras=list(theme(legend.position='top', legend.direction='vertical', axis.title.y=element_blank()),
										  #labs(x=element_blank()),
										  labs(y='Mean Difference in Accuracy'),
										  guides(fill=guide_legend(title="Model Comparison", reverse=T)),
										  coord_flip()
										  ))
}

compareOptimalDs <- function(modelVsPredTbl) compareMeanDV(modelVsPredTbl, median, list(labs(y='Mean Optimal Decay Rate Parameter (d)')))
compareOptimalAcc <- function(modelVsPredTbl) compareMeanDV(modelVsPredTbl, acc, list(labs(y='Mean Accuracy')))

wrapQuotes <- function(charVect) {
	withDBConnect(db, as.character(escape(charVect, parens=F, con=db)))
}

plotTemporal <- function(runTbls) {
	bestDTbl = runTbls$modelVsPredTbl[topHashtag & hashtagUsedP & maxNP & DVName=='topHashtagPostPriorStd']
	visCompare(runTbls$hashtagsTbl, runTbls$modelHashtagsTbl[topHashtagPostPriorStd==T], bestDTbl)
}

plotPPVTbl <- function(ppvTbl, figName) {
	ppvTbl = ppvTbl[!is.na(x)][!is.na(y)]
	ppvTbl
	renameColDVName(ppvTbl)
	myPlotPrint(ggplot(ppvTbl, aes(x, y, colour=DVName, linetype=modelType)) + 
	      geom_line() +
	      theme(legend.position='top', legend.direction='vertical') + 
	      guides(linetype=F)
	      #guides(fill=guide_legend(reverse=T)) +
	      , figName)
}

getPPVTblAll <- function(modelHashtagsTbl) {
	bestFitNameTbl = getBestFitNames(modelHashtagsTbl)
	bestFitNameTbl
	ppvTbl = bestFitNameTbl[, getPPVTbl(modelHashtagsTbl, actDVName), by=eval(colnames(bestFitNameTbl))]
	ppvTbl[, DVName := topHashtagAcrossDVFromActDV(actDVName)][, actDVName := NULL]
	ppvTbl
}

analyzeTemporal <- function(modelVsPredTbl) {
	modelVsPredTbl[, unique(datasetName)]
	#screenTbl = modelVsPredTbl[datasetName=='SOQgt300r2'][, list(user_screen_name=wrapQuotes(sample(user_screen_name, 10))), by=list(datasetName, dsetType)]
	#user_screen_names = screenTbl[, user_screen_name]
	user_screen_names = wrapQuotes(c('rickeysmiley','fashionista_com','laurenpope','mtvindia','officialrcti'))
	user_screen_names = wrapQuotes(c('fashionista_com'))
	runTbls = runPriorT(config=modConfig(defaultTConfig, list(accumModelHashtagsTbl=T,
								  query=sprintf("select %s from tweets where user_screen_name in (%s)", defaultTCols, user_screen_names))))
	plotTemporal(runTbls)
	user_screen_names = wrapQuotes(c('520957','238260','413225','807325','521180'))
	user_screen_names = wrapQuotes(c('520957','238260'))
	runTbls = runPriorSO(config=modConfig(defaultSOConfig, list(accumModelHashtagsTbl=T,
								    query=sprintf("select %s from posts where post_type_id = 1 and user_screen_name in (%s)", defaultSOCols, user_screen_names))))
	plotTemporal(runTbls)
}

getBestFitNames <- function(modelHashtagsTbl) {
	modelHashtagsTbl
	bestFitNameTbl = modelHashtagsTbl[, list(datasetNameRoot, modelType, dsetType, dsetSize, runNum)]
	setkeyv(bestFitNameTbl, colnames(bestFitNameTbl))
	bestFitNameTbl = unique(bestFitNameTbl)
	stopifnot(nrow(bestFitNameTbl) == 1)
	config = getConfigFile(bestFitNameTbl[, datasetNameRoot])
	runTbl = getConfig(config, 'runTbl')
	bestFitNameTbl = do.call(data.table, c(bestFitNameTbl, list(actDVName=runTbl[, unique(c(predName, name))])))
	bestFitNameTbl
}

analyzeModelVsPredTbl <- function(modelVsPredTbl) {
	analyzeTemporal(modelVsPredTbl)
	plotDatasetDescriptives(modelVsPredTbl)[, list(meanNUsers=mean(NUsers),totNUsers=sum(NUsers),meanNHO=mean(NHashtagObs),totNHO=sum(NHashtagObs)), by=list(dsetType)]
	# Check that totN calculated makes sense. Result should be small.
	modelVsPredTbl[topHashtag == T & d==min(d)][, list(totN, sum(NCell)), by=list(user_screen_name,d,DVName, datasetName)][!is.na(totN)][,list(res=totN-V2)][, withCI(res)]
	# Check that the Ns for each dataset look right	
	modelVsPredTbl[, list(N=.N, names=list(unique(datasetName))), by=list(dsetType, dsetGroup, runNum,datasetNameRoot)]

	dvDiffsTbl = plotDVDiffs(rbind(modelVsPredTbl[runNum==2, compare2DVs(.SD, c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2'), sortedOrder=c(2,1)), by=list(dsetType, dsetGroup), .SDcols=colnames(modelVsPredTbl)],
				       modelVsPredTbl[runNum==2, compare2DVs(.SD, c('topHashtagPostPriorStd', 'topHashtagAcrossPriorStd')), by=list(dsetType, dsetGroup), .SDcols=colnames(modelVsPredTbl)],
				       #modelVsPredTbl[DVName %in% c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2'), compare2Runs(.SD, c(1,2)), by=list(dsetType, dsetGroup), .SDcols=colnames(modelVsPredTbl)],
				       modelVsPredTbl[runNum==2 & DVName %in% c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2'), compareDBestVsMin(.SD), by=list(dsetType, dsetGroup)],
				       modelVsPredTbl[runNum==2 & DVName %in% c('topHashtagPostPriorStd'), compareDBestVsMax(.SD), by=list(dsetType, dsetGroup)]))
	dvDiffsTbl[, mean(meanVal), by=direction]
	compareOptimalDs(modelVsPredTbl[DVName %in% c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2', 'topHashtagAcrossPriorStd') & runNum == 2])
	compareOptimalAcc(modelVsPredTbl[DVName %in% c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2', 'topHashtagAcrossPriorStd') & runNum == 2])

	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='TFollowgt1kr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='TTweetsgt5e4r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='TTweetsgt1e2r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='SOgt1kr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='SOgt100kr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='SOQgt050r2'])
	
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='TFollowgt10Mr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='SOQgt500r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorOL2' & datasetName=='TFollowgt10Mr2' & d < 1])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorOL2' & datasetName=='SOQgt500r2' & d < 1])
}

analyzeContext <- function(modelHashtagTbls, modelVsPredTbl) {
	#modelHashtagsTbls = Filter(x = modelHashtagsTbls, f = function(tbl) !grepl('-500', tbl[, datasetName[1]]))
	ppvTbl = rbindlist(lapply(modelHashtagsTbls, getPPVTblAll))
	ppvTbl
	modelHashtagsTbls
	modelHashtagsTbl = modelHashtagsTbls[['TContextSji-500g1r5']]
	modelHashtagsTbl
	logit = runLogReg(modelHashtagsTbl, c('actPriorStd', 'actTweet'))
	summary(logit)
	tables()
	print(ClassLog(logit, modelHashtagsTbl$hashtagUsedP))
	modelVsPredTbl
	plotPPVTbl(ppvTbl[dsetType=='stackoverflow' & runNum==1 & dsetSize==500], 'contextPpvSO')
	plotPPVTbl(ppvTbl[dsetType=='twitter' & runNum==1 & dsetSize==500], 'contextPpvT')
	tbl = modelVsPredTbl[predUsedBest == T][dsetSize==500][grepl('^topHashtagPost', DVName)]
	tbl
	compareMeanDV(tbl[dsetType == 'stackoverflow'], acc, figName='ContextMeanDVSO')
	compareMeanDV(tbl[dsetType == 'twitter'], acc, figName='ContextMeanDVT')
}

getNcoocTbl <- function(type, chunkTableQuery, config) {
	myLog(sprintf('Getting Ncooc table for type %s', type))
	sqldf('truncate table temp_cooc')
	sqldf(sprintf("insert into temp_cooc (tag_chunk_id, context_chunk_id, pos_from_tag, partial_N)
		      select L.chunk_id as tag_chunk_id, R.chunk_id as context_chunk_id, L.pos - R.pos as pos_from_tag, count(L.post_id) as partial_N
		      from temp_tokenized as L
		      join temp_tokenized as R
		      on L.post_id = R.post_id
		      where L.post_type_id = (select tt.id from %s as tt where tt.type_name = '%s')
		      and R.post_type_id = (select tt.id from %s as tt where tt.type_name = '%s')
		      group by L.chunk_id, R.chunk_id, pos_from_tag
		      order by L.chunk_id, R.chunk_id, pos_from_tag",
		      getConfig(config, "tokenizedTypeTypeTbl"), getConfig(config, "tagTypeName"),
		      getConfig(config, "tokenizedTypeTypeTbl"), type))
	sqldf('vacuum analyze temp_cooc')
	resTbl = sqldt(sprintf('select t.type_name as tag, c.type_name as chunk, pos_from_tag, partial_N
			       from temp_cooc
			       join %s as t
			       on t.id = temp_cooc.tag_chunk_id
			       join %s as c
			       on c.id = temp_cooc.context_chunk_id
			       order by tag, chunk, pos_from_tag',
			       getConfig(config, "tokenizedChunkTypeTbl"), getConfig(config, "tokenizedChunkTypeTbl")
			       ))
	sqldf('truncate table temp_cooc')
	NcoocTbl = resTbl[, list(posFromTag=pos_from_tag, partialN=partial_n, NChunkTag=sum(partial_n)), by=list(chunk, tag)]
	setkey(NcoocTbl, chunk, tag, posFromTag)
	NcoocTbl
}

makeSubsetName <- function(subset, startId, endId) {
	paste(subset, startId, endId, sep='-')
}

makeChunkTableQuery <- function(subsetName, startId, endId, config) {
	sprintf("select * from %s(%s, %s, '%s')", getConfig(config, "makeChunkTblFun"), startId, endId, subsetName)
}

genNcoocTblSO <- function(subsetName, startId, endId) {
	config = defaultSOConfig
	fullSubsetName = makeSubsetName(subsetName, startId, endId)
	chunkTableQuery = makeChunkTableQuery(subsetName, startId, endId, config)
	genTempPostTokenizedTbl(chunkTableQuery)
	NcoocTblTitle = getNcoocTbl('title', chunkTableQuery, config) 
	NcoocTblBody = getNcoocTbl('body', chunkTableQuery, config) 
	outFile = sprintf('%s/NcoocTblTitle-%s.csv', getDirCooc(), fullSubsetName)
	myWriteCSV(NcoocTblTitle, file=outFile)
	outFile = sprintf('%s/NcoocTblBody-%s.csv', getDirCooc(), fullSubsetName)
	myWriteCSV(NcoocTblBody, file=outFile) 
	sqldf('truncate table temp_tokenized')

}

genNcoocTblTwitter <- function(subsetName, startId, endId) {
	config = defaultTConfig
	fullSubsetName = makeSubsetName(subsetName, startId, endId)
	chunkTableQuery = makeChunkTableQuery(subsetName, startId, endId, config)
	genTempPostTokenizedTbl(chunkTableQuery)
	NcoocTblTweet = getNcoocTbl('tweet', chunkTableQuery, config) 
	outFile = sprintf('%s/NcoocTblTweet-%s.csv', getDirCooc(), fullSubsetName)
	myWriteCSV(NcoocTblTweet, file=outFile)
	sqldf('truncate table temp_tokenized')
}

genAllNcoocTblTwitter <- function(startId, endId) {
	for (groupName in getConfig(defaultTConfig, 'allGroupNames')) {
		genNcoocTblTwitter(groupName, startId, endId)
	}
}

genTempPostTokenizedTbl <- function(chunkTableQuery) {
	myLog(sprintf("generating temp_tokenized table with query %s", chunkTableQuery))
	sqldf('truncate table temp_tokenized')
	sqldf(sprintf("insert into temp_tokenized %s", chunkTableQuery))
	sqldf('vacuum analyze temp_tokenized')
}

appendDTbl <- function(tbl, tblName) {
	myLog(sprintf('Writing %s rows to %s table', nrow(tbl), tblName))
	withDBConnect(dbCon, dbWriteTable(dbCon, tblName, tbl, append=T, row.names=0))
}

genTokenizedTblSO <- function(filters='1=1', bundleSize=10000) {
	query = sprintf("select id, owner_user_id, creation_date, title, body, tags
			from posts
			where post_type_id = 1
			and id not in (select distinct id from post_tokenized)
			and id not in (select post_id from post_filtered)
			and %s
			", filters)
	withDBConnect(dbCon,
		      {dbRs = dbSendQuery(dbCon, query)
		      writePartialTbl <- function(tbl) {
			      setcolorder(tbl, c('id', 'chunk', 'pos', 'type'))
			      appendDTbl(tbl, 'post_tokenized')
		      }
		      while (T) {
			      postsTbl = data.table(fetch(dbRs, n=bundleSize))
			      if (nrow(postsTbl) == 0) break
			      setupPostsTbl(postsTbl, defaultSOConfig)
			      postsTbl[, tags := NULL][, bodyNoHtml := html2txt2(body)][, body := NULL]
			      addTokenText(postsTbl, from='title')
			      tokenizedTblTitle = getTokenizedTbl(postsTbl, from='tokenText', regex=matchWhitespace)[, type := 'title']
			      addTokenText(postsTbl, from='bodyNoHtml')
			      tokenizedTblBody = getTokenizedTbl(postsTbl, from='tokenText', regex=matchWhitespace)[, type := 'body']
			      tokenizedTblTags = getTokenizedTbl(postsTbl, from='tagsNoHtml', regex=matchTag)[, type := 'tag'][, pos := NaN]
			      writePartialTbl(tokenizedTblTags)
			      writePartialTbl(tokenizedTblTitle)
			      writePartialTbl(tokenizedTblBody)
		      }
		      })
	return()
}

genTokenizedTblTwitter <- function(hashtagGroup, bundleSize=10000) {
	query = sprintf("select %s from top_hashtag_tweets 
			where hashtag_group = '%s'
			and id not in (select distinct id from top_hashtag_tokenized)
			",
			defaultTCols, hashtagGroup, hashtagGroup)
	withDBConnect(dbCon,
		      {dbRs = dbSendQuery(dbCon, query)
		      writePartialTbl <- function(tbl) {
			      setcolorder(tbl, c('id', 'chunk', 'pos', 'type'))
			      appendDTbl(tbl, 'top_hashtag_tokenized')
		      }
		      while (T) {
			      tweetsTbl = data.table(fetch(dbRs, n=bundleSize))
			      if (nrow(tweetsTbl) == 0) break
			      setupTweetsTbl(tweetsTbl, defaultTConfig)
			      tokenizedTbl = getTokenizedTbl(tweetsTbl, from='tokenText', regex=matchWhitespace)[, type := 'tweet']
			      tokenizedTbl[grepl(pattern=matchHashtag, x=chunk), type := 'hashtag']
			      writePartialTbl(tokenizedTbl)
		      }
		      })
	return()
}

addFilteredPosts <- function() {
	ids = c('17801882', '9965709', '9204391', '15837898', '18893489', '20156201', '3245809')
	reasons = c('java so', 'java so', 'java so', 'java so', 'java so', 'java so', 'nonprintable U+FFFF')
	filteredPostsTbl = data.table(post_id=ids, reason=reasons);
	setcolorder (filteredPostsTbl, c('post_id', 'reason'))
	appendDTbl(filteredPostsTbl, 'post_filtered')
}

addPostSubsets <- function() {
	postIdsTbl = sqldt('select id from posts where post_type_id = 1')
	postIdsTbl[, post_id := id][, id := NULL]
	postIdsTbl[, id := sample(1:length(post_id), size=length(post_id))]
	postIdsTbl[, group_name := 'SOShuffledFull']
	setcolorder(postIdsTbl, c('post_id', 'id', 'group_name'))
	setkey(postIdsTbl, group_name, id)
	appendDTbl(postIdsTbl, "post_subsets")
}

addTweetSubsets <- function() {
	tweetsIdGpTbl = sqldt("select id::text, hashtag_group
			      from top_hashtag_tweets
			      where hashtag_group not in (select distinct group_name from top_hashtag_subsets)
			      and retweeted = 'False'"
			      )
	partialWriteTable <- function(tbl, hashtag_group) {
		tbl[, post_id := id][, id := NULL]
		tbl[, id := sample(1:length(post_id), size=length(post_id))]
		tbl[, group_name := hashtag_group]
		setcolorder(tbl, c('post_id', 'id', 'group_name'))
		setkey(tbl, group_name, id)
		appendDTbl(tbl, "top_hashtag_subsets")
		return()
	}
	if (nrow(tweetsIdGpTbl) > 0) {
		tweetsIdGpTbl[, partialWriteTable(copy(.SD), hashtag_group), by=hashtag_group]
	}
}

runPythonFun <- function(call) {
	file = 'scrapeUsers.py'
	system2('python', args=c(sprintf('%s/%s', PATH, file), sprintf("'%s'", call)))
}

runMakeTarget <- function(call) {
	system2(sprintf('make'), args=c('-C', PATH, call))
}

runAddTweetSubsets <- addTweetSubsets
runUpdateTwitterWithNewGroup <- function() {
	runAddTweetSubsets()
	runGenTokenizedTblTwitter()
	sqldf('select * from fill_top_hashtag_tokenized_chunk_types()')
	sqldf('vacuum analyze top_hashtag_tokenized_chunk_types')
	runMakeTarget('parrunTSjisAll')
	runPythonFun('backupTopHashtags()')
}

runGenNcoocTblSO1thru100 <- function() genNcoocTblSO('SOShuffledFull', 1, 100)
runGenNcoocTblSO1thru1000 <- function() genNcoocTblSO('SOShuffledFull', 1, 1000)
runGenNcoocTblSO1thru10000 <- function() genNcoocTblSO('SOShuffledFull', 1, 10000)
runGenNcoocTblSO1thru100000 <- function() genNcoocTblSO('SOShuffledFull', 1, 100000)
runGenNcoocTblSO1thru3000000 <- function() genNcoocTblSO('SOShuffledFull', 1, 3000000)

runGenNcoocTblT11thru100 <- function() genAllNcoocTblTwitter(1, 100)
runGenNcoocTblT11thru1000 <- function() genAllNcoocTblTwitter(1, 1000)
runGenNcoocTblT11thru10000 <- function() genAllNcoocTblTwitter(1, 10000)
runGenNcoocTblT11thru100000 <- function() genAllNcoocTblTwitter(1, 100000)
runGenNcoocTblT11thru1000000 <- function() genAllNcoocTblTwitter(1, 1000000)
runGenNcoocTblT11thru3000000 <- function() genAllNcoocTblTwitter(1, 3000000)

runGenTokenizedTblSO <- function() genTokenizedTblSO()
runGenTokenizedTblTwitter <- function() {
	for (groupName in getConfig(defaultTConfig, 'allGroupNames')) {
		genTokenizedTblTwitter(groupName)
	}
}

getSjiTblSO <- function(config, startId, endId) {
	fileName = sprintf('%s.csv', makeSubsetName(getConfig(config, "groupName"), startId, endId))
	sjiTitleName = paste('NcoocTblTitle', fileName, sep='-')
	sjiBodyName = paste('NcoocTblBody', fileName, sep='-')
	sjiColClasses = c('character', 'character', 'integer', 'integer', 'integer')	
	sjiTitleTbl = myReadCSV(sprintf('%s/%s', getDirCooc(), sjiTitleName), colClasses=sjiColClasses)
	sjiBodyTbl = myReadCSV(sprintf('%s/%s', getDirCooc(), sjiBodyName), colClasses=sjiColClasses)
	sjiTitleTbl[, type := 'title']
	sjiBodyTbl[, type := 'body']
	sjiTbl = rbind(sjiTitleTbl, sjiBodyTbl)
	sjiTbl[, hashtag := tag][, tag := NULL]
	sjiTbl[, context := chunk][, chunk := NULL]
	sjiTbl[, posFromTag := 0]
	setkey(sjiTbl, context, hashtag, posFromTag, type)
	sjiTbl = sjiTbl[, list(partialN=sum(partialN), NChunkTag=sum(NChunkTag)), by=list(context, hashtag, posFromTag)]
	addSjiAttrs(sjiTbl)
	sjiTbl
}

getSjiTblT <- function(config, startId, endId) {
	fileName = sprintf('%s.csv', makeSubsetName(getConfig(config, "groupName"), startId, endId))
	sjiTblName = sprintf('NcoocTblTweet-%s', fileName)
	sjiColClasses = c('character', 'character', 'integer', 'integer', 'integer')	
	sjiTbl = myReadCSV(sprintf('%s/%s', getDirCooc(), sjiTblName), colClasses=sjiColClasses)
	sjiTbl[, context := chunk][, chunk := NULL]
	sjiTbl[, hashtag := tag][, tag := NULL]
	topHashtagsTbl = sqldt(sprintf("select hashtag from top_hashtag_hashtags where hashtag_group = '%s'", getConfig(config, "groupName")))
	sjiTbl = sjiTbl[hashtag %in% topHashtagsTbl[, hashtag]]
	sjiTbl
}

getSjiTblTOrderless <- function(config, startId, endId) {
	sjiTbl = getSjiTblT(config, startId, endId)
	sjiTbl = sjiTbl[, list(posFromTag=0, partialN=sum(partialN)), by=list(context, hashtag)]
	setkey(sjiTbl, context, hashtag, posFromTag)
	addSjiAttrs(sjiTbl)
	sjiTbl
}

getSjiTblTOrder <- function(config, startId, endId) {
	sjiTbl = getSjiTblT(config, startId, endId)
	sjiTbl = sjiTbl[, list(partialN), by=list(context, hashtag, posFromTag)]
	setkey(sjiTbl, context, hashtag, posFromTag)
	addSjiAttrs(sjiTbl)
	sjiTbl
}

getPriorTblUserSO <- function(config, startId, endId) {
	priorTblUser = sqldt(sprintf("select posts.id, user_screen_name, creation_epoch, chunk, type from posts
				     join post_tokenized
				     on posts.id = post_tokenized.id
				     where type = 'tag'
				     and post_type_id = 1
				     and owner_user_id is not null
				     and posts.id in (select post_id from post_subsets
						      where group_name = '%s'
						      and id >= %s
						      and id <= %s)
				     ", getConfig(config, "groupName"), startId, endId
				     ))
	if (getConfig(config, "convertTagSynonymsP")) convertTagSynonyms(priorTblUser)
	addDtToTbl(priorTblUser)
	priorTblUser[, hashtag := chunk][, chunk := NULL]
	setkey(priorTblUser, user_screen_name, dt, hashtag)
	priorTblUser
}

getPriorTblGlobT <- function(config, startId, endId) {
	globPriorTbl = sqldt(sprintf("select creation_epoch, chunk as hashtag from top_hashtag_tokenized as token
				 join top_hashtag_tweets as tweets
				 on tweets.id = token.id 
				 where type = 'hashtag'
				 and user_screen_name is not null
				 and chunk in (select hashtag from top_hashtag_hashtags where hashtag_group = '%s')
				 and tweets.id in (select post_id from top_hashtag_subsets
						   where group_name = '%s'
						   and id >= %s
						   and id <= %s)
				 ", getConfig(config, "groupName"),
				 getConfig(config, "groupName"), startId, endId
				 ))
	globPriorTbl[, user_screen_name := 'allUsers']
	addDtToTbl(globPriorTbl)
	setkey(globPriorTbl, user_screen_name, dt, hashtag)
	globPriorTbl
}

addSjiAttrs <- function(sjiTbl) {
	sjiTbl[, contextSums := sum(partialN), by=context]
	sjiTbl[, hashtagSums := sum(partialN), by=hashtag]
	sjiTbl[, sji := log(sum(partialN)) + log(partialN) - log(contextSums) - log(hashtagSums)]
	sjiTbl[, pHashtagGivenContext := partialN/contextSums]
	sjiTbl[, HContext := - sum(pHashtagGivenContext * log(pHashtagGivenContext)), by=context]
	sjiTbl[, EContext := 1 - HContext/max(HContext)]
}

computeActSji <- function(contextVect, sjiTbl, config) {
	contextVect
	sjiTbl
	myLog(sprintf("computing sji act for context with length %s", length(contextVect)))
	resTbl = sjiTbl[J(contextVect), nomatch=0, allow.cartesian=T]
	resTbl = resTbl[, {WContext = EContext/sum(EContext); list(act=sum(WContext * sji))}, keyby=hashtag]
	resTbl
}

wsFile = '/Volumes/SSDSupernova/RWorkspace/workspace.RData'

mySaveImage <- function() {
	eval(quote(save(list=c('sjiTblTOrderless', 'sjiTblTOrder', 'priorTblGlobT', 'sjiTblSOOrderless', 'priorTblUserSO',
			       'permEnvTblT', 'permEnvTblSO', 'permMemMatTOrder', 'permMemMatTOrderless',
			       'permMemMatSOOrderless'), file=wsFile, compress=F)),
	     envir=globalenv())
}

myLoadImage <- function() {
	eval(quote(load(file=wsFile)),
	     envir=globalenv())
}

makeCombinedMemMat <- function(sjiTbl, envTbl, config) {
	res = list()
	res[['orig']] = makeMemMat(sjiTbl, envTbl, modConfig(config, list(permUseEntropyP=F)))
	res[['entropy']] = makeMemMat(sjiTbl, envTbl, modConfig(config, list(permUseEntropyP=T)))
	res
}

getCurWorkspace <- function(maxIdSOSji, maxIdSOPrior, maxIdTSji, maxIdTPrior) {
	priorTblGlobT <<- getPriorTblGlobT(defaultTConfig, 1, maxIdTPrior)
	priorTblUserSO <<- getPriorTblUserSO(defaultSOConfig, 1, maxIdSOPrior)
	sjiTblTOrderless <<- getSjiTblTOrderless(defaultTConfig, 1, maxIdTSji)
	sjiTblTOrder <<- getSjiTblTOrder(defaultTConfig, 1, maxIdTSji)
	sjiTblSOOrderless <<- getSjiTblSO(defaultSOConfig, 1, maxIdSOSji)
	permEnvTblT <<- makeEnvironmentTbl(sjiTblTOrderless, defaultBaseConfig)
	permEnvTblSO <<- makeEnvironmentTbl(sjiTblSOOrderless, defaultBaseConfig)
	permMemMatTOrder <<- makeCombinedMemMat(sjiTblTOrder, permEnvTblT, defaultTPermConfig)
	permMemMatTOrderless <<- makeCombinedMemMat(sjiTblTOrderless, permEnvTblT, defaultTPermConfig)
	permMemMatSOOrderless <<- makeCombinedMemMat(sjiTblSOOrderless, permEnvTblSO, defaultSOPermConfig)
	return()
}

genAndSaveCurWorkspace <- function() {
	maxIdSOSji = 1e5
	maxIdSOPrior = 100e6
	maxIdTSji = 3e6 
	maxIdTPrior = 1e5 
	getCurWorkspace(maxIdSOSji, maxIdSOPrior, maxIdTSji, maxIdTPrior)
	mySaveImage()
}

computeActSjiFromContextTbl <- function(contextTbl, config) {
	contextTbl = copy(contextTbl)[, type := paste0('act', capitalize(type))]
	contextTbl[, computeActSji(chunk, get(getConfig(config, 'sjiTbl')), config), by=type]
}

funConfigWEntropy <- function(config) modConfig(config, list(permUseEntropyP=T))
funConfigNEntropy <- function(config) modConfig(config, list(permUseEntropyP=F))

computeActPermTFromContextTbl <- function(contextTbl, config) {
	contextTbl = rbind(copy(contextTbl)[,type:=paste0('act', capitalize(type),'Order')][,fun:='computeActPermOrder'][,funConfig:='funConfigNEntropy'],
			   copy(contextTbl)[,type:=paste0('act', capitalize(type),'Orderless')][,fun:='computeActPermOrderless'][,funConfig:='funConfigNEntropy'],
			   copy(contextTbl)[,type:=paste0('act', capitalize(type),'OrderEntropy')][,fun:='computeActPermOrder'][,funConfig:='funConfigWEntropy'],
			   copy(contextTbl)[,type:=paste0('act', capitalize(type),'OrderlessEntropy')][,fun:='computeActPermOrderless'][,funConfig:='funConfigWEntropy'])
	contextTbl[, get(fun[1])(chunk, pos, get(funConfig)(config)), by=type]
}

computeActPermSOFromContextTbl <- function(contextTbl, config) {
	contextTbl = rbind(copy(contextTbl)[,type:=paste0('act', capitalize(type),'Orderless')][,fun:='computeActPermOrderless'][,funConfig:='funConfigNEntropy'],
			   copy(contextTbl)[,type:=paste0('act', capitalize(type),'OrderlessEntropy')][,fun:='computeActPermOrderless'][,funConfig:='funConfigWEntropy'])
	contextTbl[, get(fun[1])(chunk, pos, get(funConfig)(config)), by=type]
}

getContextPredNames <- function(config) {
	res = getConfig(config, 'runTbl')
	res = res[, unique(predName)]
	res = res[!grepl(pattern='Prior', x=res)]
	res
}

getSjiTblWide <- function(contextTbl, config) {
	if (nrow(contextTbl) > 0) {
		sjiTbl = get(getConfig(config, 'computeActFromContextTbl'))(contextTbl, config)
	} else {
		sjiTbl = data.table(act=double(0)) # Need to add a column b/c more 0-row columns can only be added to a DT with at least one col.
	}
	if (nrow(sjiTbl) > 0) {
		sjiTblWide = dcast.data.table(sjiTbl, hashtag ~ type, value.var='act')
		for (predName in getContextPredNames(config)) {
			if (! predName %in% colnames(sjiTblWide)) {
				sjiTblWide[, eval(bquote( .(as.symbol(predName)) := NA_real_))]
			}
		}
	} else {
		sjiTblWide = copy(sjiTbl)
		lapply(lapply(getContextPredNames(config), as.symbol), function(x) sjiTblWide[, eval(bquote(.(x) :=  double(0)))])
		lapply(as.symbol('hashtag'), function(x) sjiTblWide[, eval(bquote(.(x) := character()))])
		sjiTblWide[, act := NULL][, type := NULL]
	}
	setkey(sjiTblWide, hashtag)
	setcolorder(sjiTblWide, c('hashtag', getContextPredNames(config)))
	sjiTblWide
}

getPostResTbl <- function(tokenTbl, config, id) {
	myLog(sprintf("Getting results for id=%s", id))
	tokenTbl
	dStd = getConfig(config, 'dStd')
	guardAllEqualP(tokenTbl[, creation_epoch])
	guardAllEqualP(tokenTbl[, user_screen_name])
	guardAllEqualP(tokenTbl[, user_screen_name_prior])
	guardAllEqualP(tokenTbl[, dt])
	contextTbl = tokenTbl[type != getConfig(config, "tagTypeName")]
	contextTbl
	tagTbl = tokenTbl[type == getConfig(config, "tagTypeName")]
	tagTbl
	setkey(tagTbl, chunk)
	priorTbl = getPriorForUserAtEpoch(get(getConfig(config, 'priorTbl')), tokenTbl$user_screen_name_prior[1], tokenTbl$creation_epoch[1], dStd)
	priorTbl
	setkey(priorTbl, hashtag, d)
	tempTagTbl = tagTbl[, list(hashtag=rep(chunk, each=length(dStd)), d=dStd)]
	setkey(tempTagTbl, hashtag, d)
	priorTbl = merge(priorTbl, unique(tempTagTbl), all=T)
	priorTbl[, user_screen_name := tokenTbl$user_screen_name[1]]
	priorTbl[, dt := tokenTbl[, dt[1]]]
	sjiTblWide = getSjiTblWide(contextTbl, config)
	key(priorTbl)
	key(tagTbl)
	postResTbl = sjiTblWide[priorTbl]
	postResTbl[, hashtagUsedP := F]
	postResTbl[tagTbl, hashtagUsedP := T]
	postResTbl
}

getTokenizedFromSubset <- function(minId, maxId, config) {
	resTbl = sqldt(sprintf("select tokenized_tbl.id::text, user_screen_name, creation_epoch, chunk, pos, type from %s as tokenized_tbl
			       join %s as posts_tbl
			       on tokenized_tbl.id = posts_tbl.id 
			       where tokenized_tbl.id in (select post_id from %s 
							  where id >= %s
							  and id <= %s
							  and group_name = '%s')",
			       getConfig(config, "tokenizedTbl"), getConfig(config, "postsTbl"), getConfig(config, "subsetsTbl"), minId, maxId, getConfig(config, "groupName") 
			       ))
	resTbl
}

getTokenizedFromSubsetT <- function(minId, maxId, config) {
	resTbl = getTokenizedFromSubset(minId, maxId, config)[, user_screen_name := 'allUsers']
	resTbl[, user_screen_name_prior := 'allUsers'][, user_screen_name := 'allUsers']
	addDtToTbl(resTbl)
	resTbl
}

getTokenizedFromSubsetSO <- function(minId, maxId, config) {
	resTbl = getTokenizedFromSubset(minId, maxId, config)
	resTbl[, user_screen_name_prior := user_screen_name][, user_screen_name := 'allUsers']
	addDtToTbl(resTbl)
	resTbl
}

handleNAs <- function(postResTbl, predictors) {
	#postResTbl = copy(postResTbl)
	#postResTbl[is.na(postResTbl)] = -4
	#postResTbl = lapply(postResTbl, function(col) if !is.numeric(col) col[is.na(col)] = mean(col, na.rm=T))
	for (col in predictors) {
		oldVal = postResTbl[[col]]
		meanVal = mean(oldVal, na.rm=T)
		linds = is.na(oldVal)
		myLog(sprintf('Imputing mean of %s for %s of %s values in column name %s', meanVal, sum(linds), length(linds), col))
		oldVal[linds] = meanVal 
		eval(bquote(postResTbl[, .(col) := oldVal]))
	}
	postResTbl
	#naRows = postResTbl[, predictors, with=F][, rowSums(is.na(as.matrix(.SD))) > 0]
	#postResTbl = postResTbl[!naRows]
}

updateBestFitCol <- function(postResTbl, logregTbl, bestFitName) {
	bestFitName = as.symbol(bestFitName)
	predictors = logregTbl[predName != '(Intercept)', predName]
	postResTbl[, eval(bquote(.(bestFitName) := logregTbl[predName == '(Intercept)', coeff]))]
	predictors
	postResTbl
	for (predictor in predictors) {
		sym = as.symbol(predictor)
		sym
		e = bquote(postResTbl[, .(bestFitName) := .(bestFitName) + .(sym) * .(logregTbl[predName == predictor, coeff])])
		e
		myLog(e)
		eval(e)
	}
}

getPPVTbl = function(tbl, bestFitName) {
	tbl
	bestFitName
	pred = prediction(tbl[[bestFitName]], tbl[, hashtagUsedP])
	perf = performance(pred, "tpr", "fpr")
	fp = unlist(perf@x.values)*sum(!tbl$hashtagUsedP)
	tp = unlist(perf@y.values)*sum(tbl$hashtagUsedP)
	cutoff = min(which((tp+fp) > (sum(tbl$hashtagUsedP) * 2)))
	indeces = sort(sample(cutoff, min(cutoff, 2000), prob=1/(1+1:cutoff)))
	x = tp[indeces]+fp[indeces]
	x = x/sum(tbl$hashtagUsedP)
	y = tp[indeces]/(tp[indeces]+fp[indeces])
	data.table(x=x, y=y)
}

runLogReg <- function(postResTbl, predictors) {
	model = reformulate(termlabels = predictors, response = 'hashtagUsedP')
	postResTbl
	myLogit = glm(model, data=postResTbl, weights=weights, family=binomial(link="logit"))
	myLogit
}

addWeights <- function(postResTbl) {
	#missCRWeight = postResTbl[, .N, by=hashtagUsedP][, NTot := sum(N)][, min(N)/max(N)]
	missCRWeight = 1 
	missCRWeight
	postResTbl[, weights := missCRWeight]
	postResTbl[hashtagUsedP == T, weights := 1]
	return()
}

analyzePostResTbl <- function(postResTbl, predictors, bestFitName) {
	setkey(postResTbl, user_screen_name, dt, hashtag, d)
	guardAllEqualP(postResTbl[, d])
	myLog(sprintf('Analyzing post result table for d=%s, bestFitName=%s, and predictors=%s',
		      postResTbl[, d[1]], bestFitName, paste(predictors, collapse=',')))
	#postResTbl = copy(postResTbl)
	predictors
	postResTbl
	handleNAs(postResTbl, predictors)
	addWeights(postResTbl)
	myLogit = runLogReg(postResTbl, predictors)
	coeffs = summary(myLogit)$coefficients
	logregTbl = as.data.table(coeffs)
	logregTbl[, coeff := Estimate][, Estimate := NULL]
	logregTbl
	logregTbl[, predName := rownames(coeffs)][, d := postResTbl[, d[1]]]
	summary(myLogit)
	updateBestFitCol(postResTbl, logregTbl, bestFitName)
	postResTbl
	print(summary(myLogit))
	print(ClassLog(myLogit, postResTbl$hashtagUsedP))
	myLogit
	logregTbl
}

getHashtagsTblFromSubsetTbl <- function(tokenTbl, config) {
	hashtagsTbl = tokenTbl[type==getConfig(config, 'tagTypeName')]
	hashtagsTbl[, hashtag := chunk][, chunk := NULL]
	setkey(hashtagsTbl, user_screen_name, dt, hashtag)
	hashtagsTbl
}

getFullPostResTbl <- function(tokenTbl, config) {
	postResTbl = tokenTbl[, getPostResTbl(.SD, config, id), by=id]
	postResTbl
}

analyzePostResTblAcrossDs <- function(postResTbl, runTbl) {
	analyzePostResTblForD <- function(curD) {
		tbl = postResTbl[d == curD] 
		tbl
		runTbl
		logregTbl = runTbl[, analyzePostResTbl(tbl, predName, name), by=name]
		logregTbl
		list(postResTbl=tbl, logregTbl=logregTbl)	
	}
	ds = postResTbl[, unique(d)]
	res = lapply(ds, analyzePostResTblForD)
	res
	postResTbl = rbindlist(lapply(res, `[[`, "postResTbl"))
	logregTbl = rbindlist(lapply(res, `[[`, "logregTbl"))	
	logregTbl
	postResTbl
	list(postResTbl=postResTbl, logregTbl=logregTbl)
}

runContext <- function(config, samplesPerRun, numRuns) {
	runSubset <- function(startId, endId, runNum) {
		tokenTbl = get(getConfig(config, 'getTokenizedFromSubsetFun'))(startId, endId, config)
		config
		postResTbl = getFullPostResTbl(tokenTbl, config)
		runTbl = getConfig(config, 'runTbl')
		postResTbl
		runTbl
		resTbl = analyzePostResTblAcrossDs(postResTbl, runTbl)
		logregTbl = resTbl$logregTbl
		postResTbl = resTbl$postResTbl
		setkey(postResTbl, user_screen_name, dt, hashtag, d)
		logregTbl
		hashtagsTbl = getHashtagsTblFromSubsetTbl(tokenTbl, config)
		addMetrics(hashtagsTbl, postResTbl, config)
		hashtagsTbl
		modelVsPredTbl = getModelVsPredTbl(postResTbl, hashtagsTbl, config)
		groupName='g1' #FIXME: Will need to be fixed when running all sampled datasets for Twitter
		getOutFileForName <- function(name) {
			outFile = getConfig(config, name)
			outFile = gsub('.csv$', sprintf('%sr%s%s', groupName, runNum, '.csv'), outFile)
			outFile
		}
		outFile = getOutFileForName("modelVsPredOutFile")
		writeModelVsPredTbl(modelVsPredTbl, outFile)
		outFile = getOutFileForName('modelHashtagsOutFile')
		writeModelHashtagsTbl(postResTbl, outFile)
		outFile = getOutFileForName('logregOutFile')
		writeLogregTbl(logregTbl, outFile)
		postResTbl
		list(modelVsPredTbl=modelVsPredTbl, modelHashtagsTbl=postResTbl, hashtagsTbl=hashtagsTbl, logregTbl=logregTbl)
	}
	endId = 3000000
	for (runNum in seq(numRuns)) {
		startId = endId + 1
		endId = startId + samplesPerRun - 1
		runSubset(startId, endId, runNum)
	}
}

getCurWorkspaceBy <- function(regen) {
	if (regen == 'useAlreadyLoaded') return()
	if (regen == T) {
		eval(quote(getCurWorkspace(1e5, 100e6, 3e6, 1e5)), envir=globalenv())
	} else {
		eval(quote(myLoadImage()), envir=globalenv())
	}
}

runContextWithConfig <- function(regen, samplesPerRun, numRuns=1) {
	getCurWorkspaceBy(regen)
	addNumSamples = function(str) sprintf('%s-%s', str, samplesPerRun)
	getConfigMods <- function(name) {
		list(modelVsPredOutFile=getOutFileModelVsPred(addNumSamples(name)),
		     modelHashtagsOutFile=getOutFileModelHashtags(addNumSamples(name)),
		     logregOutFile=getLogregOutFile(addNumSamples(name)))
	}
	runContextFor <- function(config) {
		runContext(config, samplesPerRun, numRuns)
	}
	runs = list(modConfig(defaultTSjiConfig, getConfigMods('TContextSji')),
		    modConfig(defaultSOSjiConfig, getConfigMods('SOContextSji')),
		    modConfig(defaultTPermConfig, getConfigMods('TContextPerm')),
		    modConfig(defaultSOPermConfig, getConfigMods('SOContextPerm'))
		    )
	lapply(runs, runContextFor)
}

runContext20 <- function(regen=F, numRuns=1) runContextWithConfig(regen=regen, 20, numRuns=numRuns)
runContext200 <- function(regen=F, numRuns=5) runContextWithConfig(regen=regen, 200, numRuns=numRuns)
runContext500 <- function(regen=F, numRuns=5) runContextWithConfig(regen=regen, 500, numRuns=numRuns)

createSampleInd <- function(tbl, num, config) {
	indName = as.symbol(paste0('ind', num))
	expr = bquote(.(indName) := sample(1:getConfig(config, 'permNRows'), size=nrow(tbl), replace=T))
	tbl[, eval(expr)]
}

allColUniqExpr <- function(colNames, resColName) {
	makeExpr = function(x) sprintf('%s != %s', x[1], x[2])
	expr = apply(combn(colNames, 2), 2, makeExpr)
	expr = paste(expr, sep='', collapse=' & ')
	expr = sprintf('%s := %s', resColName, expr)
	expr = parse(text=expr)
	expr
}

makeEnvironmentSubsetTbl <- function(tbl, config) {
	myLog(sprintf('Attempting to create vectors for %s chunks', nrow(tbl)))
	createSampleInd(tbl, 1, config)
	createSampleInd(tbl, 2, config)
	createSampleInd(tbl, 3, config)
	createSampleInd(tbl, 4, config)
	tbl
	tbl[, eval(allColUniqExpr(c('ind1', 'ind2', 'ind3', 'ind4'), 'uniq'))]
	redoTbl = tbl[uniq == F]
	if (nrow(redoTbl) == 0) {
		tbl
	} else {
		rbind(tbl[uniq == T], makeEnvironmentSubsetTbl(redoTbl[, list(chunk=chunk)], config))
	}
}

makeEnvironmentTbl <- function(sjiTbl, config) {
	permEnvTbl = with(sjiTbl, data.table(chunk=unique(context)))
	permEnvTbl
	key(sjiTbl)
	permEnvTbl = makeEnvironmentSubsetTbl(permEnvTbl, config)
	permEnvTbl[, uniq := NULL]
	setkey(permEnvTbl, chunk)
	entTbl = sjiTbl[, .N, keyby=list(context, EContext)][, N := NULL]
	stopifnot(nrow(entTbl) == entTbl[, length(unique(context))])
	permEnvTbl[entTbl, EContext := EContext]
	permEnvTbl = melt(permEnvTbl,
			  id=c('chunk', 'EContext'),
			  measure=c('ind1', 'ind2', 'ind3', 'ind4'),
			  variable.name='val',
			  value.name='ind')
	setkey(permEnvTbl, val)
	valTbl = data.table(name=c('ind1', 'ind2', 'ind3', 'ind4'), valAsNum=c(1,1,-1,-1), key='name')
	permEnvTbl[valTbl, valAsNum := valAsNum]
	permEnvTbl[, val := valAsNum][, valAsNum := NULL]
	setkey(permEnvTbl, chunk)
	permEnvTbl
}

makeMemMat <- function(sjiTbl, permEnvTbl, config) {
	memTbl = permEnvTbl[sjiTbl, allow.cartesian=T, nomatch=0]
	if (getConfig(config, 'permUseEntropyP')) {
		myLog(sprintf('Weighting val in memory matrix based on entropy'))
		memTbl[, val := val * EContext]
	}
	sjiTbl
	key(sjiTbl)
	memTbl
	key(memTbl)
	permEnvTbl
	NRows = getConfig(config, 'permNRows')
	NRows
	memTbl[, rotInd := ((ind-1 + posFromTag) %% NRows) + 1]
	memTbl = memTbl[, list(totVal=sum(val*partialN)), by=list(rotInd, hashtag)]
	db = makeDB(memTbl[, unique(hashtag)])
	db
	memMat = with(memTbl, sparseMatrix(i=rotInd, j=getHashes(hashtag, db), x=totVal, dims=c(NRows, length(db))))
	memMat = as.matrix(memMat)
	colnames(memMat) = getVals(seq(1, length=ncol(memMat)), db)
	dim(memMat)
	memMat
}

computeActPerm <- function(context, pos, permEnvTbl, permMemMat, config) {
	myLog(sprintf("computing perm act for context with length %s", length(context)))
	permEnvTbl
	permMemMat
	contextTbl = data.table(context=context, posFromTag=pos, hashtag='context', partialN=1, key='context')
	contextMemMat = makeMemMat(contextTbl, permEnvTbl, config)
	contextMemMat
	contextMemVect = rowSums(contextMemMat) 
	contextMemVect
	if (sd(contextMemVect) == 0) {
		contextCorVect = matrix(data=0, nrow=dim(permMemMat)[2], ncol=1, dimnames=list(colnames(permMemMat)))
	} else {
		contextCorVect = cor(permMemMat, contextMemVect)
	}
	resTbl = data.table(hashtag=rownames(contextCorVect), act=as.vector(contextCorVect))
	resTbl
}

getMemMatFromList <- function(lst, config) {
	acc = {
		if (getConfig(config, 'permUseEntropyP')) {
			'entropy'
		} else {
			'orig'
	}}
	lst[[acc]]
}

computeActPermOrder <- function(context, pos, config) {
	computeActPerm(context,
		       pos,
		       # Cannot get from global environment or test 'testComputePermAct' will fail
		       permEnvTbl = get(getConfig(config, 'permEnvTbl'), envir=parent.frame()),
		       permMemMat = getMemMatFromList(get(getConfig(config, 'permMemMatOrder'), envir=parent.frame()),
						      config),
		       config)
}

computeActPermOrderless <- function(context, pos, config) {
	computeActPerm(context,
		       rep(0, length(context)),
		       permEnvTbl = get(getConfig(config, 'permEnvTbl'), envir=parent.frame()),
		       permMemMat = getMemMatFromList(get(getConfig(config, 'permMemMatOrderless'), envir=parent.frame()),
						      config),
		       config)
}

curWS <- function() {
	#FIXME: Run across all four datasets (new files; changes the r number)
	#FIXME: Methods to import and anlyze coefficient tables
	#FIXME: Size of sji across RP and Bayesian (new run files)
	#FIXME: Quickly rerun logreg analysis for actDV

	runContext20(regen='useAlreadyLoaded')
	runContext500(regen='useAlreadyLoaded', numRuns=5)

	modelVsPredTbl = buildTables(file_path_sans_ext(Filter(isContextRun, list.files(path=getDirModelVsPred()))))
	modelHashtagsTbls = buildModelHashtagsTables(file_path_sans_ext(Filter(isContextRun, list.files(path=getDirModelHashtags()))))
	#modelVsPredTbl = buildTables(file_path_sans_ext(Filter(isPriorRun, list.files(path=getDirModelVsPred()))))
	modelVsPredTbl
	sjiTblTOrderless
	priorTblGlobT[, N:=.N, by=hashtag][, N := N/nrow(.SD)]
	priorTblGlobT[, N:=NULL]
	priorTblGlobT[order(N, decreasing=T)]
	context = c('a', 'it', 'i')
	pos = c(1, 3, 1)
	setLogLevel(1)
	withProf(computeActPermOrderless(context, pos, defaultSOPermConfig))
	sjiTblTOrderless
	.ls.objects(order.by="Size")
	sqldf('select hashtag_group, retweeted, count(text) from top_hashtag_tweets group by hashtag_group, retweeted order by hashtag_group, retweeted')
	sessionInfo()
	postResTblSO
	postResTblT
	postResTblT
	withProf(myLoadImage())
	priorTblGlobT[, .N, by=hashtag][, list(hashtag, p=N/sum(N))][order(p, decreasing=T)][1:50][, plot(1:length(p), p)]
	priorTblUserSO[, .N, by=hashtag][, list(hashtag, p=N/sum(N))][order(p, decreasing=T)][1:50][, plot(1:length(p), p)]
	BTbl
	test_dir(sprintf("%s/%s", PATH, 'tests'), reporter='summary')
	.ls.objects(order.by='Size')
	# Checking that tweets for twitter users from each followers_count,statuses_count scale are being collected properly
	usersWithTweetsTbl = sqldt("select distinct on (t.user_screen_name) t.user_screen_name,u.followers_count,u.statuses_count
				   from tweets as t join twitter_users as u on t.user_screen_name = u.user_screen_name"
				   )
	usersWithTweetsTbl[order(followers_count), plot(log10(followers_count))]
	usersWithTweetsTbl[order(statuses_count), plot(log10(statuses_count))]
	usersWithTweetsTbl[order(followers_count),][followers_count > 10000000]
}

#curWS()

