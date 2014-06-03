library(tools)
library(RPostgreSQL)
library(lavaan)
library(Rmisc)
library(gridExtra)
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
library(boot)
library(digest)
library(parallel)

PATH = getPathToThisFile()
FILE = getNameOfThisFile()

USER = Sys.getenv("USER")
options(sqldf.RPostgreSQL.user = USER,
	sqldf.RPostgreSQL.dbname = USER,
	sqldf.RPostgreSQL.host ="localhost"
	)
options("scipen"=100, "digits"=4)
options(error=traceback)
options(datatable.alloccol = 900)

getCores <- function(config, name) {
	cores = getConfig(config, name)
	if (any(grepl('gui', commandArgs()))) 1 else cores
}

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

withLogLevel <- function(newLogLevel, thunk) {
	.curLogLevel = getLogLevel()
	on.exit(setLogLevel(.curLogLevel))
	setLogLevel(newLogLevel)
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

# Computing CIs around statistics

CIMedian <- function(x) {
	res = boot(x,function(x,i) median(x[i]), R=1000)
	res
	res = boot.ci(res, type="norm")
	lower = unlist(res)[['normal2']]
	upper = unlist(res)[['normal3']]
	mid = median(x)
	if (is.null(lower)) lower=mid
	if (is.null(upper)) upper=mid
	c(upper, mid, lower)
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

getAllowedRetweetedVals <- function(config) {
	allowedRetweetedVals = if (getConfig(config, "includeRetweetsP")) c('True', 'False') else c('False')
	allowedRetweetedVals
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

matchWhitespace = '\\S+'
matchHashtag = '^#'
matchTag = '(?<=<)[^>]+(?=>)'

convertTagSynonymsForTokenized <- function(tokenTbl, config) {
	tagTbl = tokenTbl[type == getConfig(config, 'tagTypeName')] 
	convertTagSynonyms(tagTbl)
	tokenTbl = rbind(tokenTbl[type != getConfig(config, 'tagTypeName')], tagTbl)
	tokenTbl
}

convertTagSynonyms <- function(tagTbl) {
	myLog('converting tag synomyms for tokenized tbl')
	withKey(tagTbl, chunk,
		{tagTbl[getTagSynonymsTbl(), chunk := target_tag_name]
		})
}

computeActPrior <- function(hashtags, dtP, cTime, d) {
	myStopifnot(length(dtP) == length(hashtags))
	myStopifnot(length(dtP) == length(cTime))
	#debugPrint(hashtags)
	#debugPrint(d)
	#debugPrint(cTime)
	dtP = cTime - dtP
	indeces = dtP>0
	hashtagsSub = hashtags[indeces] 
	#debugPrint(hashtagsSub)
	cTimeSub = cTime[indeces]
	cTimeSubRep = rep(cTimeSub, times=length(d))
	dtPSub = dtP[indeces]
	dtPSubRep = rep(dtPSub, times=length(d))
	dRep = rep(d, each=length(dtPSub))
	hashtagsSubRep = rep(hashtagsSub)
	list(hashtag=hashtagsSubRep, partialAct=dtPSubRep^(-dRep), dt=cTimeSubRep, d=dRep, dtP=dtPSubRep)
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

getPriorAtEpoch <- function(priorTbl, cEpoch, d) {
	if (nrow(priorTbl) > 0) {
		myStopifnot(priorTbl[1,dt] == 0)
	}
	myLog(sprintf('computing prior at epoch %s', cEpoch))
	curUserPriorTbl = priorTbl[creation_epoch < cEpoch]
	myLog(sprintf('Using %s prior observations to compute prior', nrow(curUserPriorTbl)))
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
	modelPlots = subsetModelHashtagsTbl[, plotBuildFun(copy(.SD), user_screen_name, d), by=list(user_screen_name, d)]
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
	tagCountTbl = hashtagsTbl[, list(tagCountN=.N), keyby=list(user_screen_name, dt, id)]
	modelHashtagsTbl
	myStopifnot(key(modelHashtagsTbl)[1:3] == c('user_screen_name', 'dt', 'id'))
	modelHashtagsTbl[tagCountTbl, tagCount := tagCountN, nomatch=0]
	modelHashtagsTbl[tagCountTbl[, list(tagCountUserN=sum(tagCountN)), keyby=user_screen_name], tagCountUser := tagCountUserN]
	myLog('adding metrics for modelHashtagsTbl')
	addDVCols <- function(col, newDVPost, newDVAct) {
		col = as.symbol(col)
		newDVPost = as.symbol(newDVPost)
		newDVAct = as.symbol(newDVAct)
		col
		newDVPost
		expr = bquote(modelHashtagsTbl[order(.(col), decreasing=T), .(newDVPost) := 1:length(.(col)) <= tagCount[1], by=list(user_screen_name, dt, id, d)])
		eval(expr)
		expr = bquote(modelHashtagsTbl[is.na(.(col)) & .(newDVPost), .(newDVPost) := F])
		eval(expr)
		expr = bquote(modelHashtagsTbl[order(.(col), decreasing=T), .(newDVAct) := 1:length(.(col)) <= tagCountUser[1], by=list(user_screen_name, d)])
		eval(expr)
		expr = bquote(modelHashtagsTbl[is.na(.(col)) & .(newDVAct), .(newDVAct) := F])
		eval(expr)
		return()
	}
	for (actDV in actDVs) {
		actDV
		do.call(addDVCols, as.list(c(actDV, topHashtagDVFromActDV(actDV))))
	}
	myStopifnot(key(modelHashtagsTbl) == (c('user_screen_name', 'dt', 'id', 'hashtag', 'd')))
	myStopifnot(key(hashtagsTbl) == (c('user_screen_name', 'dt', 'id', 'hashtag')))
	modelHashtagsTbl[, hashtagUsedP := F]
	key(modelHashtagsTbl)
	modelHashtagsTbl[hashtagsTbl, hashtagUsedP := T]
	#wideTbl = hashtagsTbl[, list(usedHashtags=list(hashtag)), by=list(user_screen_name, dt)]
	#modelHashtagsTbl[wideTbl, usedHashtags := usedHashtags]
	return()
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
	setkey(modelHashtagsTbl, user_screen_name, id, hashtag, d)
	myWriteCSV(modelHashtagsTbl, file=outFile)
}

writeLogregTbl <- function(logregTbl, outFile) {
	setkey(logregTbl, bestFitName, predName, d)
	myWriteCSV(logregTbl, file=outFile)
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

getSjiTblSOUser <- function(owner_user_id, config) {
	# FIXME: This is SO specific
	query = sprintf("select chunk_types.id as chunk_id, tokenized.id::bigint as post_id, tokenized.pos as pos, types.id as post_type_id
			from post_tokenized as tokenized
			join post_tokenized_chunk_types as chunk_types
			on tokenized.chunk = chunk_types.type_name
			join post_tokenized_type_types as types
			on tokenized.type = types.type_name
			join posts as posts
			on posts.id = tokenized.id
			where owner_user_id = '%s'",
			owner_user_id)
	genTempPostTokenizedTbl(query)
	NcoocTblTitle = getNcoocTbl('title', chunkTableQuery, config) 
	NcoocTblBody = getNcoocTbl('body', chunkTableQuery, config) 
	sqldf('truncate table temp_tokenized')
	sjiTbl = initSjiTblSO(NcoocTblTitle, NcoocTblBody)
	sjiTbl
}

# FIXME: This is SO specific
makeSjiTblUser <- function(priorTblUserSubset, config) {
	sjiTblSOOrderlessUser <<- priorTblUserSubset[, getSjiTblSOUser(user_screen_name, config), by=user_screen_name]
	setkey(sjiTblSOOrderlessUser, user_screen_name, context, hashtag, posFromTag)
	permMemMatSOOrderlessUser <<- list()
	for (user in priorTblUserSubset[, unique(user_screen_name)]) {
		sjiTblCur = getSjiTblFromUserTbl(sjiTblSOOrderlessUser, user) 
		permMemMatSOOrderlessUser[[user]] <<- makeCombinedMemMatPUser(sjiTblCur, permEnvTblSO, defaultSOPermConfig)
	}
}

makeSjiTblUserDefault <- function(priorTblUserSubset, config) {}

runPrior <- function(config) {
	myStopifnot(!any(sapply(config,is.null)))
	if (getConfig(config, 'genGlobalsForPriorRun')) {
		priorTblUserSubset <<- getPriorTblUserSubset(config)
		get(getConfig(config, 'makeSjiTblUser'))(priorTblUserSubset, config)
	}
	if (is.character(getConfig(config, 'dStd')) && getConfig(config, 'dStd') == 'dFull') {
		config=modConfig(config, list(dStd=getConfig(config, 'dFull')))
	}
	config
	tokenTbl = getTokenizedForUsers(config)
	tagTbl = tokenTbl[type == getConfig(config, 'tagTypeName')] 
	tokenTbl = rbind(tokenTbl[type != getConfig(config, 'tagTypeName')], tagTbl)
	tokenTbl = tokenTbl[, setkey(.SD, id)][tagTbl[, list(id=unique(id))][, setkey(.SD, id)]]
	setkey(tokenTbl, user_screen_name, dt)
	getOutFileForNameFun <- function(name) {
		getConfig(config, name)
	}
	withLogLevel(0, runForTokenTbl(tokenTbl, config, getOutFileForNameFun))
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

makeRunTbl <- function(runs) {
	makeTbl <- function(run) {
		bestFitName = paste(run, collapse='_', sep='')
		data.table(bestFitName=bestFitName, predName=run)
	}
	if (length(runs) > 0) {
		rbindlist(lapply(runs, makeTbl))
	} else {
		data.table(bestFitName=character(0), predName=character(0))
	}
}

permNRowsSm = 100
defaultBaseConfig = list(dFull=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.8,2,5,10,20),
			 dStd=c(0.5,0.7),
			 accumModelHashtagsTbl=F,
			 modelVsPredOutFile='/tmp/modelVsPred.csv',
			 modelHashtagsOutFile='',
			 logregOutFile='',
			 actDVs = c('actPriorStd', 'actPriorOL', 'actPriorOL2'),
			 permNRowsAll = c(2048, 10000, permNRowsSm),
			 permNRows = 2048,
			 MCCORESAct = 8,
			 MCCORESReg = 4,
			 MCCORESActUser = 1,
			 query=NULL)

defaultTConfig = c(defaultBaseConfig,
		   list(dsetType='twitter',
			from='tokenText',
			convertTagSynonymsP=F,
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
			includeRetweetsP=F,
			defaultColsTokenized='defaultColsTokenizedT',
			defaultColsPriorUser='defaultColsPriorUserT'))

defaultSOConfig = c(defaultBaseConfig,
		    list(dsetType='stackoverflow',
			 convertTagSynonymsP=T,
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
			 makeChunkTblFun='make_chunk_table_SO',
			 defaultColsTokenized='defaultColsTokenizedSO',
			 defaultColsPriorUser='defaultColsPriorUserSO'
			 ))

defaultPermConfig = list(permUseEntropyP='', permUseStoplistP='', permOnlyDirectionP='', permHymanP='', permUseFreqP='', permUseWindowP='')

defaultSjiConfig = list(computeActFromContextTbl = 'computeActSjiFromContextTbl', sjiFreqP='', sjiEntropyP='')

makeBestFitsVectFun <- function(res) {
	function(name) {
		app = function(str) sprintf(str, name)
		map = function(vars) sapply(vars, app, USE.NAMES=F)
		res = lapply(res, map)
		res
	}
}

makeBestFitsVectTPerm <- function(name) {
	do.call(makeBestFitsVectFun(list(c('actPriorStd', 'actTweetOrder%s', 'actTweetOrderless%s'), c('actTweetOrder%s', 'actTweetOrderless%s'))),
		list(name))
}

makeBestFitsVectTSji <- function(name) {
	do.call(makeBestFitsVectFun(list(c('actPriorStd', 'actTweet%s'))), list(name))
}

makeBestFitsVectSOPerm <- function(name) {
	do.call(makeBestFitsVectFun(list(c('actPriorStd', 'actTitleOrderless%s', 'actBodyOrderless%s'), c('actTitleOrderless%s', 'actBodyOrderless%s'))),
		list(name))
}

makeBestFitsVectSOSji <- function(name) {
	do.call(makeBestFitsVectFun(list(c('actPriorStd', 'actTitle%s', 'actBody%s'), c('actTitle%s', 'actBody%s'))), list(name))
}

makeBestFitsStrFun <- function(res) {
	function(name) {
		app = function(str) {
			nReps = str_count(str, '%s')
			do.call(sprintf, as.list(c(str, rep(name, nReps))))
		}
		sapply(res, app, USE.NAMES=F)
	}
}

makeBestFitsStrTPerm <- function(name) {
	do.call(makeBestFitsStrFun(c('actPriorStd_actTweetOrder%s_actTweetOrderless%s',
				  'actTweetOrderless%s', 'actTweetOrder%s', 'actTweetOrder%s_actTweetOrderless%s')),
		list(name))
}

makeBestFitsStrTSji <- function(name) {
	do.call(makeBestFitsStrFun(c('actPriorStd_actTweet%s', 'actTweet%s')), list(name))
}

makeBestFitsStrSOPerm <- function(name) {
	do.call(makeBestFitsStrFun(c('actPriorStd_actTitleOrderless%s_actBodyOrderless%s',
				  'actTitleOrderless%s', 'actBodyOrderless%s', 'actTitleOrderless%s_actBodyOrderless%s')),
		list(name))
}

makeBestFitsStrSOSji <- function(name) {
	do.call(makeBestFitsStrFun(c('actPriorStd_actTitle%s_actBody%s', 'actTitle%s', 'actBody%s', 'actTitle%s_actBody%s')), list(name))
}

defaultTPermConfig = modConfig(c(defaultTConfig, defaultPermConfig,
				 list(runTbl=makeRunTbl(c(list(c('actPriorStd', 'actTweetOrder'),
							       c('actPriorStd', 'actTweetOrderless')),
							  makeBestFitsVectTPerm(''),
							  makeBestFitsVectTPerm('Entropy'),
							  makeBestFitsVectTPerm('Stoplist'),
							  makeBestFitsVectTPerm('Direction'),
							  makeBestFitsVectTPerm('Hyman'),
							  makeBestFitsVectTPerm('Freq'),
							  makeBestFitsVectTPerm('Window'),
							  makeBestFitsVectTPerm('Frentropy'),
							  makeBestFitsVectTPerm('Smdim'),
							  makeBestFitsVectTPerm('Lgdim'),
							  makeBestFitsVectTPerm('Frenthyman'),
							  makeBestFitsVectTPerm('Nenthyman'),
							  makeBestFitsVectTPerm('Freqhyman')
							  )),
				      permEnvTbl='permEnvTblT',
				      permMemMatOrder='permMemMatTOrder',
				      permMemMatOrderless='permMemMatTOrderless',
				      computeActFromContextTbl = 'computeActPermTFromContextTbl')),
			       list(accumModelHashtagsTbl=T,
				    actDVs=c('actPriorStd_actTweetOrder', 'actPriorStd_actTweetOrderless', 'actPriorStd',
					     makeBestFitsStrTPerm(''),
					     makeBestFitsStrTPerm('Entropy'),
					     makeBestFitsStrTPerm('Stoplist'),
					     makeBestFitsStrTPerm('Direction'),
					     makeBestFitsStrTPerm('Hyman'),
					     makeBestFitsStrTPerm('Freq'),
					     makeBestFitsStrTPerm('Window'),
					     makeBestFitsStrTPerm('Frentropy'),
					     makeBestFitsStrTPerm('Smdim'),
					     makeBestFitsStrTPerm('Lgdim'),
					     makeBestFitsStrTPerm('Frenthyman'),
					     makeBestFitsStrTPerm('Nenthyman'),
					     makeBestFitsStrTPerm('Freqhyman')
					     ),
				    MCCORESAct = 16
				    ))

defaultSOPermConfig = modConfig(c(defaultSOConfig, defaultPermConfig,
				  list(runTbl=makeRunTbl(c(list(c('actPriorStd', 'actTitleOrderless'),
								c('actPriorStd', 'actBodyOrderless')),
							   makeBestFitsVectSOPerm(''),
							   makeBestFitsVectSOPerm('Entropy'),
							   makeBestFitsVectSOPerm('Stoplist'),
							   makeBestFitsVectSOPerm('Direction'),
							   makeBestFitsVectSOPerm('Hyman'),
							   makeBestFitsVectSOPerm('Freq'),
							   makeBestFitsVectSOPerm('Window'),
							   makeBestFitsVectSOPerm('Frentropy'),
							   makeBestFitsVectSOPerm('Smdim'),
							   makeBestFitsVectSOPerm('Lgdim'),
							   makeBestFitsVectSOPerm('Frenthyman'),
							   makeBestFitsVectSOPerm('Nenthyman'),
							   makeBestFitsVectSOPerm('Freqhyman')
							   )),
				       permEnvTbl='permEnvTblSO',
				       permMemMatOrder='',
				       permMemMatOrderless='permMemMatSOOrderless',
				       computeActFromContextTbl = 'computeActPermSOFromContextTbl')),
				list(accumModelHashtagsTbl=T,
				     actDVs=c('actPriorStd_actTitleOrderless', 'actPriorStd_actBodyOrderless', 'actPriorStd',
					      makeBestFitsStrSOPerm(''),
					      makeBestFitsStrSOPerm('Entropy'),
					      makeBestFitsStrSOPerm('Stoplist'),
					      makeBestFitsStrSOPerm('Direction'),
					      makeBestFitsStrSOPerm('Hyman'),
					      makeBestFitsStrSOPerm('Freq'),
					      makeBestFitsStrSOPerm('Window'),
					      makeBestFitsStrSOPerm('Frentropy'),
					      makeBestFitsStrSOPerm('Smdim'),
					      makeBestFitsStrSOPerm('Lgdim'),
					      makeBestFitsStrSOPerm('Frenthyman'),
					      makeBestFitsStrSOPerm('Nenthyman'),
					      makeBestFitsStrSOPerm('Freqhyman')
					      ),
				     MCCORESAct = 16
				     ))

defaultTSjiConfig = modConfig(c(defaultTConfig, defaultSjiConfig,
				list(runTbl=makeRunTbl(c(makeBestFitsVectTSji(''),
							 makeBestFitsVectTSji('Frentropy'),
							 makeBestFitsVectTSji('Freq'),
							 makeBestFitsVectTSji('Nentropy'))))),
			      list(accumModelHashtagsTbl=T,
				   actDVs=c('actPriorStd',
					    makeBestFitsStrTSji(''),
					    makeBestFitsStrTSji('Frentropy'),
					    makeBestFitsStrTSji('Freq'),
					    makeBestFitsStrTSji('Nentropy')),
				   MCCORESAct = 8
				   ))

defaultSOSjiConfig = modConfig(c(defaultSOConfig, defaultSjiConfig,
				 list(runTbl=makeRunTbl(c(list(c('actTitle', 'actBody')),
							  makeBestFitsVectSOSji(''),
							  makeBestFitsVectSOSji('Frentropy'),
							  makeBestFitsVectSOSji('Freq'),
							  makeBestFitsVectSOSji('Nentropy'))))),
			       list(accumModelHashtagsTbl=T,
				    actDVs=c('actPriorStd', 'actTitle_actBody',
					     makeBestFitsStrSOSji(''),
					     makeBestFitsStrSOSji('Frentropy'),
					     makeBestFitsStrSOSji('Freq'),
					     makeBestFitsStrSOSji('Nentropy')),
				    MCCORESAct = 4
				    ))

defaultPConfig = list(makeSjiTblUser='makeSjiTblUserDefault',
		      genGlobalsForPriorRun=T)

defaultSOSjiPConfig = modConfig(c(defaultSOConfig, defaultSjiConfig, defaultPConfig,
				  list(runTbl=makeRunTbl(list()))),
				list(actDVs = c('actPriorStd', 'actPriorOL', 'actPriorOL2'),
				     computeActFromContextTbl='computeActNullFromContextTbl',
				     priorTbl = 'priorTblUserSubset',
				     dStd = 'dFull',
				     MCCORESActUser=16,
				     MCCORESAct=1))

defaultTSjiPConfig = modConfig(c(defaultTConfig, defaultSjiConfig, defaultPConfig,
				 list(runTbl=makeRunTbl(list()))),
			       list(actDVs=c('actPriorStd', 'actPriorOL', 'actPriorOL2'),
				    computeActFromContextTbl='computeActNullFromContextTbl',
				    tokenizedTbl = 'tweets_tokenized',
				    priorTbl = 'priorTblUserSubset',
				    postsTbl = 'tweets',
				    dStd = 'dFull',
				    MCCORESActUser=16,
				    MCCORESAct=1))

defaultPUserConfig = modConfig(defaultPConfig, list(makeSjiTblUser='makeSjiTblUser'))

defaultSOSjiPUserConfig = modConfig(c(defaultSOConfig, defaultSjiConfig, defaultPUserConfig,
				      list(runTbl=makeRunTbl(c(makeBestFitsVectSOSji(''),
							       makeBestFitsVectSOSji('Usercontext'))),
					   sjiTblUser='sjiTblSOOrderlessUser')),
				    list(actDVs=c('actPriorStd',
						  makeBestFitsStrSOSji(''),
						  makeBestFitsStrSOSji('Usercontext')),
					 computeActFromContextTbl='computeActSjiOptFromContextTbl',
					 priorTbl='priorTblUserSubset',
					 convertTagSynonymsP=T,
					 MCCORESActUser=16,
					 MCCORESAct=1))


defaultSOPermPUserConfig = modConfig(c(defaultSOConfig, defaultPermConfig, defaultPUserConfig,
				      list(runTbl=makeRunTbl(c(makeBestFitsVectSOPerm('Freqhyman'),
							       makeBestFitsVectSOPerm('Freqhyser'))),
					   permEnvTbl='permEnvTblSO',
					   permMemMatOrder='',
					   permMemMatOrderless='permMemMatSOOrderless',
					   permMemMatOrderlessUser='permMemMatSOOrderlessUser',
					   computeActFromContextTbl='computeActPermSOOptFromContextTbl'
					   )),
				    list(actDVs=c('actPriorStd',
						  makeBestFitsStrSOPerm('Freqhyman'),
						  makeBestFitsStrSOPerm('Freqhyser')),
					 priorTbl='priorTblUserSubset',
					 convertTagSynonymsP=T,
					 MCCORESActUser=16,
					 MCCORESAct=1))

defaultGGPlotOpts <- theme_bw() + theme_classic()

runPriorT <- function(config) {
	runPrior(config)
}

runPriorSO <- function(config) {
	runPrior(config)
}

defaultTCols = "id::text, user_id, user_screen_name, created_at, retweeted, in_reply_to_status_id, lang, truncated, text, creation_epoch"

getQueryUsersSubset <- function(val, from) {
	sprintf('select user_screen_name from twitter_users where %s > %d order by %s asc limit 100', from, val, from)
}

getQueryGeneralT <- function(val, from, filters) {
	sprintf('%s and user_screen_name in (%s)', filters, getQueryUsersSubset(val, from))
}

getQueryT <- function(val, filters='1=1') {
	getQueryGeneralT(val, 'followers_count', filters)
}

getQueryTStatuses <- function(val, filters='1=1') {
	getQueryGeneralT(val, 'statuses_count', filters)
}

defaultSOCols = sprintf('id, owner_user_id, user_screen_name, creation_date, creation_epoch, title, tags')

getQuerySO <- function(val) {
	sprintf('owner_user_id in (select id from users where reputation > %d order by reputation asc limit 500)', val)
}

getQuerySOQ <- function(val) {
	sprintf("owner_user_id in (select id from users where num_questions > %d order by num_questions asc limit 100)", val)
}

makeTRun <- function(val, outFileName, config) {
	function() runPriorT(config=modConfig(config, list(query=getConfig(config, "query")(val), modelVsPredOutFile=getOutFileModelVsPred(outFileName))))
}

makeTRunr1 <- function(val, outFileName, query) {
	makeTRun(val, outFileName, config=modConfig(defaultTSjiPConfig, list(query=query, includeRetweetsP=T)))
}

makeTRunr2 <- function(val, outFileName, query) {
	makeTRun(val, outFileName, config=modConfig(defaultTSjiPConfig, list(query=query, includeRetweetsP=F)))
}

queryRunTFollow1k = getQueryT
queryRunTFollow5k = function(val) getQueryT(val, filters="user_screen_name != 'g4scareers'")
queryRunTFollow10k = function(val) getQueryT(val, filters="user_screen_name != 'so_pr'")
queryRunTFollow100k = function(val) getQueryT(val, filters="user_screen_name != 'hermosa_brisa'")
queryRunTFollow1M = getQueryT
queryRunTFollow10M = function(val) getQueryT(val)

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

getSOr1Config <- function () modConfig(defaultSOSjiPConfig, list(convertTagSynonymsP=F, query=getQuerySO))
getSOr2Config <- function () modConfig(defaultSOSjiPConfig, list(convertTagSynonymsP=T, query=getQuerySO))
getSOr3Config <- function () modConfig(defaultSOSjiPConfig, list(convertTagSynonymsP=F, query=getQuerySOQ))
getSOr4Config <- function () modConfig(defaultSOSjiPConfig, list(convertTagSynonymsP=T, query=getQuerySOQ))
makeSORunr1 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr1Config())
makeSORunr2 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr2Config())
makeSORunr3 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr3Config())
makeSORunr4 <- function(val, outFileName) makeSORun(val, outFileName, config=getSOr4Config())

makeSORunSjiPUser <- function(val, outFileName) {
	function (regen=F) {
		getCurWorkspaceBy(regen, groupConfigPUser)
		makeSORun(val, sprintf('%s%s', 'SOPUserSji', outFileName), modConfig(defaultSOSjiPUserConfig, list(query=getQuerySO)))()
		makeSORun(val, sprintf('%s%s', 'SOPUserPerm', outFileName), modConfig(defaultSOPermPUserConfig, list(query=getQuerySO, genGlobalsForPriorRun=F)))()
	}
}

makeSOQRunSjiPUser <- function(val, outFileName) {
	function (regen=F) {
		getCurWorkspaceBy(regen, groupConfigPUser)
		makeSOQRun(val, sprintf('%s%s', 'SOQPUserSji', outFileName), modConfig(defaultSOSjiPUserConfig, list(query=getQuerySO)))()
		makeSOQRun(val, sprintf('%s%s', 'SOQPUserPerm', outFileName), modConfig(defaultSOPermPUserConfig, list(query=getQuerySO, genGlobalsForPriorRun=F)))()
	}
}

runPUserSOSji100k <- makeSORunSjiPUser(100000, 'gt100k')
runPUserSOSji50k <- makeSORunSjiPUser(50000, 'gt50k')
runPUserSOSji10k <- makeSORunSjiPUser(10000, 'gt10k')
runPUserSOSji5k <- makeSORunSjiPUser(5000, 'gt5k')
runPUserSOSji1k <- makeSORunSjiPUser(1000, 'gt1k')
runPUserSOSji500 <- makeSORunSjiPUser(500, 'gt500')
runPUserSOQSji500 <- makeSOQRunSjiPUser(500, 'gt500')
runPUserSOQSji400 <- makeSOQRunSjiPUser(400, 'gt400')
runPUserSOQSji300 <- makeSOQRunSjiPUser(300, 'gt300')
runPUserSOQSji200 <- makeSOQRunSjiPUser(200, 'gt200')
runPUserSOQSji100 <- makeSOQRunSjiPUser(100, 'gt100')
runPUserSOQSji050 <- makeSOQRunSjiPUser(050, 'gt050')

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
	modelVsPredTbl[grepl('r[0-9]+$', datasetName), runNum := as.numeric(sub('^.*r([0-9]+).*$', "\\1", datasetName))]
}

addGroupNum <- function(modelVsPredTbl) {
	modelVsPredTbl[grepl('g[0-9]+', datasetName), groupNum := as.numeric(sub('^.*g([0-9]+).*$', "\\1", datasetName))]
}

addSizeNum <- function(modelVsPredTbl) {
	modelVsPredTbl[grepl('s[0-9]+', datasetName), sizeNum := as.numeric(sub('^.*s([0-9]+).*$', "\\1", datasetName))]
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
		addGroupNum(tbl)
		addSizeNum(tbl)
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

addMiscellaneous <- function(modelVsPredTbl) {
	modelVsPredTbl[, predUsedBest := F]
	modelVsPredTbl[topHashtag & hashtagUsedP & maxNP, predUsedBest := T]
	modelVsPredTbl[hashtagUsedP == T, acc := NCell/totN]
}

buildTables <- function(outFileNames) {
	buildTable <- function(outFileName) {
		colClasses = c('character', 'logical', 'logical', 'numeric', 'integer', 'character', 'logical', 'integer')
		tbl = myReadCSV(getOutFileModelVsPred(outFileName), colClasses=colClasses)
		tbl[, datasetName := outFileName]
		tbl
	}
	modelVsPredTbl = rbindlist(lapply(outFileNames, buildTable))
	modelVsPredTbl
	addRunNum(modelVsPredTbl)
	addGroupNum(modelVsPredTbl)
	addSizeNum(modelVsPredTbl)
	addDatasetNameRoot(modelVsPredTbl)
	addDatasetType(modelVsPredTbl)
	addDatasetGroup(modelVsPredTbl)
	addDatasetSize(modelVsPredTbl)
	addMiscellaneous(modelVsPredTbl)
	addModelType(modelVsPredTbl)
	stopifnot(nrow(modelVsPredTbl[dsetType == 'unknown']) == 0)
	modelVsPredTbl
}

withCI <- function(dat, CIFun=CI) {
	res = CIFun(dat)
	list(N=length(dat), meanVal=res[2], minCI=res[3], maxCI=res[1])
}

#FIXME: Remove this soon 
getComparisonTbl <- function(SD) {
	resTbl = copy(SD)
	resTbl[, DVDirection := sprintf('%s%s', direction, if (DVName != '') sprintf(' for %s', DVName) else ''), by=list(direction, DVName)]
	resTbl[!is.na(diff), withCI(diff), by=list(DVDirection, direction, DVName)]
}

#FIXME: Remove this soon 
compare2DVs <- function(modelVsPredTbl, DVs, sortedOrder=c(1,2)) {
	sumTbl = modelVsPredTbl[predUsedBest == T][DVName %in% DVs,]
	setkey(sumTbl, datasetName, user_screen_name, DVName)
	sumTbl[, list(diff=acc[sortedOrder[2]]-acc[sortedOrder[1]], direction=paste(DVName[sortedOrder[2]], '-', DVName[sortedOrder[1]])), by=list(datasetName, user_screen_name)][, DVName := ''][, getComparisonTbl(.SD)]
}

#FIXME: Remove this soon 
compare2Runs <- function(modelVsPredTbl, runNums) {
	sumTbl = modelVsPredTbl[predUsedBest == T][runNum %in% runNums]
	setkey(sumTbl, datasetNameRoot, DVName, user_screen_name, runNum)
	sumTbl[, list(diff=acc[2]-acc[1], direction=sprintf('run%s - run%s', runNums[2], runNums[1])), by=list(datasetNameRoot, DVName, user_screen_name)][, getComparisonTbl(.SD)]
}

#FIXME: Remove this soon 
compareDBestVsMin <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[topHashtag & hashtagUsedP & (maxNP | d == min(d))]
	setkey(sumTbl, datasetName, user_screen_name, DVName, maxNP) 
	sumTbl[, list(diff=acc[2]-acc[1], direction=paste('best d', '-', 'min d')), by=list(datasetName, user_screen_name, DVName)][, getComparisonTbl(.SD)]
}

#FIXME: Remove this soon 
compareDBestVsMax <- function(modelVsPredTbl) {
	sumTbl = modelVsPredTbl[topHashtag & hashtagUsedP & (maxNP | d == max(d))]
	setkey(sumTbl, datasetName, user_screen_name, DVName, maxNP) 
	sumTbl[, list(diff=acc[2]-acc[1], direction=paste('best d', '-', 'max d')), by=list(datasetName, user_screen_name, DVName)][, getComparisonTbl(.SD)]
}

plotBarSumTbl <- function(sumTbl, fillCol, figName, extras=NULL, groupCol='dsetGroup') {
	groupCol = as.symbol(groupCol)
	fillCol = substitute(fillCol)
	renameCols(sumTbl)
	sumTbl
	expr = bquote(ggplot(sumTbl, aes(x=factor(.(groupCol)), y=meanVal, fill=.(fillCol))) +
		      geom_bar(aes(y=meanVal), width=0.7, position=position_dodge(), stat='identity') +
		      geom_errorbar(aes(ymin=minCI, ymax=maxCI), position=position_dodge(width=0.7), width=0.1, size=0.3) + 
		      defaultGGPlotOpts)
	plot = eval(expr)
	lapply(extras, function(extra) plot <<- plot + extra)
	myPlotPrint(plot, figName)
	sumTbl
}

compareMeanDV <- function(modelVsPredTbl, DV, extras=NULL, figName='', groupCol='dsetGroup', CIFun=CI) {
	DV = substitute(DV)
	modelVsPredTbl
	expr = bquote(modelVsPredTbl[, withCI(.(DV), CIFun=CIFun), keyby=list(DVName, .(as.symbol(groupCol)))])
	expr
	sumTbl = eval(expr)
	sumTbl
	renameColDVName(sumTbl)
	plotBarSumTbl(sumTbl, DVName, sprintf('compareMeanDV-%s-%s', deparse(DV), figName), groupCol=groupCol,
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

renameCols <- function(tbl) {
	cnames = colnames(tbl)
	if ('dsetGroup' %in% cnames) renameColDatasetGroup(tbl)
	if ('groupNum' %in% cnames) renameColGroupNum(tbl)
	if ('sizeNum' %in% cnames) renameColSizeNum(tbl)
	if ('dsetType' %in% cnames) renameColDatasetType(tbl)
}

renameColGroupNum <- function(tbl) {
	mapTbl = data.table(groupNum=as.character(c(1,2,3,4)),
			    newName=c('Dataset 1', 'Dataset 2', 'Dataset 3', 'Dataset 4'), key='groupNum')
	tbl[, groupNum := as.character(groupNum)]
	setkey(tbl, groupNum)
	tbl[mapTbl, groupNum := newName]
}

renameColSizeNum <- function(tbl) {
	mapTbl = data.table(sizeNum=as.character(c(1,6,5,4,3,2)),
			    newName=c('Testing 1e5 SO, 3e6 Twitter', '1e3 Documents', '1e4 Documents', '1e5 Documents', '1e6 Documents', '3e6 Documents'))
	tbl[, sizeNum := as.character(sizeNum)]
	setkey(tbl, sizeNum)
	tbl[mapTbl, sizeNum := newName]
}

renameColDatasetGroup <- function(tbl) {
	setkey(tbl, dsetGroup)
	mapTbl = data.table(dsetGroup = c('topReputation', 'topQuestions', 'topFollowers', 'topTweets', 'sampleAcross', 'topHashtags'),
			    newName = c('SO Top Reputation', 'SO Top Questions', 'Twitter Top Followers', 'Twitter Top Tweets',
					'SO Sample Across', 'Twitter Popular Hashtags'))
	setkey(mapTbl, dsetGroup)
	tbl[mapTbl, dsetGroup := newName]
	tbl
}

renameColDatasetType <- function(tbl) {
	setkey(tbl, dsetType)
	mapTbl = data.table(dsetType = c('twitter', 'stackoverflow'),
			    newName = c('Twitter', 'Stack Overflow'),
			    key = 'dsetType')
	tbl[mapTbl, dsetType := newName]
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

groupN <- function(n, d) {
	split(d, ceiling(seq_along(d)/n))
}

renameColDVName <- function(tbl) {
	setkey(tbl, DVName)
	mapping = c('topHashtagAcrossPriorStd', 'Standard Prior Model Relaxed Across Posts',
		    'topHashtagPostPriorStd', 'Standard Prior Model',
		    'topHashtagPostPriorOL2', 'Optimized Learning Model',
		    makeStandardMappingBayes('', 'w/ entropy'),
		    makeStandardMappingBayes('Freq', 'w/ freq'),
		    makeStandardMappingBayes('Frentropy', 'w/ entropy and freq'),
		    makeStandardMappingBayes('Nentropy', ''),
		    makeStandardMapping('', ''),
		    makeStandardMapping('Entropy', 'w/ entropy'),
		    makeStandardMapping('Stoplist', 'w/ stoplist'),
		    makeStandardMapping('Hyman', 'w/ entropy and log odds'),
		    makeStandardMapping('Direction', 'w/ entropy and direction'),
		    makeStandardMapping('Window', 'w/ entropy and window'),
		    makeStandardMapping('Freq', 'w/ freq'),
		    makeStandardMapping('Frentropy', 'w/ entropy and freq'),
		    makeStandardMapping('Smdim', sprintf('w/ entropy and %s-row matrix', permNRowsSm)),
		    makeStandardMapping('Lgdim', 'w/ entropy and 10000-row matrix'),
		    makeStandardMapping('Frenthyman', 'w/ entropy and freq and log odds'),
		    makeStandardMapping('Nenthyman', 'w/ log odds'),
		    makeStandardMapping('Freqhyman', 'w/ freq and log odds')
		    )
	mapping = groupN(2, mapping)
	mapping
	mapTbl = data.table(DVName=sapply(mapping, `[`, 1), newName=sapply(mapping, `[`, 2))
	mapTbl
	setkey(mapTbl, DVName)
	tbl[mapTbl, DVName := newName]
	tbl
}

makeRemapForMapping <- function(keyword, text) {
	function(pair) {
		args = as.list(c(pair[1], rep(keyword, str_count(pair[1], '%s'))))
		args
		pair[1] = do.call(sprintf, args)
		pair[2] = sprintf(pair[2], text)
		pair
	}
}

makeStandardMappingBayes <- function(keyword, text) {
	res = c('topHashtagAcrossTitle%sBody%s', 'Bayes combined title and body %s',
		'topHashtagAcrossTitle%s', 'Bayes only title %s',
		'topHashtagAcrossBody%s', 'Bayes only body %s',
		'topHashtagAcrossPriorStdTitle%sBody%s', 'Bayes combined full %s',
		'topHashtagAcrossTweet%s', 'Bayes only orderless context %s',
		'topHashtagAcrossPriorStdTweet%s', 'Bayes combined full %s')
	res = groupN(2, res)
	res = lapply(res, makeRemapForMapping(keyword, text))
	unlist(res)
}

makeStandardMapping <- function(keyword, text) {
	res = c('topHashtagAcrossTitleOrderless%sBodyOrderless%s', 'RP combined title and body %s',
		'topHashtagAcrossTitleOrderless%s', 'RP only title %s',
		'topHashtagAcrossBodyOrderless%s', 'RP only body %s', 
		'topHashtagAcrossTweetOrder%sTweetOrderless%s', 'RP combined orderless and order %s',
		'topHashtagAcrossTweetOrder%s', 'RP only order context %s', 
		'topHashtagAcrossTweetOrderless%s', 'RP only orderless context %s', 
		'topHashtagAcrossPriorStdTweetOrder%sTweetOrderless%s', 'RP combined full %s', 
		'topHashtagAcrossPriorStdTitleOrderless%sBodyOrderless%s', 'RP combined full %s',
		'topHashtagAcrossPriorStdTitleOrderless%s', 'RP combined prior and title %s',
		'topHashtagAcrossPriorStdBodyOrderless%s', 'RP combined prior and body %s',
		'topHashtagAcrossPriorStdTweetOrderless%s', 'RP combined prior and orderless %s',
		'topHashtagAcrossPriorStdTweetOrder%s', 'RP combined prior and order %s')
	res = groupN(2, res)
	res = lapply(res, makeRemapForMapping(keyword, text))
	unlist(res)
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

compareOptimalDs <- function(modelVsPredTbl) compareMeanDV(modelVsPredTbl, d, list(labs(y='Mean Optimal Decay Rate Parameter (d)')), CIFun=CIMedian)
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
	runTbls = runPriorT(config=modConfig(defaultTSjiPConfig, list(accumModelHashtagsTbl=T,
								      query=sprintf("user_screen_name in (%s)", user_screen_names))))
	plotTemporal(runTbls)
	user_screen_names = wrapQuotes(c('520957','238260','413225','807325','521180'))
	user_screen_names = wrapQuotes(c('520957','238260'))
	runTbls = runPriorSO(config=modConfig(defaultSOSjiPConfig, list(accumModelHashtagsTbl=T,
									query=sprintf("user_screen_name in (%s)", user_screen_names))))
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
	bestFitNameTbl = do.call(data.table, c(bestFitNameTbl, list(actDVName=runTbl[, unique(c(predName, bestFitName))])))
	bestFitNameTbl
}

analyzePrior <- function(modelVsPredTbl) {
	analyzeTemporal(modelVsPredTbl)
	plotDatasetDescriptives(modelVsPredTbl)[, list(meanNUsers=mean(NUsers),totNUsers=sum(NUsers),meanNHO=mean(NHashtagObs),totNHO=sum(NHashtagObs)), by=list(dsetType)]
	# Check that totN calculated makes sense. Result should be small.
	modelVsPredTbl[topHashtag == T & d==min(d)][, list(totN, sum(NCell)), by=list(user_screen_name,d,DVName, datasetName)][!is.na(totN)][,list(res=totN-V2)][, withCI(res)]
	# Check that the Ns for each dataset look right	
	modelVsPredTbl[, list(N=.N, names=list(unique(datasetName))), by=list(dsetType, dsetGroup, runNum,datasetNameRoot)]
	bestTbl = modelVsPredTbl[predUsedBest == T]
	#dvDiffsTbl = plotDVDiffs(rbind(bestTbl[runNum==2, compare2DVs(.SD, c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2'), sortedOrder=c(1,2)), by=list(dsetType, dsetGroup), .SDcols=colnames(bestTbl)],
	#			       bestTbl[runNum==2, compare2DVs(.SD, c('topHashtagPostPriorStd', 'topHashtagAcrossPriorStd')), by=list(dsetType, dsetGroup), .SDcols=colnames(bestTbl)]
	#			       ))
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='TFollowgt10Mr2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorStd' & datasetName=='SOQgt500r2'])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorOL2' & datasetName=='TFollowgt10Mr2' & d < 1])
	visModelVsPredTbl(modelVsPredTbl[DVName=='topHashtagPostPriorOL2' & datasetName=='SOQgt500r2' & d < 1])
	compareOptimalDs(bestTbl[DVName %in% c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2') & runNum == 2])
	compareOptimalAcc(bestTbl[DVName %in% c('topHashtagPostPriorStd', 'topHashtagPostPriorOL2') & runNum == 2])
}

asTopHashtagAcross <- function(vect) {
	paste0('topHashtagAcross', vect)
}

asTopHashtagPost <- function(vect) {
	paste0('topHashtagPost', vect)
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
	myLog(ClassLog(logit, modelHashtagsTbl$hashtagUsedP))
	modelVsPredTbl
	
	modelVsPredTbl[dsetSize==500, list(N=.N, Ndsets=length(unique(datasetName))), keyby=list(runNum, groupNum, sizeNum, dsetSize, dsetType)]
	modelVsPredTbl[, list(N=.N, Ndsets=length(unique(datasetName))), keyby=list(sizeNum, dsetSize, dsetType)]
	plotPPVTbl(ppvTbl[dsetType=='stackoverflow' & runNum==1 & dsetSize==500], 'contextPpvSO')
	plotPPVTbl(ppvTbl[dsetType=='twitter' & runNum==1 & dsetSize==500], 'contextPpvT')
	baseTblPost = modelVsPredTbl[dsetSize==500][grepl('^topHashtagPost', DVName)]
	baseTbl = modelVsPredTbl[dsetSize==500][grepl('^topHashtagAcross', DVName)]
	tbl = baseTbl[predUsedBest == T]
	tbl[, .N, by=DVName]
	# SO full
	compareMeanDV(tbl[sizeNum == 2 & dsetType == 'stackoverflow'], acc, figName='ContextMeanDVSO')
	# T full
	compareMeanDV(tbl[sizeNum == 2 & dsetType == 'twitter'], acc, figName='foo', groupCol='dsetGroup')
	compareMeanDVDefault <- function(...) {
		compareMeanDV(..., extras=list(ylab('Proportion Correct')))
	}
	# RESULT: Show base performance for both models
	# SO standard
	stdRegex = '(Entropy)|(Direction)|(Hyman)|(Stoplist)|(Freq)|(Window)|(Frentropy)|(Smdim)|(Lgdim)'
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'stackoverflow' & !grepl(stdRegex, DVName)], acc, figName='contextStandardSO')
	# T standard
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & !grepl(stdRegex, DVName)], acc, figName='contextStandardT', groupCol='dsetGroup')
	# RESULT: 4 different subsets for twitter popular hashtags show same effects
	# T standard all groups
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & !grepl(stdRegex, DVName)], acc, figName='contextStandardByGroupT', groupCol='groupNum')
	# RESULT: Entropy weighting works for RP for SO and Twitter
	# SO Entropy
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleBody',
				       'BodyOrderlessEntropy', 'Body', 'BodyOrderless',
				       'TitleOrderlessEntropy', 'Title', 'TitleOrderless',
				       'PriorStdTitleOrderlessEntropyBodyOrderlessEntropy', 'PriorStdTitleOrderlessBodyOrderless'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='EntropyVsRegSO')
	# T Entropy 
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweet',
				       'TweetOrderlessEntropy', 'TweetOrderEntropy',
				       'TweetOrderless', 'TweetOrder',
				       'PriorStdTweetOrderTweetOrderless', 'PriorStdTweetOrderEntropyTweetOrderlessEntropy'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='EntropyVsRegT', groupCol='dsetGroup')
	# RESULT: Freq weighting is better than entropy weighting for RP, and standard stoplist doesn't work well at all
	# SO compare entropy to stoplist to freq to frentropy
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleBody', 'PriorStdTitleFrentropyBodyFrentropy', 'Title',
				       'TitleNentropy', 'TitleFreq', 'TitleFrentropy', 'PriorStdTitleNentropyBodyNentropy', 'PriorStdTitleFreqBodyFreq',
				       'Body', 'BodyNentropy', 'BodyFreq', 'BodyFrentropy'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='allWeightingsSOSji')
	DVNames = asTopHashtagAcross(c('PriorStd', 'TitleOrderlessEntropy', 'TitleOrderless', 'TitleOrderlessStoplist', 'TitleOrderlessFreq', 'TitleOrderlessFrentropy',
				       'PriorStdTitleOrderlessEntropyBodyOrderlessEntropy', 'PriorStdTitleOrderlessBodyOrderless',
				       'PriorStdTitleOrderlessStoplistBodyOrderlessStoplist',
				       'PriorStdTitleOrderlessFreqBodyOrderlessFreq',
				       'PriorStdTitleOrderlessFrentropyBodyOrderlessFrentropy'
				       ))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='allWeightingsSOPerm')
	# T compare entropy to stoplist to freq to frentropy
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweet', 'PriorStdTweetFrentropy', 'Tweet',
				       'TweetNentropy', 'TweetFreq', 'TweetFrentropy', 'PriorStdTweetNentropy', 'PriorStdTweetFreq'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='allWeightingsTSji', groupCol='dsetGroup')
	DVNames = asTopHashtagAcross(c('PriorStd', 'TweetOrderlessEntropy', 'TweetOrderless', 'TweetOrderlessStoplist', 'TweetOrderlessFreq', 'TweetOrderlessFrentropy',
				       'PriorStdTweetOrderEntropyTweetOrderlessEntropy', 'PriorStdTweetOrderTweetOrderless',
				       'PriorStdTweetOrderStoplistTweetOrderlessStoplist',
				       'PriorStdTweetOrderFreqTweetOrderlessFreq',
				       'PriorStdTweetOrderFrentropyTweetOrderlessFrentropy'
				       ))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='allWeightingsTPerm', groupCol='dsetGroup')
	# RESULT: Bayes and RP with frequency cutoff scales; RP with entropy saturates for SO
	# SO Entropy all sizes
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleBody', 
				       'PriorStdTitleOrderlessEntropyBodyOrderlessEntropy', 'PriorStdTitleOrderlessBodyOrderless',
				       'PriorStdTitleOrderlessFreqBodyOrderlessFreq',
				       'PriorStdTitleFrentropyBodyFrentropy',
				       'PriorStdTitleOrderlessSmdimBodyOrderlessSmdim',
				       'PriorStdTitleOrderlessLgdimBodyOrderlessLgdim'))
	compareMeanDVDefault(tbl[dsetType == 'stackoverflow' & DVName %in% DVNames & sizeNum != 2], acc, figName='freqVsEntropyBySizeSO', groupCol='sizeNum')
	# T Entropy all sizes
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweet',
				       'PriorStdTweetOrderTweetOrderless', 'PriorStdTweetOrderEntropyTweetOrderlessEntropy',
				       'PriorStdTweetOrderFreqTweetOrderlessFreq',
				       'PriorStdTweetOrderSmdimTweetOrderlessSmdim',
				       'PriorStdTweetOrderLgdimTweetOrderlessLgdim'))
	compareMeanDVDefault(tbl[dsetType == 'twitter' & DVName %in% DVNames], acc, figName='freqVsEntropyBySizeT', groupCol='sizeNum')
	# RESULT: Increasing rows in matrix does not dramatically improve performance, even at different size datasets; entropy and freq cutoff is much more important for RP
	# SO Entropy all sizes
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleBody', 
				       'PriorStdTitleOrderlessEntropyBodyOrderlessEntropy', 'PriorStdTitleOrderlessBodyOrderless',
				       'PriorStdTitleOrderlessFreqBodyOrderlessFreq',
				       'PriorStdTitleOrderlessSmdimBodyOrderlessSmdim',
				       'PriorStdTitleOrderlessLgdimBodyOrderlessLgdim'))
	compareMeanDVDefault(tbl[dsetType == 'stackoverflow' & DVName %in% DVNames & sizeNum != 2], acc, figName='dimBySizeSO', groupCol='sizeNum')
	# T Entropy all sizes
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweet',
				       'PriorStdTweetOrderTweetOrderless', 'PriorStdTweetOrderEntropyTweetOrderlessEntropy',
				       'PriorStdTweetOrderFreqTweetOrderlessFreq',
				       'PriorStdTweetOrderSmdimTweetOrderlessSmdim',
				       'PriorStdTweetOrderLgdimTweetOrderlessLgdim'))
	compareMeanDVDefault(tbl[dsetType == 'twitter' & DVName %in% DVNames], acc, figName='dimBySizeT', groupCol='sizeNum')
	# RESULT: Logodds technique works
	# SO compare entropy w/ logodds to entropy
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleBody',
				       'PriorStdTitleOrderlessFreqBodyOrderlessFreq', 'PriorStdTitleOrderlessBodyOrderless',
				       'PriorStdTitleOrderlessFreqhymanBodyOrderlessFreqhyman',
				       'PriorStdTitleOrderlessNenthymanBodyOrderlessNenthyman'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='logoddsSO')
	# T compare entropy w/ loggodds to entropy
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweet',
				       'PriorStdTweetOrderFreqTweetOrderlessFreq', 'PriorStdTweetOrderTweetOrderless',
				       'PriorStdTweetOrderFreqhymanTweetOrderlessFreqhyman',
				       'PriorStdTweetOrderNenthymanTweetOrderlessNenthyman'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='logoddsT', groupCol='dsetGroup')
	# SO compare all additions for RP
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleOrderlessFrenthymanBodyOrderlessFrenthyman',
				       'PriorStdTitleOrderlessNenthymanBodyOrderlessNenthyman',
				       'PriorStdTitleOrderlessFreqhymanBodyOrderlessFreqhyman',
				       'PriorStdTitleOrderlessHymanBodyOrderlessHyman',
				       'PriorStdTitleOrderlessBodyOrderless',
				       'PriorStdTitleOrderlessFreqBodyOrderlessFreq',
				       'PriorStdTitleOrderlessEntropyBodyOrderlessEntropy',
				       'TitleOrderlessBodyOrderless',
				       'TitleOrderlessFreqBodyOrderlessFreq'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='logoddsSO')
	# SO compare all additions for Bayes
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleBody', 'TitleBody', 'PriorStdTitleFreqBodyFreq', 'PriorStdTitleNentropyBodyNentropy',
				       'PriorStdTitleFrentropyBodyFrentropy'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='logoddsSO')
	# T compare all additions for RP
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweetOrderFrenthymanTweetOrderlessFrenthyman',
				       'PriorStdTweetOrderNenthymanTweetOrderlessNenthyman',
				       'PriorStdTweetOrderFreqhymanTweetOrderlessFreqhyman',
				       'PriorStdTweetOrderHymanTweetOrderlessHyman',
				       'PriorStdTweetOrderTweetOrderless',
				       'PriorStdTweetOrderFreqTweetOrderlessFreq',
				       'PriorStdTweetOrderEntropyTweetOrderlessEntropy',
				       'TweetOrderFreqTweetOrderlessFreq'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='priorVsContextT')
	# T compare all additions for Bayes
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweet', 'PriorStdTweetNentropy', 'PriorStdTweetFreq', 'PriorStdTweetFrentropy', 'Tweet'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='priorVsContextT')
	# RESULT: Not much information in order for RP
	# T compare entropy w/ direction and entropy w/ window to entropy
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTweet',
				       'TweetOrderEntropy', 'Tweet', 'TweetOrder', 'TweetOrderDirection', 'TweetOrderWindow',
				       'TweetOrderlessEntropy', 'TweetOrderless', 'TweetOrderlessDirection', 'TweetOrderlessWindow',
				       'PriorStdTweetOrderEntropyTweetOrderlessEntropy', 'PriorStdTweetOrderTweetOrderless', 'PriorStdTweetOrderDirectionTweetOrderlessDirection',
				       'PriorStdTweetOrderWindowTweetOrderlessWindow'))
	compareMeanDVDefault(tbl[sizeNum == 2 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='orderT', groupCol='dsetGroup')
	# RESULT: .7 d is better for SO but not for T, lines up with 'relaxed across posts' finding for Prior dataset. Magnitude of effect is very small, not as important as using prior and weighting
	# SO and T compare two d values	
	dTbl = rbind(baseTblPost, baseTbl)[topHashtag & hashtagUsedP]
	DVNames = c('topHashtagPostPriorStd', asTopHashtagAcross(c('PriorStd', 'PriorStdTweet', 'PriorStdTweetOrderHymanTweetOrderlessHyman',
								   'PriorStdTitleBody', 'PriorStdTitleOrderlessHymanBodyOrderlessHyman')))
	dWideTbl = getDWideTbl(dTbl[sizeNum == 2 & DVName %in% DVNames])
	compareMeanDVDefault(dWideTbl, dDiff, figName='dContext', groupCol='dsetType')
}

getDWideTbl <- function(tbl) {
	tbl = copy(tbl)[, d := gsub('\\.', 'p', paste0('d', d))]
	lhs = setdiff(colnames(tbl), c('d', 'acc', 'maxNP', 'NCell', 'predUsedBest'))
	lhs = paste0(lhs, c(rep(' + ', length(lhs)-1), ' ~ '), collapse='')
	model = sprintf('%s%s', lhs, 'd')
	model
	resTbl = dcast.data.table(tbl, model, value.var='acc')
	resTbl[, dDiff := d0p7 - d0p5]
	resTbl
}

analyzeContextSmall <- function() {
	tbl = modelVsPredTbl[predUsedBest & dsetSize==20 & grepl('^topHashtagAcross', DVName)]
	compareMeanDV(tbl[sizeNum == 6 & dsetType == 'stackoverflow'], acc, figName='foo')
	compareMeanDV(tbl[sizeNum == 6 & dsetType == 'twitter'], acc, figName='foo')
	DVNames = asTopHashtagAcross(c('PriorStd', 'PriorStdTitleBody', 'PriorStdTitleOrderlessBodyOrderless',
				       'PriorStdTitleOrderlessEntropyBodyOrderlessEntropy',
				       'PriorStdTitleFrentropyBodyFrentropy', 'PriorStdTitleOrderlessFrentropyBodyOrderlessFrentropy'))
	compareMeanDV(tbl[sizeNum == 6 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='foo')
	DVNames = asTopHashtagAcross(c('PriorStdTitleOrderlessEntropyBodyOrderlessEntropy', 'PriorStdTitleOrderlessLgdimBodyOrderlessLgdim',
				       'PriorStdTitleOrderlessSmdimBodyOrderlessSmdim', 'TitleOrderlessLgdimBodyOrderlessLgdim',
				       'TitleOrderlessEntropyBodyOrderlessEntropy', 'TitleOrderlessSmdimBodyOrderlessSmdim'))
	compareMeanDV(tbl[sizeNum == 6 & dsetType == 'stackoverflow' & DVName %in% DVNames], acc, figName='foo')
	DVNames = asTopHashtagAcross(c('PriorStdTweetOrderEntropyTweetOrderlessEntropy', 'PriorStdTweetOrderSmdimTweetOrderlessSmdim',
				       'PriorStdTweetOrderLgdimTweetOrderlessLgdim', 'TweetOrderEntropyTweetOrderlessEntropy',
				       'TweetOrderSmdimTweetOrderlessSmdim', 'TweetOrderLgdimTweetOrderlessLgdim'))
	compareMeanDV(tbl[sizeNum == 6 & dsetType == 'twitter' & DVName %in% DVNames], acc, figName='foo')
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

genTokenizedTblTwitter <- function(bundleSize=10000, query, tokenizedTblName) {
	withDBConnect(dbCon,
		      {dbRs = dbSendQuery(dbCon, query)
		      writePartialTbl <- function(tbl) {
			      setcolorder(tbl, c('id', 'chunk', 'pos', 'type'))
			      appendDTbl(tbl, tokenizedTblName)
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

genTokenizedTblTwitterContext <- function(hashtagGroup) {
	query = sprintf("select %s from top_hashtag_tweets 
			where hashtag_group = '%s'
			and id not in (select distinct id from top_hashtag_tokenized)
			",
			defaultTCols, hashtagGroup, hashtagGroup)
	genTokenizedTblTwitter(query=query, tokenizedTblName='top_hashtag_tokenized')
}

genTokenizedTblTwitterPrior <- function() {
	query = sprintf("select %s from tweets
			where id not in (select distinct id from tweets_tokenized)
			and id not in (select id from tweets_filtered)",
			defaultTCols)
	genTokenizedTblTwitter(query=query, tokenizedTblName='tweets_tokenized')
}

addFilteredPosts <- function() {
	ids = c('17801882', '9965709', '9204391', '15837898', '18893489', '20156201', '3245809')
	reasons = c('java so', 'java so', 'java so', 'java so', 'java so', 'java so', 'nonprintable U+FFFF')
	filteredPostsTbl = data.table(post_id=ids, reason=reasons);
	setcolorder (filteredPostsTbl, c('post_id', 'reason'))
	appendDTbl(filteredPostsTbl, 'post_filtered')
}

addFilteredTweets <- function() {
	sqldf('truncate table tweets_filtered')
	ids = c('12466832063')
	reasons = c('nonprintable')
	filteredTweetsTbl = data.table(id=ids, reason=reasons);
	setcolorder(filteredTweetsTbl, c('id', 'reason'))
	appendDTbl(filteredTweetsTbl, 'tweets_filtered')
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
	sqldf('select * from fill_top_hashtag_tweets_creation_epoch()')
	sqldf('vacuum analyze top_hashtag_tweets')
	runMakeTarget('parrunSjisTAll')
	runPythonFun('backupTopHashtags()')
}

runGenNcoocTblSO1thru100 <- function() genNcoocTblSO('SOShuffledFull', 1, 100)
runGenNcoocTblSO1thru1000 <- function() genNcoocTblSO('SOShuffledFull', 1, 1000)
runGenNcoocTblSO1thru10000 <- function() genNcoocTblSO('SOShuffledFull', 1, 10000)
runGenNcoocTblSO1thru100000 <- function() genNcoocTblSO('SOShuffledFull', 1, 100000)
runGenNcoocTblSO1thru1000000 <- function() genNcoocTblSO('SOShuffledFull', 1, 1000000)
runGenNcoocTblSO1thru3000000 <- function() genNcoocTblSO('SOShuffledFull', 1, 3000000)

runGenNcoocTblT11thru100 <- function() genAllNcoocTblTwitter(1, 100)
runGenNcoocTblT11thru1000 <- function() genAllNcoocTblTwitter(1, 1000)
runGenNcoocTblT11thru10000 <- function() genAllNcoocTblTwitter(1, 10000)
runGenNcoocTblT11thru100000 <- function() genAllNcoocTblTwitter(1, 100000)
runGenNcoocTblT11thru1000000 <- function() genAllNcoocTblTwitter(1, 1000000)
runGenNcoocTblT11thru3000000 <- function() genAllNcoocTblTwitter(1, 3000000)

runGenTokenizedTblSO <- function() genTokenizedTblSO()
runGenTokenizedTblTwitterContext <- function() {
	for (groupName in getConfig(defaultTConfig, 'allGroupNames')) {
		genTokenizedTblTwitterContext(groupName)
	}
}

runGenTokenizedTblTwitterPrior <- genTokenizedTblTwitterPrior

getSjiTblSO <- function(config, startId, endId) {
	fileName = sprintf('%s.csv', makeSubsetName(getConfig(config, "groupName"), startId, endId))
	sjiTitleName = paste('NcoocTblTitle', fileName, sep='-')
	sjiBodyName = paste('NcoocTblBody', fileName, sep='-')
	sjiColClasses = c('character', 'character', 'integer', 'integer', 'integer')	
	sjiTitleTbl = myReadCSV(sprintf('%s/%s', getDirCooc(), sjiTitleName), colClasses=sjiColClasses)
	sjiBodyTbl = myReadCSV(sprintf('%s/%s', getDirCooc(), sjiBodyName), colClasses=sjiColClasses)
	initSjiTblSO(sjiTitleTbl, sjiBodyTbl)
}

initSjiTblSO <- function(sjiTitleTbl, sjiBodyTbl) {
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
	initSjiTblT(sjiTbl)
}

initSjiTblT <- function(sjiTbl) {
	sjiTbl[, context := chunk][, chunk := NULL]
	sjiTbl[, hashtag := tag][, tag := NULL]
	topHashtagsTbl = sqldt(sprintf("select hashtag from top_hashtag_hashtags where hashtag_group = '%s'", getConfig(config, "groupName")))
	sjiTbl = sjiTbl[hashtag %in% topHashtagsTbl[, hashtag]]
	sjiTbl
}

getSjiTblTOrderless <- function(config, startId, endId) {
	sjiTbl = getSjiTblT(config, startId, endId)
	initSjiTblTOrderless(sjiTbl)
}

initSjiTblTOrderless <- function(sjiTbl) {
	sjiTbl = initSjiTblT(sjiTbl)
	sjiTbl = sjiTbl[, list(posFromTag=0, partialN=sum(partialN)), by=list(context, hashtag)]
	setkey(sjiTbl, context, hashtag, posFromTag)
	addSjiAttrs(sjiTbl)
	sjiTbl
}

getSjiTblTOrder <- function(config, startId, endId) {
	sjiTbl = getSjiTblT(config, startId, endId)
	initSjiTblTOrder(sjiTbl)
}

initSjiTblTOrder <- function(sjiTbl) {
	sjiTbl = initSjiTblT(sjiTbl)
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

defaultColsPriorUserSO = 'creation_epoch, user_screen_name, chunk'
defaultColsPriorUserT = sprintf('%s, %s', defaultColsPriorUserSO, 'posts_tbl.retweeted')

getPriorTblUserSubset <- function(config) {
	tbl = sqldt(sprintf("select %s from %s as tokenized_tbl
			    join %s as posts_tbl
			    on posts_tbl.id = tokenized_tbl.id
			    where type = '%s'
			    and user_screen_name is not null
			    and %s
			    and tokenized_tbl.id in (select id from %s where %s)",
			    get(getConfig(config, 'defaultColsPriorUser')),
			    getConfig(config, "tokenizedTbl"), getConfig(config, "postsTbl"),
			    getConfig(config, "tagTypeName"), getConfig(config, 'query'),
			    getConfig(config, 'postsTbl'), getConfig(config, 'query')
			    ))
	if (getConfig(config, "convertTagSynonymsP")) convertTagSynonyms(tbl)
	tbl[, hashtag := chunk][, chunk := NULL]
	if (getConfig(config, 'dsetType') == 'twitter') {
		tbl = tbl[retweeted %in% getAllowedRetweetedVals(config)]
	}
	addDtToTbl(tbl)
	setkey(tbl, user_screen_name, dt, hashtag)
	tbl
}

createStoplistTbl <- function() {
	sqldf('truncate table stoplists')
	sqldf('create table stoplists (
	      chunk text not null,
	      lst text not null,
	      primary key (chunk, lst))')
	stoplistTbl = myReadCSV(sprintf('%s/dissertationData/stoplists/smart.txt', PATH), header=F)
	stoplistTbl[, chunk := V1][, V1 := NULL][, lst := 'smart']
	setkey(stoplistTbl, chunk, lst)
	myStopifnot(!duplicated(stoplistTbl))
	appendDTbl(stoplistTbl, 'stoplists')
}

getStoplistTbl <- function() {
	resTbl = sqldt("select chunk from stoplists where lst = 'smart'")
	setkey(resTbl, chunk)
	resTbl
}

addSjiAttrs <- function(sjiTbl) {
	sjiTbl[, contextSums := sum(partialN), by=context]
	sjiTbl[, hashtagSums := sum(partialN), by=hashtag]
	sjiTbl[, sji := log(sum(partialN)) + log(partialN) - log(contextSums) - log(hashtagSums)]
	sjiTbl[, pHashtagGivenContext := partialN/contextSums]
	sjiTbl[, HContext := - sum(pHashtagGivenContext * log(pHashtagGivenContext)), by=context]
	sjiTbl[, EContext := 1 - HContext/max(HContext)]
	sjiTbl[, stopWordWeight := 1]
	stoplistTbl = getStoplistTbl()
	sjiTbl[stoplistTbl, stopWordWeight := 0]
	totFreq = sjiTbl[J(unique(context)), mult='first'][, sum(contextSums)]
	sjiTbl[, pFreq := contextSums/totFreq]
	sjiTbl[, freqWeight := 1]
	sjiTbl[pFreq > .0004, freqWeight := 0]
	sjiTbl
}

computeActSji <- function(contextVect, sjiTbl, userScreenName, config) {
	contextVect
	myLog(sprintf("computing sji act for context with length %s", length(contextVect)))
	resTbl = sjiTbl[J(contextVect), nomatch=0, allow.cartesian=T]
	if (getConfig(config, 'sjiFreqP')) {
		resTbl = resTbl[freqWeight > 0]
	}
	if (! getConfig(config, 'sjiEntropyP')) {
		resTbl[, EContext := 1]
	}
	resTbl = resTbl[, {WContext = EContext/sum(EContext); list(act=sum(WContext * sji))}, keyby=hashtag]
	resTbl
}

getSjiTblFromUserTbl <- function(sjiTbl, userScreenName) {
	curSjiTbl = sjiTbl[J(userScreenName)][, user_screen_name := NULL]
	setkey(curSjiTbl, context, hashtag, posFromTag)
	curSjiTbl
}

computeActSjiUser <- function(contextVect, sjiTbl, userScreenName, config) {
	curSjiTbl = getSjiTblFromUserTbl(sjiTbl, userScreenName)
	resTbl = computeActSji(contextVect, curSjiTbl, userScreenName, config)
	resTbl
}

getWsFile <- function(groupConfig) {
	wsFile = with(groupConfig, sprintf('%s/workspace-g%ss%s%s.RData', '/Volumes/SuperSupernova/RWorkspace', groupNum, sizeNum, configType))
	wsFile
}

mySaveImage <- function(groupConfig) {
	file=getWsFile(groupConfig)
	eval(bquote(save(list=c('sjiTblTOrderless', 'sjiTblTOrder', 'priorTblGlobT', 'sjiTblSOOrderless', 'priorTblUserSO',
			       'permEnvTblT', 'permEnvTblSO', 'permMemMatTOrder', 'permMemMatTOrderless',
			       'permMemMatSOOrderless'), file=.(file), compress=F)),
	     envir=globalenv())
}

myLoadImage <- function(groupConfig) {
	file=getWsFile(groupConfig)
	eval(bquote(load(file=.(file))),
	     envir=globalenv())
}

getFunConfigModsPerm <- function(permUseEntropyP=F, permUseStoplistP=F, permOnlyDirectionP=F, permHymanP=F,
			     permUseFreqP=F, permUseWindowP=F, permNRows=getConfig(defaultBaseConfig, 'permNRows')) {
	list(permUseEntropyP=permUseEntropyP,
	     permUseStoplistP=permUseStoplistP,
	     permOnlyDirectionP=permOnlyDirectionP,
	     permHymanP=permHymanP,
	     permUseFreqP=permUseFreqP,
	     permUseWindowP=permUseWindowP,
	     permNRows=permNRows)
}

getFunConfigModsSji <- function(sjiFreqP=F, sjiEntropyP=T) {
	list(sjiFreqP=sjiFreqP,
	     sjiEntropyP=sjiEntropyP)
}

funConfigOrig <- function(config) modConfig(config, getFunConfigModsPerm())
funConfigEntropy <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T))
funConfigStoplist <- function(config) modConfig(config, getFunConfigModsPerm(permUseStoplistP=T))
funConfigFreq <- function(config) modConfig(config, getFunConfigModsPerm(permUseFreqP=T))
funConfigDirection <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T, permOnlyDirectionP=T))
funConfigHyman <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T, permHymanP=T))
funConfigWindow <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T, permUseWindowP=T))
funConfigFrentropy <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T, permUseFreqP=T))
funConfigSmdim <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T, permNRows=permNRowsSm))
funConfigLgdim <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T, permNRows=10000))
funConfigFrenthyman <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=T, permUseFreqP=T, permHymanP=T)) 
funConfigNenthyman <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=F, permUseFreqP=F, permHymanP=T)) 
funConfigFreqhyman <- function(config) modConfig(config, getFunConfigModsPerm(permUseEntropyP=F, permUseFreqP=T, permHymanP=T)) 

funConfigOrigSji <- function(config) modConfig(config, getFunConfigModsSji())
funConfigFrentropySji <- function(config) modConfig(config, getFunConfigModsSji(sjiFreqP=T))
funConfigNentropySji <- function(config) modConfig(config, getFunConfigModsSji(sjiEntropyP=F))
funConfigFreqSji <- function(config) modConfig(config, getFunConfigModsSji(sjiFreqP=T, sjiEntropyP=F))

makeCombinedMemMat <- function(sjiTbl, envTbl, config) {
	res = list()
	res[['direction']] = makeMemMat(sjiTbl, envTbl, funConfigDirection(config))
	res[['entropy']] = makeMemMat(sjiTbl, envTbl, funConfigEntropy(config))
	res[['stoplist']] = makeMemMat(sjiTbl, envTbl, funConfigStoplist(config))
	res[['freq']] = makeMemMat(sjiTbl, envTbl, funConfigFreq(config))
	res[['window']] = makeMemMat(sjiTbl, envTbl, funConfigWindow(config))
	res[['orig']] = makeMemMat(sjiTbl, envTbl, funConfigOrig(config))
	res[['frentropy']] = makeMemMat(sjiTbl, envTbl, funConfigFrentropy(config))
	res[['lgdim']] = makeMemMat(sjiTbl, envTbl, funConfigLgdim(config))
	res[['smdim']] = makeMemMat(sjiTbl, envTbl, funConfigSmdim(config))
	res
}

makeCombinedMemMatPUser <- function(sjiTbl, envTbl, config) {
	res = list()
	res[['freq']] = makeMemMat(sjiTbl, envTbl, funConfigFreq(config))
	res
}

makeCombinedEnvironmentTbl <- function(sjiTbl, config) {
	res = list()
	accs = as.character(getConfig(config, 'permNRowsAll'))
	for (acc in accs) {
		res[[acc]] = makeEnvironmentTbl(sjiTbl, modConfig(config, list(permNRows=as.integer(acc))))
	}
	res
}

plotMemMat <- function() {
	sdStoplist = apply(permMemMatSOOrderless$stoplist, 2, sd)
	sdFreq = apply(permMemMatSOOrderless$freq, 2, sd)
	sdEntropy = apply(permMemMatSOOrderless$entropy, 2, sd)
	sdOrig = apply(permMemMatSOOrderless$orig, 2, sd)
	sdStoplist = apply(permMemMatTOrderless$stoplist, 2, sd)
	sdFreq = apply(permMemMatTOrderless$freq, 2, sd)
	sdEntropy = apply(permMemMatTOrderless$entropy, 2, sd)
	sdOrig = apply(permMemMatTOrderless$orig, 2, sd)
	permEnvTblSO[, .N, by=stopWordWeight]
	permEnvTblSO[, .N, by=freqWeight]
	permEnvTblT[, .N, by=stopWordWeight]
	permEnvTblT[, .N, by=freqWeight]
	permEnvTblSO
	sd(sdOrig)
	sd(sdEntropy)
	sd(sdFreq)
	sd(sdStoplist)
	plot(sdOrig)
	plot(sdStoplist)
	plot(sdEntropy)
	plot(sdFreq)
}

getCurWorkspace <- function(groupConfig) {
	getConfig(groupConfig, 'genFun')(groupConfig)
}

getCurWorkspaceContext <- function(groupConfig) {
	maxIdSOSji = getConfig(groupConfig, 'maxIdSOSji')
	maxIdSOPrior = getConfig(groupConfig, 'maxIdSOPrior')
	maxIdTSji = getConfig(groupConfig, 'maxIdTSji')
	maxIdTPrior = getConfig(groupConfig, 'maxIdTPrior')
	configT = modConfig(defaultTConfig, list(groupName=getConfig(groupConfig, 'groupName')))
	configTPerm = modConfig(defaultTPermConfig, list(groupName=getConfig(groupConfig, 'groupName')))
	priorTblGlobT <<- getPriorTblGlobT(configT, 1, maxIdTPrior)
	priorTblUserSO <<- getPriorTblUserSO(defaultSOConfig, 1, maxIdSOPrior)
	sjiTblTOrderless <<- getSjiTblTOrderless(configT, 1, maxIdTSji)
	sjiTblTOrder <<- getSjiTblTOrder(configT, 1, maxIdTSji)
	sjiTblSOOrderless <<- getSjiTblSO(defaultSOConfig, 1, maxIdSOSji)
	permEnvTblT <<- makeCombinedEnvironmentTbl(sjiTblTOrderless, defaultBaseConfig)
	permEnvTblSO <<- makeCombinedEnvironmentTbl(sjiTblSOOrderless, defaultBaseConfig)
	permMemMatTOrder <<- makeCombinedMemMat(sjiTblTOrder, permEnvTblT, configTPerm)
	permMemMatTOrderless <<- makeCombinedMemMat(sjiTblTOrderless, permEnvTblT, configTPerm)
	permMemMatSOOrderless <<- makeCombinedMemMat(sjiTblSOOrderless, permEnvTblSO, defaultSOPermConfig)
	return()
}

getCurWorkspacePUser <- function(groupConfig) {
	maxIdSOSji = getConfig(groupConfig, 'maxIdSOSji')
	maxIdSOPrior = getConfig(groupConfig, 'maxIdSOPrior')
	maxIdTSji = getConfig(groupConfig, 'maxIdTSji')
	maxIdTPrior = getConfig(groupConfig, 'maxIdTPrior')
	sjiTblSOOrderless <<- getSjiTblSO(defaultSOConfig, 1, maxIdSOSji)
	permEnvTblSO <<- makeCombinedEnvironmentTbl(sjiTblSOOrderless, modConfig(defaultBaseConfig, list(permNRowsAll=getConfig(defaultBaseConfig, 'permNRows'))))
	permMemMatSOOrderless <<- makeCombinedMemMatPUser(sjiTblSOOrderless, permEnvTblSO, defaultSOPermConfig)
}

genAndSaveCurWorkspace <- function(groupConfig) {
	getCurWorkspace(groupConfig)
	mySaveImage(groupConfig)
}

computeActSjiFromContextTbl <- function(contextTbl, tagTbl, config) {
	contextTbl = rbind(makeComputeActRunTbl(contextTbl, '', 'computeActSji', 'funConfigOrigSji'),
			   makeComputeActRunTbl(contextTbl, 'Frentropy', 'computeActSji', 'funConfigFrentropySji'),
			   makeComputeActRunTbl(contextTbl, 'Nentropy', 'computeActSji', 'funConfigNentropySji'),
			   makeComputeActRunTbl(contextTbl, 'Freq', 'computeActSji', 'funConfigFreqSji'))
	contextTbl[, get(fun[1])(chunk, get(getConfig(config, 'sjiTbl')), unique(user_screen_name), get(funConfig)(config)), by=type]
}

computeActSjiOptFromContextTbl <- function(contextTbl, tagTbl, config) {
	contextTbl = rbind(makeComputeActRunTbl(contextTbl, '', 'computeActSji', 'funConfigOrigSji')[, sjiTblName := getConfig(config, 'sjiTbl')],
			   makeComputeActRunTbl(contextTbl, 'Usercontext', 'computeActSjiUser', 'funConfigOrigSji')[, sjiTblName := getConfig(config, 'sjiTblUser')])
	contextTbl
	contextTbl[, get(fun[1])(chunk, get(sjiTblName), unique(user_screen_name), get(funConfig)(config)), by=type]
}

computeActNullFromContextTbl <- function(contextTbl, tagTbl, config) {
	data.table(type=character(), hashtag=character(), act=double())
}

makeComputeActRunTbl <- function(tbl, typeExtra, fun, funConfig) {
	eval(bquote(copy(tbl)[, type := paste0('act', capitalize(type), .(typeExtra))][, fun := .(fun)][, funConfig := .(funConfig)]))
}

makeComputeActRunTblPerm <- function(tbl, typeExtra, funConfig) {
	makeComputeActRunTbl(tbl, typeExtra, 'computeActPerm', funConfig)
}

makeComputeActRunTblPermOrderless <- function(tblOrderless, typeExtra, funConfig) {
	makeComputeActRunTblPerm(tblOrderless, sprintf('%s%s', 'Orderless', typeExtra), funConfig)[, fun := paste0(fun, 'Orderless')]
}

makeComputeActRunTblPermOrders <- function(tblOrder, tblOrderless, typeExtra, funConfig) {
	rbind(makeComputeActRunTblPerm(tblOrder,
				       typeExtra = sprintf('%s%s', 'Order', typeExtra),
				       funConfig = funConfig)[, fun := paste0(fun, 'Order')],
	      makeComputeActRunTblPermOrderless(tblOrderless, typeExtra, funConfig))
}

makeComputeActRunTblPermOrdersAll <- function(tblOrder, tblOrderless, lst) {
	res = groupN(2, lst)
	typeExtras = sapply(res, `[[`, 1, USE.NAMES=F)
	funConfigs = sapply(res, `[[`, 2, USE.NAMES=F)
	fun <- function(i) makeComputeActRunTblPermOrders(tblOrder, tblOrderless, typeExtras[i], funConfigs[i])
	rbindlist(lapply(1:length(typeExtras), fun))
}
	
makeComputeActRunTblPermOrderlessAll <- function(tblOrderless, lst) {
	res = groupN(2, lst)
	typeExtras = sapply(res, `[[`, 1, USE.NAMES=F)
	funConfigs = sapply(res, `[[`, 2, USE.NAMES=F)
	fun <- function(i) makeComputeActRunTblPermOrderless(tblOrderless, typeExtras[i], funConfigs[i])
	rbindlist(lapply(1:length(typeExtras), fun))
}

permTypeConfigs = c('', 'funConfigOrig',
		    'Entropy', 'funConfigEntropy',
		    'Stoplist', 'funConfigStoplist',
		    'Direction', 'funConfigDirection',
		    'Hyman', 'funConfigHyman',
		    'Freq', 'funConfigFreq',
		    'Window', 'funConfigWindow',
		    'Frentropy', 'funConfigFrentropy',
		    'Smdim', 'funConfigSmdim',
		    'Lgdim', 'funConfigLgdim',
		    'Frenthyman', 'funConfigFrenthyman',
		    'Nenthyman', 'funConfigNenthyman',
		    'Freqhyman', 'funConfigFreqhyman')

computeActPermTFromContextTbl <- function(contextTbl, tagTbl, config) {
	userScreenName = contextTbl[, guardAllEqualP(user_screen_name)[1]]
	contextTbl = getContextTbl(contextTbl, tagTbl)
	cTblOrder = contextTbl[orderType=='order']
	cTblOrderless = contextTbl[orderType=='orderless']
	contextTbl = makeComputeActRunTblPermOrdersAll(cTblOrder, cTblOrderless, permTypeConfigs)
	contextTbl[, get(fun[1])(chunk, posFromTag, userScreenName, get(funConfig)(config)), by=type]
}

computeActPermSOFromContextTbl <- function(contextTbl, tagTbl, config) {
	userScreenName = contextTbl[, guardAllEqualP(user_screen_name)[1]]
	contextTbl = getContextTbl(contextTbl, tagTbl)
	cTblOrderless = contextTbl[orderType == 'orderless']
	contextTbl = makeComputeActRunTblPermOrderlessAll(cTblOrderless, permTypeConfigs)
	contextTbl[, get(fun[1])(chunk, posFromTag, userScreenName, get(funConfig)(config)), by=type]
}

computeActPermSOOptFromContextTbl <- function(contextTbl, tagTbl, config) {
	userScreenName = contextTbl[, guardAllEqualP(user_screen_name)[1]]
	contextTbl = getContextTbl(contextTbl, tagTbl)
	cTblOrderless = contextTbl[orderType == 'orderless']
	contextTbl = rbind(makeComputeActRunTbl(cTblOrderless, 'OrderlessFreqhyman', 'computeActPermOrderless', 'funConfigFreqhyman'),
			   makeComputeActRunTbl(cTblOrderless, 'OrderlessFreqhyser', 'computeActPermOrderlessUser', 'funConfigFreqhyman'))
	contextTbl[, get(fun[1])(chunk, posFromTag, userScreenName, get(funConfig)(config)), by=type]
}

getContextPredNames <- function(config) {
	res = getConfig(config, 'runTbl')
	res = res[, unique(predName)]
	res = res[!grepl(pattern='Prior', x=res)]
	res
}

getSjiTblWide <- function(contextTbl, tagTbl, config) {
	if (nrow(contextTbl) > 0) {
		sjiTbl = get(getConfig(config, 'computeActFromContextTbl'))(contextTbl, tagTbl, config)
	} else {
		sjiTbl = data.table(act=double(0)) # Need to add a column b/c more 0-row columns can only be added to a DT with at least one col.
	}
	sjiTbl
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
	sjiTblWide
	setcolorder(sjiTblWide, c('hashtag', getContextPredNames(config)))
	sjiTblWide
}

setColOrderWithAtFront <- function(tbl, front) {
	front = c('hashtag', 'd')
	order = c(front, setdiff(colnames(tbl), front))
	setcolorder(tbl, order)
	tbl
}

getContextTbl <- function(contextTbl, tagTbl) {
	resTbl = setkey(contextTbl[, id:=1:nrow(.SD)], user_screen_name)[setkey(copy(tagTbl), user_screen_name), allow.cartesian=T, nomatch=0]
	resTbl = resTbl[, list(id, chunk, hashtag=chunk.1, pos, hashtagPos=pos.1, type)][, posFromTag := pos - hashtagPos]
	resTbl = resTbl[, list(chunk, posFromTag, type)][, orderType := 'order']
	resTbl = rbind(resTbl, contextTbl[, list(chunk, posFromTag=rep(0, nrow(.SD)), type, orderType=rep('orderless', nrow(.SD)))])
	resTbl
}

getPostResTbl <- function(tokenTbl, config, id) {
	myLog(sprintf("Getting results for id=%s", id))
	tokenTbl
	dStd = getConfig(config, 'dStd')
	contextTbl = tokenTbl[type != getConfig(config, "tagTypeName")]
	tagTbl = tokenTbl[type == getConfig(config, "tagTypeName")]
	setkey(tagTbl, chunk)
	tagTbl
	priorTbl = getPriorForUserAtEpoch(get(getConfig(config, 'priorTbl')), tokenTbl$user_screen_name_prior[1], tokenTbl$creation_epoch[1], dStd)
	priorTbl
	setkey(priorTbl, hashtag, d)
	if (nrow(tagTbl) > 0) {
		tempTagTbl = tagTbl[, list(hashtag=rep(chunk, each=length(dStd)), d=dStd)]
		setkey(tempTagTbl, hashtag, d)
		priorTbl = merge(priorTbl, unique(tempTagTbl), all=T)
	} else {
		setColOrderWithAtFront(priorTbl, c('hashtag', 'd'))
	}
	priorTbl[, user_screen_name := tokenTbl$user_screen_name[1]]
	priorTbl[, dt := tokenTbl[, dt[1]]]
	sjiTblWide = getSjiTblWide(contextTbl, tagTbl, config)
	postResTbl = sjiTblWide[priorTbl]
	postResTbl[, hashtagUsedP := F]
	postResTbl[tagTbl, hashtagUsedP := T]
	postResTbl
}

getTokenizedFromSubset <- function(minId, maxId, config) {
	resTbl = sqldt(sprintf("select %s from %s as tokenized_tbl
			       join %s as posts_tbl
			       on tokenized_tbl.id = posts_tbl.id 
			       where tokenized_tbl.id in (select post_id from %s 
							  where id >= %s
							  and id <= %s
							  and group_name = '%s')",
			       get(getConfig(config, 'defaultColsTokenized')),
			       getConfig(config, "tokenizedTbl"), getConfig(config, "postsTbl"), getConfig(config, "subsetsTbl"), minId, maxId, getConfig(config, "groupName") 
			       ))
	resTbl
}

defaultColsTokenizedSO = "tokenized_tbl.id::text, user_screen_name, creation_epoch, chunk, pos, type"
defaultColsTokenizedT = sprintf('%s, %s', defaultColsTokenizedSO, 'posts_tbl.retweeted')

getTokenizedForUsers <- function(config) {
	resTbl = sqldt(sprintf("select %s from %s as tokenized_tbl
			       join %s as posts_tbl
			       on tokenized_tbl.id = posts_tbl.id
			       where 1=1
			       and %s
			       and tokenized_tbl.id in (select id from %s where %s)",
			       get(getConfig(config, 'defaultColsTokenized')), getConfig(config, "tokenizedTbl"), getConfig(config, "postsTbl"), getConfig(config, 'query'),
			       getConfig(config, 'postsTbl'), getConfig(config, 'query')
			       ))
	if (getConfig(config, "convertTagSynonymsP")) {
		resTbl = convertTagSynonymsForTokenized(resTbl, config)
	}
	if (getConfig(config, 'dsetType') == 'twitter') {
		resTbl = resTbl[retweeted %in% getAllowedRetweetedVals(config)]
	}
	addDtToTbl(resTbl)
	resTbl[, user_screen_name_prior := user_screen_name]
	resTbl
}

getTokenizedFromSubsetT <- function(minId, maxId, config) {
	resTbl = getTokenizedFromSubset(minId, maxId, config)[, user_screen_name := 'allUsers']
	resTbl[, user_screen_name_prior := 'allUsers'][, user_screen_name := 'allUsers']
	addDtToTbl(resTbl)
	resTbl
}

getTokenizedFromSubsetSO <- function(minId, maxId, config) {
	minId
	maxId
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
		if (is.nan(meanVal)) {
			myLog('Using mean of 0 since all rows are NAs')
			meanVal = 0
		}
		meanVal
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

samplePostResTbl <- function(postResTbl, predictors) {
	nones = nrow(postResTbl[hashtagUsedP == T])
	nzeros = nrow(postResTbl[hashtagUsedP == F])
	res = sapply(predictors, function(p) postResTbl[, p, with=F])
	res = Reduce(`+`, res)
	topRows = order(res, decreasing=T)
	nranked = nzeros*.4
	nleft = nzeros - nranked
	nsampled = nzeros*.4 
	#nsampled = nones * 3
	set.seed(digestAsInteger(list(predictors, postResTbl[1:100])))
	postResTblSample = rbind(postResTbl[hashtagUsedP == T],
				 postResTbl[topRows][hashtagUsedP == F][1 : nranked],
				 postResTbl[topRows][hashtagUsedP == F][nranked + 1 : nzeros][sample(nleft, nsampled)])
	postResTblSample
}

samplePostResTbl <- function(postResTbl, predictors) {
	postResTbl
}

runLogReg <- function(postResTbl, predictors) {
	model = reformulate(termlabels = predictors, response = 'hashtagUsedP')
	postResTblSample = samplePostResTbl(postResTbl, predictors)
	myLogit = glm(model, data=postResTblSample, weights=weights, family=binomial(link="logit"))
	myLog(summary(myLogit))
	myLog(ClassLog(myLogit, postResTblSample$hashtagUsedP))
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

preProcessPostResTbl <- function(postResTbl, runTbl) {
	guardAllEqualP(postResTbl[, d])
	guardAllEqualP(postResTbl[, user_screen_name])
	preProcess <- function(predictors) {
		handleNAs(postResTbl, predictors)
	}
	runTbl[, preProcess(predName), by=bestFitName]
	addWeights(postResTbl)
}

analyzePostResTbl <- function(postResTbl, predictors, bestFitName) {
	guardAllEqualP(postResTbl[, d])
	guardAllEqualP(postResTbl[, user_screen_name])
	myLog(sprintf('Analyzing post result table for d=%s, bestFitName=%s, and predictors=%s',
		      postResTbl[, d[1]], bestFitName, paste(predictors, collapse=',')))
	#postResTbl = copy(postResTbl)
	predictors
	postResTbl
	myLogit = runLogReg(postResTbl, predictors)
	coeffs = summary(myLogit)$coefficients
	logregTbl = as.data.table(coeffs)
	logregTbl[, coeff := Estimate][, Estimate := NULL]
	logregTbl
	logregTbl[, predName := rownames(coeffs)][, d := postResTbl[, d[1]]]
	logregTbl[, user_screen_name := postResTbl[, user_screen_name[1]]]
	logregTbl[, bestFitName := bestFitName]
	postResTbl
	myLogit
	logregTbl
}

getHashtagsTblFromSubsetTbl <- function(tokenTbl, config) {
	hashtagsTbl = tokenTbl[type==getConfig(config, 'tagTypeName')]
	hashtagsTbl[, hashtag := chunk][, chunk := NULL]
	setkey(hashtagsTbl, user_screen_name, dt, id, hashtag)
	hashtagsTbl
}

getFullPostResTbl <- function(tokenTbl, config) {
	myStopifnot(nrow(tokenTbl[, .N, by=list(id, user_screen_name, creation_epoch, dt, user_screen_name_prior)]) == length(tokenTbl[, unique(id)]))
	tokenTbl
	subFun <- function(curId) {
		res = getPostResTbl(tokenTbl[id == curId], config, curId)
		res[, id := curId]
	}
	postResTbl = rbindlist(mclapply(tokenTbl[, unique(id)], subFun, mc.cores=getCores(config, 'MCCORESAct')))
	#postResTbl = tokenTbl[, getPostResTbl(.SD, config, id[1]), by=id, .SDcols=colnames(tokenTbl)]
	postResTbl
}

analyzePostResTblAcrossDs <- function(postResTbl, runTbl, config) {
	analyzePostResTblForD <- function(curD) {
		tbl = postResTbl[d == curD] 
		preProcessPostResTbl(tbl, runTbl)
		tbl
		runTbl
		if (nrow(runTbl) > 0) {
			runTbl
			bestFitNames = runTbl[, unique(bestFitName)]
			analyzeFun <- function(curBestFitName) {
				analyzePostResTbl(tbl, runTbl[bestFitName==curBestFitName, predName], curBestFitName)
			}
			logregTbl = rbindlist(mclapply(bestFitNames, analyzeFun, mc.cores=getCores(config, 'MCCORESReg')))
			logregTbl[, updateBestFitCol(tbl, .SD, bestFitName), by=bestFitName]
		} else {
			logregTbl = data.table(bestFitName=character(), predName=character(), d=double(), user_screen_name=character())
		}
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

runForTokenTblForUser <- function(tokenTbl, config) {
	tokenTbl
	config
	postResTbl = getFullPostResTbl(tokenTbl, config)
	postResTbl
	tokenTbl
	runTbl = getConfig(config, 'runTbl')
	runTbl
	resTbl = analyzePostResTblAcrossDs(postResTbl, runTbl, config)
	resTbl
	logregTbl = resTbl$logregTbl
	postResTbl = resTbl$postResTbl
	setkey(postResTbl, user_screen_name, dt, id, hashtag, d)
	logregTbl
	hashtagsTbl = getHashtagsTblFromSubsetTbl(tokenTbl, config)
	addMetrics(hashtagsTbl, postResTbl, config)
	hashtagsTbl
	modelVsPredTbl = getModelVsPredTbl(postResTbl, hashtagsTbl, config)
	if (getConfig(config, "accumModelHashtagsTbl") == F) {
		postResTbl = data.table()
	}
	list(modelVsPredTbl=modelVsPredTbl, modelHashtagsTbl=postResTbl, hashtagsTbl=hashtagsTbl, logregTbl=logregTbl)
}

runForTokenTbl <- function(tokenTbl, config, getOutFileForNameFun=identity) {
	user_screen_names = unique(tokenTbl[, user_screen_name])
	res = mclapply(user_screen_names,
		       function(x) runForTokenTblForUser(tokenTbl[user_screen_name==x], config),
		       mc.cores = getCores(config, 'MCCORESActUser'))
	modelVsPredTbl = rbindlist(lapply(res, `[[`, 'modelVsPredTbl'))
	modelHashtagsTbl = rbindlist(lapply(res, `[[`, 'modelHashtagsTbl'))
	hashtagsTbl = rbindlist(lapply(res, `[[`, 'hashtagsTbl'))
	logregTbl = rbindlist(lapply(res, `[[`, 'logregTbl'))
	outFile = getOutFileForNameFun("modelVsPredOutFile")
	writeModelVsPredTbl(modelVsPredTbl, outFile)
	if (getConfig(config, 'modelHashtagsOutFile') != '') {
		outFile = getOutFileForNameFun('modelHashtagsOutFile')
		writeModelHashtagsTbl(modelHashtagsTbl, outFile)
	}
	if (getConfig(config, 'logregOutFile') != '') {
		outFile = getOutFileForNameFun('logregOutFile')
		writeLogregTbl(logregTbl, outFile)
	}
	list(modelVsPredTbl=modelVsPredTbl, modelHashtagsTbl=modelHashtagsTbl, hashtagsTbl=hashtagsTbl, logregTbl=logregTbl)
}

runContext <- function(config, samplesPerRun, numRuns) {
	myLog(sprintf("running context with act/actUser/reg cores: %s/%s/%s",
		      getCores(config, 'MCCORESAct'), getCores(config, 'MCCORESActUser'), getCores(config, 'MCCORESReg')))
	endId = 3000000
	for (runNum in seq(numRuns)) {
		getOutFileForNameFun <- function(name) {
			outFile = getConfig(config, name)
			outFile = gsub('.csv$', sprintf('r%s%s', runNum, '.csv'), outFile)
			outFile
		}
		startId = endId + 1
		endId = startId + samplesPerRun - 1
		tokenTbl = get(getConfig(config, 'getTokenizedFromSubsetFun'))(startId, endId, config)
		runForTokenTbl(tokenTbl, config, getOutFileForNameFun)
	}
}

getCurWorkspaceBy <- function(regen, groupConfig) {
	if (regen == 'useAlreadyLoaded') return()
	if (regen == T) {
		eval(bquote(getCurWorkspace(.(groupConfig))), envir=globalenv())
	} else {
		eval(bquote(myLoadImage(.(groupConfig))), envir=globalenv())
	}
}

runContextWithConfig <- function(regen, samplesPerRun, numRunsT, numRunsSO, groupConfig) {
	getContextRunConfig <- function(config, name) {
		addNumSamples = function(str) sprintf('%s-%s', str, samplesPerRun)
		addGroupName = function(str) sprintf('%sg%s', str, getConfig(groupConfig, 'groupNum'))
		addSizeName = function(str) sprintf('%ss%s', str, getConfig(groupConfig, 'sizeNum'))
		addAll = function(str) addSizeName(addGroupName(addNumSamples(str)))
		genModelHashtagsP = {if (getConfig(groupConfig, 'groupNum') == 1 &
					 getConfig(groupConfig, 'sizeNum') %in% c(1,2)) T else F}
		getConfigMods <- function(name, addFun) {
			list(modelVsPredOutFile=getOutFileModelVsPred(addFun(name)),
			     modelHashtagsOutFile={if (!genModelHashtagsP) '' else getOutFileModelHashtags(addFun(name))},
			     logregOutFile=getLogregOutFile(addFun(name)))
		}
		modConfig(config, getConfigMods(name, addAll))
	}
	getCurWorkspaceBy(regen, groupConfig)
	updateGroupName <- function(config) modConfig(config, list(groupName=getConfig(groupConfig, 'groupName')))
	runContext(updateGroupName(getContextRunConfig(defaultTSjiConfig, 'TContextSji')), samplesPerRun, numRunsT)
	runContext(updateGroupName(getContextRunConfig(defaultTPermConfig, 'TContextPerm')), samplesPerRun, numRunsT)
	if (getConfig(groupConfig, 'groupNum') == 1) {
		runContext(getContextRunConfig(defaultSOSjiConfig, 'SOContextSji'), samplesPerRun, numRunsSO)
		runContext(getContextRunConfig(defaultSOPermConfig, 'SOContextPerm'), samplesPerRun, numRunsSO)
	}
}

groupConfigS1 <- list(sizeNum=1, maxIdSOSji=1e5, maxIdSOPrior=100e6, maxIdTSji=3e6, maxIdTPrior=1e5)
# FIXME: Change to 3Mil for maxIdSOSji when possible
groupConfigS2 <- list(sizeNum=2, maxIdSOSji=1e6, maxIdSOPrior=100e6, maxIdTSji=3e6, maxIdTPrior=1e5) 
groupConfigS3 <- list(sizeNum=3, maxIdSOSji=1e6, maxIdSOPrior=100e6, maxIdTSji=1e6, maxIdTPrior=1e5)
groupConfigS4 <- list(sizeNum=4, maxIdSOSji=1e5, maxIdSOPrior=100e6, maxIdTSji=1e5, maxIdTPrior=1e5)
groupConfigS5 <- list(sizeNum=5, maxIdSOSji=1e4, maxIdSOPrior=100e6, maxIdTSji=1e4, maxIdTPrior=1e5)
groupConfigS6 <- list(sizeNum=6, maxIdSOSji=1e3, maxIdSOPrior=100e6, maxIdTSji=1e3, maxIdTPrior=1e5)

groupConfigG1 <- list(groupNum=1, groupName = '2014-02-27 17:13:30 initial')
groupConfigG2 <- list(groupNum=2, groupName = '2014-03-17 11:28:15 trendsmap')
groupConfigG3 <- list(groupNum=3, groupName = '2014-03-24 13:06:19 trendsmap')
groupConfigG4 <- list(groupNum=4, groupName = '2014-04-04 15:03:59 trendsmap')

groupConfigContext <- list(genFun=getCurWorkspaceContext, configType='')
groupConfigPUser <- c(list(genFun=getCurWorkspacePUser, configType='PUser', groupNum=1),
		      groupConfigS6)

groupConfigG1S1 <- c(groupConfigS1, groupConfigG1, groupConfigContext)
groupConfigG2S1 <- c(groupConfigS1, groupConfigG2, groupConfigContext)
groupConfigG3S1 <- c(groupConfigS1, groupConfigG3, groupConfigContext)
groupConfigG4S1 <- c(groupConfigS1, groupConfigG4, groupConfigContext)

groupConfigG1S2 <- c(groupConfigS2, groupConfigG1, groupConfigContext)
groupConfigG2S2 <- c(groupConfigS2, groupConfigG2, groupConfigContext)
groupConfigG3S2 <- c(groupConfigS2, groupConfigG3, groupConfigContext)
groupConfigG4S2 <- c(groupConfigS2, groupConfigG4, groupConfigContext)

groupConfigG1S3 <- c(groupConfigS3, groupConfigG1, groupConfigContext)
groupConfigG2S3 <- c(groupConfigS3, groupConfigG2, groupConfigContext)
groupConfigG3S3 <- c(groupConfigS3, groupConfigG3, groupConfigContext)
groupConfigG4S3 <- c(groupConfigS3, groupConfigG4, groupConfigContext)

groupConfigG1S4 <- c(groupConfigS4, groupConfigG1, groupConfigContext)
groupConfigG2S4 <- c(groupConfigS4, groupConfigG2, groupConfigContext)
groupConfigG3S4 <- c(groupConfigS4, groupConfigG3, groupConfigContext)
groupConfigG4S4 <- c(groupConfigS4, groupConfigG4, groupConfigContext)

groupConfigG1S5 <- c(groupConfigS5, groupConfigG1, groupConfigContext)
groupConfigG2S5 <- c(groupConfigS5, groupConfigG2, groupConfigContext)
groupConfigG3S5 <- c(groupConfigS5, groupConfigG3, groupConfigContext)
groupConfigG4S5 <- c(groupConfigS5, groupConfigG4, groupConfigContext)

groupConfigG1S6 <- c(groupConfigS6, groupConfigG1, groupConfigContext)
groupConfigG2S6 <- c(groupConfigS6, groupConfigG2, groupConfigContext)
groupConfigG3S6 <- c(groupConfigS6, groupConfigG3, groupConfigContext)
groupConfigG4S6 <- c(groupConfigS6, groupConfigG4, groupConfigContext)

buildRunFunContext <- function(regen, numRunsT, numRunsSO, samplesPerRun, groupConfig) {
	eval(bquote(
		    function(regen=.(regen), numRunsT=.(numRunsT), numRunsSO=.(numRunsSO)) {
			    runContextWithConfig(regen=regen, samplesPerRun=samplesPerRun, numRunsT=numRunsT, numRunsSO=numRunsSO, groupConfig=groupConfig)
		    }))
}

runContext20g1s1 <- buildRunFunContext(regen=F, numRunsT=1, numRunsSO=1, samplesPerRun=20, groupConfig=groupConfigG1S1)
runContext20g1s6 <- buildRunFunContext(regen=F, numRunsT=1, numRunsSO=1, samplesPerRun=20, groupConfig=groupConfigG1S6)

#runContext200g1s1 <- buildRunFunContext(regen=F, numRunsT=10, numRunsSO=5, samplesPerRun=200, groupConfig=groupConfigG1S1)
#runContext200g2s1 <- buildRunFunContext(regen=F, numRunsT=10, numRunsSO=5, samplesPerRun=200, groupConfig=groupConfigG2S1)
#runContext200g3s1 <- buildRunFunContext(regen=F, numRunsT=10, numRunsSO=5, samplesPerRun=200, groupConfig=groupConfigG3S1)
#runContext200g4s1 <- buildRunFunContext(regen=F, numRunsT=10, numRunsSO=5, samplesPerRun=200, groupConfig=groupConfigG4S1)

#runContext500g1s2Test <- buildRunFunContext(regen=F, numRunsT=1, numRunsSO=1, samplesPerRun=500, groupConfig=groupConfigG1S2)
runContext500g1s2 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG1S2)
runContext500g2s2 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG2S2)
runContext500g3s2 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG3S2)
runContext500g4s2 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG4S2)

runContext500g1s3 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG1S3)
runContext500g2s3 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG2S3)
runContext500g3s3 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG3S3)
runContext500g4s3 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG4S3)

runContext500g1s4 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG1S4)
runContext500g2s4 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG2S4)
runContext500g3s4 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG3S4)
runContext500g4s4 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG4S4)

runContext500g1s5 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG1S5)
runContext500g2s5 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG2S5)
runContext500g3s5 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG3S5)
runContext500g4s5 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG4S5)

runContext500g1s6 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG1S6)
runContext500g2s6 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG2S6)
runContext500g3s6 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG3S6)
runContext500g4s6 <- buildRunFunContext(regen=F, numRunsT=15, numRunsSO=5, samplesPerRun=500, groupConfig=groupConfigG4S6)

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

digestAsInteger <- function(x) {
	hexval <- paste0("0x",digest(x,"crc32"))
	intval <- type.convert(hexval) %% .Machine$integer.max
	intval
}

makeEnvironmentTbl <- function(sjiTbl, config) {
	permEnvTbl = with(sjiTbl, data.table(chunk=unique(context)))
	permEnvTbl
	cnames = setdiff(colnames(sjiTbl), c('pFreq', 'freqWeight'))
	set.seed(digestAsInteger(sjiTbl[1:100, cnames, with=F]))
	permEnvTbl = makeEnvironmentSubsetTbl(permEnvTbl, config)
	permEnvTbl[, uniq := NULL]
	setkey(permEnvTbl, chunk)
	permEnvTbl = melt(permEnvTbl,
			  id=c('chunk'),
			  measure=c('ind1', 'ind2', 'ind3', 'ind4'),
			  variable.name='valIndID',
			  value.name='ind')
	setkey(permEnvTbl, valIndID)
	valTbl = data.table(name=c('ind1', 'ind2', 'ind3', 'ind4'), valAsNum=c(1,1,-1,-1), key='name')
	permEnvTbl[valTbl, val := valAsNum]
	setkey(permEnvTbl, chunk)
	addEnvironmentTblAttrs(permEnvTbl, sjiTbl)
	permEnvTbl
}

addEnvironmentTblAttrs <- function(permEnvTbl, sjiTbl) {
	entTbl = sjiTbl[, .N, keyby=list(context, EContext)][, N := NULL]
	myStopifnot(nrow(entTbl) == entTbl[, length(unique(context))])
	permEnvTbl[entTbl, EContext := EContext]
	stopwordWeightTbl = sjiTbl[, .N, keyby=list(context, stopWordWeight)][, N := NULL]
	myStopifnot(nrow(stopwordWeightTbl) == stopwordWeightTbl[, length(unique(context))])
	permEnvTbl[stopwordWeightTbl, stopWordWeight := stopWordWeight]
	freqWeightTbl = sjiTbl[, .N, keyby=list(context, freqWeight)][, N := NULL]
	myStopifnot(nrow(freqWeightTbl) == freqWeightTbl[, length(unique(context))])
	permEnvTbl[freqWeightTbl, freqWeight := freqWeight]
	permEnvTbl
}

makeMemMat <- function(sjiTbl, permEnvTbl, config) {
	permEnvTbl = getEnvTblFromList(permEnvTbl, config)
	agg <- function(curValIndID) {
		myLog(sprintf('Generating memory matrix for all vals in env table with valIndID=%s', curValIndID))
		makeMemMatInner(sjiTbl, permEnvTbl[valIndID == curValIndID], config)
	}
	res = lapply(permEnvTbl[, unique(valIndID)], agg)
	myStopifnot(length(unique(lapply(res, colnames))) == 1)
	res = Reduce(`+`, res)
	res
}

makeMemMatInner <- function(sjiTbl, permEnvTbl, config) {
	memTbl = permEnvTbl[sjiTbl, allow.cartesian=T, nomatch=0]
	sjiTbl
	permEnvTbl
	memTbl
	if (getConfig(config, 'permUseEntropyP')) {
		debugPrint(sprintf('Weighting val in memory matrix based on entropy'))
		memTbl[, val := val * EContext]
	}
	if (getConfig(config, 'permUseStoplistP')) {
		debugPrint(sprintf('Weighting val in memory matrix based on stoplist'))
		memTbl[, val := val * stopWordWeight]
	}
	if (getConfig(config, 'permUseFreqP')) {
		debugPrint(sprintf('Weighting val in memory matrix based on frequency'))
		memTbl[, val := val * freqWeight]
	}
	if (getConfig(config, 'permOnlyDirectionP')) {
		debugPrint(sprintf('Collapsing posFromTag to sign(posFromTag)'))
		memTbl[, posFromTag := sign(posFromTag)]
	}
	if (getConfig(config, 'permUseWindowP')) {
		cnt = memTbl[abs(posFromTag) > 2, .N]
		debugPrint(sprintf('Trimming %s of %s rows from memory table to constrain window size', cnt, nrow(memTbl)))
		memTbl[abs(posFromTag) > 2, val := 0]
	}
	NRows = getConfig(config, 'permNRows')
	NRows
	memTbl[, rotInd := ((ind-1 + posFromTag) %% NRows) + 1]
	memTbl = memTbl[, list(totVal=sum(val*partialN)), by=list(rotInd, hashtag)]
	db = makeDB(memTbl[, unique(hashtag)])
	db
	memMat = with(memTbl, sparseMatrix(i=rotInd, j=getHashes(hashtag, db), x=totVal, dims=c(NRows, length(db))))
	memMat = as.matrix(memMat)
	colnames(memMat) = getVals(seq(1, length=ncol(memMat)), db)
	memMat = memMat[, apply(memMat, 2, sd) > 0, drop=F]
	memMat
}

myCor <- function(permMemMat, contextMemVect) {
	stats::cor(permMemMat, contextMemVect)
	#WGCNA::corFast(permMemMat, contextMemVect) # Slower currently
}

computeActPerm <- function(context, pos, permEnvTbl, permMemMat, config) {
	myLog(sprintf("computing perm act for context with length %s", length(context)))
	permEnvTbl
	permMemMat
	contextTbl = data.table(context=context, posFromTag=pos, hashtag='context', partialN=1, key='context')
	contextTbl
	contextMemMat = makeMemMatInner(contextTbl, permEnvTbl, config)
	contextMemMat
	contextMemVect = rowSums(contextMemMat) 
	contextMemVect
	if (sd(contextMemVect) == 0) {
		contextCorVect = matrix(data=0, nrow=dim(permMemMat)[2], ncol=1, dimnames=list(colnames(permMemMat)))
	} else {
		contextCorVect = myCor(permMemMat, contextMemVect)
	}
	resTbl = data.table(hashtag=rownames(contextCorVect), act=as.vector(contextCorVect))
	if (getConfig(config, 'permHymanP')) {
		resTbl
		resTbl[, cdf := pnorm(act, mean=mean(act), sd=sd(act))]
		resTbl[cdf >= .9999, cdf := .9999] # So that log(1/0)'s don't produce Inf's
		resTbl[, actLog := log(cdf/(1-cdf))]
		resTbl[, act := actLog][, actLog := NULL][, cdf := NULL]
	}
	resTbl
}

getMemMatFromList <- function(lst, config) {
	permUseEntropyP = getConfig(config, 'permUseEntropyP')
	permUseStoplistP = getConfig(config, 'permUseStoplistP')
	permOnlyDirectionP = getConfig(config, 'permOnlyDirectionP')
	permUseFreqP = getConfig(config, 'permUseFreqP')
	permUseWindowP = getConfig(config, 'permUseWindowP')
	permNRows = getConfig(config, 'permNRows')
	acc = {
		if (permUseEntropyP & !permOnlyDirectionP & !permUseFreqP & !permUseStoplistP & !permUseWindowP & permNRows == 2048) {
			'entropy'
		} else if (!permUseEntropyP & !permOnlyDirectionP & !permUseFreqP & permUseStoplistP & !permUseWindowP & permNRows == 2048) {
			'stoplist'
		} else if (permUseEntropyP & permOnlyDirectionP & !permUseFreqP & !permUseStoplistP & !permUseWindowP & permNRows == 2048) { 
			'direction'
		} else if (!permUseEntropyP & !permOnlyDirectionP & permUseFreqP & !permUseStoplistP & !permUseWindowP & permNRows == 2048) {
			'freq'
		} else if (permUseEntropyP & !permOnlyDirectionP & !permUseFreqP & !permUseStoplistP & permUseWindowP & permNRows == 2048) {
			'window'
		} else if (permUseEntropyP & !permOnlyDirectionP & permUseFreqP & !permUseStoplistP & !permUseWindowP & permNRows == 2048) {
			'frentropy'
		} else if (permUseEntropyP & !permOnlyDirectionP & !permUseFreqP & !permUseStoplistP & !permUseWindowP & permNRows == permNRowsSm) {
			'smdim'
		} else if (permUseEntropyP & !permOnlyDirectionP & !permUseFreqP & !permUseStoplistP & !permUseWindowP & permNRows == 10000) {
			'lgdim'
		} else {
			myStopifnot(!permUseEntropyP & !permOnlyDirectionP & !permUseFreqP & !permUseStoplistP & !permUseWindowP & (permNRows == 2048 | permNRows < 10)) # '< 10' for unit tests
			'orig'
		}
	}
	myStopifnot(acc %in% names(lst))
	lst[[acc]]
}

getEnvTblFromList <- function(lst, config) {
	acc = as.character(getConfig(config, 'permNRows'))
	myStopifnot(acc %in% names(lst))
	lst[[acc]]
}

computeActPermOrder <- function(context, pos, user, config) {
	# Cannot get from global environment or test 'testComputePermAct' will fail
	envTbl = getEnvTblFromList(get(getConfig(config, 'permEnvTbl'), envir=parent.frame()), config)
	memMat = getMemMatFromList(get(getConfig(config, 'permMemMatOrder'), envir=parent.frame()), config)
	computeActPerm(context, pos, permEnvTbl = envTbl, permMemMat = memMat, config)
}

computeActPermOrderless <- function(context, pos, user, config) {
	envTbl = getEnvTblFromList(get(getConfig(config, 'permEnvTbl'), envir=parent.frame()), config)
	memMat = getMemMatFromList(get(getConfig(config, 'permMemMatOrderless'), envir=parent.frame()), config)
	computeActPerm(context, pos, permEnvTbl = envTbl, permMemMat = memMat, config)
}

computeActPermOrderlessUser <- function(context, pos, user, config) {
	envTbl = getEnvTblFromList(get(getConfig(config, 'permEnvTbl'), envir=parent.frame()), config)
	memMat = getMemMatFromList(get(getConfig(config, 'permMemMatOrderlessUser'), envir=parent.frame())[[user]], config)
	computeActPerm(context, pos, permEnvTbl = envTbl, permMemMat = memMat, config)
}

runGenAndSaveCurWorkspaceg1s1 <- function() genAndSaveCurWorkspace(groupConfigG1S1)

runGenAndSaveCurWorkspaceg1s2 <- function() genAndSaveCurWorkspace(groupConfigG1S2)
runGenAndSaveCurWorkspaceg2s2 <- function() genAndSaveCurWorkspace(groupConfigG2S2)
runGenAndSaveCurWorkspaceg3s2 <- function() genAndSaveCurWorkspace(groupConfigG3S2)
runGenAndSaveCurWorkspaceg4s2 <- function() genAndSaveCurWorkspace(groupConfigG4S2)

runGenAndSaveCurWorkspaceg1s3 <- function() genAndSaveCurWorkspace(groupConfigG1S3)
runGenAndSaveCurWorkspaceg2s3 <- function() genAndSaveCurWorkspace(groupConfigG2S3)
runGenAndSaveCurWorkspaceg3s3 <- function() genAndSaveCurWorkspace(groupConfigG3S3)
runGenAndSaveCurWorkspaceg4s3 <- function() genAndSaveCurWorkspace(groupConfigG4S3)

runGenAndSaveCurWorkspaceg1s4 <- function() genAndSaveCurWorkspace(groupConfigG1S4)
runGenAndSaveCurWorkspaceg2s4 <- function() genAndSaveCurWorkspace(groupConfigG2S4)
runGenAndSaveCurWorkspaceg3s4 <- function() genAndSaveCurWorkspace(groupConfigG3S4)
runGenAndSaveCurWorkspaceg4s4 <- function() genAndSaveCurWorkspace(groupConfigG4S4)

runGenAndSaveCurWorkspaceg1s5 <- function() genAndSaveCurWorkspace(groupConfigG1S5)
runGenAndSaveCurWorkspaceg2s5 <- function() genAndSaveCurWorkspace(groupConfigG2S5)
runGenAndSaveCurWorkspaceg3s5 <- function() genAndSaveCurWorkspace(groupConfigG3S5)
runGenAndSaveCurWorkspaceg4s5 <- function() genAndSaveCurWorkspace(groupConfigG4S5)

runGenAndSaveCurWorkspaceg1s6 <- function() genAndSaveCurWorkspace(groupConfigG1S6)
runGenAndSaveCurWorkspaceg2s6 <- function() genAndSaveCurWorkspace(groupConfigG2S6)
runGenAndSaveCurWorkspaceg3s6 <- function() genAndSaveCurWorkspace(groupConfigG3S6)
runGenAndSaveCurWorkspaceg4s6 <- function() genAndSaveCurWorkspace(groupConfigG4S6)

curWS <- function() {
	# FIXME: Add twitter runs
	# FIXME: Add user-centered sji to popular-users dataset
	# FIXME: Run popular-users dataset with sji computation
	# FIXME: Address low prior predictability for SO
	# FIXME: Methods to import and anlyze coefficient tables
	# FIXME: Make sure word order low predictiveness is fully justified
	# FIXME: Def. look at coefficient tables
	runPUserSOSji100k(regen='useAlreadyLoaded')
	withProf(runContext20g1s1(regen='useAlreadyLoaded'))
	setLogLevel(2)
	runContext20g1s6(regen='useAlreadyLoaded')
	withProf(runContext500g1s2(regen='useAlreadyLoaded'))
	runContext500g1s2Test(regen='useAlreadyLoaded')
	withProf(runContext500g1s3(regen='useAlreadyLoaded'))
	modelVsPredTbl = buildTables(file_path_sans_ext(Filter(isContextRun, list.files(path=getDirModelVsPred()))))
	modelHashtagsTbls = buildModelHashtagsTables(file_path_sans_ext(Filter(isContextRun, list.files(path=getDirModelHashtags()))))
	modelHashtagsTbl = modelHashtagsTbls[["TContextPerm-20g1s1r1"]]
	modelVsPredTbl = buildTables(file_path_sans_ext(Filter(isPriorRun, list.files(path=getDirModelVsPred()))))
	modelVsPredTbl
	sjiTblTOrderless
	sessionInfo()
	mySaveImage(groupConfigG1S1)
	mySaveImage(groupConfigG1S6)
	withProf(myLoadImage(groupConfigG1S1))
	withProf(myLoadImage(groupConfigG1S6))
	withProf(myLoadImage(groupConfigG1S2))
	withProf(myLoadImage(groupConfigG1S3))
	test_dir(sprintf("%s/%s", PATH, 'tests'), reporter='summary')
	tables()
	.ls.objects(order.by='Size')
	lapply(permMemMatSOOrderless, dim)
	as.sparseMatrix(permMemMatSOOrderless[['orig']])
	# Checking that tweets for twitter users from each followers_count,statuses_count scale are being collected properly
	usersWithTweetsTbl = sqldt("select distinct on (t.user_screen_name) t.user_screen_name,u.followers_count,u.statuses_count
				   from tweets as t join twitter_users as u on t.user_screen_name = u.user_screen_name"
				   )
	usersWithTweetsTbl[order(followers_count), plot(log10(followers_count))]
	usersWithTweetsTbl[order(statuses_count), plot(log10(statuses_count))]
	usersWithTweetsTbl[order(followers_count),][followers_count > 10000000]
}

