# --------------------------------------------------------------------------------------------------------
# Driver/analyzer for model

# Takes a tag folder as input
# Reads in all tags (and title chunks associated with that tag) in that folder
# Calls the model for each title/tag pair
# Runs analysis on the results
# --------------------------------------------------------------------------------------------------------

# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Load up the libraries
library(stringr)
library(popbio)
library(multicore)
library(data.table)
library(plyr)

# A few helper functions
rateVals3 = function(priorsIndeces, act, observed, tagFile) {
	cutoff = 200
	vals = as.vector(B[priorsIndeces]*coeffsGlobal$prior 
		+ act$sjiTitle[priorsIndeces]*coeffsGlobal$sjiTitle 
		+ act$sjiBody[priorsIndeces])*coeffsGlobal$sjiBody
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = priorsIndeces[res$ix]
	#sortedChunkHashes = sortedChunkHashes[1:cutoff]
	#sampleIndeces = union(union(observed, sortedChunkHashes), sample(priorsIndeces, 100, replace=F))
	#sampleIndeces = union(sortedChunkHashes[1:cutoff], union(observed, sample(priorsIndeces, 500, replace=F, prob=vals)))
	probs = 1/(1+exp(-vals/.5))
	sampleIndeces = union(sortedChunkHashes[1:cutoff], union(observed, sample(priorsIndeces, 800, replace=F, prob=probs)))
	targetP = rep(0, length(sampleIndeces))
	targetP[match(observed, sampleIndeces)] = 1
	tags = getChunks(sampleIndeces, dbPriors)
	priors = as.vector(B[sampleIndeces])
	sjiTitle = as.vector(act$sjiTitle[sampleIndeces])
	sjiBody = as.vector(act$sjiBody[sampleIndeces])
	userIdPriors = as.vector(getPriorsForPost(getPostIdForTagFile(tagFile)))[sampleIndeces]
	return(data.frame(tag=tags, sjiTitle=sjiTitle, sjiBody=sjiBody, prior=priors, targetP=targetP, act=priors + sjiTitle + sjiBody,
		userIdPriors=userIdPriors))
}

getObservedContext = function(tagFile, tagDir) {
	bodyDir = makeBodyDir(getSubsetId(tagDir))
	titleDir = makeTitleDir(getSubsetId(tagDir))
	ret = list()
	if ( !any(grep("*.csv", tagFile)) ) {
		observed = readLines(str_c(PATH, "/../html-to-text/", tagDir, "/", tagFile), warn = F)
		observed = replaceSynonyms(observed, dbSynonyms)
		titleContext = readLines(str_c(PATH, "/../html-to-text/", titleDir, "/", tagFile), warn = F)
		bodyContext = readLines(str_c(PATH, "/../html-to-text/", bodyDir, "/", tagFile), warn = F)
		ret = list(observed=observed, titleContext=titleContext, bodyContext=bodyContext)
	}
	return(ret)
}

ratePost = function(tagFile, tagDir) {
	res = data.frame()
	if ( length(lst <- getObservedContext(tagFile, tagDir)) > 0 ) {
		observed = lst$observed
		titleContext = lst$titleContext
		bodyContext = lst$bodyContext
		print(str_c("working tag file ", tagFile))
		tAct = act(getChunkHashes(titleContext, dbContext), B, sji)
		bAct = act(getChunkHashes(bodyContext, dbContext), B, sji)
		cAct = list()
		cAct$sjiTitle = tAct$sji
		cAct$sjiBody = bAct$sji
		ret = rateVals3(priorsIndeces, cAct, getChunkHashes(observed, dbPriors), tagFile)
		ret$tagFile=tagFile
		res = rbind(res, ret)
	}
	return(res)
}

getTagFiles = function(tagDir) {
	list.files(path=str_c(PATH, "/../html-to-text/", tagDir), recursive=T)
}

replaceSynonyms = function(chunks, dbSynonyms) {
	ret = dbSynonyms[chunks]
	ret[is.na(ret)] = chunks[is.na(ret)]
	return(as.vector(ret))
}

makeTagDir = function(subsetId) {
	str_c("tag-subset-", subsetId, "/nlp-huge")
}

makeTitleDir = function(subsetId) {
	str_c("title-subset-", subsetId, "/nlp-huge")
}

makeBodyDir = function(subsetId) {
	str_c("body-subset-", subsetId, "/nlp-huge")
}

getSubsetId = function(str) {
	str_extract(str_extract(str, "subset-[0-9]+"), "[0-9]+$")
}

getPostIdForTagFile = function(tagFile) {
	as.integer(str_extract(tagFile, "[^/][0-9]+$"))
}

ratePosts = function(subsetId) {
	tagDir = makeTagDir(subsetId)
	tagFiles = getTagFiles(tagDir)
	#res = rbind.fill(parallel::mclapply(tagFiles, ratePost, mc.cores=3, mc.preschedule=T))
	res = rbind.fill(lapply(tagFiles, function(tagFile) {ratePost(tagFile, tagDir)}))
	return(res)
}

writePosts = function(res, testSubset, modelSubset, id) {
	write.csv(res, file=str_c(PATH, "/LogReg-", testSubset, "-", modelSubset, "-", id, ".csv"))
}

runSet = function(sets=c(8:17), id=1) {
	lapply(sets, function(set) {writePosts(ratePosts(set), set, getSubsetId(sjiCSV), id)})
}

coeffsGlobal=list(sjiTitle=1.40, sjiBody=2.36, prior=1.08)

# Load up the synonyms
colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character")
synonymsFrm = read.csv(str_c(PATH, "/../html-to-text/synonyms/synonyms.csv"), header=T, sep=",", colClasses=colClasses)
dbSynonyms = makeDb(synonymsFrm, namesAcc="SourceTagName", valsAcc="TargetTagName")

printP = 0
#writePosts(ratePosts(6), 6, getSubsetId(sjiCSV), 15)

