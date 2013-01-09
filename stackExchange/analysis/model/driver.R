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
rateVals = function(subsetIndeces, act, observed) {
	vals = as.vector(act$act[subsetIndeces])
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = subsetIndeces[res$ix]
	cutoff = 10
	sortedChunkHashes = sortedChunkHashes[1:cutoff]
	targetP = rep(0,cutoff)
	targetP[match(observed, sortedChunkHashes)] = 1
	sortedChunks = getChunks(sortedChunkHashes, db)
	#myPrint(sortedChunkHashes)
	#myPrint(observed)
	return(data.frame(tag=sortedChunks, act=res$x[1:cutoff], targetP=targetP, rank=1:cutoff))
}

rateVals2 = function(subsetIndeces, act, observed, tagFile) {
	vals = as.vector(act$sji[subsetIndeces] * 1.3 + B[subsetIndeces])
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = subsetIndeces[res$ix]
	cutoff = min(length(res$ix), 1000)
	revCutoff = length(res$ix) - cutoff + 1
	#sortedChunkHashes = union(sortedChunkHashes[1:cutoff], sortedChunkHashes[revCutoff:length(sortedChunkHashes)])
	sortedChunkHashes = sortedChunkHashes[1:cutoff]
	sortedChunkHashes = union(sortedChunkHashes, observed)
	targetP = rep(0, length(sortedChunkHashes))
	target2P = targetP
	observedVals = sort(vals[match(observed, subsetIndeces)], decreasing=T, index.return=T)
	myPrint(observedVals)
	myPrint(observed)
	topValHash = observed[observedVals$ix[1]]
	myPrint(topValHash)
	targetP[match(observed, sortedChunkHashes)] = 1
	target2P[match(topValHash, sortedChunkHashes)] = 1
	sortedChunks = getChunks(sortedChunkHashes, db)
	priors = as.vector(B[sortedChunkHashes])
	sjis = as.vector(act$sji[sortedChunkHashes])
	return(data.frame(tag=sortedChunks, sji=sjis, prior=priors, targetP=targetP, target2P=target2P, act=priors+sjis, tagFile=tagFile))
}

rateVals3 = function(subsetIndeces, act, observed, tagFile) {
	cutoff = 200
	vals = as.vector(B[subsetIndeces] + act$sjiTitle[subsetIndeces]*1.02 + act$sjiBody[subsetIndeces])*1.88
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = subsetIndeces[res$ix]
	#sortedChunkHashes = sortedChunkHashes[1:cutoff]
	#sampleIndeces = union(union(observed, sortedChunkHashes), sample(subsetIndeces, 100, replace=F))
	#sampleIndeces = union(sortedChunkHashes[1:cutoff], union(observed, sample(subsetIndeces, 500, replace=F, prob=vals)))
	probs = 1/(1+exp(-vals/.5))
	sampleIndeces = union(sortedChunkHashes[1:cutoff], union(observed, sample(subsetIndeces, 800, replace=F, prob=probs)))
	targetP = rep(0, length(sampleIndeces))
	targetP[match(observed, sampleIndeces)] = 1
	tags = getChunks(sampleIndeces, db)
	priors = as.vector(B[sampleIndeces])
	sjiTitle = as.vector(act$sjiTitle[sampleIndeces])
	sjiBody = as.vector(act$sjiBody[sampleIndeces])
	return(data.frame(tag=tags, sjiTitle=sjiTitle, sjiBody=sjiBody, prior=priors, targetP=targetP, act=priors + sjiTitle + sjiBody))
}

getObservedContext = function(tagFile) {
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

ratePost = function(tagFile) {
	res = data.frame()
	if ( length(lst <- getObservedContext(tagFile)) > 0 ) {
		observed = lst$observed
		titleContext = lst$titleContext
		bodyContext = lst$bodyContext
		myPrint(str_c("working tag file ", tagFile))
		tAct = act(getChunkHashes(titleContext, dbContext), B, sji)
		bAct = act(getChunkHashes(bodyContext, dbContext), B, sji)
		cAct = list()
		cAct$sjiTitle = tAct$sji
		cAct$sjiBody = bAct$sji
		ret = rateVals3(priorsIndeces, cAct, getChunkHashes(observed, db), tagFile)
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

# Load up the synonyms
colClasses=c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character")
synonymsFrm = read.csv(str_c(PATH, "/../html-to-text/synonyms/synonyms.csv"), header=T, sep=",", colClasses=colClasses)
dbSynonyms = makeDb(synonymsFrm, namesAcc="SourceTagName", valsAcc="TargetTagName")

# Source the model
#source(str_c(PATH, "/model.R"))

# Determine tag files
tagDir = "tag-subset-6/nlp-huge"
titleDir = "title-subset-6/nlp-huge"
bodyDir = "body-subset-6/nlp-huge"
#tagDir = "tag/nlp"
#titleDir = "title/nlp"
W = 1

tagFiles = getTagFiles(tagDir)

# Run the model for each title/tag pair, and analyse results
printP = 1
#res = rbind.fill(parallel::mclapply(tagFiles, ratePost, mc.cores=2, mc.preschedule=T))
res = rbind.fill(lapply(tagFiles, ratePost))

write.csv(res, file=str_c(PATH, "/LogReg.csv"))


# Save current objects so that they can be referenced from LaTeX document
#save.image(file = str_c(PATH, "/", ".RData"))

myPrint("done")
