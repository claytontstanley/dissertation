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
	vals = as.vector(B[priorsIndeces] + act$sjiTitle[priorsIndeces]*1.02 + act$sjiBody[priorsIndeces])*1.88
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
printP = 0
#res = rbind.fill(parallel::mclapply(tagFiles, ratePost, mc.cores=3, mc.preschedule=T))
res = rbind.fill(lapply(tagFiles, ratePost))

write.csv(res, file=str_c(PATH, "/LogReg.csv"))


# Save current objects so that they can be referenced from LaTeX document
#save.image(file = str_c(PATH, "/", ".RData"))

print("done")
