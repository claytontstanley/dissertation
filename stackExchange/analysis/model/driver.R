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
	#print(sortedChunkHashes)
	#print(observed)
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
	print(observedVals)
	print(observed)
	topValHash = observed[observedVals$ix[1]]
	print(topValHash)
	targetP[match(observed, sortedChunkHashes)] = 1
	target2P[match(topValHash, sortedChunkHashes)] = 1
	sortedChunks = getChunks(sortedChunkHashes, db)
	priors = as.vector(B[sortedChunkHashes])
	sjis = as.vector(act$sji[sortedChunkHashes])
	return(data.frame(tag=sortedChunks, sji=sjis, prior=priors, targetP=targetP, target2P=target2P, act=priors+sjis, tagFile=tagFile))
}

getObservedContext = function(tagFile) {
	ret = list()
	if ( !any(grep("*.csv", tagFile)) ) {
		observed = readLines(str_c(PATH, "/../html-to-text/", tagDir, "/", tagFile), warn = F)
		context = readLines(str_c(PATH, "/../html-to-text/", titleDir, "/", tagFile), warn = F)
		ret = list("observed" = observed, "context" = context)
	}
	return(ret)
}

ratePost = function(tagFile) {
	res = data.frame()
	if ( length(lst <- getObservedContext(tagFile)) > 0 ) {
		observed = lst$observed
		context = lst$context
		print(str_c("working tag file ", tagFile))
		cAct = act(getChunkHashes(context, dbContext), B, sji)
		res = rbind(res, rateVals2(priorsIndeces, cAct, getChunkHashes(observed, db), tagFile))
	}
	return(res)
}

getTagFiles = function(tagDir) {
	list.files(path=str_c(PATH, "/../html-to-text/", tagDir), recursive=T)
}

# Source the model
#source(str_c(PATH, "/model.R"))

# Determine tag files
tagDir = "tag-subset-7/nlp-huge"
titleDir = "title-subset-7/nlp-huge"
tagDir = "tag/nlp"
titleDir = "title/nlp"
W = 1

tagFiles = getTagFiles(tagDir)

# Run the model for each title/tag pair, and analyse results
res = do.call(rbind, parallel::mclapply(tagFiles, ratePost, mc.cores=2, mc.preschedule=T))
#res = do.call(rbind, lapply(tagFiles, ratePost))

write.csv(res, file=str_c(PATH, "/LogReg.csv"))


# Save current objects so that they can be referenced from LaTeX document
#save.image(file = str_c(PATH, "/", ".RData"))

print("done")
