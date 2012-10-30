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

# A few helper functions
plotHighest = function(subsetIndeces, vals, db) {
	dev.new()
	topNum = 20
	vals = as.vector(vals[subsetIndeces])
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = subsetIndeces[res$ix]
	x = sortedChunkHashes[1:topNum]
	xnames = getChunks(x, db)
	y = res$x[1:topNum]
	plot(1:topNum, y, xaxt="n", ann=F)
	axis(1, at=1:topNum, labels=xnames, las=3)
}

rateVals = function(subsetIndeces, vals, observed) {
	vals = as.vector(vals[subsetIndeces])
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
		print(str_c("working tag file ", tagFile))
		observed = lst$observed
		context = lst$context
		cAct = act(getChunkHashes(context, db), B, sji)
		res = rbind(res, rateVals(priorsIndeces, cAct$act, getChunkHashes(observed, db)))
	}
	return(res)
}

visPost = function(tagFile) {
	if ( length(lst <- getObservedContext(tagFile)) > 0 ) {
		observed = lst$observed
		context = lst$context
		cAct = act(getChunkHashes(context, db), B, sji)
		plotHighest(priorsIndeces, cAct$act, db)
		title(paste(context, collapse=" "))
		title(ylab="Total Activation")
		plotHighest(priorsIndeces, cAct$sji, db)
		title(str_c(paste(context, collapse=" "), "\n", paste(observed, collapse=" ")))
		title(ylab="sji Activation")
	}
}

# Source the model
source(str_c(PATH, "/model.R"))

# Determine tag files
#tagDir = "tag-subset-6/nlp-huge"
#titleDir = "title-subset-6/nlp-huge"
tagDir = "tag/nlp"
titleDir = "title/nlp"

tagFiles = list.files(path=str_c(PATH, "/../html-to-text/", tagDir), recursive=T)

# Run the model for each title/tag pair, and analyse results
res = do.call(rbind, multicore::mclapply(tagFiles, ratePost, mc.preschedule=F))

write.csv(res, file=str_c(PATH, "/LogReg.csv"))
dev.new()
logi.hist.plot(res$act, res$targetP, boxp=F, type="hist", col="gray")

cAct = act(c(1:5), B, sji)
write.csv(data.frame(ChunkHash=priorsIndeces, Activation=as.vector(cAct$act[priorsIndeces])), file=str_c(PATH, "/", "Act.csv"))

# Save current objects so that they can be referenced from LaTeX document
save.image(file = str_c(PATH, "/", ".RData"))
