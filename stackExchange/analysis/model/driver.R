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

Rprof()

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

rateVals = function(subsetIndeces, vals, db, observed) {
	vals = as.vector(vals[subsetIndeces])
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = subsetIndeces[res$ix]
	matchIndeces = match(observed, sortedChunkHashes)
	return(data.frame(rank=matchIndeces, tag=getChunks(observed, db), act=res$x[matchIndeces], meanAct=mean(vals)))
}

# Source the model
source(str_c(PATH, "/model.R"))

# Determine tag files
tagDir = "tag/nlp"
titleDir = "title/nlp"
tagFiles = list.files(path=str_c(PATH, "/../html-to-text/", tagDir), recursive=T)
res = data.frame()

# Run the model for each title/tag pair, and analyse results
for (tagFile in tagFiles) {
	if ( !any(grep("*.csv", tagFile)) ) {
		print(str_c("working tag file ", tagFile))
		observed = readLines(str_c(PATH, "/../html-to-text/", tagDir, "/", tagFile), warn = F)
		context = readLines(str_c(PATH, "/../html-to-text/", titleDir, "/", tagFile), warn = F)
	
		cAct = act(getChunkHashes(context, db), B, sji)
		res = rbind(res, rateVals(priorsIndeces, cAct$act, db, getChunkHashes(observed, db)))
	}
}

# Run the model through mockup data, and save results for regression testing

inputFile = str_c(PATH, "/tests.txt")
con = file(inputFile, open = "r")
dataList = list()
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    myVector = (strsplit(oneLine, ","))
    myVector = myVector[[1]]
    dataList = c(dataList,list(myVector))
}
close(con)

for (run in dataList) {
	observed = getChunkHashes(c("php", "lisp", "c#"), db)
	cAct = act(getChunkHashes(run, db), B, sji)
	plotHighest(priorsIndeces, cAct$act, db)
	title(paste(run, collapse=" "))
	title(ylab="Total Activation")
	plotHighest(priorsIndeces, cAct$sji, db)
	title(paste(run, collapse=" "))
	title(ylab="sji Activation")
}

cAct = act(c(1:5), B, sji)
write.csv(data.frame(ChunkHash=priorsIndeces, Activation=as.vector(cAct$act[priorsIndeces])), file=str_c(PATH, "/", "Act.csv"))

# Save current objects so that they can be referenced from LaTeX document
save.image(file = str_c(PATH, "/", ".RData"))

Rprof(NULL) 
summaryRprof() 
