frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

library(stringr)

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

source(str_c(PATH, "/model.R"))

tagDir = "tags/nlp"
titleDir = "title/nlp"
tagFiles = list.files(path=str_c(PATH, "/../html-to-text/", tagDir), recursive=T)
res = data.frame()


for (tagFile in tagFiles) {
	print(str_c("working tag file ", tagFile))
	observed = readLines(str_c(PATH, "/../html-to-text/", tagDir, "/", tagFile), warn = F)
	context = readLines(str_c(PATH, "/../html-to-text/", titleDir, "/", tagFile), warn = F)
	print(context)
	
	#cAct = act(getChunkHashes(context, db), B, sji)
	#res = rbind(res, rateVals(priorsIndeces, cAct$act, db, getChunkHashes(observed, db)))

}


inputFile = str_c(PATH, "/tests.txt")
con = file(inputFile, open = "r")
dataList = list()
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    myVector = (strsplit(oneLine, ","))
    myVector = myVector[[1]]
    dataList = c(dataList,list(myVector))
}
close(con)

res = data.frame()

for (run in dataList) {
	observed = getChunkHashes(c("php", "lisp", "c#"), db)
	cAct = act(getChunkHashes(run, db), B, sji)
	plotHighest(priorsIndeces, cAct$act, db)
	title(paste(run, collapse=" "))
	title(ylab="Total Activation")
	plotHighest(priorsIndeces, cAct$sji, db)
	title(paste(run, collapse=" "))
	title(ylab="sji Activation")
	res = rbind(res, rateVals(priorsIndeces, cAct$act, db, observed))
}

cAct = act(c(1:5), B, sji)
write.csv(data.frame(ChunkHash=priorsIndeces, Activation=as.vector(cAct$act[priorsIndeces])), file=str_c(PATH, "/", "Act.csv"))

# Save current objects so that they can be referenced from LaTeX document
save.image(file = str_c(PATH, "/", ".RData"))
