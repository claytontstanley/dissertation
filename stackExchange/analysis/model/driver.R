# Setup variables and helper functions
wideScreen(150)
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

library(stringr)

plotHighest = function(subsetIndeces, vals, db) {
	dev.new()
	vals = as.vector(vals[subsetIndeces])
	res = sort(vals, decreasing=T, index.return=T)
	topNum = 20
	x = subsetIndeces[res$ix[1:topNum]]
	xnames = getChunks(x, db)
	y = res$x[1:topNum]
	plot(1:topNum, y, xaxt="n", ann=F)
	axis(1, at=1:topNum, labels=xnames, las=3)
}

source(str_c(PATH, "/model.R"))

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
