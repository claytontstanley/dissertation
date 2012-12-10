# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Source the analyzer utility functions
source(str_c(PATH, "/analyzer.R"))

con = dbConnect(MySQL())

fullDataset = list()
fullDataset$postCount = dbGetQuery(con,'select count(*) from sotero.posts where posttypeid = 1')
fullDataset$userCount = dbGetQuery(con, 'select count(*) from users')
fullDataset$cSharpCount = priorsFrm$ChunkCount[match("c#", priorsFrm$Chunk)]
fullDataset$numItemsRemoved = dim(contextWeightsFrm)[1] - dim(filteredFrm)[1]
fullDataset$fractionRemoved = 1 - dim(filteredFrm)[1] / dim(contextWeightsFrm)[1]

descripts = c()
descripts["sjiCells"] = nnzero(N)
descripts["sjiObservations"] = sum(N)
descripts["sjiDensity"] = nnzero(N) / prod(dim(N))
descripts["tagCells"] = nnzero(priors)
descripts["tagObservations"] = sum(priors)
descripts["tagDensity"] = nnzero(priors) / length(priors)
descripts["contextCells"] = nnzero(context)
descripts["contextObservations"] = sum(context)
descripts["contextDensity"] = nnzero(context) / length(context)
descriptsFrm = data.frame(descripts)

asFig("tagActDis")
hist(as.vector(B[priorsIndeces]), main="Distribution of tag activations", xlab="Activation")
devOff()

asFig("tagActSorted")
main="Activations of individual tags when sorted by activation"
xlab="Sorted tag ID"
ylab="Activation"
plot(1:length(priorsIndeces), sort(as.vector(B[priorsIndeces]), decreasing=T), main=main, xlab=xlab, ylab=ylab)
devOff()

asFig("sjiActDis")
hist(with(summary(sji), x), main="Distribution of sji associations", xlab="Activation")
devOff()

figName = "sjiActScatter"
png(str_c(PATH, "/Pictures/", figName, ".png"), width=480*10, height=480*10, res=72*10)
with(summary(sji), plot(i,j, main="Scatter of sji sparse matrix", xlab="i index", ylab="j index", cex=.2))
devOff()

figName = "sjiActScatterLowRes"
png(str_c(PATH, "/Pictures/", figName, ".png"))
with(summary(sji), plot(i,j, main="Scatter of sji sparse matrix", xlab="i index", ylab="j index", cex=.2))
devOff()

#lapply(getChunkHashes(c("php", "the", "?", "lisp"), db), contextWeight)

asFig("attentionalDis")
hist(rep(as.vector(contextWeights[contextWeightsIndeces]), as.vector(NRowSums[contextWeightsIndeces])), main="Distribution of attentional weights", xlab="Weight")
devOff()

asFig("attentionalN")
main="Attentional weights as a function of #observations for each cue"
xlab="log #observations for cue"
ylab="Attentional weight"
plot(log(as.vector(NRowSums[contextWeightsIndeces])), contextWeights[contextWeightsIndeces], main=main, xlab=xlab, ylab=ylab)
textVect = c("php", "lisp", "the", "?", "xml", "foo", "very", "much", "well", "binding", "network", "a", "baz")
textHash = getChunkHashes(textVect, db)
textxy(log(as.vector(NRowSums[textHash])), contextWeights[textHash], textVect, cx=1, dcol = "orange")
devOff()

run1 = runLogReg("LogReg-6-4-2.csv", "run1LogReg")

tagDir = "tag-subset-6/nlp-huge"
titleDir = "title-subset-6/nlp-huge"
tagFiles = getTagFiles(tagDir)
W=run1$W
visPost(tagFiles[2])
visPost(tagFiles[40])
visPost(tagFiles[120])

run2 = runLogReg("LogReg-7-4.csv", "run2LogReg")

run1Compressed = runLogReg("LogReg-6-4-3.csv", "run1CompressedLogReg")
run2Compressed = runLogReg("LogReg-7-4-1.csv", "run2CompressedLogReg")

run1HighEntropy = runLogReg("LogReg-6-4-5.csv", "run1BadContextLogReg")

coeffsFrm = do.call(rbind, lapply(list(run1, run2, run1Compressed, run2Compressed, run1HighEntropy), function(run) data.frame(summary(run$logit)$coefficients, row.names=NULL)))

coeffsFrm$parameter = rep(rownames(summary(run2$logit)$coefficients), 5)
coeffsFrm$run = c("1", "", "", "2", "", "", "1", "", "", "2", "", "", "1", "", "")
coeffsFrm$runName = c("uncompressed", rep(c(""), 5), "compressed", rep(c(""), 5), "highEntropy", "", "")
coeffsFrm = coeffsFrm[,c(7,6,5,1:4)]

genFitFrm = function(run2) {
	mcFadden = run2$classLog$mcFadden
	overall = run2$classLog$overall
	classtab = as.vector(run2$classLog$classtab)
	rawtab = as.vector(run2$classLog$rawtab)
	result = as.matrix(c(mcFadden, overall, classtab, rawtab))
	rownames(result) = c("mcFadden", "overall", "CRs", "FAs", "Misses", "Hits", "numCRs", "numFAs", "numMisses", "numHits")
	data.frame(result)
}

fitFrm = do.call(cbind, lapply(list(run1, run2, run1Compressed, run2Compressed, run1HighEntropy), genFitFrm))
colnames(fitFrm) = c("run1", "run2", "run1Compressed", "run2Compressed", "run1HighEntropy")

# Save current objects so that they can be referenced from LaTeX document
savedVars=c("descriptsFrm", "coeffsFrm", "fitFrm", "fullDataset")
save(list=savedVars, file = str_c(PATH, "/", ".RData"))
