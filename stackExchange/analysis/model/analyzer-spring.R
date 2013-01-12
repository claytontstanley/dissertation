# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Source the analyzer utility functions
source(str_c(PATH, "/analyzer.R"))

runSample = runLogReg("LogReg.csv", "runSample")

visPost(tagFiles[2], runSample$coeffs)
visPost(tagFiles[40], runSample$coeffs)
visPost(tagFiles[120], runSample$coeffs)

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

#figName = "sjiActScatter"
#png(str_c(PATH, "/Pictures/", figName, ".png"), width=480*10, height=480*10, res=72*10)
#with(summary(sji), plot(i,j, main="Scatter of sji sparse matrix", xlab="i index", ylab="j index", cex=.2))
#devOff()

figName = "attentionalN"
png(str_c(PATH, "/Pictures/", figName, ".png"), width=480*10, height=480*10, res=72*10)
main="Attentional weights as a function of #observations for each cue"
xlab="log #observations for cue"
ylab="Attentional weight"
plot(log(as.vector(NRowSums[contextWeightsIndeces])), contextWeights[contextWeightsIndeces], main=main, xlab=xlab, ylab=ylab)
textVect = c("php", "lisp", "the", "?", "xml", "foo", "very", "much", "well", "binding", "network", "a", "baz")
textHash = getChunkHashes(textVect, dbContext)
textxy(log(as.vector(NRowSums[textHash])), contextWeights[textHash], textVect, cx=1, dcol = "orange")
devOff()

break()

asFig("attentionalDis")
hist(rep(as.vector(contextWeights[contextWeightsIndeces]), as.vector(NRowSums[contextWeightsIndeces])), main="Distribution of attentional weights", xlab="Weight")
devOff()

