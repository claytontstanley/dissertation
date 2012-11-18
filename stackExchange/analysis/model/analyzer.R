# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Load up the libraries
library(stringr)
library(popbio)
library(sqldf)
library(QuantPsyc)
library(multicore)
library(reshape)

asFig = function(figName) {
	pdf(str_c(PATH, "/Pictures/", figName, ".pdf"))
}

devOff = function() {
	dev.off()
}

logReg = function(tag, res) {
	print(str_c("working ", length(tag), " tags"))
	dfSubset = res[res$tag %in% tag,]
	#dev.new()
	#logi.hist.plot(dfSubset$act, dfSubset$targetP, boxp=T, type="hist", col="gray")
	#title(main=tag)
	mylogit = glm(targetP ~ sji + prior, data=dfSubset, family="binomial")
	#mylogit = glm(targetP ~ act, data=dfSubset, family="binomial")
	print(summary(mylogit))
	tmp = ClassLog(mylogit, dfSubset$targetP)
	print(tmp) 
	tmp2 = melt(tmp$classtab)
	print(tmp2)
	valsSubset = tmp2$value
	names(valsSubset) = paste(tmp2$Var.1, tmp2$resp, sep="_")
	vals = c(rep(0, 4))
	names(vals) = c("FALSE_0", "TRUE_0", "FALSE_1", "TRUE_1")
	vals[names(valsSubset)] = valsSubset
	coeffs = summary(mylogit)$coefficients[,"Estimate"]
	Ns = c(length(dfSubset[dfSubset$targetP == 1,]$targetP), length(dfSubset[dfSubset$targetP == 0,]$targetP), dim(dfSubset)[1])
	names(Ns) = c('NTagged', 'NNotTagged', 'N')
	#p = summary(mylogit)$coefficients["act",'Pr(>|z|)']
	#return(with(tmp, data.frame(mcFadden=mcFadden,overall=overall, tag=tag, t(vals), t(Ns), p=p)))
	return(list("W"=coeffs["sji"]/coeffs["prior"], "logit"=mylogit, "classLog"=tmp))
}

plotHighest = function(subsetIndeces, vals, db) {
	topNum = 20
	vals = as.vector(vals[subsetIndeces])
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = subsetIndeces[res$ix]
	x = sortedChunkHashes[1:topNum]
	xnames = getChunks(x, db)
	y = res$x[1:topNum]
	plot(1:topNum, y, xaxt="n", ann=F)
	axis(1, at=1:topNum, labels=xnames, las=3, cex.axis=.8)
}

visPost = function(tagFile) {
	if ( length(lst <- getObservedContext(tagFile)) > 0 ) {
		observed = lst$observed
		context = lst$context
		cAct = act(getChunkHashes(context, dbContext), B, sji)
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-act"))
		plotHighest(priorsIndeces, cAct$act, db)
		weights = round(as.vector(contextWeights[getChunkHashes(context, db)]), 2)
		title(str_c(paste(context, collapse=" "), "\n", paste(weights, collapse=" "), "\n", paste(observed, collapse=" ")), cex.main=.9)
		title(ylab="Total Activation")
		devOff()
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-sji"))
		plotHighest(priorsIndeces, cAct$sji, db)
		title(str_c(paste(context, collapse=" "), "\n", paste(weights, collapse=" "), "\n", paste(observed, collapse=" ")), cex.main=.9)
		title(ylab="sji Activation")
		devOff()
	}
}

contextWeight = function(index) {
	count = sum(N[index,priorsIndeces])
	ps = N[index, priorsIndeces] / count
	ps = ps[ps != 0]
	sdev = sd(ps)
	Hs = ps * log(ps)
	H = -sum(Hs)
	#dev.new()
	#hist(ps, xlab="", ylab="", main="", ylim=c(0,50), xlim=c(0,.4))
	#title(xlab="Nji/Nj")
	#title(ylab="Frequency")
	#title(str_c("Entropy distribution for context chunk: ", getChunks(index, db)))
	#dev.new()
	#hist(Hs)
	#print(str_c(count, "->", getChunks(index, db), "->", H))
	data.frame(Chunk=getChunks(index, db), ChunkHash=index, sdev=sdev, H=H, N=count)
}

generateContextWeights = function () {
	filteredIndeces = contextFrm$ChunkHash[contextFrm$ChunkCount > 1]
	contextWeightsFrm = do.call(rbind, mclapply(filteredIndeces, contextWeight, mc.cores = 4, mc.preschedule=T))
	write.csv(contextWeightsFrm, file=str_c(PATH, "/", contextWeightsCSV))	
}

sjiRank = function(row, sji) {
	temp = as.numeric(sji[row,priorsIndeces])
	temp = temp[temp != 0]
	return(data.frame(chunkHash=row, chunk=getChunks(row, db), sd=sd(temp), MADZero=mean(abs(temp)), N=length(temp)))
}

descripts = c()
descripts["sjiCells"] = nnzero(sji)
descripts["sjiObservations"] = sum(sji)
descripts["sjiDensity"] = nnzero(sji) / prod(dim(sji))
descripts["tagCells"] = nnzero(priors)
descripts["tagObservations"] = sum(priors)
descripts["tagDensity"] = nnzero(priors) / length(priors)
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
hist(as.vector(contextWeights[contextWeightsIndeces]), main="Distribution of attentional weights", xlab="Weight")
devOff()

asFig("attentionalN")
main="Attentional weights as a function of #observations for each cue"
xlab="log #observations for cue"
ylab="Attentional weight"
plot(log(as.vector(NRowSums[contextWeightsIndeces])), contextWeights[contextWeightsIndeces], main=main, xlab=xlab, ylab=ylab)
devOff()

runLogReg = function(logRegFile, figName) {
	res = read.csv(str_c(PATH, "/", logRegFile))
	logRegRes = logReg(res$tag, res)
	res$act = res$sji * logRegRes$W + res$prior
	asFig(figName)
	logi.hist.plot(res$act, res$targetP, boxp=T, type="hist", col="gray")
	devOff()
	return(append(logRegRes, list("res"=res)))
}

run1 = runLogReg("LogReg-6-4-2.csv", "run1LogReg")

tagDir = "tag-subset-6/nlp-huge"
titleDir = "title-subset-6/nlp-huge"
tagFiles = getTagFiles(tagDir)
W=run1$W
visPost(tagFiles[2])
visPost(tagFiles[40])
visPost(tagFiles[120])

run2 = runLogReg("LogReg-7-4.csv", "run2LogReg")

coeffsFrm = rbind(data.frame(summary(run2$logit)$coefficients, row.names=NULL), data.frame(summary(run1$logit)$coefficients, row.names=NULL))
coeffsFrm$parameter = rep(rownames(summary(run2$logit)$coefficients), 2)
coeffsFrm$run = c("1", "", "", "2", "", "")
coeffsFrm = coeffsFrm[,c(6,5,1:4)]

genFitFrm = function(run2) {
	mcFadden = run2$classLog$mcFadden
	overall = run2$classLog$overall
	classtab = as.vector(run2$classLog$classtab)
	rawtab = as.vector(run2$classLog$rawtab)
	result = as.matrix(c(mcFadden, overall, classtab, rawtab))
	rownames(result) = c("mcFadden", "overall", "CRs", "FAs", "Misses", "Hits", "numCRs", "numFAs", "numMisses", "numHits")
	data.frame(result)
}

fitFrm = cbind(genFitFrm(run1), genFitFrm(run2))
colnames(fitFrm) = c("run1", "run2")

# Save current objects so that they can be referenced from LaTeX document
savedVars=c("descriptsFrm", "coeffsFrm", "fitFrm")
save(list=savedVars, file = str_c(PATH, "/", ".RData"))




