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
library(calibrate)
library(RMySQL)
library(multicore)

asFig = function(figName) {
	pdf(str_c(PATH, "/Pictures/", figName, ".pdf"))
}

devOff = function() {
	dev.off()
}

logReg = function(tag, res) {
	myPrint(str_c("working ", length(tag), " tags"))
	dfSubset = res[res$tag %in% tag,]
	#dev.new()
	#logi.hist.plot(dfSubset$act, dfSubset$targetP, boxp=T, type="hist", col="gray")
	#title(main=tag)
	mylogit = glm(targetP ~ sjiBody + sjiTitle + prior, data=dfSubset, family="binomial")
	myPrint(summary(mylogit))
	tmp = ClassLog(mylogit, dfSubset$targetP)
	myPrint(tmp) 
	tmp2 = melt(tmp$classtab)
	myPrint(tmp2)
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
	return(list("logit"=mylogit, "classLog"=tmp))
}

runLogReg = function(logRegFile, figName) {
	res = read.csv(str_c(PATH, "/", logRegFile))
	logRegRes = logReg(res$tag, res)
	coeffs = summary(logRegRes$logit)$coefficients[,"Estimate"]
	res$act = res$sjiTitle * coeffs["sjiTitle"] + res$sjiBody * coeffs["sjiBody"] + res$prior
	asFig(figName)
	logi.hist.plot(res$act, res$targetP, boxp=T, type="hist", col="gray")
	devOff()
	return(append(logRegRes, list("res"=res, coeffs=as.list(coeffs))))
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

addLabels = function(observed, titleContext, ylab) {
		weights = round(as.vector(contextWeights[getChunkHashes(titleContext, db)]), 2)
		title(str_c(paste(titleContext, collapse=" "), "\n", paste(weights, collapse=" "), "\n", paste(observed, collapse=" ")), cex.main=.9)
		title(ylab=ylab)
}

visPost = function(tagFile, coeffs=list(sjiTitle=1, sjiBody=1)) {
	if ( length(lst <- getObservedContext(tagFile)) > 0 ) {
		observed = lst$observed
		titleContext = lst$titleContext
		bodyContext = lst$bodyContext
		tAct = act(getChunkHashes(titleContext, dbContext), B, sji)
		bAct = act(getChunkHashes(bodyContext, dbContext), B, sji)
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-act"))
		plotHighest(priorsIndeces, B+tAct$sji*coeffs$sjiTitle+bAct$sji*coeffs$sjiBody, db)
		addLabels(observed, titleContext, "Total Activation")
		devOff()
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-sjiTitle"))
		plotHighest(priorsIndeces, tAct$sji*coeffs$sjiTitle, db)
		addLabels(observed, titleContext, "sji Title")
		devOff()
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-sjiBody"))
		plotHighest(priorsIndeces, bAct$sji*coeffs$sjiBody, db)
		addLabels(observed, titleContext, "sjiBody")
		devOff()
	}
}

contextWeight = function(index) {
	vect = as.vector(N[index, priorsIndeces])
	count = sum(vect)
	ps = vect / count
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
	#myPrint(str_c(count, "->", getChunks(index, db), "->", H))
	data.frame(Chunk=getChunks(index, db), ChunkHash=index, sdev=sdev, H=H, N=count)
}

contextWeight2 = function(indeces) {
	arr = N[indeces,priorsIndeces]
	arrSums = rowSums(arr)
	ps = with(summary(arr), sparseMatrix(i=i, j=j, x=x/arrSums[i]))
	Hs = with(summary(ps), sparseMatrix(i=i, j=j, x=x*log(x)))
	H = -rowSums(Hs)
	data.frame(Chunk=getChunks(indeces, db), ChunkHash=indeces, H=H, N=arrSums)
}

generateContextWeights = function () {
	contextWeightsFrm = contextWeight2(contextIndeces)
	write.csv(contextWeightsFrm, file=str_c(PATH, "/", contextWeightsCSV))	
}

sjiRank = function(row, sji) {
	temp = as.numeric(sji[row,priorsIndeces])
	temp = temp[temp != 0]
	return(data.frame(chunkHash=row, chunk=getChunks(row, db), sd=sd(temp), MADZero=mean(abs(temp)), N=length(temp)))
}

