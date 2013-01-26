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
library(multicore)
library(ROCR)

asFig = function(figName) {
	pdf(str_c(PATH, "/Pictures/", figName, ".pdf"))
}

devOff = function() {
	dev.off()
}

logReg = function(tag, res, model) {
	myPrint(str_c("working ", length(tag), " tags"))
	dfSubset = res[res$tag %in% tag,]
	mylogit = glm(model, data=dfSubset, family="binomial")
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
	return(list("logit"=mylogit, "classLog"=tmp))
}

addColumns = function(res) {
	coeffs = coeffs
	coeffs[["offset"]] = NULL
	res$act=computeAct(res, coeffs)
	resTbl = data.table(res, key=c("tagFile"))
	resTbl = resTbl[, mean := mean(act), by=c("tagFile")]
	resTbl = resTbl[, median := median(act), by=c("tagFile")]
	resTbl = resTbl[, max := max(act), by=c("tagFile")]
	resTbl = resTbl[, sd := sd(act), by=c("tagFile")]
	resTbl = resTbl[, z := scale(act), by=c("tagFile")]
	resTbl = resTbl[, sum := sum(act), by=c("tagFile")]
	resTbl = resTbl[, meanN := mean(sort(act, decreasing=T)[1:5]), by=c("tagFile")]
	resTbl = resTbl[, rank := sort(act, decreasing=F, index.return=T)$ix, by=c("tagFile")]
	res = data.frame(resTbl)
	res$offset = res$meanN
	return(res)
}

addColumnsPost = function(res){
	res$shuffledAct = with(res, sample(act, length(act)))
	res$cSharp = with(res, as.numeric(tag=="c#")+rnorm(length(tag), mean=0, sd=.0001))
	return(res)
}

computeAct = function(res, coeffs) {
	print(coeffs)
	act = 0
	for(name in names(coeffs)) {
		if( name == '(Intercept)') {
			act = act+coeffs[[name]]
		} else {
			act = act+res[[name]]*coeffs[[name]]
		}
	}
	act
}

runLogRegFrm = function(res, model=formula(targetP ~ sjiBody + sjiTitle + prior + offset)) {
	res = addColumns(res)
	logRegRes = logReg(res$tag, res, model)
	coeffs = as.list(summary(logRegRes$logit)$coefficients[,"Estimate"])
	res$act = computeAct(res, coeffs)
	return(append(logRegRes, list("res"=res, coeffs=coeffs)))	
}

runLogReg = function(logRegFile, figName) {
	frm = read.csv(str_c(PATH, "/", logRegFile))
	res = runLogRegFrm(frm)
	asFig(figName)
	logi.hist.plot(res$act, res$targetP, boxp=T, type="hist", col="gray")
	devOff()
	return(res)
}

getFrms = function(frms=c(8:17)) {
	lapply(frms, function(id) {read.csv(str_c(PATH, "/", "LogReg-", id, "-4-1.csv"))})
}

generateItems = function(model) {
	frms = getFrms(c(8:9))
	for(i in c(1:4)) {
		res = runLogRegFrm(frms[[1]], model)
		coeffs <<- res$coeffs
	}
	frms = adjustFrms(frms)
	items = makeItems(frms)
}

adjustFrms = function(frms) {
	foo = function(frm) {
		frm = addColumns(frm)
		frm$act = computeAct(frm, coeffs)
		return(frm)		
	}
	lapply(frms, foo)
}

makeItems = function(frms) {
	cnt = 0
	foo = function(frm) {
		cnt <<- cnt+1
		list(act=frm$act, targetP=frm$targetP, tagFile=frm$tagFile, col=colors[cnt])
	}
	lapply(frms, foo)
}

plotROCColumn = function(dFrame, column) {
	pred = prediction(dFrame[[column]], dFrame$targetP)
	perf = performance(pred, "tpr", "fpr")
	nSamples = length(unlist(perf@x.values))
	indeces = sort(sample(nSamples, 2000, prob=1/(1+1000*unlist(perf@x.values))))
	fp = unlist(perf@x.values)*sum(!dFrame$targetP)
	tp = unlist(perf@y.values)*sum(dFrame$targetP)
	x = tp[indeces]+fp[indeces]
	x = x/length(unique(dFrame$tagFile))
	y = tp[indeces]/(tp[indeces]+fp[indeces])
	return(list(x=x, y=y))
	xlim = c(0, sum(dFrame$targetP)/length(unique(dFrame$tagFile)))
	if(cnt != 1) {
		lines(x, y, col=col, typ="p", pch=cnt)
	} else {
		plot(x, y, xlim=xlim, ylim=c(0, 1), col=col, pch=cnt, xlab="average number of tags per post", ylab="model tag proportion correct")
	}		
}

colors = c("red", "green", "blue", "orange", "yellow", "black", "grey", "purple", "magenta", "cyan", "pink")

plotROC2 = function(items) {
	dev.new()
	colors = unlist(lapply(items, function(item) {item$col}))
	foo = function(item) {
		data.frame(act=item$act, targetP=item$targetP, tagFile=item$tagFile)
	}
	ROCs = lapply(items, function(item) {plotROCColumn(foo(item), "act")})
	cnt = 0
	columns=as.character(c(1:length(items)))
	xlim = c(0, sum(items[[1]]$targetP)/length(unique(items[[1]]$tagFile)))
	for(ROC in ROCs) {
		cnt = cnt + 1
		col = colors[cnt]
		if(cnt != 1) {
			lines(ROC$x, ROC$y, col=col, typ="p", pch=c(cnt, rep(NA, 9)))
		} else {
			plot(ROC$x, ROC$y, xlim=xlim, ylim=c(.3, 1), col=col, pch=c(cnt, rep(NA, 9)), xlab="average number of tags per post", ylab="model tag proportion correct")
		}
	}	
	legend("topright", legend=columns, col=colors, pch=1:length(columns))
}

plotHighest = function(contextIndeces, vals) {
	topNum = 20
	vals = as.vector(vals[contextIndeces])
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = contextIndeces[res$ix]
	x = sortedChunkHashes[1:topNum]
	xnames = getChunks(x, dbPriors)
	y = res$x[1:topNum]
	plot(1:topNum, y, xaxt="n", ann=F)
	axis(1, at=1:topNum, labels=xnames, las=3, cex.axis=.8)
}

addLabels = function(observed, titleContext, ylab) {
		weights = round(as.vector(contextWeights[getChunkHashes(titleContext, dbContext)]), 2)
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
		plotHighest(priorsIndeces, B+tAct$sji*coeffs$sjiTitle+bAct$sji*coeffs$sjiBody)
		addLabels(observed, titleContext, "Total Activation")
		devOff()
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-sjiTitle"))
		plotHighest(priorsIndeces, tAct$sji*coeffs$sjiTitle)
		addLabels(observed, titleContext, "sji Title")
		devOff()
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-sjiBody"))
		plotHighest(priorsIndeces, bAct$sji*coeffs$sjiBody)
		addLabels(observed, titleContext, "sjiBody")
		devOff()
	}
}

contextWeight2 = function(indeces) {
	arr = N
	arrSums = rowSums(arr)
	ps = with(summary(arr), sparseMatrix(i=i, j=j, x=x/arrSums[i]))
	Hs = with(summary(ps), sparseMatrix(i=i, j=j, x=x*log(x)))
	H = -rowSums(Hs)
	data.frame(Chunk=getChunks(indeces, dbContext), ChunkHash=contextHashIndeces[indeces], H=H, N=arrSums)
}

generateContextWeights = function () {
	contextWeightsFrm = contextWeight2(contextIndeces)
	write.csv(contextWeightsFrm, file=str_c(PATH, "/", contextWeightsCSV))	
}
