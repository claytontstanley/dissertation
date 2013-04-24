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
	mylogit = glm(model, data=dfSubset, family=binomial(link="logit"))
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

addColumns = function(res, coeffs) {
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

runLogRegFrm = function(res, coeffs=coeffsGlobal, model=formula(targetP ~ sjiBody + sjiTitle + prior + offset)) {
	res = addColumns(res, coeffs)
	logRegRes = logReg(res$tag, res, model)
	coeffs = as.list(summary(logRegRes$logit)$coefficients[,"Estimate"])
	res$act = computeAct(res, coeffs)
	return(append(logRegRes, list("res"=res, coeffs=coeffs)))	
}

plotFrm = function(res, figName) {
	asFig(figName)
	logi.hist.plot(res$act, res$targetP, boxp=T, type="hist", col="gray")
	devOff()
}

getFrms = function(testDatasets, runId) {
	lapply(testDatasets, function(id) {read.csv(str_c(PATH, "/", "LogReg-", id, "-4-", runId, ".csv"))})
}

getAllFrmsForModel = function(model, coeffs=coeffsGlobal, frms=c(8:17), runId=1) {
	frms = getFrms(frms, runId)
	frms = adjustFrms(frms, coeffs, model)
}

recomputeActivation = function(frm, coeffs) {
	frm = addColumns(frm, coeffs)
	frm$act = computeAct(frm, coeffs)
	frm
}

adjustFrms = function(frms, coeffs, model) {
	for(i in c(1:4)) {
		res = runLogRegFrm(frms[[1]], coeffs, model)
		coeffs = res$coeffs
		classLog = res$classLog
		logit = summary(res$logit)
	}
	frms = lapply(frms, function(frm) recomputeActivation(frm, coeffs))
	return(lapply(frms, function(frm) list(frm=frm, coeffs=coeffs, classLog=classLog, logit=logit)))
}

getAllFrms = function() {
	models=list(
		formula(targetP ~ prior + sjiTitle + sjiBody + offset),
		formula(targetP ~ prior + sjiTitle + offset),
		formula(targetP ~ prior + sjiBody + offset), 
		formula(targetP ~ prior + sjiTitle + sjiBody)
		)
	frms = list()
	for(model in models) {
		frms = append(frms, getAllFrmsForModel(frms=c(8:9), runId=1, model=model))
	}
	models=list(
		models[[1]],
		formula(targetP ~ prior + sjiTitle + sjiBody),
		formula(targetP ~ prior + sjiTitle)
		)
	for(model in models) {
		frms = append(frms, getAllFrmsForModel(frms=8, runId=2, model=model))
	}
	frms
}

plotAllFrms = function(frms=getAllFrms()) {
	legendText = c("Full Model, Calibration", "Full Model, Test", "Without Body Words", "Without Title Words", 
		"Without Offset", "Without Entropy Weighting", "Without Offset & Entropy", "Without Offset, Entropy, & Body")
	items = makeItems(frms[c(1,2,3,5,7,9,10,11)], legendText)
	plotROC2(items)
}

makeItems = function(frms, legendText=as.character(c(1:length(frms)))) {
	cnt = 0
	foo = function(frm) {
		frm = frm$frm
		cnt <<- cnt+1
		list(act=frm$act, targetP=frm$targetP, tagFile=frm$tagFile, col=colors[cnt], legendText=legendText[cnt], shapes=shapes[cnt])
	}
	lapply(frms, foo)
}

plotROCColumn = function(dFrame, column) {
	pred = prediction(dFrame[[column]], dFrame$targetP)
	perf = performance(pred, "tpr", "fpr")
	fp = unlist(perf@x.values)*sum(!dFrame$targetP)
	tp = unlist(perf@y.values)*sum(dFrame$targetP)
	cutoff = min(which((tp+fp) > (sum(dFrame$targetP) * 2)))
	indeces = sort(sample(cutoff, min(cutoff, 2000), prob=1/(1+1:cutoff)))
	x = tp[indeces]+fp[indeces]
	x = x/sum(dFrame$targetP)
	y = tp[indeces]/(tp[indeces]+fp[indeces])
	return(list(x=x, y=y))	
}

colors = c("red", "green", "blue", "orange", "yellow", "black", "grey", "purple", "magenta", "cyan", "pink")
shapes = c(1, 2, 3, 4, 5, 20, 7, 8, 9, 10, 11)
colors = rep("black", 10)

plotROC2 = function(items, figName="ROC") {
	asFig(figName)
	colors = unlist(lapply(items, function(item) {item$col}))
	shapes = unlist(lapply(items, function(item) {item$shape}))
	foo = function(item) {
		data.frame(act=item$act, targetP=item$targetP, tagFile=item$tagFile)
	}
	ROCs = lapply(items, function(item) {plotROCColumn(foo(item), "act")})
	cnt = 0
	columns=unlist(lapply(items, function(item) item$legendText))
	xlim = c(0, 1.3)
	for(ROC in ROCs) {
		cnt = cnt + 1
		col = colors[cnt]
		if(cnt != 1) {
			lines(ROC$x, ROC$y, col=col, typ="p", pch=c(shapes[cnt], rep(NA, 32)))
		} else {
			plot(ROC$x, ROC$y, xlim=xlim, ylim=c(.2, .9), col=col, pch=c(shapes[cnt], rep(NA, 32)),
				xlab="Proportion of model tag count to observed tag count", ylab="Model tag proportion correct")
		}
	}	
	legend("topright", legend=columns, col=colors, pch=shapes[1:length(columns)])
	with(items[[1]], abline(v=length(unique(tagFile))/sum(targetP), lty=3))
	dev.off()
}

getAccuracyAtNAverageTags = function(frm, N=1) {
	propTagged = with(frm, length(unique(tagFile))/sum(targetP) * N)
	res = plotROCColumn(frm, "act")
	res$y[max(which(res$x<propTagged))]
}

plotHighest = function(contextIndeces, vals, topNum) {
	vals = lapply(vals, function(val) {val[contextIndeces]})
	frm = data.frame(vals)
	vals = rowSums(frm)
	res = sort(vals, decreasing=T, index.return=T)
	sortedChunkHashes = contextIndeces[res$ix]
	x = sortedChunkHashes[1:topNum]
	xnames = getChunks(x, dbPriors)
	for (name in colnames(frm)) {
		offset = min(frm[[name]][x])
		if (offset < 0) {
			frm[[name]] = frm[[name]] - offset
		}
	}
	par(mar=c(10,4.1,4.1,2.1))
	barplot(t(frm[x,]), legend=colnames(frm), names.arg=xnames, las=3)
}

addLabels = function(observed, titleContext, ylab) {
		weights = round(as.vector(contextWeights[getChunkHashes(titleContext, dbContext)]), 2)
		titleText = paste(titleContext, collapse=" ")
		padding = floor( 1.8 * (nchar(titleText) - sum(nchar(weights)))/(length(titleContext)-1) )
		titleWeights = paste(weights, collapse=paste(rep(" ", padding), collapse=""))
		title(str_c(titleText, "\n", titleWeights, "\n", paste(observed, collapse=" ")), cex.main=.9)
		title(ylab=ylab)
}

visPost = function(tagFile, tagDir, coeffs=coeffsGlobal, topNum=12) {
	if ( length(lst <- getObservedContext(tagFile, tagDir)) > 0 ) {
		observed = lst$observed
		titleContext = lst$titleContext
		bodyContext = lst$bodyContext
		tAct = act(getChunkHashes(titleContext, dbContext), B, sji)
		bAct = act(getChunkHashes(bodyContext, dbContext), B, sji)
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-act"))
		plotHighest(priorsIndeces, list(prior=B, sjiTitle=tAct$sji*coeffs$sjiTitle, sjiBody=bAct$sji*coeffs$sjiBody), topNum=topNum)
		addLabels(observed, titleContext, "Total Activation")
		devOff()
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-sjiTitle"))
		plotHighest(priorsIndeces, list(sjiTitle=tAct$sji*coeffs$sjiTitle), topNum=topNum)
		addLabels(observed, titleContext, "sji Title")
		devOff()
		asFig(str_c("visPost-", sub("^.*/", "", tagFile), "-sjiBody"))
		plotHighest(priorsIndeces, list(sjiBody=bAct$sji*coeffs$sjiBody), topNum=topNum)
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

getCoeffs = function(baseFrms) {
	lapply(baseFrms, function(baseFrm) baseFrm$coeffs)
}

getClassLog = function(baseFrms) {
	lapply(baseFrms, function(baseFrm) baseFrm$classLog)
}

getLogitSummary = function(baseFrms) {
	lapply(baseFrms, function(baseFrm) baseFrm$logit)
}

getBaseFrmsDescriptives = function(baseFrms) {
	lapply(baseFrms, function(baseFrm) {
		by(baseFrm$frm$act, baseFrm$frm$targetP, summary)
	})
}

assignCoeffs = function(baseFrms, coeffs) {
	lapply(1:length(baseFrms), function(i) {
		baseFrm = baseFrms[[i]]
		baseFrm$coeffs = coeffs[[i]]
		baseFrm
	})
}

sortBaseFrm = function(baseFrm) {
	baseFrm[with(baseFrm, order(-act)),]
}

sortBaseFrms = function(baseFrms) {
	lapply(1:length(baseFrms), function(i) {
		baseFrm = baseFrms[[i]]
		baseFrm$frm = sortBaseFrm(baseFrm$frm)
		baseFrm
	})
}

recomputeActivations = function(baseFrms) {
	lapply(baseFrms, function(baseFrm) {
		baseFrm$frm = recomputeActivation(baseFrm$frm, baseFrm$coeffs)
		baseFrm
	})
}


combinePs = function(psGlobal, psLocal, count, maxCount) {
	count = pmin(count, maxCount)
	(psGlobal*(maxCount-count+1) + psLocal*count) / (maxCount + 1)
}

computeCombinedPrior = function(baseFrm, maxCount) {
	baseFrm$combinedPriorProb = with(baseFrm, combinePs(globalPriorProb, userPriorProb, userIdTagCount, maxCount))
	baseFrm$combinedPrior = with(baseFrm, log(combinedPriorProb/(1-combinedPriorProb)))
	baseFrm
}

analyzeBaseFrmPrior = function(baseFrm) {
	adjustFrms(list(baseFrm), coeffsGlobal, model=formula(targetP ~ prior + sjiTitle + sjiBody + offset))
}

analyzeBaseFrmUserPrior = function(baseFrm) {
	baseFrm$userPrior[is.infinite(baseFrm$userPrior)] = with(baseFrm, min(userPrior[!is.infinite(userPrior)]))
	adjustFrms(list(baseFrm), coeffsGlobal, model=formula(targetP ~ userPrior + sjiTitle + sjiBody + offset))
}

analyzeBaseFrmCombinedPrior = function(baseFrm, maxCount) {
	baseFrm = computeCombinedPrior(baseFrm, maxCount=maxCount)
	adjustFrms(list(baseFrm), coeffsGlobal, model=formula(targetP ~ combinedPrior + sjiTitle + sjiBody + offset))
}


getPriorsPerformance = function(baseFrm) {
	baseFrms = analyzeBaseFrmPrior(baseFrm)
	baseFrms[2] = analyzeBaseFrmUserPrior(baseFrm)
	priorAt3 = getAccuracyAtNAverageTags(baseFrms[[1]]$frm, N=3)
	userPriorAt3 = getAccuracyAtNAverageTags(baseFrms[[2]]$frm, N=3)
	list(priorAt3=priorAt3, userPriorAt3=userPriorAt3)
}

extractPerformance = function(priorsPerfs) {
	list(
		priorsAt3=lapply(priorsPerfs, function(x) x$priorAt3),
		userPriorsAt3=lapply(priorsPerfs, function(x) x$userPriorAt3))
}

getBaseFrmSubset = function(baseFrm, gt, lt) {
	subset(baseFrm, userIdTagCount > gt & userIdTagCount < lt)
}

plotPriorsPerformance = function(baseFrm) {
	asFig("transPoint")
	points = list(c(0, 5), c(.5, 5.5), c(1, 6), c(1.5, 6.5), c(2, 7), c(2.5, 7.5), c(3, 8), c(3.5, 8.5), c(4, 9), c(4.5, 9.5), c(5, 10), c(5, 25), c(10, 30), c(15, 35), c(20, 40))
	perfs = lapply(points, function(pt) getPriorsPerformance(getBaseFrmSubset(baseFrm, pt[1], pt[2])))
	ys = extractPerformance(perfs)
	xs = as.numeric(lapply(points, function(point) point[1] + (point[2] - point[1]) / 2))
	errs = as.numeric(lapply(points, function(point) (point[2] - point[1]) / 2))
	delta = as.numeric(ys$priorsAt3) - as.numeric(ys$userPriorsAt3)
	main = "Performance delta between global and user prior vs user tag count"
	plotCI(xs, delta, errs, err="x", main=main, xlab="user tag count", ylab="performance difference when producing 3 tags per post")
	abline(h=0, lty=3)
	dev.off()
}

plotPairsCombined = function(baseFrm) {
	size = 10000
	figName="pairsCombined"
	png(str_c(PATH, "/Pictures/", figName, ".png"), width=480*10, height=480*10, res=72*10)
	#asFig(figName)
	pairs(targetP ~ combinedPrior + sjiTitle + sjiBody + offset, baseFrm[sample(nrow(baseFrm), size),], cex=.5)
	dev.off()	
}

subsetBaseFrm = function(baseFrm) {
	trials = sample(unique(baseFrm$tagFile), 1000)
	trials = originalTagFiles
	baseFrm = subset(baseFrm, tagFile %in% trials)
	baseFrmPresent = subset(baseFrm, targetP==1)
	subsetPresent = baseFrmPresent[sample(dim(baseFrmPresent)[1], 1000),]
	baseFrmAbsent = subset(baseFrm, targetP==0)
	baseFrmAbsent = baseFrmAbsent[sample(dim(baseFrmAbsent)[1], 40000),]
	subsetAbsent = baseFrmAbsent[sample(dim(baseFrmAbsent)[1], 800000, replace=T),]
#	return(rbind(subsetPresent, subsetAbsent))
	return(rbind(baseFrmPresent, baseFrmAbsent))
}

subsetBaseFrmNewRepd = function(baseFrm) {
	trials = originalTagFiles
	baseFrm = subset(baseFrm, tagFile %in% trials)
	baseFrmPresent = subset(baseFrm, targetP==1)
	baseFrmAbsent = subset(baseFrm, targetP==0)
	baseFrmAbsent = baseFrmAbsent[sample(dim(baseFrmAbsent)[1], 800000, replace=T),]
	return(rbind(baseFrmPresent, baseFrmAbsent))
}

modifyBaseFrm = function(baseFrm) {
	baseFrm$userIdPriorsP = as.numeric(baseFrm$userIdPriors != 0)
	baseFrm$userIdLogPriors = with(baseFrm, log(userIdPriors + 1))
	baseFrm$globalPriorProb = with(baseFrm, exp(prior)/( exp(prior) + 1))
	baseFrm$userPriorProb = with(baseFrm, userIdPriors/userIdTagCount)
	baseFrm$userPriorProb[is.nan(baseFrm$userPriorProb)] = 0
	baseFrm$userPrior = with(baseFrm, log(userPriorProb/(1-userPriorProb)))
	baseFrm$weights = rep(1, length(baseFrm$targetP))
	baseFrm = computeCombinedPrior(baseFrm, 10)
	popTotal = length(priorsIndeces) * length(unique(baseFrm$tagFile))
	#baseFrm$weights[baseFrm$targetP==0] = as.integer(ceiling(popTotal / sum(baseFrm$targetP==0)))
	baseFrm$weights[baseFrm$targetP==1] = .071
	baseFrm
}

