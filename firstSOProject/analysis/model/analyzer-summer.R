# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Source the analyzer utility functions
source(str_c(PATH, "/analyzer.R"))

addTagLabel = function() {
	runSet(8,8)
	runSet(18,3)
	#runSet(20,1)
}

analyzeForSummer = function() {
	samplesPerPost = 5
	baseFrmWeight = 400/5
	baseFrm10kWeight = 40/5
	baseFrmFull = updateAllBaseFrm(getFrms(8, 8)[[1]])
	baseFrm10kFull = updateAllBaseFrm(getFrms(18,3)[[1]])
	baseFrm = updateAllBaseFrm(baseFrmFull, samplesPerPost=samplesPerPost)
	baseFrm10k = updateAllBaseFrm(baseFrm10kFull, samplesPerPost=samplesPerPost)
	baseFrms = analyzeBaseFrmPrior(updateAllBaseFrm(baseFrm, sampledWeight=baseFrmWeight))
	baseFrms[2] = analyzeBaseFrmPrior(updateAllBaseFrm(baseFrm10k, sampledWeight=baseFrm10kWeight))
	baseFrms[3] = analyzeBaseFrmPrior(updateAllBaseFrm(baseFrm10k, sampledWeight=baseFrm10kWeight * 10))
	baseFrms[4] = analyzeBaseFrmCombinedPrior(updateAllBaseFrm(baseFrm10k, sampledWeight=baseFrm10kWeight * 10))
	baseFrms[5] = analyzeBaseFrmCombinedPrior(updateAllBaseFrm(baseFrm10k, sampledWeight=baseFrm10kWeight))
	baseFrms[6] = analyzeBaseFrmCombinedPrior(updateAllBaseFrm(baseFrm, sampledWeight=baseFrmWeight))
	baseFrms[7] = analyzeBaseFrmUserPrior(updateAllBaseFrm(baseFrm, sampledWeight=baseFrmWeight))
	baseFrms[8] = analyzeBaseFrmUserPrior(updateAllBaseFrm(baseFrm10k, sampledWeight=baseFrm10kWeight * 10))
	
	lambdaFun = function(psGlobal, psLocal, count) combinePsByExp(psGlobal, psLocal, count, 0.002626)
	baseFrms[9] = analyzeBaseFrmCombinedPrior(updateAllBaseFrm(baseFrm, sampledWeight=baseFrmWeight, combinePsFun=lambdaFun))
		
	legendText = c("1k posts 400 sampled per post", "10k/40", "10k/400 weights", "10k/400 weights combined", "10k/40 combined", "1k/400 combined", "1k/400 user", "10k/400 user weights", "1k/400 combined exp")
	lambdaFun = function(lst) lapply(lst, function(baseFrm) list(frm=baseFrm))
	baseFrmsFull = assignCoeffs(lambdaFun(list(baseFrmFull, baseFrm10kFull, baseFrm10kFull, baseFrm10kFull, baseFrm10kFull, baseFrmFull, baseFrmFull, baseFrm10kFull, baseFrmFull)), getCoeffs(baseFrms))
	baseFrmsFull = recomputeActivations(baseFrmsFull)
	items = makeItems(baseFrmsFull, legendText)
	plotROC2(items[c(1,6,7,9)])
	
	
	getCoeffs(baseFrms)
	getCoeffs(baseFrms[c(1,9,10)])
	getClassLog(baseFrms[c(1,9,10)])
	getCoeffs(baseFrms[c(2, 3, 8)])
	getCoeffs(baseFrms[c(1,6,7)])
	getClassLog(baseFrms[c(1,6,7)])
	getAccuracyAtNAverageTags(baseFrms[[9]]$frm)
	
 	tagFiles=getTagFiles(makeTagDir(8))
	visPost(tagFiles[115], makeTagDir(8), coeffs=baseFrms[[6]]$coeffs, topNum=20)
}

computeAt = function(prob) {
	function (psGlobal, psLocal, count) {
		psGlobal*prob + (1-prob)*psLocal
	}
}

getProbForRange = function(baseFrm10k, minCount, maxCount) {
	baseFrmSubset = subsetByTagCount(baseFrm10k, minCount, maxCount)
	dim(baseFrmSubset)
	lambdaFun = function(prob) analyzeBaseFrmCombinedPrior(updateAllBaseFrm(baseFrmSubset, sampledWeight=baseFrm10kWeight * 10, combinePsFun=computeAt(prob)))
	baseFrmsSubset = lapply(list(.001, .01, .1, .3, .5, .7, .9, .99, .999), lambdaFun)
	accuracyFun = function(baseFrmSubset) getAccuracyAtNAverageTags(baseFrmSubset$frm, N=3)
	lapply(baseFrmsSubset, accuracyFun)
}

getProbsForAllRanges = function(ranges) {
	lambdaFun = function(range) getProbForRange(baseFrm10k, range[1], range[2])
	lapply(ranges, lambdaFun)
}

getMeanTagCountForRange = function(baseFrm10k, minCount, maxCount) {
	baseFrmSubset = subsetByTagCount(baseFrm10k, minCount, maxCount)
	baseFrmSubset = subset(baseFrmSubset, !duplicated(tagFile))
	mean(baseFrmSubset$userIdTagCount)
}

getMeanTagCountForAllRanges = function(ranges) {
	lambdaFun = function(range) getMeanTagCountForRange(baseFrm10k, range[1], range[2])
	lapply(ranges, lambdaFun)
}
	
tempFun = function() {
	ranges = list(c(0, 0), c(1, 7), c(3, 7), c(7, 14), c(14, 25), c(25, 40), c(40, 65), c(65, 100), c(110, 200), c(200, 450), c(450, 500000))
	accRanges2 = getProbsForAllRanges(ranges)
	x = unlist(getMeanTagCountForAllRanges(ranges))
	ys
	plotProbsForAllRanges(accRanges2)
	y = c(1, .9, .9, .8, .7, .7, .7, .7, .6, .6, .3)
	tempFrm = data.frame(x, y)
	mod = nls(y ~ exp(-b * x), data=tempFrm, start=list(b=0))
	plot(x, y, xlim=c(0, 900), ylim=c(0,1))
	lines(x, predict(mod, list(x=x)))
	summary(mod)
}	
	
plotProbsForAllRanges = function(accRanges) {
	dev.new()
	count = 0
	colors = c("red", "green", "blue", "orange", "yellow", "black", "grey", "purple", "magenta", "cyan", "pink")
	xs = c(.001, .01, .1, .3, .5, .7, .9, .99, .999)
	for (accRange in accRanges) {
		count = count + 1
		if (count==1) {
		    plot(xs, unlist(accRange), typ="l", col=colors[count], xlim=c(0,1), ylim=c(.4,.6))
		} else {
			lines(xs, unlist(accRange), typ="l", col=colors[count])
		}
	}
}

subsetByTagCount = function(baseFrm, minCount, maxCount) {
	subset(baseFrm, userIdTagCount >= minCount & userIdTagCount <= maxCount)
}

addWeightsBaseFrm = function(baseFrm, taggedWeight=1, sampledWeight=1, sortedWeight=1) {
	weights = c(taggedWeight, sortedWeight, sampledWeight)
	names(weights) = c("tagged", "sorted", "sampled")
	baseFrm$weights = weights[as.character(baseFrm$type)]
	baseFrm
}

sampleBaseFrm = function(baseFrm, samplesPerPost, replace=F) {
	sampledFrm = subset(baseFrm, type=='sampled')
	sampledFrm = sampledFrm[sample(1:nrow(sampledFrm), samplesPerPost*length(unique(sampledFrm$tagFile)), replace=replace),]
	rbind(subset(baseFrm, type=='sorted'), subset(baseFrm, type=='tagged'), sampledFrm)
}

updateAllBaseFrm = function(baseFrm, samplesPerPost=F, replace=F, taggedWeight=1, sampledWeight=1, 
							sortedWeight=1, combinePsFun=function(psGlobal, psLocal, count) combinePs(psGlobal, psLocal, count, 5)) {
	if (samplesPerPost != F) {
		baseFrm = sampleBaseFrm(baseFrm, samplesPerPost, replace=replace)
	}
	baseFrm = addWeightsBaseFrm(baseFrm, taggedWeight=taggedWeight, sampledWeight=sampledWeight, sortedWeight=sortedWeight)
	baseFrm = modifyBaseFrm(baseFrm, combinePsFun=combinePsFun)
}