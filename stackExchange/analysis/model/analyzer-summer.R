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
		
	legendText = c("1k posts 400 sampled per post", "10k/40", "10k/400 weights", "10k/400 weights combined", "10k/40 combined", "1k/400 combined", "1k/400 user", "10k/400 user weights")
	lambdaFun = function(lst) lapply(lst, function(baseFrm) list(frm=baseFrm))
	baseFrmsFull = assignCoeffs(lambdaFun(list(baseFrmFull, baseFrm10kFull, baseFrm10kFull, baseFrm10kFull, baseFrm10kFull, baseFrmFull, baseFrmFull, baseFrm10kFull)), getCoeffs(baseFrms))
	baseFrmsFull = recomputeActivations(baseFrmsFull)
	items = makeItems(baseFrmsFull, legendText)
	plotROC2(items[c(1,6,7)])
	
	
	getCoeffs(baseFrms)
	getCoeffs(baseFrms[c(1,9,10)])
	getClassLog(baseFrms[c(1,9,10)])
	getCoeffs(baseFrms[c(2, 3, 8)])
	getCoeffs(baseFrms[c(1,6,7)])
	getClassLog(baseFrms[c(1,6,7)])
	getAccuracyAtNAverageTags(baseFrmsFull[[6]]$frm)
	
 	tagFiles=getTagFiles(makeTagDir(8))
	visPost(tagFiles[115], makeTagDir(8), coeffs=baseFrms[[6]]$coeffs, topNum=20)
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

updateAllBaseFrm = function(baseFrm, samplesPerPost=F, replace=F, taggedWeight=1, sampledWeight=1, sortedWeight=1, maxCount=5) {
	if (samplesPerPost != F) {
		baseFrm = sampleBaseFrm(baseFrm, samplesPerPost, replace=replace)
	}
	baseFrm = addWeightsBaseFrm(baseFrm, taggedWeight=taggedWeight, sampledWeight=sampledWeight, sortedWeight=sortedWeight)
	baseFrm = modifyBaseFrm(baseFrm, maxCount=maxCount)
}