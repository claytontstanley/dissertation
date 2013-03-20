# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Source the analyzer utility functions
source(str_c(PATH, "/analyzer.R"))

library(Hmisc)

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

analyzeForPresentation = function() {
	frms = getAllFrmsForModel(model=formula(targetP ~ prior + sjiTitle + sjiBody + offset), frms=8, runId=1)
	tagFiles = getTagFiles(makeTagDir(6))
	visPost(tagFiles[2], makeTagDir(6), frms[[1]]$coeffs)
	visPost(tagFiles[120], makeTagDir(6), frms[[1]]$coeffs)
	tagFiles = getTagFiles(makeTagDir(8))
	visPost(tagFiles[105], makeTagDir(8), frms[[1]]$coeffs)
	
	
	asFig("tagActDis")
	hist(as.vector(B[priorsIndeces]), main="Distribution of tag activations", xlab="Activation")
	devOff()
	
	asFig("tagActSorted")
	main="Activations of individual tags when sorted by activation"
	xlab="Sorted tag ID"
	ylab="Activation"
	plot(1:length(priorsIndeces), sort(as.vector(B[priorsIndeces]), decreasing=T), main=main, xlab=xlab, ylab=ylab)
	devOff()
	
	asFig("TagActZipf")
	main="Log-log plot of tag freqency vs tag rank"
	plot(log(1:length(priors)), log(sort(priors, decreasing=T)), main=main, xlab="Log tag rank", ylab="Log tag frequency")
	devOff()

	asFig("sjiActDis")
	hist(with(summary(sji), x), main="Distribution of sji associations", xlab="Activation")
	devOff()
	
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
	dist = rep(as.vector(contextWeights[contextWeightsIndeces]), as.vector(NRowSums[contextWeightsIndeces])) 
	hist(dist, main="Distribution of attentional weights", xlab="Weight")
	devOff()
	
	figName = "sjiActScatter"
	png(str_c(PATH, "/Pictures/", figName, ".png"), width=480*10, height=480*10, res=72*10)
	with(summary(sji), plot(i,j, main="Scatter of sji sparse matrix", xlab="i index", ylab="j index", cex=.2))	
	devOff()

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

analyzeForICCM = function() {
	runSet(sets=8, id=1)
	runSet(sets=9, id=1)
	prevFrm = getFrms(8,1)[[1]]
	runFromPrevious(prevFrm, 8, 2)
	frms = getAllFrms()
	frms = sortBaseFrms(frms)
	plotAllFrms(frms=frms)
	tagFiles=getTagFiles(makeTagDir(8))
	visPost(tagFiles[105], makeTagDir(8), coeffs=frms[[1]]$coeffs)
}

makeMultivariateROC = function(baseFrms, figName, legendText) {
	items = makeItems(baseFrms, legendText)
	plotROC2(items, figName)
	plotFrm(baseFrms[[1]]$frm, "LogReg-with-user-log-priors-added")
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

analyzeBaseFrmForMultivariate = function(baseFrm, maxCount) {
	baseFrms = analyzeBaseFrmPrior(baseFrm)
	baseFrms[2] = analyzeBaseFrmCombinedPrior(baseFrm, maxCount)
	baseFrms[3] = analyzeBaseFrmUserPrior(baseFrm)
	baseFrms
}

fooFun = function() {
	baseFrms = baseFrmsNoWeighting4
	baseFrms=assignCoeffs(baseFrms, getCoeffs(baseFrmsNoWeighting2.2))
	baseFrms=assignCoeffs(baseFrmsNoWeighting, coeffsGlobal)
	baseFrms=recomputeActivations(baseFrms)
	baseFrms = sortBaseFrms(baseFrms)
	cor(baseFrms[[2]]$frm$act, baseFrmsNoWeighting[[2]]$frm$act)
	mad(baseFrms[[2]]$frm$act, baseFrmsNoWeighting[[2]]$frm$act)
	cumsum(head(baseFrms[[2]]$frm, n=10000)$targetP)
	cumsum(head(baseFrmsNoWeighting[[2]]$frm, n=1000)$targetP)
	cumsum(head(baseFrmsWeighting[[2]]$frm, n=1000)$targetP)
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
	points = list(c(0, 5), c(.5, 5.5), c(1, 6), c(1.5, 6.5), c(2, 7), c(2.5, 7.5), c(3, 8), c(3.5, 8.5), c(4, 9), c(4.5, 9.5), c(5, 10), c(5, 25), c(10, 30), c(15, 35), c(20, 40))
	perfs = lapply(points, function(pt) getPriorsPerformance(getBaseFrmSubset(baseFrm, pt[1], pt[2])))
	ys = extractPerformance(perfs)
	xs = as.numeric(lapply(points, function(point) point[1] + (point[2] - point[1]) / 2))
	errs = as.numeric(lapply(points, function(point) (point[2] - point[1]) / 2))
	delta = as.numeric(ys$priorsAt3) - as.numeric(ys$userPriorsAt3)
	main = "Performance delta between global and user prior vs user tag count"
	plotCI(xs, delta, errs, err="x", main=main, xlab="user tag count", ylab="performance difference when producing 3 tags per post")
	abline(h=0, lty=3)
}

analyzeForMultivariate = function() {
	runSet(sets=18, id=2)
	runSet(sets=8, id=7)
	baseFrm = getFrms(18, 2)[[1]]
	baseFrm = modifyBaseFrm(baseFrm)
	plotPriorsPerformance(baseFrm)
	baseFrms = analyzeBaseFrmForMultivariate(baseFrm, maxCount=5)
	baseFrms10000 = baseFrms
	legendText = c("With global prior", "With combined prior", "With user prior")
 	makeMultivariateROC(baseFrms, "usersROC", legendText)
 	
 	tagFiles = getTagFiles(makeTagDir(19))
	visPost(tagFiles[3], makeTagDir(19), baseFrms[[2]]$coeffs)
	visPost(tagFiles[121], makeTagDir(19), baseFrms[[2]]$coeffs)
	visPost(tagFiles[105], makeTagDir(19), baseFrms[[2]]$coeffs)	
	
	#Ecdf(baseFrm$userIdTagCount, what='1-F')	
	baseFrm = getFrms(8, 1)[[1]]
	baseFrms = analyzeBaseFrmForMultivariate(baseFrm, maxCount=5)
	baseFrms1000 = baseFrms
	
	#baseFrms = baseFrms10000
	#baseFrms = assignCoeffs(baseFrms, getCoeffs(baseFrms1000))
	#baseFrms = recomputeActivations(baseFrms)
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




