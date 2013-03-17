# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Source the analyzer utility functions
source(str_c(PATH, "/analyzer.R"))

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

maxCount = 50
combinePs = function(psGlobal, psLocal, count) {
	count = pmin(count, maxCount)
	(psGlobal*(maxCount-count+1) + psLocal*count) / (maxCount + 1)
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

analyzeBaseFrmForMultivariate = function(baseFrm) {
	baseFrms = adjustFrms(list(baseFrm), coeffsGlobal, model=formula(targetP ~ prior + userIdLogPriors + sjiTitle + sjiBody + offset))
	baseFrms[2] = adjustFrms(list(baseFrms[[1]]$frm), coeffsGlobal, model=formula(targetP ~ prior + sjiTitle + sjiBody + offset))
	baseFrms[3] = adjustFrms(list(baseFrms[[1]]$frm), coeffsGlobal, model=formula(targetP ~ combinedPrior + sjiTitle + sjiBody + offset))
	#tempFrm = baseFrms[[1]]$frm
	#tempFrm = subset(tempFrm, userIdTagCount > 500)
	#tempFrm$userPrior[is.infinite(tempFrm$userPrior)] = -10000
	#baseFrms[4] = adjustFrms(list(tempFrm), coeffsGlobal, model=formula(targetP ~ prior + sjiTitle + sjiBody + offset))
	baseFrms
}

#dev.new()
#hist(subset(baseFrm, targetP==1)$act)
#?hist

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

convertToSummary = function(baseFrms) {
	lapply(baseFrms, function(baseFrm) {
		baseFrm$logit = summary(baseFrm$logit)
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

analyzeForMultivariate = function() {
	runSet(sets=18, id=2)
	runSet(sets=8, id=4)
	runSet(sets=8, id=7)
	originalBaseFrm = getFrms(8,3)[[1]]
	originalBaseFrm = modifyBaseFrm(originalBaseFrm)
	baseFrm = getFrms(18, 2)[[1]]
	baseFrm = modifyBaseFrm(baseFrm)
	baseFrmSubset = subsetBaseFrmNewRepd(baseFrm)
	baseFrmSubset = baseFrm
	baseFrms = analyzeBaseFrmForMultivariate(baseFrmSubset)
  	legendText = c("With user log priors added", "With global prior", "With combined prior") #, "With only user prior")
	makeMultivariateROC(baseFrms, "usersROC", legendText)
	makeMultivariateROC(subset(baseFrm, userIdTagCount > 100), "usersROC gt 100")
}

subsetBaseFrm = function(baseFrm) {
	trials = sample(unique(baseFrm$tagFile), 1000)
	trials = originalTagFiles
	baseFrm = subset(baseFrm, tagFile %in% trials)
	baseFrmPresent = subset(baseFrm, targetP==1)
	subsetPresent = baseFrmPresent[sample(dim(baseFrmPresent)[1], 1000),]
	baseFrmAbsent = subset(baseFrm, targetP==0)
	baseFrmAbsent = subset(originalBaseFrm, targetP==0)
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
	baseFrm$combinedPriorProb = with(baseFrm, combinePs(globalPriorProb, userPriorProb, userIdTagCount))
	baseFrm$combinedPrior = with(baseFrm, log(combinedPriorProb/(1-combinedPriorProb)))
	baseFrm$weights = rep(1, length(baseFrm$targetP))
	popTotal = length(priorsIndeces) * length(unique(baseFrm$tagFile))
	#baseFrm$weights[baseFrm$targetP==0] = as.integer(ceiling(popTotal / sum(baseFrm$targetP==0)))
	baseFrm$weights[baseFrm$targetP==1] = .071
	baseFrm
}




