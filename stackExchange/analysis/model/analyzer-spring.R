# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Source the analyzer utility functions
source(str_c(PATH, "/analyzer.R"))

analyzeForPresentation = function() {
	frms = getAllFrmsForModel(model=formula(targetP ~ prior + sjiTitle + sjiBody + offset), frms=8, runId=1)
	tagFiles = getTagFiles(makeTagDir(8))	
	visPost(tagFiles[2], makeTagDir(8), frms[[1]]$coeffs)
	
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
	
	#figName = "sjiActScatter"
	#png(str_c(PATH, "/Pictures/", figName, ".png"), width=480*10, height=480*10, res=72*10)
	#with(summary(sji), plot(i,j, main="Scatter of sji sparse matrix", xlab="i index", ylab="j index", cex=.2))	
	#devOff()

}

maxCount = 50
combinePs = function(psGlobal, psLocal, count) {
	count = pmin(count, maxCount)
	(psGlobal*(maxCount-count+1) + psLocal*count) / (maxCount + 1)
}

analyzeForICCM = function(frms=getAllFrms()) {
	plotAllFrms(frms=frms)
	tagFiles=getTagFiles(makeTagDir(8))
	visPost(tagFiles[105], makeTagDir(8), coeffs=frms[[1]]$coeffs)
}

makeMultivariateROC = function(baseFrm, figName) {
	baseFrms = adjustFrms(list(baseFrm), coeffsGlobal, model=formula(targetP ~ prior + userIdLogPriors + sjiTitle + sjiBody + offset))
	baseFrms[2] = getAllFrmsForModel(model=formula(targetP ~ prior + sjiTitle + sjiBody + offset), frms=8, runId=1)
	baseFrms[3] = adjustFrms(list(baseFrms[[1]]$frm), coeffsGlobal, model=formula(targetP ~ combinedPrior + sjiTitle + sjiBody + offset))
	#tempFrm = baseFrms[[1]]$frm
	#tempFrm = subset(tempFrm, userIdTagCount > 500)
	#tempFrm$userPrior[is.infinite(tempFrm$userPrior)] = -10000
	#baseFrms[4] = adjustFrms(list(tempFrm), coeffsGlobal, model=formula(targetP ~ prior + sjiTitle + sjiBody + offset))
	legendText = c("With user log priors added", "With global prior", "With combined prior", "With only user prior")
	items = makeItems(baseFrms, legendText)
	plotROC2(items, figName)
}

analyzeForMultivariate = function() {
	runSet(sets=8, id=3)
	baseFrm = getFrms(8, 3)[[1]]
	baseFrm = modifyBaseFrm(baseFrm)
	makeMultivariateROC(baseFrm, "usersROC")
	makeMultivariateROC(subset(baseFrm, userIdTagCount > 100), "usersROC gt 100")
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
	baseFrm
}




