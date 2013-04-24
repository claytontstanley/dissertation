# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Source the analyzer utility functions
source(str_c(PATH, "/analyzer.R"))

#FIXME: This library is probably needed, but won't currently load on R 3.0
#library(Hmisc)

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

analyzeForICCM = function() {
#	runSet(sets=8, id=1)
#	runSet(sets=9, id=1)
#	prevFrm = getFrms(8,1)[[1]]
#	runFromPrevious(prevFrm, 8, 2)
	frms = getAllFrms()
	frms = sortBaseFrms(frms)
	plotAllFrms(frms=frms)
	tagFiles=getTagFiles(makeTagDir(8))
	visPost(tagFiles[105], makeTagDir(8), coeffs=frms[[1]]$coeffs, topNum=12)
}

analyzeBaseFrmForMultivariate = function(baseFrm, maxCount) {
	baseFrms = adjustFrms(list(baseFrm), coeffsGlobal, model=formula(targetP ~ prior + sjiTitle + offset))
	baseFrms[2] = analyzeBaseFrmPrior(baseFrm)
	baseFrms[3] = analyzeBaseFrmUserPrior(baseFrm)
	baseFrms[4] = analyzeBaseFrmCombinedPrior(baseFrm, maxCount)
	baseFrms
}

analyzeForMultivariate = function() {
	#runSet(sets=18, id=2)
	#runSet(sets=8, id=7)
	baseFrm = getFrms(18, 2)[[1]]
	baseFrm = modifyBaseFrm(baseFrm)
	plotPriorsPerformance(baseFrm)
	baseFrms = analyzeBaseFrmForMultivariate(baseFrm, maxCount=5)
	legendText = c("Without body words", "With global prior", "With user prior", "With combined prior")
 	makeMultivariateROC(baseFrms, "usersROC", legendText)
 	
 	tagFiles=getTagFiles(makeTagDir(8))
	visPost(tagFiles[105], makeTagDir(8), coeffs=baseFrms[[4]]$coeffs, topNum=20)
	
	plotAllFrms()
	plotPairsCombined(baseFrms[[1]]$frm)

	
	#Ecdf(baseFrm$userIdTagCount, what='1-F')	
	#baseFrm = getFrms(8, 1)[[1]]
	#baseFrms = analyzeBaseFrmForMultivariate(baseFrm, maxCount=5)
	#baseFrms1000 = baseFrms
	#baseFrms = baseFrms10000
	#baseFrms = assignCoeffs(baseFrms, getCoeffs(baseFrms1000))
	#baseFrms = recomputeActivations(baseFrms)
}

makeMultivariateROC = function(baseFrms, figName, legendText) {
	items = makeItems(baseFrms, legendText)
	plotROC2(items, figName)
	plotFrm(baseFrms[[1]]$frm, "LogReg-with-user-log-priors-added")
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

