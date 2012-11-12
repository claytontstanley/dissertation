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
	return(coeffs["sji"]/coeffs["prior"])
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

res = read.csv(str_c(PATH, "/LogReg.csv"))
#res = read.csv(str_c(PATH, "/LogReg-6-4-2.csv"))
#tags = sqldf('select tag, count(tag) as count from res group by tag order by count desc')


logRegRes = logReg(res$tag, res)

W = logRegRes
res$act = res$sji * logRegRes + res$prior

mylogit2 = glm(targetP ~ act, data=res, family="binomial")



descripts = c()
descripts["sjiCells"] = nnzero(sji)
descripts["sjiObservations"] = sum(sji)
descripts["sjiDensity"] = nnzero(sji) / prod(dim(sji))
descripts["tagCells"] = nnzero(priors)
descripts["tagObservations"] = sum(priors)
descripts["tagDensity"] = nnzero(priors) / length(priors)
descriptsFrm = data.frame(descripts)

dev.new()
hist(as.vector(B[priorsIndeces]), main="Distribution of tag activations", xlab="Activation")

dev.new()
main="Activations of individual tags when sorted by activation"
xlab="Sorted tag ID"
ylab="Activation"
plot(1:length(priorsIndeces), sort(as.vector(B[priorsIndeces]), decreasing=T), main=main, xlab=xlab, ylab=ylab)

dev.new()
hist(with(summary(sji), x), main="Distribution of sji associations", xlab="Activation")

dev.new()
with(summary(sji), plot(i,j, main="Scatter of sji sparse matrix", xlab="i index", ylab="j index", cex=.2))

lapply(getChunkHashes(c("php", "the", "?", "lisp"), db), contextWeight)

dev.new()
hist(as.vector(contextWeights[contextWeightsIndeces]), main="Distribution of attentional weights", xlab="Weight")

dev.new()
main="Attentional weights as a function of #observations for each cue"
xlab="log #observations for cue"
ylab="Attentional weight"
plot(log(as.vector(NRowSums[contextWeightsIndeces])), contextWeights[contextWeightsIndeces], main=main, xlab=xlab, ylab=ylab)

visPost(tagFiles[2])
visPost(tagFiles[40])
visPost(tagFiles[120])

dev.new()
logi.hist.plot(res$act, res$targetP, boxp=T, type="hist", col="gray")




# Save current objects so that they can be referenced from LaTeX document
save.image(file = str_c(PATH, "/", ".RData"))





