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

logReg = function(tag) {
	print(str_c("working tag ", tag))
	dfSubset = res[res$tag %in% tag,]
	#dev.new()
	#logi.hist.plot(dfSubset$act, dfSubset$targetP, boxp=T, type="hist", col="gray")
	#title(main=tag)
	mylogit = glm(targetP ~ act, data=dfSubset, family="binomial")
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
	Ns = c(length(dfSubset[dfSubset$targetP == 1,]$targetP), length(dfSubset[dfSubset$targetP == 0,]$targetP), dim(dfSubset)[1])
	names(Ns) = c('NTagged', 'NNotTagged', 'N')
	p = summary(mylogit)$coefficients["act",'Pr(>|z|)']
	return(with(tmp, data.frame(mcFadden=mcFadden,overall=overall, tag=tag, t(vals), t(Ns), p=p)))
}

sjiRank = function(row, sji) {
	temp = as.numeric(sji[row,priorsIndeces])
	temp = temp[temp != 0]
	return(data.frame(chunkHash=row, chunk=getChunks(row, db), sd=sd(temp), MADZero=mean(abs(temp)), N=length(temp)))
}

res = read.csv(str_c(PATH, "/LogReg.csv"))
#res = read.csv(str_c(PATH, "/LogReg-5-4.csv"))
tags = sqldf('select tag, count(tag) as count from res group by tag order by count desc')

dev.new()
logi.hist.plot(res$rank, res$targetP, boxp=F, type="hist", col="gray")
#logReg(tags$tag)

break

leftChunkHashes=unique(chunkFrm$LeftChunkHash)
sjiRankRes = do.call(rbind, multicore::mclapply(leftChunkHashes, function(hash) sjiRank(hash, sji)))

lst = as.list(c(1:100))
lst = lapply(lst, function(row) tags[row,]$tag)
dFrame = do.call(rbind, multicore::mclapply(lst, logReg))



