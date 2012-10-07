# Setup variables and helper functions
wideScreen(150)
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

library(Matrix)
library(stringr)

sdiv <- function(X, Y, names=dimnames(X)) {
  sX <- summary(X)
  sY <- summary(Y)
  sRes <- merge(sX, sY, by=c("i", "j"))
  sparseMatrix(i=sRes[,1], j=sRes[,2], x=sRes[,3]/sRes[,4],dimnames=names)
}

hashOf = function(chunk, db) {
	db[chunk,]$chunkHash
}

getContext = function(chunks, db) {
	with(db[chunks,], chunkHash)
}



act = function(context, B, sji) {
	sji = sji[context,]
	sji = colMeans(sji, sparseResult=T)
	act = B + sji
	return(act)
}

colClasses=c("character", "integer", "character", "integer", "integer")
chunkFrm = read.csv(str_c(PATH, "/", "title-chunks.csv"), header=T, sep=",", colClasses=colClasses)
N = with(chunkFrm, sparseMatrix(i=LeftChunkHash, j=RightChunkHash, x=ChunkCount))

chunk = with(chunkFrm, rbind(data.frame(chunk=LeftChunk), data.frame(chunk=RightChunk)))
chunkHash = with(chunkFrm, rbind(data.frame(chunkHash=LeftChunkHash), data.frame(chunkHash=RightChunkHash)))
chunkHashFrame = data.frame(cbind(chunk, chunkHash))
db = unique(chunkHashFrame)
rownames(db) = with(db, chunk)

colClasses=c("character", "integer", "integer")
priorsFrm = read.csv(str_c(PATH, "/", "tag-priors.csv"), header=T, sep=",", colClasses=colClasses)
priors = with(priorsFrm, sparseVector(i=ChunkHash, x=ChunkCount, length=dim(N)[2]))
priorsIndeces = with(priorsFrm, ChunkHash)
priorsN = sum(priors)
priorsP = priors/priorsN
priorsLogs = priorsP
priorsLogs[priorsIndeces] = priorsP[priorsIndeces] / (1 - priorsP[priorsIndeces])
B = priorsLogs
B[priorsIndeces] = log(as.vector(priorsLogs[priorsIndeces]))

NRowSums = rowSums(N, sparseResult=TRUE)
NColSums = colSums(N, sparseResult=TRUE)
NSum = sum(N)
NProdSums = with(summary(N), sparseMatrix(i=i, j=j, x=rowSums(N)[i] * colSums(N)[j]))
NCellSums = NSum * sdiv(N, NProdSums)
sji = with(summary(NCellSums), sparseMatrix(i=i, j=j, x=log(x)))

write.csv(summary(sji), file=str_c(PATH, "/", "sji.csv"))
write.csv(summary(NProdSums), file=str_c(PATH, "/", "NProdSums.csv"))
write.csv(data.frame(ChunkHash=priorsIndeces, B=as.vector(B[priorsIndeces])), file=str_c(PATH, "/", "B.csv"))

tmpAct = act(c(1:5), B, sji)
write.csv(data.frame(ChunkHash=priorsIndeces, Activation=as.vector(tmpAct[priorsIndeces])), file=str_c(PATH, "/", "Act.csv"))

hist(as.vector(tmpAct[priorsIndeces]), breaks=50)
hist(as.vector(B[priorsIndeces]), breaks=50)
 
 
 
 
 
 

