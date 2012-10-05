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

colClasses=c("character", "integer", "character", "integer", "integer")
testCSV = read.csv(str_c(PATH, "/", "title-chunks.csv"), header=T, sep=",",colClasses=colClasses)

chunk = with(testCSV, rbind(data.frame(chunk=LeftChunk), data.frame(chunk=RightChunk)))
chunkHash = with(testCSV, rbind(data.frame(chunkHash=LeftChunkHash), data.frame(chunkHash=RightChunkHash)))
chunkHashFrame = data.frame(cbind(chunk, chunkHash))
db = unique(chunkHashFrame)

N = with(testCSV, sparseMatrix(i=LeftChunkHash, j=RightChunkHash, x=ChunkCount))

NRowSums = rowSums(N, sparseResult=TRUE)
NColSums = colSums(N, sparseResult=TRUE)
NSum = sum(N)
NProdSums = with(summary(N), sparseMatrix(i=i, j=j, x=rowSums(N)[i] * colSums(N)[j]))

sji = NSum * sdiv(N, NProdSums)

write.csv(summary(sji), file=str_c(PATH, "/", "sji.csv"))
write.csv(summary(NProdSums), file=str_c(PATH, "/", "NProdSums.csv"))


