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
  sparseMatrix(i=sRes[,1], j=sRes[,2], x=sRes[,3]/sRes[,4],
               dimnames=names)
}

testCSV = read.csv(str_c(PATH, "/", "title-chunks.csv"), header=T, sep=",")
testCSV$LeftChunk = as.character(testCSV$LeftChunk)
testCSV$RightChunk = as.character(testCSV$RightChunk)

N = sparseMatrix(i=testCSV[[2]], j=testCSV[[4]], x=testCSV[[5]])

NRowSums = rowSums(N, sparseResult = TRUE)
NColSums = colSums(N, sparseResult = TRUE)
NSum = sum(N)
NProdSums = with(summary(N), sparseMatrix(i = i, j = j, x = rowSums(N)[i] * colSums(N)[j]))

sji = NSum * sdiv(N, NProdSums)


#print(testCSV)

