# --------------------------------------------------------------------------------------------------------
# ACT-R DM activation equations implemented in R

# sji is implemented as a sparse array
# Context and possible retrieval chunks are on vertical and horizontal axes respectively
# Bi is a sparse row vector

# Chunknames are hashed to a unique integer value for each chunk
# Hashing procedure is done outside of R (in MySQL)
# Chunks are are already hashed when read in
# This allows the sji and Bi structures to be indexed by integer hash (O(1) lookup times as a consequence)
# ChunkHash <-> Chunk mappings are maintained in the db hash table
# --------------------------------------------------------------------------------------------------------

# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Load up the libraries
library(Matrix)
library(stringr)
library(fastmatch)

# And helper functions
sdiv <- function(X, Y, names=dimnames(X)) {
  sX <- summary(X)
  sY <- summary(Y)
  sRes <- merge(sX, sY, by=c("i", "j"))
  sparseMatrix(i=sRes[,1], j=sRes[,2], x=sRes[,3]/sRes[,4],dimnames=names)
}

# Interface to retrieve chunkHash for chunk name
getChunkHashes = function(chunks, db) {
	ret = db[chunks]
	ret = ret[!is.na(ret)]
#	stopifnot(length(ret) > 0)
	print(str_c(length(chunks), "->", length(ret)))
	return(ret)
}

# And vice versa
getChunks = function(chunkHashes, db) {
	return(names(db[fmatch(chunkHashes, db)]))
}

# Calculate total activation, given base-level activation, sji associations, and context
act = function(context, B, sji) {
	sjiSubset = sji[context,] * W
	if( length(context) > 1 ) sjiSubset = colMeans(sjiSubset, sparseResult=T)
	if( length(context) == 0) {
		sjiSubset = B
		sjiSubset[priorsIndeces] = 0
	}
	act = B + sjiSubset
	return(list(act=act, sji=sjiSubset))
}

W = 1
priorsCSV = 'tag-priors-subset-4.csv'
sjiCSV = 'title-chunks-subset-4.csv'
#priorsCSV = 'tag-priors.csv'
#sjiCSV = 'title-chunks.csv'

#logRegResultCSV = "/LogReg-subset-2.csv"
#sjiRankResultCSV = "/sjiRank-subset-2.csv"

# Read in table of tag occurance counts; use this to build the base-level activation vector
# Calculates base levels by using the log odds ratio for each tag.
# Does not take into account the fact that tag likelihoods change over time.
colClasses=c("character", "integer", "integer")
priorsFrm = read.csv(str_c(PATH, "/", priorsCSV), header=T, sep=",", colClasses=colClasses)

# Read in table of context chunk -> tag chunk occurance counts
colClasses=c("character", "integer", "character", "integer", "integer")
chunkFrm = read.csv(str_c(PATH, "/", sjiCSV), header=T, sep=",", colClasses=colClasses)

# Perform any filtering on the context and priors frames
#chunkFrm = chunkFrm[chunkFrm$ChunkCount > 1000,]
#chunkFrm = chunkFrm[chunkFrm$RightChunk %in% observedTags,]
#observedTags = unique(chunkFrm$RightChunk)
#logRegRes = read.csv(str_c(PATH, "/", logRegResultCSV))
#observedTags = unique(logRegRes$tag)
#observedTags = priorsFrm$Chunk[1:2000]
#observedTags = priorsFrm$Chunk
#priorsFrm = priorsFrm[priorsFrm$Chunk %in% observedTags,]

# Convert tag occurance counts to base level activations, and place in a sparse vector
priors = with(priorsFrm, sparseVector(i=ChunkHash, x=ChunkCount, length=max(ChunkHash)))
priorsIndeces = with(priorsFrm, ChunkHash)
priorsP = priors/sum(priors)
priorsLogs = priorsP
priorsLogs[priorsIndeces] = priorsP[priorsIndeces] / (1 - priorsP[priorsIndeces])
B = priorsLogs
B[priorsIndeces] = log(as.vector(priorsLogs[priorsIndeces]))

# Construct the db of chunk (character) <-> chunkHash (integer) associations
# Use a named vector, so that chunk lookups by chunkHash are O(1), and
# chunkHash lookups by chunk are log(n) (when using the fmatch function)
chunk = with(chunkFrm, rbind(data.frame(chunk=LeftChunk), data.frame(chunk=RightChunk)))
chunkHash = with(chunkFrm, rbind(data.frame(chunkHash=LeftChunkHash), data.frame(chunkHash=RightChunkHash)))
chunkHashFrame = data.frame(cbind(chunk, chunkHash))
chunkHashFrame = chunkHashFrame[!duplicated(chunkHashFrame$chunkHash),]
db = chunkHashFrame$chunkHash
names(db) = chunkHashFrame$chunk

# Use occurance counts of context -> tags to build sji strength associations
N = with(chunkFrm, sparseMatrix(i=LeftChunkHash, j=RightChunkHash, x=ChunkCount))
NRowSums = rowSums(N, sparseResult=TRUE)
NColSums = colSums(N, sparseResult=TRUE)
NSum = sum(N)
NProdSums = with(summary(N), sparseMatrix(i=i, j=j, x=rowSums(N)[i] * colSums(N)[j]))
NCellSums = NSum * sdiv(N, NProdSums)
sji = with(summary(NCellSums), sparseMatrix(i=i, j=j, x=log(x)))

# Write relevant model component values to files, so that changes to the model can be regression tested (using git diff).
write.csv(summary(sji), file=str_c(PATH, "/", "sji.csv"))
write.csv(summary(NProdSums), file=str_c(PATH, "/", "NProdSums.csv"))
write.csv(data.frame(ChunkHash=priorsIndeces, B=as.vector(B[priorsIndeces])), file=str_c(PATH, "/", "B.csv"))
