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
library(sqldf)

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

makeDb = function(frm) {
	frm = frm[!duplicated(frm$chunkHash),]
	db = frm$chunkHash
	names(db) = frm$chunk
	db
}

makeSparseTagVector = function(vals) {
	ret = B
	ret[priorsIndeces] = vals
	ret
}

# Calculate total activation, given base-level activation, sji associations, and context
act = function(context, B, sji) {
	weightsSubset = as.vector(contextWeights[context])
	#if( sum(weightsSubset) > 0) {
	#	weightsSubset = weightsSubset / sum(weightsSubset)
	#}
	#weightsSubset = rep(1/length(context), length(context))
	print(weightsSubset)
	print(context)
	sjiSubset = as.matrix(sji[context,priorsIndeces])
	if( length(context) > 1 ) {
		sjiSubset = as.vector(t(sjiSubset) %*% weightsSubset)
	} 
	if( length(context) == 1) {
		sjiSubset = as.vector(sjiSubset * weightsSubset)
	}
	if( length(context) == 0) {
		sjiSubset = rep(0, length(priorsIndeces))
	}
	sjiSubset = sjiSubset * W
	sjiSubset = makeSparseTagVector(sjiSubset)
	act = makeSparseTagVector(as.vector(B[priorsIndeces]) + as.vector(sjiSubset[priorsIndeces]))
	return(list(act=act, sji=sjiSubset))
}

W = 1
contextWeightsCSV = 'contextWeights.csv'
#priorsCSV = 'tag-priors-subset-4.csv'
#sjiCSV = 'title-chunks-subset-4.csv'
priorsCSV = 'tag-priors.csv'
sjiCSV = 'title-chunks.csv'

# Read in table of tag occurance counts; use this to build the base-level activation vector
# Calculates base levels by using the log odds ratio for each tag.
# Does not take into account the fact that tag likelihoods change over time.
colClasses=c("character", "integer", "integer", "character")
occurancesFrm = read.csv(str_c(PATH, "/", priorsCSV), header=T, sep=",", colClasses=colClasses)
priorsFrm = occurancesFrm[occurancesFrm$ChunkType == "tag",]
contextFrm = occurancesFrm[occurancesFrm$ChunkType == "title",]

# Read in table of context chunk -> tag chunk occurance counts
colClasses=c("character", "integer", "character", "integer", "integer")
chunkFrm = read.csv(str_c(PATH, "/", sjiCSV), header=T, sep=",", colClasses=colClasses)

# Read in table for attentional context weighting
colClasses=c("character", "character", "integer", "numeric", "numeric", "numeric")
contextWeightsFrm = read.csv(str_c(PATH, "/", contextWeightsCSV), header=T, sep=",", colClasses=colClasses)
contextWeightsFrm$logEntropy = 1 - contextWeightsFrm$H / max(contextWeightsFrm$H)
filteredFrm = contextWeightsFrm
filteredFrm = filteredFrm[filteredFrm$logEntropy > .2,]

# Perform any filtering on the context and priors frames
#observedTags = priorsFrm$Chunk
#priorsFrm = priorsFrm[priorsFrm$Chunk %in% observedTags,]
chunkFrm = chunkFrm[chunkFrm$LeftChunkHash %in% contextWeightsFrm$ChunkHash,]
contextFrm = contextFrm[contextFrm$ChunkHash %in% contextWeightsFrm$ChunkHash,]

# Generate context and prior indeces and counts
contextIndeces = with(contextFrm, ChunkHash)
priorsIndeces = with(priorsFrm, ChunkHash)
priors = with(priorsFrm, sparseVector(i=ChunkHash, x=ChunkCount, length=max(ChunkHash)))
context = with(contextFrm, sparseVector(i=ChunkHash, x=ChunkCount, length=max(ChunkHash)))

# Build a sparse vector of context attentional weighting
contextWeightsIndeces = with(contextWeightsFrm, ChunkHash)
contextWeights = with(contextWeightsFrm, sparseVector(i=ChunkHash, x=logEntropy, length=max(ChunkHash)))

# Convert tag occurance counts to base level activations, and place in a sparse vector
priorsP = priors/sum(priors)
priorsOdds = priorsP
priorsOdds[priorsIndeces] = priorsP[priorsIndeces] / (1 - priorsP[priorsIndeces])
B = priorsOdds
B[priorsIndeces] = log(as.vector(priorsOdds[priorsIndeces]))

# Construct the db of chunk (character) <-> chunkHash (integer) associations
# Use a named vector, so that chunk lookups by chunkHash are O(1), and
# chunkHash lookups by chunk are log(n) (when using the fmatch function)
leftFrm = with(chunkFrm, cbind(data.frame(chunk=LeftChunk), data.frame(chunkHash=LeftChunkHash)))
rightFrm = with(chunkFrm, cbind(data.frame(chunk=RightChunk), data.frame(chunkHash=RightChunkHash)))
dbContext = makeDb(leftFrm)
db = makeDb(rbind(leftFrm, rightFrm))

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
cAct = act(c(1:5), B, sji)
write.csv(data.frame(ChunkHash=priorsIndeces, Activation=as.vector(cAct$act[priorsIndeces])), file=str_c(PATH, "/", "Act.csv"))
