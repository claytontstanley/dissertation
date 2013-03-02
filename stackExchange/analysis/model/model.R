# --------------------------------------------------------------------------------------------------------
# ACT-R DM activation equations implemented in R

# sji is implemented as a sparse matrix 
# Context and possible retrieval chunks are on vertical and horizontal axes respectively
# Bi is a row vector

# Chunknames are hashed to a unique integer value for each chunk
# Hashing procedure is done outside of R (in PostgreSQL)
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
library(plyr)
library(data.table)

printP = 1
myPrint = function(str) {
	if( printP == 1) {
		print(str)
	}
}

# Interface to retrieve chunkHash for chunk name
getChunkHashes = function(chunks, db) {
	ret = db[fmatch(chunks, names(db))]
	ret = ret[!is.na(ret)]
#	stopifnot(length(ret) > 0)
	myPrint(str_c(length(chunks), "->", length(ret)))
	return(ret)
}

# And vice versa
getChunks = function(chunkHashes, db) {
	return(names(db[fmatch(chunkHashes, db)]))
}

makeDb = function(frm, valsAcc = "ChunkHash", namesAcc = "Chunk") {
	frm = frm[!duplicated(frm[[namesAcc]]),]
	db = frm[[valsAcc]]
	names(db) = frm[[namesAcc]]
	db
}

getLogOdds = function(priors) {
	priorsP = priors/sum(priors)
	priorsOdds = priorsP
	priorsOdds[priorsIndeces] = priorsP[priorsIndeces] / (1 - priorsP[priorsIndeces])
	B = priorsOdds
	B[priorsIndeces] = log(as.vector(priorsOdds[priorsIndeces]))
	B
}

getPriorsForPost = function(PostId) {
	userId = as.numeric(userIdForPostId[PostId])
	as.vector(priorsByUserId[userId,])
}

# Calculate total activation, given base-level activation, sji associations, and context
act = function(context, B, sji) {
	weights = contextWeights[context]
	print(sum(weights))
	if( sum(weights) > 0) {
		weights = weights / sum(weights)
	}
	weightsSubset = rep(0, dim(sji)[1])
	weightsSubset[context] = weights * table(context)[as.character(context)]
	myPrint(paste(getChunks(context, dbContext), weights))
	sjiSubset = weightsSubset %*% sji
	return(list(sji=sjiSubset))
}

contextWeightsCSV = 'contextWeights.csv'
priorsCSV = 'tag-priors-subset-4.csv'
sjiCSV = 'title-chunks-subset-4.csv'
priorsByUserIdCSV = 'tagPriorsByUser-subset-4.csv'
priorsCSV = 'tag-priors.csv'
sjiCSV = 'title-chunks.csv'
priorsByUserIdCSV = 'tagPriorsByUser.csv'

Rprof()

myPrint('
# Read in table of tag occurance counts; use this to build the base-level activation vector
# Calculates base levels by using the log odds ratio for each tag.
# Does not take into account the fact that tag likelihoods change over time.
')
colClasses=c("character", "integer", "integer", "character")
occurancesFrm = read.csv(str_c(PATH, "/", priorsCSV), header=T, sep=",", colClasses=colClasses)

myPrint('# Read in table of context chunk -> tag chunk occurance counts')
colClasses=c("character", "integer", "character", "integer", "integer")
chunkFrm = read.csv(str_c(PATH, "/", sjiCSV), header=T, sep=",", colClasses=colClasses)

myPrint('# Read in table for attentional context weighting')
colClasses=c("character", "character", "integer", "numeric", "numeric")
contextWeightsFrm = read.csv(str_c(PATH, "/", contextWeightsCSV), header=T, sep=",", colClasses=colClasses)
contextWeightsFrm$logEntropy = 1 - contextWeightsFrm$H / max(contextWeightsFrm$H)

myPrint('# Read in table for tag counts by userId')
colClasses=c("integer", "character", "integer", "integer", "character")
occurancesByUserIdFrm = read.csv(str_c(PATH, "/", priorsByUserIdCSV), header=T, sep=",", colClasses=colClasses)

myPrint('# Read in table for post id user id mapping')
colClasses=c("integer", "integer")
userIdForPostIdFrm = read.csv(str_c(PATH, "/../html-to-text/idUserId/idUserId.csv"), header=T, sep=",", colClasses=colClasses)

myPrint('# Collapse all context chunktypes on occurancesFrm')
occurancesFrm$ChunkType[occurancesFrm$ChunkType != 'tag'] = 'context'
occurancesTbl = data.table(occurancesFrm, key=c("Chunk", "ChunkHash", "ChunkType"))
occurancesTbl = occurancesTbl[, list(ChunkCount=sum(ChunkCount)), by=c("Chunk", "ChunkHash", "ChunkType")]
occurancesFrm = data.frame(occurancesTbl)
rm(occurancesTbl)

myPrint('# Perform any filtering on the context and priors frames')
chunkFrm = subset(chunkFrm, LeftChunkHash %in% contextWeightsFrm$ChunkHash)
occurancesFrm = subset(occurancesFrm, ChunkType == "tag" | ChunkHash %in% contextWeightsFrm$ChunkHash)
contextWeightsFrm = subset(contextWeightsFrm, ChunkHash %in% with(occurancesFrm, ChunkHash[ChunkType == "context"]))

myPrint('# Compress ChunkHash indeces in data frames')
tagSubsetIndeces = which(occurancesFrm$ChunkType == "tag")
contextSubsetIndeces = which(occurancesFrm$ChunkType == "context")
tagHashIndeces = occurancesFrm$ChunkHash[tagSubsetIndeces]
contextHashIndeces = occurancesFrm$ChunkHash[contextSubsetIndeces]
occurancesFrm$ChunkHash[tagSubsetIndeces] = fmatch(occurancesFrm$ChunkHash[tagSubsetIndeces], tagHashIndeces)
occurancesFrm$ChunkHash[contextSubsetIndeces] = fmatch(occurancesFrm$ChunkHash[contextSubsetIndeces], contextHashIndeces)
chunkFrm$LeftChunkHash = fmatch(chunkFrm$LeftChunkHash, contextHashIndeces)
chunkFrm$RightChunkHash = fmatch(chunkFrm$RightChunkHash, tagHashIndeces)
contextWeightsFrm$ChunkHash = fmatch(contextWeightsFrm$ChunkHash, contextHashIndeces)
occurancesByUserIdFrm$ChunkHash = fmatch(occurancesByUserIdFrm$ChunkHash, tagHashIndeces)
rm(tagSubsetIndeces)
rm(contextSubsetIndeces)

priorsFrm = occurancesFrm[occurancesFrm$ChunkType == "tag",]
contextFrm = occurancesFrm[occurancesFrm$ChunkType == "context",]

myPrint('# Generate context and prior indeces and counts')
contextIndeces = with(contextFrm, ChunkHash)
priorsIndeces = with(priorsFrm, ChunkHash)
priors = with(priorsFrm, as.vector(sparseVector(i=ChunkHash, x=ChunkCount, length=max(ChunkHash))))
context = with(contextFrm, as.vector(sparseVector(i=ChunkHash, x=ChunkCount, length=max(ChunkHash))))

myPrint('# Build a vector of context attentional weighting')
contextWeightsIndeces = with(contextWeightsFrm, ChunkHash)
contextWeights = with(contextWeightsFrm, as.vector(sparseVector(i=ChunkHash, x=logEntropy, length=max(ChunkHash))))
rm(contextWeightsFrm)

myPrint('# Convert tag occurance counts to base level activations, and place in a row vector')
B = getLogOdds(priors)

myPrint('
# Construct the db of chunk (character) <-> chunkHash (integer) associations
# Use a named vector, so that chunk lookups by chunkHash are O(log(n)), and
# chunkHash lookups by chunk are O(log(n)) (when using the fmatch function)
')
dbContext = makeDb(contextFrm)
dbPriors = makeDb(priorsFrm)
rm(contextFrm)
rm(priorsFrm)
rm(occurancesFrm)

myPrint('# Use occurance counts of context -> tags to build sji strength associations')
N = with(chunkFrm, sparseMatrix(i=LeftChunkHash, j=RightChunkHash, x=ChunkCount))
NRowSums = rowSums(N)
NColSums = colSums(N)
rm(chunkFrm)
NSum = sum(N)
sji = with(summary(N), sparseMatrix(i=i, j=j, x=log( (NSum * x) / (rowSums(N)[i] * colSums(N)[j]))))

myPrint('# build priorsByUserId and userIdForPostId')
priorsByUserId = with(occurancesByUserIdFrm, sparseMatrix(i=UserId, j=ChunkHash, x=ChunkCount))
userIdForPostId = with(userIdForPostIdFrm, sparseVector(i=id, x=userId, length=max(id)))
rm(userIdForPostIdFrm)
rm(occurancesByUserIdFrm)

myPrint('# Write relevant model component values to files, so that changes to the model can be regression tested (using git diff).')
write.csv(summary(sji), file=str_c(PATH, "/", "sji.csv"))
write.csv(data.frame(Chunk=getChunks(priorsIndeces, dbPriors), ChunkHash=priorsIndeces, B=as.vector(B[priorsIndeces])), file=str_c(PATH, "/", "B.csv"))
write.csv(data.frame(sort(dbPriors)), file=str_c(PATH, "/", "dbPriors.csv"))
write.csv(data.frame(sort(dbContext)), file=str_c(PATH, "/", "dbContext.csv"))
write.csv(data.frame(as.vector(userIdForPostId)), file=str_c(PATH, "/", "userIdForPostId.csv"))

Rprof(NULL)
print(summaryRprof())
