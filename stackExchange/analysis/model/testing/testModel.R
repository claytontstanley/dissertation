# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

writeStruct = function(struct, name=as.character(substitute(struct))) {
	if( mode(struct) == "list") {
		for( element in names(struct)) {
			writeStruct(struct[[element]], name=str_c(name, '-', element))
		}
	} else 	{
		csvName = str_c(name, ".csv")
		print(str_c("writing to csv: ", csvName))
		write.csv(data.frame(vect=as.vector(struct[priorsIndeces])), file=str_c(PATH, "/", csvName))
	}
}

res = list()

Rprof()

res$standard = act(getChunkHashes(c("i", "want", "to", "learn", "php", "codez"), dbContext), B, sji)
res$noContext = act(getChunkHashes(c(), dbContext), B, sji)
res$oneContext = act(getChunkHashes(c("the"), dbContext), B, sji)

Rprof(NULL)
print(summaryRprof())

writeStruct(res)

