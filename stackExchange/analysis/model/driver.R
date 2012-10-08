# Setup variables and helper functions
wideScreen(150)
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

library(stringr)

source(str_c(PATH, "/model.R"))

cAct = act(getChunkHashes(c("lisp", "git"), db), B, sji)
plotHighest(priorsIndeces, cAct$act, db)
plotHighest(priorsIndeces, cAct$sji, db)

cAct = act(c(1:5), B, sji)
write.csv(data.frame(ChunkHash=priorsIndeces, Activation=as.vector(cAct$act[priorsIndeces])), file=str_c(PATH, "/", "Act.csv"))


# Save current objects so that they can be referenced from LaTeX document
save.image(file = str_c(PATH, "/", ".RData"))
