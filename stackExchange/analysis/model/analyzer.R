# Figure out the path to this file
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

# Load up the libraries
library(stringr)
library(popbio)

res = read.csv(str_c(PATH, "/LogReg-subset-2.csv"))

logi.hist.plot(res$act, res$targetP, boxp=F, type="hist", col="gray")
