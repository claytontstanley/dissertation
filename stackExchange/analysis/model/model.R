# Setup variables and helper functions
wideScreen(150)
frameFiles = lapply(sys.frames(), function(x) x$ofile)
frameFiles = Filter(Negate(is.null), frameFiles)
PATH = dirname(frameFiles[[length(frameFiles)]])

library(Matrix)
library(stringr)


testCSV = read.csv(str_c(PATH, "/", "title-chunks.csv"), header=T, sep=",")

print(testCSV)

N = sparseMatrix(i=testCSV[[2]], j=testCSV[[4]])

#print(testCSV)

