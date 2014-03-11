
unit:
	Rscript -e 'source("preProcess.R")' -e 'test_dir(sprintf("%s/%s", PATH, "tests"))'

run%:
	Rscript -e 'source("preProcess.R")' -e 'run$*()'

createSOSmall:
	(cd /Volumes/SuperSupernova/stackExchange/ && rm stackOverflowSmall/* && cd stackOverflow2014 && find *.xml -print -exec bash -ic "(head '{}' && tail '{}') > ../stackOverflowSmall/{}" \;)

runAll: unit
	make -s getSjis | parallel -P 1 --progress make {}
	make -s getPriors | parallel -P 4 --progress make {}

getPriors:
	grep '^run.* <- make' preProcess.R | awk '{print $$1}'

getSjis:
	egrep '^runGenNcoocTblSO1thru10+ <-' preProcess.R | awk '{print $$1}'

