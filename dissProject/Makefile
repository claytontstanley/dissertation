
unit:
	Rscript -e 'source("preProcess.R")' -e 'test_dir(sprintf("%s/%s", PATH, "tests"))'

run%:
	Rscript -e 'source("preProcess.R")' -e 'run$*()'

createSOSmall:
	(cd /Volumes/SuperSupernova/stackExchange/ && rm stackOverflowSmall/* && cd stackOverflow2014 && find *.xml -print -exec bash -ic "(head '{}' && tail '{}') > ../stackOverflowSmall/{}" \;)

parrunPriors: NP = 4
parrunSjis: NP = 1
parrunTSjis: NP = 1

parrun%:
	make -s get$* | parallel -P ${NP} --progress make {}

runAll: unit parrunPriors parrunSjis parrunTSjis
	exit 0

getPriors:
	grep '^run.* <- make' preProcess.R | awk '{print $$1}'

getSjis:
	egrep '^runGenNcoocTblSO1thru10+ <-' preProcess.R | awk '{print $$1}'

getTSjis:
	egrep '^runGenNcoocTblT.*10+ <-' preProcess.R | awk '{print $$1}'

