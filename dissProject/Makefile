
unit:
	Rscript -e 'source("preProcess.R")' -e 'myLoadImage()' -e 'test_dir(sprintf("%s/%s", PATH, "tests"))'

run%:
	Rscript -e 'source("preProcess.R")' -e 'run$*()'

createSOSmall:
	(cd /Volumes/SuperSupernova/stackExchange/ && rm stackOverflowSmall/* && cd stackOverflow2014 && find *.xml -print -exec bash -ic "(head '{}' && tail '{}') > ../stackOverflowSmall/{}" \;)

parrunPriors: NP = 4
parrunSjis: NP = 1
parrunTSjis: NP = 1
parrunTSjisAll: NP = 1
parrunContext: NP = 1
parrunGenAndSaveCurWorkspace: NP = 1

parrun%:
	make -s get$* | parallel -P ${NP} --progress make {}

runAll: unit parrunPriors parrunSjis parrunTSjis parrunContext
	exit 0

getPriors:
	grep '^run.* <- make' preProcess.R | awk '{print $$1}'

getSjis:
	egrep '^runGenNcoocTblSO1thru10+ <-' preProcess.R | awk '{print $$1}'

getTSjis:
	egrep '^runGenNcoocTblT.*10+ <-' preProcess.R | awk '{print $$1}'

getTSjisAll:
	egrep '^runGenNcoocTblT.* <-' preProcess.R | awk '{print $$1}'

getContext:
	egrep '^runContext[0-9]+.+<-' preProcess.R | awk '{print $$1}'

getGenAndSaveCurWorkspace:
	egrep '^runGenAndSaveCurWorkspace.*s[4-6] <-' preProcess.R | awk '{print $$1}'

checkPostgresTimes:
	sudo tail -n 10000 /opt/local/var/db/postgresql93/defaultdb/system.log | egrep '(statement:|duration:)'

showLatestUpdates:
	find dissertationData/ -type f -exec stat --format '%Y :%y %n' {} \; | sort -n | cut -d: -f2- | tail
