
rFile = preProcess.R

unit:
	Rscript -e 'source("${rFile}")' -e 'myLoadImage()' -e 'test_dir(sprintf("%s/%s", PATH, "tests"))'

run%:
	Rscript -e 'source("${rFile}")' -e 'run$*()'

createSOSmall:
	(cd /Volumes/SuperSupernova/stackExchange/ && rm stackOverflowSmall/* && cd stackOverflow2014 && find *.xml -print -exec bash -ic "(head '{}' && tail '{}') > ../stackOverflowSmall/{}" \;)

parrunPriors: NP = 4
parrunSjis: NP = 1
parrunTSjis: NP = 1
parrunTSjisAll: NP = 1
parrunContext: NP = 1
parrunContextS1: NP = 1
parrunContextS4: NP = 2 
parrunContextS5: NP = 2
parrunContextS6: NP = 4
parrunGenAndSaveCurWorkspace: NP = 1

parrun%:
	make -s get$* | parallel -P ${NP} --progress make {}

parrunContextAll: parrunContextS1 parrunContextS4 parrunContextS5 parrunContextS6
	exit 0

runAll: unit parrunPriors parrunSjis parrunTSjis parrunContext
	exit 0

getPriors:
	grep '^run.* <- make' ${rFile} | awk '{print $$1}'

getSjis:
	egrep '^runGenNcoocTblSO1thru10+ <-' ${rFile} | awk '{print $$1}'

getTSjis:
	egrep '^runGenNcoocTblT.*10+ <-' ${rFile} | awk '{print $$1}'

getTSjisAll:
	egrep '^runGenNcoocTblT.* <-' ${rFile} | awk '{print $$1}'

# FIXME: Only running s1's for now
getContext: getContextS1

getContextS%:
	egrep '^runContext[0-9]+.*s$*.*<-' ${rFile} | awk '{print $$1}'

# FIXME: Only running s[4-6]'s for now
getGenAndSaveCurWorkspace:
	egrep '^runGenAndSaveCurWorkspace.*s[4-6] <-' ${rFile} | awk '{print $$1}'

checkPostgresTimes:
	sudo tail -n 10000 /opt/local/var/db/postgresql93/defaultdb/system.log | egrep '(statement:|duration:)'

showLatestUpdates:
	find dissertationData/ -type f -exec stat --format '%Y :%y %n' {} \; | sort -n | cut -d: -f2- | tail
