
rFile = preProcess.R

unit:
	Rscript -e 'source("${rFile}")' -e 'myLoadImage(groupConfigG1S6)' -e 'test_dir(sprintf("%s/%s", PATH, "tests"))'

run%:
	Rscript -e 'source("${rFile}")' -e 'run$*()'

createSOSmall:
	(cd /Volumes/SuperSupernova/stackExchange/ && rm stackOverflowSmall/* && cd stackOverflow2014 && find *.xml -print -exec bash -ic "(head '{}' && tail '{}') > ../stackOverflowSmall/{}" \;)

parrunPUsers: NP = 1
parrunPriors: NP = 1
parrunSjisSOAll: NP = 1
parrunSjisTAll: NP = 1
parrunContext: NP = 1
parrunContextS1: NP = 2
parrunContextS2: NP = 1
parrunContextS3: NP = 1
parrunContextS4: NP = 2
parrunContextS5: NP = 2
parrunContextS6: NP = 2
parrunGenAndSaveCurWorkspace: NP = 1

parrun%:
	make -s get$* | parallel -P ${NP} --progress make {}

parrunContextAll: parrunContextS1 parrunContextS2 parrunContextS3 parrunContextS4 parrunContextS5 parrunContextS6
	exit 0

runAll: unit parrunPriors parrunSjis parrunSjisT parrunContext
	exit 0

# FIXME: Change prior runs to runPrior...
getPriors:
	egrep '^run((SO)|(SOQ)|(TTweets)|(TFollow)).* <- make' ${rFile} | awk '{print $$1}'

getPUsers:
	egrep '^runPUser.* <- make' ${rFile} | awk '{print $$1}'

getSjisSOAll:
	egrep '^runGenNcoocTblSO.* <-' ${rFile} | awk '{print $$1}'

getSjisTAll:
	egrep '^runGenNcoocTblT.* <-' ${rFile} | awk '{print $$1}'

# FIXME: Only running s1's for now
getContext: getContextS1

getContextS%:
	egrep '^runContext[0-9]+.*s$*.*<-' ${rFile} | awk '{print $$1}'

getGenAndSaveCurWorkspace:
	egrep '^runGenAndSaveCurWorkspace.*s[123456] <-' ${rFile} | awk '{print $$1}'

checkPostgresTimes:
	sudo tail -n 10000 /opt/local/var/db/postgresql93/defaultdb/system.log | egrep '(statement:|duration:)'

showLatestUpdates:
	find dissertationData/ -type f -exec stat --format '%Y :%y %n' {} \; | sort -n | cut -d: -f2- | tail
