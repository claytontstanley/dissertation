
unit:
	Rscript -e 'source("preProcess.R")' -e 'test_dir(sprintf("%s/%s", PATH, "tests"))'

run%:
	Rscript -e 'source("preProcess.R")' -e 'run$*()'


createSOSmall:
	(cd /Volumes/SuperSupernova/stackExchange/ && rm stackOverflowSmall/* && cd stackOverflow2014 && find *.xml -print -exec bash -ic "(head '{}' && tail '{}') > ../stackOverflowSmall/{}" \;)

runAll:
	grep '^run.* <- make' preProcess.R | awk '{print $1}' | parallel -P 3 --progress make {}

