
unit:
	Rscript -e 'source("preProcess.R")' -e 'test_dir(sprintf("%s/%s", PATH, "tests"))'

run%:
	Rscript -e 'source("preProcess.R")' -e 'run$*()'


createSOSmall:
	(cd /Volumes/SuperSupernova/stackExchange/ && rm stackOverflowSmall/* && cd stackOverflow2014 && find *.xml -print -exec bash -ic "(head '{}' && tail '{}') > ../stackOverflowSmall/{}" \;)

