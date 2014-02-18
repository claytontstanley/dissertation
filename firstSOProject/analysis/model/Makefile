csvs = $(shell find . -maxdepth 1 -name 'title-chunks*.csv' -o -name 'tag-priors*.csv' -o -name 'tagPriors*.csv' | tr '\n' ' ')

all: model.R

driver.R model.R: alwaysRemake
	rlwrap R --no-restore -e 'source("$@")'


allCsvs: ${csvs}

run-command-chilLaptop : url = chilLaptop
run-command-chilDesktop : url = chilDesktop
run-command-% : url = $*.local

%.csv : subset = tag
%-1.csv : subset = tag-subset-1
%-4.csv : subset = tag-subset-4

${csvs}: %.csv: alwaysRemake
	funName=$$(echo "$*" | egrep -o '^[a-z]+-?[A-Za-z]+' | sed 's/-/_/'); \
	psql -c "copy (select * from $$funName('${subset}')) to stdout with csv header" > $@

alwaysRemake:
	@ exit 0
