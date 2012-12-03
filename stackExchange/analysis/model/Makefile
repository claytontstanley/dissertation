csvs = $(shell find . -maxdepth 1 -name 'title-chunks*.csv' -o -name 'tag-priors*.csv' | tr '\n' ' ')

all: .RData

.RData: driver.R model.R
	rlwrap R --no-restore -e 'source("$<")'

allCsvs: ${csvs}

run-command-chilLaptop : url = chilLaptop
run-command-chilDesktop : url = chilDesktop
run-command-% : url = $*.local

%.csv : subset = tag
%-1.csv : subset = tag-subset-1
%-4.csv : subset = tag-subset-4

${csvs}: %.csv: alwaysRemake
	 psql -c "copy (select * from $(subst -,_,$*)('${subset}')) to stdout with csv header" > $@

alwaysRemake:
	@ exit 0
