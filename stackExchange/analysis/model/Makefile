all: .RData

.RData: driver.R model.R
	rlwrap R --no-restore -e 'source("$<")'

tag-priors.csv: alwaysRemake
	 psql -c "copy (select * from tag_priors('tag')) to stdout with csv header" > $@

title-chunks.csv: alwaysRemake
	 psql -c "copy (select * from title_chunks('tag')) to stdout with csv header" > $@


alwaysRemake:
	@ exit 0
