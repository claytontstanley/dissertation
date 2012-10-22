mysql = mysql

create-chunks create-nohtml: create-%:
	< create-$*.sql ${mysql}

load-chunks load-nohtml: load-%: create-%
	< load-$*.sql ${mysql}

update-chunks update-nohtml: update-%: load-%

update-chunks:
	< $@.sql ${mysql}

update-all: update-nohtml update-chunks

processAll: extract-all 

processAll:
	python -c 'from processPython import *; $@()'

build-all: build-nohtml build-chunks

build-nohtml build-chunks: processAll

build-nohtml build-chunks extract-all create-all-symlinks create-all-post-ids-csv:
	rlwrap ccl -l test.lisp -e '($@)' -e '(quit)'

post-ids.csv: export-post-ids.sql
	< $< ${mysql} > $@
