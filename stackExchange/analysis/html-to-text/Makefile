mysql = mysql

create-chunks create-nohtml: create-%:
	< create-$*.sql ${mysql}

load-all: load-nohtml load-chunks

load-chunks load-nohtml: load-%: create-%
	< load-$*.sql ${mysql}

update-chunks: load-chunks
	< $@.sql ${mysql}

update-nohtml: load-nohtml

update-all: update-nohtml update-chunks

process-all: extract-all 
	python process-all.py 

build-all: build-nohtml build-chunks

build-nohtml build-chunks: process-all

build-nohtml build-chunks extract-all create-all-symlinks create-all-post-ids-csv:
	rlwrap ccl -l test.lisp -e '($@)' -e '(quit)'

post-ids.csv: export-post-ids.sql
	< $< ${mysql} > $@
