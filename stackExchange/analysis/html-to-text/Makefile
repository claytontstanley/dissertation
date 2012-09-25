mysql = mysql

create-chunks create-nohtml: create-%:
	< create-$*.sql ${mysql}

load-all: load-nohtml load-chunks

load-chunks load-nohtml: load-%: create-%
	< load-$*.sql ${mysql}

process-all: extract-all 
	python process-all.py 

build-all: build-nohtml build-chunks

build-nohtml build-chunks: process-all

build-nohtml build-chunks extract-all:
	rlwrap ccl -l test.lisp -e '($@)' -e '(quit)'

