mysql = mysql

create-chunks create-nohtml: create-%:
	< create-$*.sql ${mysql}

load-all: load-nohtml load-chunks

load-chunks load-nohtml: load-%: create-%
	< load-$*.sql ${mysql}

extract-all:
	rlwrap ccl -l test.lisp -e '($@)' -e '(quit)'

html-to-text: extract-all 
	python html-to-text.py 

build-all: build-nohtml build-chunks

build-nohtml build-chunks: html-to-text
	rlwrap ccl -l test.lisp -e '($@)' -e '(quit)'

