
mysql = mysql

load: create-nohtml load-nohtml

load-chunks: create-chunks
	< loadChunks.sql ${mysql}

create-chunks:
	< createChunks.sql ${mysql}

create-nohtml:
	< create-nohtml.sql ${mysql}

load-nohtml:
	< load-nohtml.sql ${mysql}

convert:
	rlwrap ccl -l test.lisp -e '(extract-all)' -e '(quit)'

html-to-text: convert
	python html-to-text.py 

build-all: build-nohtml build-chunks

build-nohtml: html-to-text
	rlwrap ccl -l test.lisp -e '(build-nohtml)' -e '(quit)'

build-chunks: html-to-text
	rlwrap ccl -l test.lisp -e '(build-chunks)' -e '(quit)'
