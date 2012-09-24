
mysql = mysql

load: createTable loadTable

createTable:
	< createTable.sql ${mysql}

loadTable:
	< loadTable.sql ${mysql}

convert:
	rlwrap ccl -l test.lisp -e '(extract-all)' -e '(quit)'

html-to-text: convert
	python html-to-text.py 

build: html-to-text
	rlwrap ccl -l test.lisp -e '(build-csv)' -e '(quit)'

build-chunks: html-to-text
	rlwrap ccl -l test.lisp -e '(build-chunks)' -e '(quit)'
