
mysql = mysql

load: createTable loadTable

createTable:
	< createTable.sql ${mysql}

loadTable:
	< loadTable.sql ${mysql}

convert:
	rlwrap ccl -l test.lisp -e '(extract-html)' -e '(quit)'

html-to-text: convert clean-nohtml
	python html-to-text.py 

build: html-to-text
	rlwrap ccl -l test.lisp -e '(build-csv)' -e '(quit)'

clean-nohtml:
	rm -rf nohtml
