
load: createTable loadTable

createTable:
	cat createTable.sql | mysql

loadTable:
	cat loadTable.sql | mysql

convert:
	rlwrap ccl -l test.lisp -e '(quit)'
