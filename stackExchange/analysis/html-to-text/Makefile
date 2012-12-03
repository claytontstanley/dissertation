mysql = mysql
psql = psql

createChunks createNohtml: create%:
	< create$*.sql ${psql}

loadChunks loadNohtml: load%: create%

updateChunks updateNohtml: update%: load%

updateChunks:
	< $@.sql ${psql}

updateAll: updateNohtml updateChunks

processAll: extract-all 

processAll loadChunks loadNohtml:
	python -c 'from processPython import *; $@()'

build-all: build-nohtml build-chunks

build-nohtml build-chunks: processAll

build-nohtml build-chunks extract-all create-all-symlinks create-all-post-ids-csv:
	rlwrap ccl -l process-lisp.lisp -e '($@)' -e '(quit)'

post-ids.csv: export-post-ids.sql
	< $< ${mysql} > $@
