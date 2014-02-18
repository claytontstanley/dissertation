mysql = mysql
psql = psql
ccl = ~/bin/ccl-mod

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

trim-chunks trim-chunks-huge build-nohtml build-chunks extract-all create-all-symlinks create-all-post-ids-csv:
	rlwrap ${ccl} -l process-lisp.lisp -e '($@)' -e '(quit)'

post-ids.csv: export-post-ids.sql
	< $< ${mysql} > $@

removeNulls = sed '/\x0/d'
removeMangled = egrep -v '^"194017052","4297959","'

process-chunks.csv process-chunks-huge.csv: process-%.csv:
	make -s trim-$* | ${removeNulls} | ${removeMangled} > $*-processed.csv

split-%.csv:
	mkdir $* 
	csplit -f $*/mlog- -b %03d.csv -n 3 -k $*.csv +1000000 {*}
