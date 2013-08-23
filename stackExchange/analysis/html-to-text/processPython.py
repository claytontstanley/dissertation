from __future__ import division
from __future__ import with_statement
import nltk, re, pprint
import sys
import os, errno
import codecs
from boilerpipe.extract import Extractor
import itertools
import os.path
from grizzled.os import working_directory
import MySQLdb
import string
import pg

def convert(html):
	extractor = Extractor(extractor='KeepEverythingExtractor', html=html)
	extracted_text = extractor.getText()
	extracted_html = extractor.getHTML()
	return extracted_text.rstrip()

def nlp(txt):
	wnl = nltk.WordNetLemmatizer()
	sentences = nltk.sent_tokenize(txt)
	tokens = [nltk.word_tokenize(sen) for sen in sentences]
	tokens = list(itertools.chain.from_iterable(tokens))
	tokens = [wnl.lemmatize(t) for t in tokens]
	words = [w.lower() for w in tokens]
	return "\n".join(item for item in words)

def nlpTags(txt):
	words = nltk.regexp_tokenize(txt, pattern=r"(?<=<)[^>]+(?=>)")
	return "\n".join(item for item in words)


def mkdir_p(path):
	try:
		os.makedirs(path)
	except OSError as exc: # Python >2.5
		if exc.errno == errno.EEXIST:
			pass
		else: raise

def convert_all(indir, fun, outdir):
	"""Performs a transformation of calling fun on all text in all files in indir.
	Stores results as files in outdir.
	Outdir and indir will have the same directory structure.
	"""
	cnt = 0
	# For a dir structure: a/1/c a/1/c2 a/2/d a/2/c, loop over all of the files (c c2 d c)
	# and paths to those files (a/1 a/1 a/2 a/2). If your dir structure is only a flat single
	# level, the paths to the files (dirname) will remain constant.
	for dirname, dirnames, filenames in os.walk(indir):
		for post_id in filenames:
			cnt = cnt + 1
			print "processing file " + str(cnt) + " with id " + str(post_id)
			fname = os.path.join(dirname, post_id)
			with codecs.open(fname, 'r', 'utf-8') as f:
				html = f.read()
			# Perform the actual transformation on the text by calling the function
			# passed into convert_all, and providing it with the raw text.
			txt = fun(html)
			newdirname = dirname.replace(indir,outdir)
			mkdir_p(newdirname)
			with codecs.open(os.path.join(newdirname, post_id), "w", 'utf-8') as text_file:
				text_file.write(txt)

_dir = os.path.dirname(os.path.abspath(__file__))
#db = MySQLdb.connect(host="127.0.0.1", read_default_file="~/.my.cnf")
cur = pg.connect(host="127.0.0.1")
#cur = db.cursor()

def loadSubset(file, subset):
	ldir = _dir
	cur.query('create temporary table foo (id integer, subset varchar(255))')
	cmd = "copy foo (id) from '${ldir}/${file}' delimiters ',' csv"
	cmd = string.Template(cmd).substitute(locals())
	print cmd
	cur.query(cmd)
	cmd = "update foo set subset = '${subset}'"
	cmd = string.Template(cmd).substitute(locals())
	cur.query(cmd)
	cur.query('insert into subsets(id, subset) select * from foo')
	cur.query('drop table foo')

def loadSynonyms(file):
	ldir = _dir
	cmd = "copy tagsynonyms from '${ldir}/${file}' delimiter ',' csv header"
	cmd = string.Template(cmd).substitute(locals())
	cur.query(cmd)

def loadChunksFromFile(csvFile):
	ldir = _dir
	cmd = "copy chunksBase (chunkId, Id, chunk, chunkType) from '${ldir}/${csvFile}' delimiters ',' csv"
	cmd = string.Template(cmd).substitute(locals())
	print cmd
	cur.query(cmd)

def loadChunksFromDir(thisDir):
	for dirname, dirnames, filenames in os.walk(thisDir):
		for csvFile in filenames:
			relPathToFile = "${thisDir}/$csvFile"
			relPathToFile = string.Template(relPathToFile).substitute(locals())
			loadChunksFromFile(relPathToFile)

def loadChunks():
	ldir = _dir
	with working_directory(ldir):
		loadSynonyms("synonyms/synonyms.csv")
		loadChunksFromFile('chunks-huge-processed.csv')
		loadSubset("tag/nlp/nlp.csv", "tag")
		loadSubset("title/nlp/nlp.csv", "title")
		loadSubset("tag-subset-1/nlp-huge/nlp-huge.csv", "tag-subset-1")
		loadSubset("title-subset-1/nlp-huge/nlp-huge.csv", "title-subset-1")
		loadSubset("tag-subset-4/nlp-huge/nlp-huge.csv", "tag-subset-4")
		loadSubset("title-subset-4/nlp-huge/nlp-huge.csv", "title-subset-4")

def loadNohtml():
	with working_directory(_dir):
		csvFile = "nohtml.csv"
		cmd = "load data local infile '${csvFile}' into table posts_nohtml fields terminated by ',' enclosed by '\"' escaped by '' (Id, Body)"
		cmd = string.Template(cmd).substitute(locals())
		print cmd
		cur.execute(cmd)

def processAll():
	with working_directory(_dir):
		convert_all('body/raw', convert, 'body/nohtml')
		convert_all('body/nohtml', nlp, 'body/nlp')
		convert_all('title/raw', nlp, 'title/nlp')
		convert_all('tag/raw', nlpTags, 'tag/nlp')
		#e.g., convert_all('body/nohtml-huge', nlp, 'body/nlp-huge')
