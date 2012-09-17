import sys
import os, errno
import codecs
from boilerpipe.extract import Extractor

def convert(html):
	extractor = Extractor(extractor='KeepEverythingExtractor', html=html)
	extracted_text = extractor.getText()
	extracted_html = extractor.getHTML()
	return extracted_text.rstrip()

def mkdir_p(path):
	try:
		os.makedirs(path)
	except OSError as exc: # Python >2.5
		if exc.errno == errno.EEXIST:
			pass
		else: raise

#f = open(sys.argv[1])
#html = f.read()

def convert_all():
	cnt = 0
	for dirname, dirnames, filenames in os.walk('body/html'):
		for post_id in filenames:
			cnt = cnt + 1
			print "processing file " + str(cnt) + " with id " + str(post_id)
			fname = os.path.join(dirname, post_id)
			with codecs.open(fname, 'r', 'utf-8') as f:
				html = f.read()
			txt = convert(html)
			newdirname = dirname.replace('html','nohtml')
			mkdir_p('body/nohtml')
			mkdir_p(newdirname)	
			with codecs.open(os.path.join(newdirname, post_id), "w", 'utf-8') as text_file:
				text_file.write(txt)

convert_all()
