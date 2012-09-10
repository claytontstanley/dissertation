#!/usr/bin/env python
# http://paste.org/8946 "no" 2009
# Updated by Tero Karvinen http://TeroKarvinen.com

import xml.sax.handler
import xml.sax
import sys
class SOHandler(xml.sax.handler.ContentHandler):
	def __init__(self):
		self.errParse = 0

        def startElement(self, name, attributes):
                if name != "row":
                        self.table = name;
                        self.outFile = open(name+".sql","w")
                        self.errfile = open(name+".err","w")
                else:
                        skip = 0
                        currentRow = u"insert into "+self.table+"("
                        for attr in attributes.keys():
                                currentRow += str(attr) + ","
                        currentRow = currentRow[:-1]
                        currentRow += u") values ("
                        for attr in attributes.keys():
                                try:
                                        currentRow += u'"{0}",'.format(attributes[attr].replace('\\','\\\\').replace('"', '\\"').replace("'", "\\'"))
                                except UnicodeEncodeError:
                                        self.errParse += 1;
                                        skip = 1;
                                        self.errfile.write(currentRow)
                        if skip != 1:
                                currentRow = currentRow[:-1]
                                currentRow += u");"
                                #print len(attributes.keys())
                                self.outFile.write(currentRow.encode("utf-8"))
                                self.outFile.write("")
				self.outFile.write("\n")
                                self.outFile.flush()
                                print currentRow.encode("utf-8");

        def characters(self, data):
                pass

        def endElement(self, name):
                pass

if len(sys.argv) < 2:
        print "Give me an xml file argument!"
        sys.exit(1)

parser = xml.sax.make_parser()
handler = SOHandler()
parser.setContentHandler(handler)
parser.parse(sys.argv[1])
print handler.errParse
