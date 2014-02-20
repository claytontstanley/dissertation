from bs4 import BeautifulSoup
import unicodecsv
import os
import os.path
import sys
# lint_ignore=E302,E501

_dir = os.path.dirname(os.path.abspath(__file__))

with open("%s" % (sys.argv[1]), 'rb') as f:
    reader = unicodecsv.reader(f, encoding='utf-8')
    res = []
    row = reader.next()
    for count, row in enumerate(reader):
        html = row[0]
        #raw = nltk.clean_html(html)
        text = BeautifulSoup(html).get_text()
        res.append([html, text])

with open("%s" % (sys.argv[2]), 'wb') as f:
    writer = unicodecsv.writer(f, encoding='utf-8', quoting=unicodecsv.QUOTE_ALL)
    writer.writerows(res)

exit()
