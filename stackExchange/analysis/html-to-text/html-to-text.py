import sys
from boilerpipe.extract import Extractor

f = open(sys.argv[1])
html = f.read()
#html = "<p>I'm new to C#, and I want to use a track-bar to change a form's opacity.</p>"

extractor = Extractor(extractor='KeepEverythingExtractor', html=html)
extracted_text = extractor.getText()
extracted_html = extractor.getHTML()

print extracted_text.rstrip()
