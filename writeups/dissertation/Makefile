outFile = dissertation
mendeleyDB = Library/Application Support/Mendeley Desktop/clayton.stanley@rice.edu@www.mendeley.com.sqlite
figPdfs = $(shell cat ${outFile}.tex | grep '\.pdf' | perl -pe 's/^.*{(.*)([^}]+)}+$$/$$1$$2/' | perl -pe 's|^|figures/dissertation/|')

SHELL := /bin/bash

all: ${outFile}.pdf

${outFile}.pdf: %.pdf: static pdfs alwaysRemake
	latexmk -pdf $*.tex

alwaysRemake:
	@exit 0

clean:
	latexmk -C
	rm ${outFile}.bbl

fix-bibliography:
	< fixBibliography.sql sqlite3 ~/"${mendeleyDB}"

pdfs: ${figPdfs}

${figPdfs}: %-crop.pdf: %.pdf
	pdfcrop --verbose $*
	#R -q -e 'embedFonts("$*-crop.pdf", format="pdfwrite", options="-dSubsetFonts=true -dEmbedAllFonts=true -dPDFSETTINGS=/printer")'

static:
	! grep 'NoZero.*0\.' dissertation.tex
	! grep -i ' very ' dissertation.tex

showFigModDates:
	git gp includegraphics dissertation.tex | egrep -o '{[^{]*}+$$' | sed 's/{//g' | sed 's/}//g' | perl -pe 's|^|figures/dissertation/|' | xargs ls -lt 
