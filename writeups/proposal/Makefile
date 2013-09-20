outFile = proposal
mendeleyDB = Library/Application Support/Mendeley Desktop/clayton.stanley@rice.edu@www.mendeley.com.sqlite
figPdfs = $(shell cat ${outFile}.tex | grep '\.pdf' | perl -pe 's/^.*{(.*)-crop([^}]+)}+$$/$$1$$2/' | perl -pe 's|^|figures/dissertation/|')

${outFile}.pdf: ${outFile}.tex alwaysRemake
	latexmk -pdf $<

alwaysRemake:
	@exit 0

clean:
	latexmk -C
	rm ${outFile}.bbl

fix-bibliography:
	< fixBibliography.sql sqlite3 ~/"${mendeleyDB}"

pdfs: ${figPdfs}

${figPdfs}: %.pdf: alwaysRemake
	pdfcrop --verbose $@
	#R -q -e 'embedFonts("$*-crop.pdf", format="pdfwrite", options="-dSubsetFonts=true -dEmbedAllFonts=true -dPDFSETTINGS=/printer")'

foo:
	echo "${figPdfs}"
