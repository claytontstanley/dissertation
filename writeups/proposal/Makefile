outFile = proposal
mendeleyDB = Library/Application Support/Mendeley Desktop/clayton.stanley@rice.edu@www.mendeley.com.sqlite


${outFile}.pdf: ${outFile}.tex alwaysRemake
	latexmk -pdf $<

alwaysRemake:
	@exit 0

clean:
	latexmk -C
	rm ${outFile}.bbl

fix-bibliography:
	< fixBibliography.sql sqlite3 ~/"${mendeleyDB}"

