outFile = proposal

${outFile}.pdf: ${outFile}.tex alwaysRemake
	latexmk -pdf $<

alwaysRemake:
	@exit 0

clean:
	latexmk -C
	rm ${outFile}.bbl
