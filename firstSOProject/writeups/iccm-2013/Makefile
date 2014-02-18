
outFile = writeup
sshUser = clayton
sshUrl = chil.rice.edu
sshCmd = ${sshUser}@${sshUrl}
depBasePath = /Users/Shared
depDir = stackOverflowICCM
depPath = ${depBasePath}/${depDir}

figPdfs = $(shell cat ${outFile}.tex | grep '\.pdf' | perl -pe 's/^.*{(.*)-crop(.*)}}/$$1$$2/' | sed 's/^/pictures\//')

${outFile}.pdf: ${outFile}.tex always-remake
	latexmk -pdf $<

always-remake:
	@exit 0

clean:
	latexmk -C
	rm ${outFile}.bbl

deploy:
	rsync -avzLe ssh ./ ${sshCmd}:${depPath}
	ssh ${sshCmd} 'cd ${depPath}; make clean; make'

pdfs: ${figPdfs}

${figPdfs}: %.pdf: always-remake
	pdfcrop $@
	R -q -e 'embedFonts("$*-crop.pdf", format="pdfwrite", options="-dSubsetFonts=true -dEmbedAllFonts=true -dPDFSETTINGS=/printer")'

