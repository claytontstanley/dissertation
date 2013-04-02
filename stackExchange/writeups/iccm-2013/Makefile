
outFile = writeup
sshUser = clayton
sshUrl = chil.rice.edu
sshCmd = ${sshUser}@${sshUrl}
depBasePath = /Users/Shared
depDir = stackOverflowICCM
depPath = ${depBasePath}/${depDir}

${outFile}.pdf: ${outFile}.tex alwaysRemake
	latexmk -pdf $<

alwaysRemake:
	@exit 0

clean:
	latexmk -C
	rm ${outFile}.bbl

deploy:
	rsync -avzLe ssh ./ ${sshCmd}:${depPath}
	ssh ${sshCmd} 'cd ${depPath}; make clean; make'

cropped-pdfs:
	cat ${outFile}.tex | grep '\.pdf' | perl -pe 's/^.*{(.*)-crop(.*)}}/$$1$$2/' | sed 's/^/pictures\//' | xargs -n 1 pdfcrop

