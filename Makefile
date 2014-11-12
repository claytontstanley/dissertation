

push:
	git push
	git push github master
	git push raid master
	(cd submodules/dissertationData && git push && git push raid master)

