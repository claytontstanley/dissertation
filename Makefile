SHELL := /bin/bash

push:
	git push
	git push github master
	git push raid master
	(cd submodules/dissertationData && git push && git push raid master)
	ssh chil 'cd /Volumes/RedGiant/Projects/stanley-dissertation && git ru && git rb && git su'

post-clone:
	./check-dev-env.sh
	git submodule update --recursive --init
	make -C submodules/dissertationData pull
	make -C submodules/dissertationData extract
