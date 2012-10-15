#!/bin/bash

# both $1 and $2 are absolute paths
# returns $2 relative to $1
function relativeTo {
	source=$1
	target=$2
	common_part=$source
	back=
	while [ "${target#$common_part}" = "${target}" ]; do
		common_part=${common_part%/*}
		back="../${back}"
	done
	echo ${back}${target#$common_part/}
}

thisDir=$(pwd -P)
srcLst=$1
desLst=$2

exec 3< <(printf "$srcLst\n" )
exec 4< <(printf "$desLst\n" )

while read -r -u 3 src; read -r -u 4 des; do
	echo "working $src -> $des"
	srcFname=${src##*/}	
	srcDir=${src%/*}
	desFname=${des##*/}
	desDir=${des%/*}
	mkdir -p $desDir
	relPath=$(relativeTo "$thisDir/$desDir" "$thisDir/$srcDir")
	ln -sf "$relPath/$srcFname" "$desDir/$desFname"
done
