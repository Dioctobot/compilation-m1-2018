#!/usr/bin/env sh

check () {
    PACKAGE="$1"
    echo -n Looking for $PACKAGE...
    OUT=$(ocamlfind query "$PACKAGE" 2>&1)
    if [ $? -eq 0 ]; then
	echo Found here $OUT
    else
	echo $OUT
	exit 1
    fi
}

echo '---------------------'
echo 'Checking dependencies'
echo '---------------------'
check pprint
check lambda-term
check sexplib
check ppx_sexp_conv
