#!/bin/bash

function make_preamble {
local M=$1

cat > $PREAMBLE  <<EOF
#load "../BASE/sugar.cmo";;
#load "../BASE/identifier.cmo";;
#load "../BASE/fix.cmo";;
#load "../BASE/mrproper.cmo";;
#load "unix.cma";;
#load "str.cma";;
#load "threads.cma";;
#load "endpoint.cmo";;
#load "preludeExtra.cmo";;
#load "option.cmo";;
#load "stackExtra.cmo";;
#load "listExtra.cmo";;
#load "stringExtra.cmo";;
#load "strExtra.cmo";;
#load "filenameExtra.cmo" ;;
#load "unixExtra.cmo" ;;
EOF

if echo "Array Filename List Prelude String Str Sys Unix" | grep -wq "$M"; then
cat >> $PREAMBLE  <<EOF
open ${M}Extra;;
Printf.printf "(* ${M} is the merge of standard and extra modules. *) \n";;
EOF
fi

cat >> $PREAMBLE  <<EOF
Printf.printf "(* Tip: if you want see the signature just type: module M = ${M};; *) \n";;
open ${M};;
Printf.printf "open ${M};;\n";;
EOF

}


cd ../_build/EXTRA || { echo "Please call \"make\" from the base directory, then return here and retry."; exit 1; }

if echo "Array Endpoint Filename List Option Prelude Stack String Str Sys Unix" | grep -wq "$1"; then
 MODULE=$1
else
 echo "Usage: $(basename $0) <module>"
 echo "where module is one of \"Array\", \"Endpoint\", \"Filename\", \"List\", \"Option\", \"Prelude\", \"Stack\", \"String\", \"Str\", \"Sys\", \"Unix\""
 echo "This script calls ocaml loading all needed cmo/cma and opening the specified module."
 exit 2
fi

PREAMBLE=/tmp/$(basename $0).preamble.$RANDOM.ml;
make_preamble $MODULE

if which rlwrap >/dev/null; then
 rlwrap ocaml -I ../BASE/ -I +threads -init $PREAMBLE;
else
 echo "Install rlwrap please (on debian/ubuntu: apt-get install rlwrap)"
fi

rm -f $PREAMBLE
