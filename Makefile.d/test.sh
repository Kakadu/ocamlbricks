#!/bin/bash

#    This file is part of our reusable OCaml BRICKS library
#    Copyright (C) 2009 Jean-Vincent Loddo
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Usage:
# test.sh -l "$(LIBRARIES_TO_LINK)" -i "$(DIRECTORIES_TO_INCLUDE)"

function usage {
 echo 'Usage (in a Makefile):'
 echo 'test.sh -l "$(LIBRARIES_TO_LINK)" -i "$(DIRECTORIES_TO_INCLUDE)"'
 exit 1
}

################################
#    Set ocaml parameters      #
################################

# Force make if needed
[[ -f _build/ocamlbricks.cma ]] || make

[[ $1 = "-l" ]] || usage
LIST=$(echo $2)
for i in $LIST; do
  LIBRARIES_TO_LINK+=" $i.cma"
done
shift 2

[[ $1 = "-i" ]] || usage
shift
LIST=$(echo $@)
for i in $LIST; do
 INCLUDE_LIBS+=" -I +$i"
done

INCLUDES=$(cd _build ; find -type d -printf "-I %p\n")

################################
#         Preamble             #
################################

function prefix {
 local i RESULT NOTDIR DIR
 for i in "$@"; do
  DIR=${i%/*}
  NOTDIR=${i##*/}
  RESULT=${NOTDIR%.*}
  if   [[ "$RESULT" = "$NOTDIR" ]]; then echo "$i";
  elif [[ "$DIR" = "$i" ]];         then echo "$RESULT";
  else echo "$DIR/$RESULT";
  fi
 done
}

function make_preamble {
 cd _build/
 LIST=$(find . -type f -name "*.cmo" -exec basename {} \; | sort)
 M=$(prefix $LIST | fmt)
cat > $PREAMBLE  <<EOF
Printf.printf "You can open and test the following modules:\n---\n%s\n---\n" "${M}";;
Printf.printf "Tip: if you want see the signature just type: module M = <Name>;;\n";;
Printexc.record_backtrace true;;
Ocamlbricks_log.enable ();;
EOF
 cd ..
}

export OCAMLRUNPARAM=-b

PREAMBLE=/tmp/$(basename $0).preamble.$RANDOM.ml;
make_preamble

cd _build
STUFF="$INCLUDE_LIBS $LIBRARIES_TO_LINK $INCLUDES"
echo ocamlmktop -o toplevel -thread -custom dynlink.cma $STUFF ocamlbricks.cma
ocamlmktop -g -o toplevel -thread -custom dynlink.cma $STUFF ocamlbricks.cma

if which rlwrap >/dev/null; then
 rlwrap ./toplevel $STUFF -init $PREAMBLE || CODE=$?
else
 echo "Suggestion: install rlwrap for testing with readline (on debian/ubuntu: apt-get install rlwrap)"
 ./toplevel $STUFF -init $PREAMBLE || CODE=$?
fi

sleep 0.5
rm -f $PREAMBLE
exit $CODE
