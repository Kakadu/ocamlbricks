#!/bin/bash

#    This file is part of our reusable OCaml BRICKS library
#    Copyright (C) 2009 2012  Jean-Vincent Loddo
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
# $0 -l "$(LIBRARIES_TO_LINK)" -i "$(DIRECTORIES_TO_INCLUDE)"

source $(dirname $0)/test_scripts_starting_section.sh

STUFF="$INCLUDE_LIBS $LIBRARIES_TO_LINK $INCLUDES"
CMD="ocamlmktop -g -o toplevel -thread -custom dynlink.cma $STUFF ocamlbricks.cma"
echo + $CMD
eval $CMD
# Using ocamlfind instead of $STUFF, the previous command shoud be:
# ocamlfind ocamlmktop -g -o toplevel -thread -custom dynlink.cma -I +lablgtk2 -I +camlp4 lablgtk.cma -I . ocamlbricks.cma -linkpkg -package unix,str,threads

if which rlwrap >/dev/null; then
 rlwrap ./toplevel $STUFF -init $PREAMBLE || CODE=$?
else
 echo "Suggestion: install rlwrap for testing with readline (on debian/ubuntu: apt-get install rlwrap)"
 ./toplevel $STUFF -init $PREAMBLE || CODE=$?
fi

sleep 0.5
rm -f $PREAMBLE
exit $CODE
