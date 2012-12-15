#!/bin/bash

#    This file is part of our reusable OCaml BRICKS library
#    Copyright (C) 2012  Jean-Vincent Loddo
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

which ocamlfind &>/dev/null || {
  echo "Error: $0: ocamlfind not found; install it please."
  exit 2
}

source $(dirname $0)/test_scripts_starting_section.sh

# Create the module that will start utop:
MYUTOP_START=myutop_start.cmo
ocamlfind ocamlc -c -linkpkg -package utop -o $MYUTOP_START -impl /dev/stdin <<<"let () = UTop_main.main ()"

STUFF="$INCLUDE_LIBS $LIBRARIES_TO_LINK $INCLUDES"
TOPLEVEL=./myutop
MKTOP="ocamlfind ocamlmktop -o $TOPLEVEL -thread -custom dynlink.cma $STUFF ocamlbricks.cma lwt-unix.cma $MYUTOP_START -linkpkg -package react,lwt,utop"
echo + $MKTOP 
eval $MKTOP

# If you want immediately all utop related stuff available in the toplevel, uncomment the following lines:
#STUFF_UTOP="-I $(ocamlfind query lwt) -I $(ocamlfind query utop) -I $(ocamlfind query react) -I $(ocamlfind query lambda-term) -I $(ocamlfind query zed) -I $(ocamlfind query camomile) bigarray.cma lwt.cma react.cma lwt-react.cma lwt-unix.cma camomile.cma zed.cma lambda-term.cma utop.cma"
#$TOPLEVEL $STUFF $STUFF_UTOP -init $PREAMBLE

echo "UTop.set_profile UTop.Light;;" >>$PREAMBLE
$TOPLEVEL $STUFF -init $PREAMBLE

sleep 0.5
rm -f $PREAMBLE
exit $CODE
