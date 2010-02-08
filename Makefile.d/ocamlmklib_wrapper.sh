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
# ocamlmklib_wrapper $(OTHER_LIBRARY_FILES_TO_INSTALL)"

function usage {
 echo 'Usage (in a Makefile):'
 echo '$(basename $0) $(C_OBJECTS_TO_LINK)'
 exit 1
}

OBJECTS=$(for i in "$@"; do echo $i.o; done)
INCLUDES=$(cd _build ; find -type d -printf "-I %p\n")
CMO=$(ocamlobjinfo _build/ocamlbricks.cma | awk '/Unit name/{x=tolower(substr($3,1,1)); r=substr($3,2); printf("%s%s.cmo\n",x,r);}')
CMX=$(ocamlobjinfo _build/ocamlbricks.cma | awk '/Unit name/{x=tolower(substr($3,1,1)); r=substr($3,2); printf("%s%s.cmx\n",x,r);}')

set -e
cd _build/
echo "Rebuilding library with ocamlmklib..."
set -x
ocamlmklib -custom -o ocamlbricks $OBJECTS $INCLUDES $CMO
ocamlmklib -custom -o ocamlbricks $OBJECTS $INCLUDES $CMX
set +x
ls -l ocamlbricks.cm{,x}a
