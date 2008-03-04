#!/bin/sh -
# Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Generate vc_icons.ml

echo <<'EOF'
(* The file vc_icons.ml is automatically generated from rebuild-icons.sh
 * Any changes you make will be lost.
 *)

EOF
echo

# Open any modules which may use icons.
echo "open Vc_connection_dlg"
echo

while [ $# -gt 0 ]; do
    size="$1"
    name="$2"
    filename="$3"
    shift 3

    gdk-pixbuf-mlsource "$filename"
    echo ";;"

    name=`echo -n $name | tr -cs '[0-9a-zA-Z]' '_'`

    echo "icon_${size}x${size}_$name := Some (pixbuf ()) ;;"
done