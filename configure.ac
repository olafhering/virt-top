# virt-top
# Copyright (C) 2007-2008 Red Hat Inc., Richard W.M. Jones
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

dnl Process this file with autoconf to produce a configure script.

AC_INIT([virt-top],[1.0.8])
AC_CONFIG_MACRO_DIR([m4])

AC_PROG_INSTALL

dnl Check for basic OCaml environment & findlib.
AC_PROG_OCAML
AC_PROG_FINDLIB

if test "x$OCAMLFIND" = "xno"; then
    AC_MSG_ERROR([OCaml findlib is required])
fi

dnl Use ocamlfind to find the required packages ...

dnl Check for required OCaml packages.
AC_CHECK_OCAML_PKG(unix)
if test "x$OCAML_PKG_unix" = "xno"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'unix'])
fi

AC_CHECK_OCAML_PKG(extlib)
if test "x$OCAML_PKG_extlib" = "xno"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'extlib'])
fi

AC_CHECK_OCAML_PKG(libvirt)
if test "x$OCAML_PKG_libvirt" = "xno"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'libvirt'])
fi

AC_CHECK_OCAML_PKG(curses)
if test "x$OCAML_PKG_curses" = "xno"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'curses'])
fi

dnl Check for optional OCaml packages.
AC_CHECK_OCAML_PKG(gettext)
AC_CHECK_OCAML_PKG(xml-light)
AC_CHECK_OCAML_PKG(csv)
AC_CHECK_OCAML_PKG(calendar)

AC_SUBST(OCAML_PKG_unix)
AC_SUBST(OCAML_PKG_extlib)
AC_SUBST(OCAML_PKG_curses)
AC_SUBST(OCAML_PKG_gettext)
AC_SUBST(OCAML_PKG_xml_light)
AC_SUBST(OCAML_PKG_csv)
AC_SUBST(OCAML_PKG_calendar)

dnl Check for optional perldoc (for building manual pages).
AC_CHECK_PROG(HAVE_PERLDOC,perldoc,perldoc)

dnl Check for recommended ocaml-gettext tool.
AC_CHECK_PROG(OCAML_GETTEXT,ocaml-gettext,ocaml-gettext)

dnl Check for msgfmt tool.
AC_CHECK_PROG(MSGFMT,msgfmt,msgfmt)

dnl Write gettext modules for the programs.
dnl http://www.le-gall.net/sylvain+violaine/documentation/ocaml-gettext/html/reference-manual/ch03s04.html
AC_MSG_NOTICE([creating src/opt_gettext.ml])
rm -f src/opt_gettext.ml
echo "(* This file is generated automatically by ./configure. *)" > src/opt_gettext.ml
if test "x$OCAML_PKG_gettext" != "xno"; then
    # Gettext module is available, so use it.
    cat <<EOT >>src/opt_gettext.ml
module Gettext = Gettext.Program (
  struct
    let textdomain = "virt-top"
    let codeset = None
    let dir = None
    let dependencies = [[]]
  end
) (GettextStub.Native)
EOT
else
    # No gettext module is available, so fake the translation functions.
    cat <<EOT >>src/opt_gettext.ml
module Gettext = struct
  external s_ : string -> string = "%identity"
  external f_ : ('a -> 'b, 'c, 'd) format -> ('a -> 'b, 'c, 'd) format
    = "%identity"
  let sn_ : string -> string -> int -> string
    = fun s p n -> if n = 1 then s else p
  let fn_ : ('a -> 'b, 'c, 'd) format -> ('a -> 'b, 'c, 'd) format -> int
      -> ('a -> 'b, 'c, 'd) format
    = fun s p n -> if n = 1 then s else p
end
EOT
fi

dnl Summary.
echo "------------------------------------------------------------"
echo "Thanks for downloading" $PACKAGE_STRING
echo "------------------------------------------------------------"

dnl Produce output files.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile
	Make.rules
	po/Makefile
	src/Makefile
	src/version.ml
	])
AC_OUTPUT
