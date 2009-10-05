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

AC_INIT(virt-top,1.0.4)

AC_PROG_INSTALL

dnl Check for basic OCaml environment & findlib.
AC_PROG_OCAML
AC_PROG_FINDLIB

if test "x$OCAMLFIND" = "x"; then
    AC_MSG_ERROR([OCaml findlib is required])
fi

dnl Use ocamlfind to find the required packages ...

dnl Check for required OCaml packages.
AC_CHECK_OCAML_PKG(unix)
if test "x$pkg_unix" != "xyes"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'unix'])
fi

AC_CHECK_OCAML_PKG(extlib)
if test "x$pkg_extlib" != "xyes"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'extlib'])
fi

AC_CHECK_OCAML_PKG(libvirt)
if test "x$pkg_libvirt" != "xyes"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'libvirt'])
fi

AC_CHECK_OCAML_PKG(curses)
if test "x$pkg_curses" != "xyes"; then
    AC_MSG_ERROR([Cannot find required OCaml package 'curses'])
fi

dnl Check for optional OCaml packages.
AC_CHECK_OCAML_PKG(gettext)
AC_CHECK_OCAML_PKG(xml-light)
AC_CHECK_OCAML_PKG(csv)

dnl Need to check which version of calendar is installed.
AC_CHECK_OCAML_MODULE(calendar,pkg_calendar2,CalendarLib.Date,[+calendar])
if test "x$pkg_calendar2" = "xno"; then
    AC_CHECK_OCAML_PKG(calendar)
fi

AC_SUBST(pkg_unix)
AC_SUBST(pkg_extlib)
AC_SUBST(pkg_curses)
AC_SUBST(pkg_gettext)
AC_SUBST(pkg_xml_light)
AC_SUBST(pkg_csv)
AC_SUBST(pkg_calendar)
AC_SUBST(pkg_calendar2)

dnl Check for optional perldoc (for building manual pages).
AC_CHECK_PROG(HAVE_PERLDOC,perldoc,perldoc)

dnl Check for recommended ocaml-gettext tool.
AC_CHECK_PROG(OCAML_GETTEXT,ocaml-gettext,ocaml-gettext)

dnl Check for msgfmt tool.
AC_CHECK_PROG(MSGFMT,msgfmt,msgfmt)

dnl Check for optional NSIS (for building a Windows installer).
dnl XXX NSIS support is probably broken at the moment XXX
AC_ARG_WITH([nsis],
	[AS_HELP_STRING([--with-nsis],
	    [use NSIS to build a Windows installer])],
	[],
	[with_nsis=no])

MAKENSIS=
LIBVIRT_DLL_PATH=
LIBXDR_DLL_PATH=
LIBXML2_DLL_PATH=
GNUTLS_DLL_PATH=
GTK_PATH=
GTK_DLL_PATH=

[
msys_to_win_dir () {
    eval pushd "\$$1" > /dev/null
    eval $1=`pwd -W`
    popd > /dev/null
}
]

if test "x$with_nsis" != "xno"; then
    AC_PATH_PROG(MAKENSIS,makensis,[],[$with_nsis:$PATH])
    if test "x$MAKENSIS" = "x"; then
	AC_MSG_FAILURE([--with-nsis was given, but could not find MAKENSIS.EXE])
    fi

    # MAKENSIS is set so we will build a rule for making a Windows
    # installer.  To support this, generate wininstaller.nsis.
    saved_IFS=$IFS
    IFS=$PATH_SEPARATOR
    for d in $PATH; do
    	IFS=$saved_IFS
	echo Checking $d for DLLs ... >&5
	if test -f "$d/libvirt-0.dll"; then
	    LIBVIRT_DLL_PATH="$d"
	fi
	if test -f "$d/libxdr.dll"; then
	    LIBXDR_DLL_PATH="$d"
	fi
	if test -f "$d/libxml2-2.dll"; then
	    LIBXML2_DLL_PATH="$d"
	fi
	if test -f "$d/libgpg-error-0.dll"; then
	    GNUTLS_DLL_PATH="$d"
	fi
	if test -f "$d/libgtk-win32-2.0-0.dll"; then
	    GTK_DLL_PATH="$d"
	    GTK_PATH="$d/.."
	fi
    done
    IFS=$saved_IFS

    if test "x$LIBVIRT_DLL_PATH" = "x"; then
    	AC_MSG_FAILURE([cannot find libvirt-0.dll in PATH])
    fi
    if test "x$LIBXDR_DLL_PATH" = "x"; then
    	AC_MSG_FAILURE([cannot find libxdr.dll in PATH])
    fi
    if test "x$LIBXML2_DLL_PATH" = "x"; then
    	AC_MSG_FAILURE([cannot find libxml2-2.dll in PATH])
    fi
    if test "x$GNUTLS_DLL_PATH" = "x"; then
    	AC_MSG_FAILURE([cannot find GnuTLS DLLs in PATH])
    fi
    if test "x$GTK_DLL_PATH" = "x"; then
        AC_MSG_WARN([cannot find GTK DLLs in PATH])
    fi

    # Change the paths to Windows paths.
    msys_to_win_dir LIBVIRT_DLL_PATH
    msys_to_win_dir LIBXDR_DLL_PATH
    msys_to_win_dir LIBXML2_DLL_PATH
    msys_to_win_dir GNUTLS_DLL_PATH
    if test "x$GTK_DLL_PATH" != "x"; then
    	msys_to_win_dir GTK_DLL_PATH
    	msys_to_win_dir GTK_PATH
    fi
fi
AC_SUBST(MAKENSIS)
AC_SUBST(LIBVIRT_DLL_PATH)
AC_SUBST(LIBXDR_DLL_PATH)
AC_SUBST(LIBXML2_DLL_PATH)
AC_SUBST(GNUTLS_DLL_PATH)
AC_SUBST(GTK_DLL_PATH)
AC_SUBST(GTK_PATH)

dnl Write gettext modules for the programs.
dnl http://www.le-gall.net/sylvain+violaine/documentation/ocaml-gettext/html/reference-manual/ch03s04.html
for d in virt-top; do
    f=`echo $d | tr - _`_gettext.ml
    AC_MSG_NOTICE([creating $d/$f])
    rm -f $d/$f
    echo "(* This file is generated automatically by ./configure. *)" > $d/$f
    if test "x$pkg_gettext" != "xno"; then
        # Gettext module is available, so use it.
        cat <<EOT >>$d/$f
module Gettext = Gettext.Program (
  struct
    let textdomain = "$d"
    let codeset = None
    let dir = None
    let dependencies = [[]]
  end
) (GettextStub.Native)
EOT
    else
        # No gettext module is available, so fake the translation functions.
        cat <<EOT >>$d/$f
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
done

dnl Summary.
echo "------------------------------------------------------------"
echo "Thanks for downloading" $PACKAGE_STRING
echo "------------------------------------------------------------"

dnl Produce output files.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile
	Make.rules
	po/Makefile
	virt-top/Makefile
	])
if test "x$MAKENSIS" != "x"; then
	AC_CONFIG_FILES([wininstaller.nsis])
fi
AC_OUTPUT
