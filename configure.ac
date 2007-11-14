# ocaml-libvirt
# Copyright (C) 2007 Red Hat Inc., Richard W.M. Jones
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

AC_INIT(ocaml-libvirt,0.3.3.1)

dnl Check for basic C environment.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_CPP

AC_C_PROTOTYPES
test "x$U" != "x" && AC_MSG_ERROR(Compiler not ANSI compliant)

AC_PROG_CC_C_O

dnl Check for libvirt development environment.
AC_ARG_WITH(libvirt,
	AC_HELP_STRING([--with-libvirt=PATH],[Set path to installed libvirt]),
	[if test "x$withval" != "x"; then
	   CFLAGS="$CFLAGS -I$withval/include"
	   LDFLAGS="$LDFLAGS -L$withval/lib"
	 fi
	])
AC_CHECK_LIB(virt,virConnectOpen,
	[],
	AC_MSG_ERROR([You must install libvirt library]))
AC_CHECK_HEADER([libvirt/libvirt.h],
	[],
	AC_MSG_ERROR([You must install libvirt development package]))

dnl Check for libvirt >= 0.2.1 (our minimum supported version).
dnl See: http://libvirt.org/hvsupport.html
AC_CHECK_FUNC(virConnectGetCapabilities,
	[],
	AC_MSG_ERROR([You must have libvirt >= 0.2.1]))

dnl Check for optional libvirt functions added since 0.2.1.
dnl See: http://libvirt.org/hvsupport.html
AC_CHECK_FUNCS([virConnectGetHostname virConnectGetURI virDomainBlockStats virDomainGetSchedulerParameters virDomainGetSchedulerType virDomainInterfaceStats virDomainMigrate virDomainSetSchedulerParameters virNodeGetFreeMemory virNodeGetCellsFreeMemory])

dnl Check for optional ncurses.
AC_CHECK_LIB(ncurses,initscr)

dnl Check for basic OCaml environment & findlib.
AC_PROG_OCAML
AC_PROG_FINDLIB

dnl Check for required OCaml packages.
AC_CHECK_OCAML_PKG(unix)
if test "x$pkg_unix" != "xyes"; then
   AC_MSG_ERROR([Cannot find required OCaml package 'unix'])
fi
AC_CHECK_OCAML_PKG(extlib)
if test "x$pkg_extlib" != "xyes"; then
   AC_MSG_ERROR([Cannot find required OCaml package 'extlib'])
fi

dnl Check for optional OCaml packages.
AC_CHECK_OCAML_PKG(lablgtk2)
AC_CHECK_OCAML_PKG(curses)
AC_CHECK_OCAML_PKG(gettext)
AC_CHECK_OCAML_PKG(xml-light)
AC_CHECK_OCAML_PKG(csv)
AC_CHECK_OCAML_PKG(calendar)

AC_SUBST(pkg_lablgtk2)
AC_SUBST(pkg_curses)
AC_SUBST(pkg_gettext)
AC_SUBST(pkg_xml_light)
AC_SUBST(pkg_csv)
AC_SUBST(pkg_calendar)

dnl Check for optional perldoc (for building manual pages).
AC_CHECK_PROG(HAVE_PERLDOC,perldoc,perldoc)

dnl Summary.
echo "------------------------------------------------------------"
echo "Thanks for downloading" $PACKAGE_STRING
echo "  OCaml libvirt bindings   . yes"
echo "  example programs   . . . . yes"
echo "  mlvirsh    . . . . . . . . yes"
echo -n "  mlvirtmanager    . . . . . "
if test "x$pkg_lablgtk2" = "xyes"; then
   echo "yes"
else
   echo "no (needs optional lablgtk2)"
fi
echo -n "  virt-top     . . . . . . . "
if test "x$pkg_curses" = "xyes"; then
   echo "yes"
else
   echo "no (needs optional ocaml-curses)"
fi
echo -n "  virt-df      . . . . . . . "
if test "x$pkg_xml_light" = "xyes"; then
   echo "yes"
else
   echo "no (needs optional xml-light)"
fi
echo "------------------------------------------------------------"

dnl Produce output files.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([META
	libvirt/libvirt_version.ml
	Makefile
	libvirt/Makefile
	examples/Makefile
	mlvirsh/Makefile
	mlvirtmanager/Makefile
	virt-top/Makefile
	virt-df/Makefile
	])
AC_OUTPUT
