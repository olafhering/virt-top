# $Id: Makefile.in,v 1.2 2007/08/21 14:24:38 rjones Exp $

INSTALL		:= /usr/bin/install -c

prefix		= /usr/local
exec_prefix	= ${prefix}
bindir		= ${exec_prefix}/bin

OCAMLCPACKAGES	:= -package extlib,unix,lablgtk2 -I ../libvirt
OCAMLCFLAGS	:= -g
OCAMLCLIBS	:= -linkpkg

OCAMLOPTPACKAGES := $(OCAMLCPACKAGES)
OCAMLOPTFLAGS	:=
OCAMLOPTLIBS	:= $(OCAMLCLIBS)

export LIBRARY_PATH=../libvirt
export LD_LIBRARY_PATH=../libvirt

BYTE_TARGETS	:= mlvirtmanager
OPT_TARGETS	:= mlvirtmanager.opt

VIRTMANAGER_OBJS := \
	mlvirtmanager_helpers.cmo \
	mlvirtmanager_connections.cmo \
	mlvirtmanager_domain_ops.cmo \
	mlvirtmanager_mainwindow.cmo \
	mlvirtmanager.cmo

VIRTMANAGER_XOBJS := $(VIRTMANAGER_OBJS:.cmo=.cmx)

all: $(BYTE_TARGETS)

opt: $(OPT_TARGETS)

mlvirtmanager: $(VIRTMANAGER_OBJS)
	ocamlfind ocamlc $(OCAMLCPACKAGES) $(OCAMLCFLAGS) $(OCAMLCLIBS) \
	  ../libvirt/mllibvirt.cma gtkInit.cmo -o $@ $<

mlvirtmanager.opt: $(VIRTMANAGER_XOBJS)
	ocamlfind ocamlopt \
	  $(OCAMLOPTPACKAGES) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) \
	  ../libvirt/mllibvirt.cmxa gtkInit.cmx -o $@ $<

install:
	if [ -x mlvirtmanager.opt ]; then \
	  mkdir -p $(DESTDIR)$(bindir); \
	  $(INSTALL) -m 0755 mlvirtmanager.opt $(DESTDIR)$(bindir)/mlvirtmanager; \
	fi

include ../Make.rules
