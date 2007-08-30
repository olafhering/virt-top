# $Id: Makefile.in,v 1.2 2007/08/21 13:24:08 rjones Exp $

CFLAGS		:= -g -O2 -I/home/rjones/local/include \
		   -I.. \
		   -I$(shell ocamlc -where) \
		   -Wall -Werror \
		   -fPIC \
		   -g
LDFLAGS		:=  -L/home/rjones/local/lib

OCAMLCPACKAGES	:= -package extlib,unix
OCAMLCFLAGS	:= -g
OCAMLCLIBS	:= -linkpkg

OCAMLOPTPACKAGES := $(OCAMLCPACKAGES)
OCAMLOPTFLAGS	:=
OCAMLOPTLIBS	:= $(OCAMLCLIBS)

export LIBRARY_PATH=.
export LD_LIBRARY_PATH=.

BYTE_TARGETS	:= libvirt.cma
OPT_TARGETS	:= libvirt.cmxa

all: $(BYTE_TARGETS)

opt: $(OPT_TARGETS)

libvirt.cma: libvirt_c.o libvirt.cmo libvirt_version.cmo
	ocamlmklib -o mllibvirt $^ $(LDFLAGS) -lvirt

libvirt.cmxa: libvirt_c.o libvirt.cmx libvirt_version.cmx
	ocamlmklib -o mllibvirt $^ $(LDFLAGS) -lvirt

libvirt.cmo: libvirt.cmi
libvirt.cmi: libvirt.mli

libvirt_version.cmo: libvirt_version.cmi
libvirt_version.cmi: libvirt_version.mli

install:
	ocamlfind install libvirt ../META *.so *.a *.cmx *.cma *.cmxa *.mli

include ../Make.rules
