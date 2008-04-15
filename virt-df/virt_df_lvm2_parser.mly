/* 'df' command for virtual domains.  -*- text -*-
   (C) Copyright 2007-2008 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Parser for LVM2 metadata.
   ocamlyacc tutorial:
   http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/
 */

%{
  open Virt_df_lvm2_metadata
%}

%token LBRACE RBRACE			/* { } */
%token LSQUARE RSQUARE			/* [ ] */
%token EQ				/* = */
%token COMMA				/* , */
%token <string> STRING			/* "string" */
%token <int64> INT			/* an integer */
%token <float> FLOAT			/* a float */
%token <string> IDENT			/* a naked keyword/identifier */
%token EOF				/* end of file */

%start input
%type <Virt_df_lvm2_metadata.metadata> input

%%

input	: lines EOF	{ List.rev $1 }
	;

lines	: /* empty */	{ [] }
	| lines line	{ $2 :: $1 }
	;

line	: /* empty */	/* These dummy entries get removed after parsing. */
			{ ("", String "") }
	| IDENT EQ value
			{ ($1, $3) }
	| IDENT LBRACE lines RBRACE
			{ ($1, Metadata (List.rev $3)) }
	;

value	: STRING	{ String $1 }
	| INT		{ Int $1 }
	| FLOAT		{ Float $1 }
	| LSQUARE list RSQUARE
			{ List (List.rev $2) }
	;

list	: /* empty */	{ [] }
	| value		{ [$1] }
	| list COMMA value
			{ $3 :: $1 }
	;
