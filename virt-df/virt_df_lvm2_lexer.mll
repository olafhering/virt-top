(* 'df' command for virtual domains.
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
 *)

(* Scanner for LVM2 metadata.
 * ocamllex tutorial:
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamllex-tutorial/
 *)

{
  open Printf
  open Lexing

  open Virt_df
  open Virt_df_lvm2_parser

  (* Temporary buffer used for parsing strings, etc. *)
  let tmp = Buffer.create 80

  exception Error of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphau = ['a'-'z' 'A'-'Z' '_']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9']
let alnumu = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let ident = alphau alnumu*

let whitespace = [' ' '\t' '\r' '\n']+

let escaped_char = '\\' _

rule token = parse
  (* ignore whitespace and comments *)
  | whitespace
  | '#' [^ '\n']*
      { token lexbuf }

  (* scan single character tokens *)
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | '['  { LSQUARE }
  | ']'  { RSQUARE }
  | '='  { EQ }
  | ','  { COMMA }

  (* strings - see LVM2/lib/config/config.c *)
  | '"'
      {
	Buffer.reset tmp;
	STRING (dq_string lexbuf)
      }
  | '\''
      {
	Buffer.reset tmp;
	STRING (dq_string lexbuf)
      }

  (* floats *)
  | ('-'? digit+ '.' digit*) as f
      {
	let f = float_of_string f in
	FLOAT f
      }

  (* integers *)
  | ('-'? digit+) as i
      {
	let i = Int64.of_string i in
	INT i
      }

  (* identifiers *)
  | ident as id
      { IDENT id }

  (* end of file *)
  | eof
      { EOF }

  | _ as c
      { raise (Error (sprintf "%c: invalid character in input" c)) }

and dq_string = parse
  | '"'
      { Buffer.contents tmp }
  | escaped_char as str
      { Buffer.add_char tmp str.[1]; dq_string lexbuf }
  | eof
      { raise (Error "unterminated string in metadata") }
  | _ as c
      { Buffer.add_char tmp c; dq_string lexbuf }

and q_string = parse
  | '\''
      { Buffer.contents tmp }
  | escaped_char as str
      { Buffer.add_char tmp str.[1]; q_string lexbuf }
  | eof
      { raise (Error "unterminated string in metadata") }
  | _ as c
      { Buffer.add_char tmp c; q_string lexbuf }

{
  (* Demonstration of how to wrap the token function
     with extra debugging statements:
  let token lexbuf =
    try
      let r = token lexbuf in
      if debug then
	eprintf "Lexer: token returned is %s\n"
	  (match r with
	   | LBRACE -> "LBRACE"
	   | RBRACE -> "RBRACE"
	   | LSQUARE -> "LSQUARE"
	   | RSQUARE -> "RSQUARE"
	   | EQ -> "EQ"
	   | COMMA -> "COMMA"
	   | STRING s -> sprintf "STRING(%S)" s
	   | INT i -> sprintf "INT(%Ld)" i
	   | FLOAT f -> sprintf "FLOAT(%g)" f
	   | IDENT s -> sprintf "IDENT(%s)" s
           | EOF -> "EOF");
      r
    with
      exn ->
	prerr_endline (Printexc.to_string exn);
	raise exn
  *)

  (* Lex and parse input.
   *
   * Return the parsed metadata structure if everything went to plan.
   * Raises [Error msg] if there was some parsing problem.
   *)
  let rec parse_lvm2_metadata_from_string str =
    let lexbuf = Lexing.from_string str in
    parse_lvm2_metadata lexbuf
  and parse_lvm2_metadata_from_channel chan =
    let lexbuf = Lexing.from_channel chan in
    parse_lvm2_metadata lexbuf
  and parse_lvm2_metadata lexbuf =
    try
      input token lexbuf
    with
    | Error _ as exn -> raise exn
    | Parsing.Parse_error -> raise (Error "Parse error")
    | exn -> raise (Error ("Exception: " ^ Printexc.to_string exn))
}
