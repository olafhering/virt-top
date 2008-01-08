(* Wrapper around 'gcc'.  On MinGW, this wrapper understands the '@...'
 * syntax for extending the command line.
 *)

open Printf
open Unix

let (//) = Filename.concat

(* Substitute any @... arguments with the file content. *)
let rec input_all_lines chan =
  try
    let line = input_line chan in
    line :: input_all_lines chan
  with
    End_of_file -> []

let argv = Array.map (
  fun arg ->
    if arg.[0] = '@' then (
      let chan = open_in (String.sub arg 1 (String.length arg - 1)) in
      let lines = input_all_lines chan in
      close_in chan;
      lines
    ) else
      [arg]
) Sys.argv

let argv = Array.to_list argv
let argv = List.flatten argv

(* Find the real gcc.exe on $PATH, but ignore any '.' elements in the path.
 * Note that on Windows, $PATH is split with ';' characters.
 *)
let rec split_find str sep f =
  try
    let i = String.index str sep in
    let n = String.length str in
    let str, str' = String.sub str 0 i, String.sub str (i+1) (n-i-1) in
    match f str with
    | None -> split_find str' sep f  (* not found, keep searching *)
    | Some found -> found
  with
    Not_found ->
      match f str with
      | None -> raise Not_found (* not found at all *)
      | Some found -> found

let exists filename =
  try access filename [F_OK]; true with Unix_error _ -> false

let gcc =
  split_find (Sys.getenv "PATH") ';'
    (function
     | "." -> None (* ignore current directory in path *)
     | path ->
       let gcc = path // "gcc.exe" in
       if exists gcc then Some gcc else None)

(* Finally execute the real gcc with the full argument list.
 * Can't use execv here because then the parent process (ocamlopt) thinks
 * that this process has finished and deletes all the temp files.  Stupid
 * Windoze!
 *)
let _ =
  let argv = List.map Filename.quote (List.tl argv) in
  let cmd = String.concat " " (gcc :: argv) in
  eprintf "mingw-gcc-wrapper: %s\n%!" cmd;
  let r = Sys.command cmd in
  exit r
