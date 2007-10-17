(* virt-manager-like graphical management tool.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
*)

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

(* Given two lists, xs and ys, return a list of items which have been
 * added to ys, items which are the same, and items which have been
 * removed from ys.
 * Returns a triplet (list of added, list of same, list of removed).
 *)
let differences xs ys =
  let rec d = function
    | [], [] -> (* Base case. *)
	([], [], [])
    | [], ys -> (* All ys have been added. *)
	(ys, [], [])
    | xs, [] -> (* All xs have been removed. *)
	([], [], xs)
    | (x :: xs), (y :: ys) when x = y -> (* Not added or removed. *)
	let added, unchanged, removed = d (xs, ys) in
	added, x :: unchanged, removed
    | (x :: xs), ((y :: _) as ys) when x < y -> (* x removed. *)
	let added, unchanged, removed = d (xs, ys) in
	added, unchanged, x :: removed
    | ((x :: _) as xs), (y :: ys) (* when x > y *) -> (* y added. *)
	let added, unchanged, removed = d (xs, ys) in
	y :: added, unchanged, removed
  in
  d (List.sort compare xs, List.sort compare ys)

let string_of_domain_state = function
  | D.InfoNoState -> "unknown"
  | D.InfoRunning -> "running"
  | D.InfoBlocked -> "blocked"
  | D.InfoPaused -> "paused"
  | D.InfoShutdown -> "shutdown"
  | D.InfoShutoff -> "shutoff"
  | D.InfoCrashed -> "crashed"

(* Filter top level rows (only) in a tree_store.  If function f returns
 * true then the row remains, but if it returns false then the row is
 * removed.
 *)
let rec filter_top_level_rows (model : GTree.tree_store) f =
  match model#get_iter_first with
  | None -> ()
  | Some iter -> filter_rows model f iter

(* Filter rows in a tree_store at a particular level. *)
and filter_rows model f row =
  let keep = f row in
  let iter_still_valid =
    if not keep then model#remove row else model#iter_next row in
  if iter_still_valid then filter_rows model f row

(* Find the first top level row matching predicate f and return it. *)
let rec find_top_level_row (model : GTree.tree_store) f =
  match model#get_iter_first with
  | None -> raise Not_found (* no rows *)
  | Some row -> find_row model f row

(* Find the first row matching predicate f at a particular level. *)
and find_row model f row =
  if f row then row
  else if model#iter_next row then find_row model f row
  else raise Not_found

(* Iterate over top level rows (only) in a tree_store. *)
let rec iter_top_level_rows (model : GTree.tree_store) f =
  match model#get_iter_first with
  | None -> ()
  | Some iter -> iter_rows model f iter

(* Iterate over rows in a tree_store at a particular level. *)
and iter_rows model f row =
  f row;
  if model#iter_next row then iter_rows model f row
