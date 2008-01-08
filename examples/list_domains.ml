(* Simple demo program showing how to list out domains.
   Usage: list_domains [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

let () =
  try
    let name =
      if Array.length Sys.argv >= 2 then
	Some (Sys.argv.(1))
      else
	None in
    let conn = C.connect_readonly ?name () in

    (* List running domains. *)
    let n = C.num_of_domains conn in
    let ids = C.list_domains conn n in
    let domains = Array.map (D.lookup_by_id conn) ids in
    Array.iter (
      fun dom ->
	printf "%8d %s\n%!" (D.get_id dom) (D.get_name dom)
    ) domains;

    (* List inactive domains. *)
    let n = C.num_of_defined_domains conn in
    let names = C.list_defined_domains conn n in
    Array.iter (
      fun name ->
	printf "inactive %s\n%!" name
    ) names;
  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  (* Run the garbage collector which is a good way to check for
   * memory corruption errors and reference counting issues in libvirt.
   *)
  Gc.compact ()
