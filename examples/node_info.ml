(* Simple demo program showing node info.
   Usage: node_info [URI]
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/
 *)

open Printf

module C = Libvirt.Connect

let () =
  try
    let name =
      if Array.length Sys.argv >= 2 then
	Some (Sys.argv.(1))
      else
	None in
    let conn = C.connect_readonly ?name () in

    (* Get node_info, hostname, etc. *)
    let node_info = C.get_node_info conn in

    printf "model = %s\n" node_info.C.model;
    printf "memory = %Ld K\n" node_info.C.memory;
    printf "cpus = %d\n" node_info.C.cpus;
    printf "mhz = %d\n" node_info.C.mhz;
    printf "nodes = %d\n" node_info.C.nodes;
    printf "sockets = %d\n" node_info.C.sockets;
    printf "cores = %d\n" node_info.C.cores;
    printf "threads = %d\n%!" node_info.C.threads;

    let hostname = C.get_hostname conn in

    printf "hostname = %s\n%!" hostname;

    let uri = C.get_uri conn in

    printf "uri = %s\n%!" uri

  with
    Libvirt.Virterror err ->
      eprintf "error: %s\n" (Libvirt.Virterror.to_string err)

let () =
  (* Run the garbage collector which is a good way to check for
   * memory corruption errors and reference counting issues in libvirt.
   *)
  Gc.compact ()
