(* 'df' command for virtual domains.
 *
 * Support for LVM2 PVs.
 *)

open Printf

(* Int64 operators for convenience. *)
let (+^) = Int64.add
let (-^) = Int64.sub
let ( *^ ) = Int64.mul
let (/^) = Int64.div

let probe_lvm2 target part_type fd start size =
  Virt_df.ProbeFailed "LVM2 not supported yet"

(* Register with main code. *)
let () =
  Virt_df.fs_register
    [ 0x8e ]				(* Partition type. *)
    probe_lvm2
