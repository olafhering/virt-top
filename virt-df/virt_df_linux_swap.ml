(* 'df' command for virtual domains.
 * $Id$
 *
 * Support for Linux swap partitions.
 *)

(* Int64 operators for convenience. *)
let (+^) = Int64.add
let (-^) = Int64.sub
let ( *^ ) = Int64.mul
let (/^) = Int64.div

let probe_swap target part_type fd start size =
  Virt_df.Swap {
    Virt_df.swap_name = "Linux swap";
    swap_block_size = 4096L;		(* XXX *)
    swap_blocks_total = size *^ 512L /^ 4096L;
  }

(* Register with main code. *)
let () =
  Virt_df.fs_register
    [ 0x82 ]				(* Partition type. *)
    probe_swap
