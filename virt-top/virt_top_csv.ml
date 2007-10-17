(* 'top'-like tool for libvirt domains.
 * $Id: virt_top_csv.ml,v 1.1 2007/08/23 11:09:19 rjones Exp $
 *
 * This file contains all code which requires CSV support.
 *)

(* Output channel, or None if CSV output not enabled. *)
let chan = ref None ;;

Virt_top.csv_start :=
  fun filename ->
    chan := Some (open_out filename) ;;

Virt_top.csv_write :=
  fun row ->
    match !chan with
    | None -> ()			(* CSV output not enabled. *)
    | Some chan ->
	Csv.save_out chan [row];
	(* Flush the output to the file immediately because we don't
	 * explicitly close this file.
	 *)
	flush chan
