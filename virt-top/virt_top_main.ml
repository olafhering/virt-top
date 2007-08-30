(* 'top'-like tool for libvirt domains.
 * $Id: virt_top_main.ml,v 1.2 2007/08/30 13:16:57 rjones Exp $
 *
 * Just contains the main function.
 *)

open Curses

open Virt_top

(* Note: make sure we catch any exceptions and clean up the display.
 *
 * Note (2): make sure all exit paths call the GC so that we can check
 * that all allocated resources are being counted properly (by running
 * the program under --debug ...).
 *)
let error =
  let state = start_up () in

  try
    main_loop state;
    endwin ();
    false
  with
  | Libvirt.Virterror err ->
      endwin ();
      prerr_endline (Libvirt.Virterror.to_string err);
      true
  | exn ->
      endwin ();
      prerr_endline ("Error: " ^ Printexc.to_string exn);
      true

let () =
  Gc.compact (); (* See note above. *)

  exit (if error then 1 else 0)
