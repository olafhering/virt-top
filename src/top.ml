(* 'top'-like tool for libvirt domains.
   (C) Copyright 2007-2017 Richard W.M. Jones, Red Hat Inc.
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

open Printf
open ExtList
open Curses

open Opt_gettext.Gettext
open Utils
open Types
open Collect
open Screen

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

let rcfile = ".virt-toprc"

(* Hooks for CSV support (see [opt_csv.ml]). *)
let csv_start : (string -> unit) ref =
  ref (
    fun _ -> failwith (s_"virt-top was compiled without support for CSV files")
  )

(* Hook for calendar support (see [opt_calendar.ml]). *)
let parse_date_time : (string -> float) ref =
  ref (
    fun _ ->
      failwith (s_"virt-top was compiled without support for dates and times")
  )

(* Init file. *)
type init_file = NoInitFile | DefaultInitFile | InitFile of string

(* Settings. *)
let quit = ref false
let delay = ref 3000 (* milliseconds *)
let historical_cpu_delay = ref 20 (* secs *)
let iterations = ref (-1)
let end_time = ref None
let batch_mode = ref false
let secure_mode = ref false
let sort_order = ref Processor
let display_mode = ref TaskDisplay
let uri = ref None
let debug_file = ref ""
let csv_enabled = ref false
let csv_cpu = ref true
let csv_mem = ref true
let csv_block = ref true
let csv_net = ref true
let init_file = ref DefaultInitFile
let script_mode = ref false
let stream_mode = ref false
let block_in_bytes = ref false

(* Function to read command line arguments and go into curses mode. *)
let start_up () =
  (* Read command line arguments. *)
  let rec set_delay newdelay =
    if newdelay <= 0. then
      failwith (s_"-d: cannot set a negative delay");
    delay := int_of_float (newdelay *. 1000.)
  and set_uri = function "" -> uri := None | u -> uri := Some u
  and set_sort order = sort_order := sort_order_of_cli order
  and set_pcpu_mode () = display_mode := PCPUDisplay
  and set_net_mode () = display_mode := NetDisplay
  and set_block_mode () = display_mode := BlockDisplay
  and set_csv filename =
    (!csv_start) filename;
    csv_enabled := true
  and no_init_file () = init_file := NoInitFile
  and set_init_file filename = init_file := InitFile filename
  and set_end_time time = end_time := Some ((!parse_date_time) time)
  and display_version () =
    printf "virt-top %s ocaml-libvirt %s\n"
      Version.version Libvirt_version.version;
    exit 0
  in
  let argspec = Arg.align [
    "-1", Arg.Unit set_pcpu_mode,
      " " ^ s_"Start by displaying pCPUs (default: tasks)";
    "-2", Arg.Unit set_net_mode,
      " " ^ s_"Start by displaying network interfaces";
    "-3", Arg.Unit set_block_mode,
      " " ^ s_"Start by displaying block devices";
    "-b", Arg.Set batch_mode,
      " " ^ s_"Batch mode";
    "-c", Arg.String set_uri,
      "uri " ^ s_"Connect to libvirt URI";
    "--connect", Arg.String set_uri,
      "uri " ^ s_"Connect to libvirt URI";
    "--csv", Arg.String set_csv,
      "file " ^ s_"Log statistics to CSV file";
    "--no-csv-cpu", Arg.Clear csv_cpu,
      " " ^ s_"Disable CPU stats in CSV";
    "--no-csv-mem", Arg.Clear csv_mem,
      " " ^ s_"Disable memory stats in CSV";
    "--no-csv-block", Arg.Clear csv_block,
      " " ^ s_"Disable block device stats in CSV";
    "--no-csv-net", Arg.Clear csv_net,
      " " ^ s_"Disable net stats in CSV";
    "-d", Arg.Float set_delay,
      "delay " ^ s_"Delay time interval (seconds)";
    "--debug", Arg.Set_string debug_file,
      "file " ^ s_"Send debug messages to file";
    "--end-time", Arg.String set_end_time,
      "time " ^ s_"Exit at given time";
    "--hist-cpu", Arg.Set_int historical_cpu_delay,
      "secs " ^ s_"Historical CPU delay";
    "--init-file", Arg.String set_init_file,
      "file " ^ s_"Set name of init file";
    "--no-init-file", Arg.Unit no_init_file,
      " " ^ s_"Do not read init file";
    "-n", Arg.Set_int iterations,
      "iterations " ^ s_"Number of iterations to run";
    "-o", Arg.String set_sort,
      "sort " ^ sprintf (f_"Set sort order (%s)")
        "cpu|mem|time|id|name|netrx|nettx|blockrdrq|blockwrrq";
    "-s", Arg.Set secure_mode,
      " " ^ s_"Secure (\"kiosk\") mode";
    "--script", Arg.Set script_mode,
      " " ^ s_"Run from a script (no user interface)";
    "--stream", Arg.Set stream_mode,
      " " ^ s_"dump output to stdout (no userinterface)";
    "--block-in-bytes", Arg.Set block_in_bytes,
      " " ^ s_"show block device load in bytes rather than reqs";
    "--version", Arg.Unit display_version,
      " " ^ s_"Display version number and exit";
  ] in
  let anon_fun str =
    raise (Arg.Bad (sprintf (f_"%s: unknown parameter") str)) in
  let usage_msg = s_"virt-top : a 'top'-like utility for virtualization

SUMMARY
  virt-top [-options]

OPTIONS" in
  Arg.parse argspec anon_fun usage_msg;

  (* Read the init file. *)
  let try_to_read_init_file filename =
    let config = read_config_file filename in
    (* Replacement functions that raise better errors when
     * parsing the init file.
     *)
    let int_of_string s =
      try int_of_string s
      with Invalid_argument _ ->
        failwithf (f_"%s: could not parse '%s' in init file: expecting an integer")
          filename s in
    let float_of_string s =
      try float_of_string s
      with Invalid_argument _ ->
        failwithf (f_"%s: could not parse '%s' in init file: expecting a number")
          filename s in
    let bool_of_string s =
      try bool_of_string s
      with Invalid_argument _ ->
        failwithf (f_"%s: could not parse '%s' in init file: expecting %s")
          filename s "true|false" in
    List.iter (
      function
      | _, "display", mode -> display_mode := display_of_cli mode
      | _, "delay", secs -> set_delay (float_of_string secs)
      | _, "hist-cpu", secs -> historical_cpu_delay := int_of_string secs
      | _, "iterations", n -> iterations := int_of_string n
      | _, "sort", order -> set_sort order
      | _, "connect", uri -> set_uri uri
      | _, "debug", filename -> debug_file := filename
      | _, "csv", filename -> set_csv filename
      | _, "csv-cpu", b -> csv_cpu := bool_of_string b
      | _, "csv-mem", b -> csv_mem := bool_of_string b
      | _, "csv-block", b -> csv_block := bool_of_string b
      | _, "csv-net", b -> csv_net := bool_of_string b
      | _, "batch", b -> batch_mode := bool_of_string b
      | _, "secure", b -> secure_mode := bool_of_string b
      | _, "script", b -> script_mode := bool_of_string b
      | _, "stream", b -> stream_mode := bool_of_string b
      | _, "block-in-bytes", b -> block_in_bytes := bool_of_string b
      | _, "end-time", t -> set_end_time t
      | _, "overwrite-init-file", "false" -> no_init_file ()
      | lineno, key, _ ->
	  eprintf (f_"%s:%d: configuration item ``%s'' ignored\n%!")
	    filename lineno key
    ) config
  in
  (match !init_file with
   | NoInitFile -> ()
   | DefaultInitFile ->
       let home = try Sys.getenv "HOME" with Not_found -> "/" in
       let filename = home // rcfile in
       try_to_read_init_file filename
   | InitFile filename ->
       try_to_read_init_file filename
  );

  (* Connect to the hypervisor before going into curses mode, since
   * this is the most likely thing to fail.
   *)
  let conn =
    let name = !uri in
    try C.connect_readonly ?name ()
    with
      Libvirt.Virterror err ->
	prerr_endline (Libvirt.Virterror.to_string err);
	(* If non-root and no explicit connection URI, print a warning. *)
	if Unix.geteuid () <> 0 && name = None then (
	  print_endline (s_"NB: If you want to monitor a local hypervisor, you usually need to be root");
	);
	exit 1 in

  (* Get the node_info.  This never changes, right?  So we get it just once. *)
  let node_info = C.get_node_info conn in

  (* Hostname and libvirt library version also don't change. *)
  let hostname =
    try C.get_hostname conn
    with
    (* qemu:/// and other URIs didn't support virConnectGetHostname until
     * libvirt 0.3.3.  Before that they'd throw a virterror. *)
    | Libvirt.Virterror _
    | Libvirt.Not_supported "virConnectGetHostname" -> "unknown" in

  let libvirt_version =
    let v, _ = Libvirt.get_version () in
    v / 1_000_000, (v / 1_000) mod 1_000, v mod 1_000 in

  (* Open debug file if specified.
   * NB: Do this just before jumping into curses mode.
   *)
  (match !debug_file with
   | "" -> (* No debug file specified, send stderr to /dev/null unless
	    * we're in script mode.
	    *)
       if not !script_mode && not !stream_mode then (
	 let fd = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o644 in
	 Unix.dup2 fd Unix.stderr;
	 Unix.close fd
       )
   | filename -> (* Send stderr to the named file. *)
       let fd =
	 Unix.openfile filename [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC]
	   0o644 in
       Unix.dup2 fd Unix.stderr;
       Unix.close fd
  );

  (* Curses voodoo (see ncurses(3)). *)
  if not !script_mode && not !stream_mode then (
    ignore (initscr ());
    ignore (cbreak ());
    ignore (noecho ());
    nonl ();
    let stdscr = stdscr () in
    ignore (intrflush stdscr false);
    ignore (keypad stdscr true);
    ()
  );

  (* This tuple of static information is called 'setup' in other parts
   * of this program, and is passed to other functions such as redraw and
   * main_loop.  See [main.ml].
   *)
  (conn,
   !batch_mode, !script_mode, !csv_enabled, !stream_mode, (* immutable modes *)
   node_info, hostname, libvirt_version (* info that doesn't change *)
  )

(* Sleep in seconds. *)
let sleep = Unix.sleep

(* Sleep in milliseconds. *)
let millisleep n =
  ignore (Unix.select [] [] [] (float n /. 1000.))

(* The curses getstr/getnstr functions are just weird.
 * This helper function also enables echo temporarily.
 *)
let get_string maxlen =
  ignore (echo ());
  let str = String.create maxlen in
  let ok = getstr str in (* Safe because binding calls getnstr. *)
  ignore (noecho ());
  if not ok then ""
  else (
    (* Chop at first '\0'. *)
    try
      let i = String.index str '\000' in
      String.sub str 0 i
    with
      Not_found -> str (* it is full maxlen bytes *)
  )

(* Main loop. *)
let rec main_loop ((_, batch_mode, script_mode, csv_enabled, stream_mode, _, _, _)
		     as setup) =
  let csv_flags = !csv_cpu, !csv_mem, !csv_block, !csv_net in

  if csv_enabled then
    Csv_output.write_csv_header csv_flags !block_in_bytes;

  while not !quit do
    (* Collect stats. *)
    let state = collect setup in
    let pcpu_display =
      if !display_mode = PCPUDisplay then Some (collect_pcpu state)
      else None in
    (* Redraw display. *)
    if not script_mode && not stream_mode then
      Redraw.redraw !display_mode !sort_order
                    setup !block_in_bytes !historical_cpu_delay
                    state pcpu_display;

    (* Update CSV file. *)
    if csv_enabled then
      Csv_output.append_csv setup csv_flags !block_in_bytes state;

    (* Append to stream output file. *)
    if stream_mode then
      Stream_output.append_stream setup !block_in_bytes state;

    (* Clear up unused virDomainPtr objects. *)
    Gc.compact ();

    (* Max iterations? *)
    if !iterations >= 0 then (
      decr iterations;
      if !iterations = 0 then quit := true
    );

    (* End time?  We might need to adjust the precise delay down if
     * the delay would be longer than the end time (RHBZ#637964).  Note
     * 'delay' is in milliseconds.
     *)
    let delay =
      match !end_time with
      | None ->
          (* No --end-time option, so use the current delay. *)
          !delay
      | Some end_time ->
	  let delay_secs = float !delay /. 1000. in
	  if end_time <= state.rd_time +. delay_secs then (
            quit := true;
            let delay = int_of_float (1000. *. (end_time -. state.rd_time)) in
            if delay >= 0 then delay else 0
          ) else
            !delay in
    (*eprintf "adjusted delay = %d\n%!" delay;*)

    (* Get next key.  This does the sleep. *)
    if not batch_mode && not script_mode && not stream_mode then
      get_key_press setup delay
    else (
      (* Batch mode, script mode, stream mode.  We didn't call
       * get_key_press, so we didn't sleep.  Sleep now, unless we are
       * about to quit.
       *)
      if not !quit || !end_time <> None then
	millisleep delay
    )
  done

and get_key_press setup delay =
  (* Read the next key, waiting up to 'delay' milliseconds. *)
  timeout delay;
  let k = getch () in
  timeout (-1); (* Reset to blocking mode. *)

  if k >= 0 && k <> 32 (* ' ' *) && k <> 12 (* ^L *) && k <> Key.resize
  then (
    if k = Char.code 'q' then quit := true
    else if k = Char.code 'h' then show_help setup
    else if k = Char.code 's' || k = Char.code 'd' then change_delay ()
    else if k = Char.code 'M' then sort_order := Memory
    else if k = Char.code 'P' then sort_order := Processor
    else if k = Char.code 'T' then sort_order := Time
    else if k = Char.code 'N' then sort_order := DomainID
    else if k = Char.code 'F' then change_sort_order ()
    else if k = Char.code '0' then set_tasks_display ()
    else if k = Char.code '1' then toggle_pcpu_display ()
    else if k = Char.code '2' then toggle_net_display ()
    else if k = Char.code '3' then toggle_block_display ()
    else if k = Char.code 'W' then write_init_file ()
    else if k = Char.code 'B' then toggle_block_in_bytes_mode ()
    else unknown_command k
  )

and change_delay () =
  print_msg
    (sprintf (f_"Change delay from %.1f to: ") (float !delay /. 1000.));
  let str = get_string 16 in
  (* Try to parse the number. *)
  let error =
    try
      let newdelay = float_of_string str in
      if newdelay <= 0. then (
	print_msg (s_"Delay must be > 0"); true
      ) else (
	delay := int_of_float (newdelay *. 1000.); false
      )
    with
      Failure "float_of_string" ->
	print_msg (s_"Not a valid number"); true in
  refresh ();
  sleep (if error then 2 else 1)

and change_sort_order () =
  clear ();
  let lines, cols = get_size () in

  mvaddstr top_lineno 0 (s_"Set sort order for main display");
  mvaddstr summary_lineno 0 (s_"Type key or use up and down cursor keys.");

  attron A.reverse;
  mvaddstr header_lineno 0 (pad cols "KEY   Sort field");
  attroff A.reverse;

  let accelerator_key = function
    | Memory -> "(key: M)"
    | Processor -> "(key: P)"
    | Time -> "(key: T)"
    | DomainID -> "(key: N)"
    | _ -> (* all others have to be changed from here *) ""
  in

  let rec key_of_int = function
    | i when i < 10 -> Char.chr (i + Char.code '0')
    | i when i < 20 -> Char.chr (i + Char.code 'a')
    | _ -> assert false
  and int_of_key = function
    | k when k >= 0x30 && k <= 0x39 (* '0' - '9' *) -> k - 0x30
    | k when k >= 0x61 && k <= 0x7a (* 'a' - 'j' *) -> k - 0x61 + 10
    | k when k >= 0x41 && k <= 0x6a (* 'A' - 'J' *) -> k - 0x41 + 10
    | _ -> -1
  in

  (* Display possible sort fields. *)
  let selected_index = ref 0 in
  List.iteri (
    fun i ord ->
      let selected = !sort_order = ord in
      if selected then selected_index := i;
      mvaddstr (domains_lineno+i) 0
	(sprintf "  %c %s %s %s"
	   (key_of_int i) (if selected then "*" else " ")
	   (printable_sort_order ord)
	   (accelerator_key ord))
  ) all_sort_fields;

  move message_lineno 0;
  refresh ();
  let k = getch () in
  if k >= 0 && k <> 32 && k <> Char.code 'q' && k <> 13 then (
    let new_order, loop =
      (* Redraw the display. *)
      if k = 12 (* ^L *) then None, true
      (* Make the UP and DOWN arrow keys do something useful. *)
      else if k = Key.up then (
	if !selected_index > 0 then
	  Some (List.nth all_sort_fields (!selected_index-1)), true
	else
	  None, true
      )
      else if k = Key.down then (
	if !selected_index < List.length all_sort_fields - 1 then
	  Some (List.nth all_sort_fields (!selected_index+1)), true
	else
	  None, true
      )
      (* Also understand the regular accelerator keys. *)
      else if k = Char.code 'M' then
	Some Memory, false
      else if k = Char.code 'P' then
	Some Processor, false
      else if k = Char.code 'T' then
	Some Time, false
      else if k = Char.code 'N' then
	Some DomainID, false
      else (
	(* It's one of the KEYs. *)
	let i = int_of_key k in
	if i >= 0 && i < List.length all_sort_fields then
	  Some (List.nth all_sort_fields i), false
	else
	  None, true
      ) in

    (match new_order with
     | None -> ()
     | Some new_order ->
	 sort_order := new_order;
	 print_msg (sprintf "Sort order changed to: %s"
		      (printable_sort_order new_order));
	 if not loop then (
	   refresh ();
	   sleep 1
	 )
    );

    if loop then change_sort_order ()
  )

(* Note: We need to clear_pcpu_display_data every time
 * we _leave_ PCPUDisplay mode.
 *)
and set_tasks_display () =		(* key 0 *)
  if !display_mode = PCPUDisplay then clear_pcpu_display_data ();
  display_mode := TaskDisplay

and toggle_pcpu_display () =		(* key 1 *)
  display_mode :=
    match !display_mode with
    | TaskDisplay | NetDisplay | BlockDisplay -> PCPUDisplay
    | PCPUDisplay -> clear_pcpu_display_data (); TaskDisplay

and toggle_net_display () =		(* key 2 *)
  display_mode :=
    match !display_mode with
    | PCPUDisplay -> clear_pcpu_display_data (); NetDisplay
    | TaskDisplay | BlockDisplay -> NetDisplay
    | NetDisplay -> TaskDisplay

and toggle_block_display () =		(* key 3 *)
  display_mode :=
    match !display_mode with
    | PCPUDisplay -> clear_pcpu_display_data (); BlockDisplay
    | TaskDisplay | NetDisplay -> BlockDisplay
    | BlockDisplay -> TaskDisplay

and toggle_block_in_bytes_mode () =      (* key B *)
  block_in_bytes :=
    match !block_in_bytes with
    | false -> true
    | true  -> false

(* Write an init file. *)
and write_init_file () =
  match !init_file with
  | NoInitFile -> ()			(* Do nothing if --no-init-file *)
  | DefaultInitFile ->
      let home = try Sys.getenv "HOME" with Not_found -> "/" in
      let filename = home // rcfile in
      _write_init_file filename
  | InitFile filename ->
      _write_init_file filename

and _write_init_file filename =
  try
    (* Create the new file as filename.new. *)
    let chan = open_out (filename ^ ".new") in

    let time = Unix.gettimeofday () in
    let tm = Unix.localtime time in
    let printable_date_time =
      sprintf "%04d-%02d-%02d %02d:%02d:%02d"
	(tm.Unix.tm_year + 1900) (tm.Unix.tm_mon+1) tm.Unix.tm_mday
	tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
    let username =
      try
	let uid = Unix.geteuid () in
	(Unix.getpwuid uid).Unix.pw_name
      with
	Not_found -> "unknown" in

    let fp = fprintf in
    let nl () = fp chan "\n" in
    let () = fp chan (f_"# %s virt-top configuration file\n") rcfile in
    let () = fp chan (f_"# generated on %s by %s\n") printable_date_time username in
    nl ();
    fp chan "display %s\n" (cli_of_display !display_mode);
    fp chan "delay %g\n" (float !delay /. 1000.);
    fp chan "hist-cpu %d\n" !historical_cpu_delay;
    if !iterations <> -1 then fp chan "iterations %d\n" !iterations;
    fp chan "sort %s\n" (cli_of_sort_order !sort_order);
    (match !uri with
     | None -> ()
     | Some uri -> fp chan "connect %s\n" uri
    );
    if !batch_mode = true then fp chan "batch true\n";
    if !secure_mode = true then fp chan "secure true\n";
    nl ();
    output_string chan (s_"# To send debug and error messages to a file, uncomment next line\n");
    fp chan "#debug virt-top.out\n";
    nl ();
    output_string chan (s_"# Enable CSV output to the named file\n");
    fp chan "#csv virt-top.csv\n";
    nl ();
    output_string chan (s_"# To protect this file from being overwritten, uncomment next line\n");
    fp chan "#overwrite-init-file false\n";

    close_out chan;

    (* If the file exists, rename it as filename.old. *)
    (try Unix.rename filename (filename ^ ".old")
     with Unix.Unix_error _ -> ());

    (* Rename filename.new to filename. *)
    Unix.rename (filename ^ ".new") filename;

    print_msg (sprintf (f_"Wrote settings to %s") filename);
    refresh ();
    sleep 2
  with
  | Sys_error err ->
      print_msg (s_"Error" ^ ": " ^ err);
      refresh (); sleep 2
  | Unix.Unix_error (err, fn, str) ->
      print_msg (s_"Error" ^ ": " ^
		   (Unix.error_message err) ^ " " ^ fn ^ " " ^ str);
      refresh ();
      sleep 2

and show_help (_, _, _, _, _, _, hostname,
	       (libvirt_major, libvirt_minor, libvirt_release)) =
  clear ();

  (* Get the screen/window size. *)
  let lines, cols = get_size () in

  (* Banner at the top of the screen. *)
  let banner =
    sprintf (f_"virt-top %s ocaml-libvirt %s libvirt %d.%d.%d by Red Hat")
      Version.version
      Libvirt_version.version
      libvirt_major libvirt_minor libvirt_release in
  let banner = pad cols banner in
  attron A.reverse;
  mvaddstr 0 0 banner;
  attroff A.reverse;

  (* Status. *)
  mvaddstr 1 0
    (sprintf
       (f_"Delay: %.1f secs; Batch: %s; Secure: %s; Sort: %s")
       (float !delay /. 1000.)
       (if !batch_mode then s_"On" else s_"Off")
       (if !secure_mode then s_"On" else s_"Off")
       (printable_sort_order !sort_order));
  mvaddstr 2 0
    (sprintf
       (f_"Connect: %s; Hostname: %s")
       (match !uri with None -> s_"default" | Some s -> s)
       hostname);

  (* Misc keys on left. *)
  let banner = pad 38 (s_"MAIN KEYS") in
  attron A.reverse;
  mvaddstr header_lineno 1 banner;
  attroff A.reverse;

  let get_lineno =
    let lineno = ref domains_lineno in
    fun () -> let i = !lineno in incr lineno; i
  in
  let key keys description =
    let lineno = get_lineno () in
    move lineno 1; attron A.bold; addstr keys; attroff A.bold;
    move lineno 10; addstr description
  in
  key "space ^L" (s_"Update display");
  key "q"        (s_"Quit");
  key "d s"      (s_"Set update interval");
  key "h"        (s_"Help");
  key "B"        (s_"toggle block info req/bytes");

  (* Sort order. *)
  ignore (get_lineno ());
  let banner = pad 38 (s_"SORTING") in
  attron A.reverse;
  mvaddstr (get_lineno ()) 1 banner;
  attroff A.reverse;

  key "P" (s_"Sort by %CPU");
  key "M" (s_"Sort by %MEM");
  key "T" (s_"Sort by TIME");
  key "N" (s_"Sort by ID");
  key "F" (s_"Select sort field");

  (* Display modes on right. *)
  let banner = pad 39 (s_"DISPLAY MODES") in
  attron A.reverse;
  mvaddstr header_lineno 40 banner;
  attroff A.reverse;

  let get_lineno =
    let lineno = ref domains_lineno in
    fun () -> let i = !lineno in incr lineno; i
  in
  let key keys description =
    let lineno = get_lineno () in
    move lineno 40; attron A.bold; addstr keys; attroff A.bold;
    move lineno 49; addstr description
  in
  key "0" (s_"Domains display");
  key "1" (s_"Toggle physical CPUs");
  key "2" (s_"Toggle network interfaces");
  key "3" (s_"Toggle block devices");

  (* Update screen and wait for key press. *)
  mvaddstr (lines-1) 0
    (s_"More help in virt-top(1) man page. Press any key to return.");
  refresh ();
  ignore (getch ())

and unknown_command k =
  print_msg (s_"Unknown command - try 'h' for help");
  refresh ();
  sleep 1
