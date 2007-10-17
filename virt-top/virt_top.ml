(* 'top'-like tool for libvirt domains.
 *)

open Printf
open ExtList
open Curses

open Virt_top_utils

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

(* Hook for XML support (see virt_top_xml.ml). *)
let parse_device_xml : (int -> [>`R] D.t -> string list * string list) ref =
  ref (
    fun _ _ -> [], []
  )

(* Hooks for CSV support (see virt_top_csv.ml). *)
let csv_start : (string -> unit) ref =
  ref (
    fun _ -> failwith "virt-top was compiled without support for CSV"
  )
let csv_write : (string list -> unit) ref =
  ref (
    fun _ -> ()
  )

(* Int64 operators for convenience. *)
let (+^) = Int64.add
let (-^) = Int64.sub
let ( *^ ) = Int64.mul
let (/^) = Int64.div

(* Sort order. *)
type sort_order =
  | DomainID | DomainName | Processor | Memory | Time
  | NetRX | NetTX | BlockRdRq | BlockWrRq
let all_sort_fields = [
  DomainID; DomainName; Processor; Memory; Time;
  NetRX; NetTX; BlockRdRq; BlockWrRq
]
let printable_sort_order = function
  | Processor -> "%CPU"
  | Memory -> "%MEM"
  | Time -> "TIME (CPU time)"
  | DomainID -> "Domain ID"
  | DomainName -> "Domain name"
  | NetRX -> "Net RX bytes"
  | NetTX -> "Net TX bytes"
  | BlockRdRq -> "Block read reqs"
  | BlockWrRq -> "Block write reqs"
let sort_order_of_cli = function
  | "cpu" | "processor" -> Processor
  | "mem" | "memory" -> Memory
  | "time" -> Time
  | "id" -> DomainID
  | "name" -> DomainName
  | "netrx" -> NetRX | "nettx" -> NetTX
  | "blockrdrq" -> BlockRdRq | "blockwrrq" -> BlockWrRq
  | str -> failwith (str ^ ": sort order should be: cpu|mem|time|id|name|netrx|nettx|blockrdrq|blockwrrq")
let cli_of_sort_order = function
  | Processor -> "cpu"
  | Memory -> "mem"
  | Time -> "time"
  | DomainID -> "id"
  | DomainName -> "name"
  | NetRX -> "netrx"
  | NetTX -> "nettx"
  | BlockRdRq -> "blockrdrq"
  | BlockWrRq -> "blockwrrq"

(* Current major display mode: TaskDisplay is the normal display. *)
type display = TaskDisplay | PCPUDisplay | BlockDisplay | NetDisplay

let display_of_cli = function
  | "task" -> TaskDisplay
  | "pcpu" -> PCPUDisplay
  | "block" -> BlockDisplay
  | "net" -> NetDisplay
  | str -> failwith (str ^ ": display should be task|pcpu|block|net")
let cli_of_display = function
  | TaskDisplay -> "task"
  | PCPUDisplay -> "pcpu"
  | BlockDisplay -> "block"
  | NetDisplay -> "net"

(* Init file. *)
type init_file = NoInitFile | DefaultInitFile | InitFile of string

(* Settings. *)
let quit = ref false
let delay = ref 3000 (* milliseconds *)
let historical_cpu_delay = ref 20 (* secs *)
let iterations = ref (-1)
let batch_mode = ref false
let secure_mode = ref false
let sort_order = ref Processor
let display_mode = ref TaskDisplay
let uri = ref None
let debug_file = ref ""
let csv_enabled = ref false
let csv_cpu = ref true
let csv_block = ref true
let csv_net = ref true
let init_file = ref DefaultInitFile
let script_mode = ref false

(* Function to read command line arguments and go into curses mode. *)
let start_up () =
  (* Read command line arguments. *)
  let rec set_delay newdelay =
    if newdelay <= 0. then
      failwith "-d: cannot set a negative delay";
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
  in
  let argspec = Arg.align [
    "-1", Arg.Unit set_pcpu_mode, " Start by displaying pCPUs (default: tasks)";
    "-2", Arg.Unit set_net_mode, " Start by displaying network interfaces";
    "-3", Arg.Unit set_block_mode, " Start by displaying block devices";
    "-b", Arg.Set batch_mode, " Batch mode";
    "-c", Arg.String set_uri, "uri Connect to URI (default: Xen)";
    "--connect", Arg.String set_uri, "uri Connect to URI (default: Xen)";
    "--csv", Arg.String set_csv, "file Log statistics to CSV file";
    "--no-csv-cpu", Arg.Clear csv_cpu, " Disable CPU stats in CSV";
    "--no-csv-block", Arg.Clear csv_block, " Disable block device stats in CSV";
    "--no-csv-net", Arg.Clear csv_net, " Disable net stats in CSV";
    "-d", Arg.Float set_delay, "delay Delay time interval (seconds)";
    "--debug", Arg.Set_string debug_file, "file Send debug messages to file";
    "--hist-cpu", Arg.Set_int historical_cpu_delay, "secs Historical CPU delay";
    "--init-file", Arg.String set_init_file, "file Set name of init file";
    "--no-init-file", Arg.Unit no_init_file, " Do not read init file";
    "-n", Arg.Set_int iterations, "iterations Number of iterations to run";
    "-o", Arg.String set_sort, "sort Set sort order (cpu|mem|time|id|name)";
    "-s", Arg.Set secure_mode, " Secure (\"kiosk\") mode";
    "--script", Arg.Set script_mode, " Run from a script (no user interface)";
  ] in
  let anon_fun str = raise (Arg.Bad (str ^ ": unknown parameter")) in
  let usage_msg = "virt-top : a 'top'-like utility for virtualization

SUMMARY
  virt-top [-options]

OPTIONS" in
  Arg.parse argspec anon_fun usage_msg;

  (* Read the init file. *)
  let try_to_read_init_file filename =
    let config = read_config_file filename in
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
      | _, "csv-block", b -> csv_block := bool_of_string b
      | _, "csv-net", b -> csv_net := bool_of_string b
      | _, "batch", b -> batch_mode := bool_of_string b
      | _, "secure", b -> secure_mode := bool_of_string b
      | _, "script", b -> script_mode := bool_of_string b
      | _, "overwrite-init-file", "false" -> no_init_file ()
      | lineno, key, _ ->
	  eprintf "%s:%d: configuration item ``%s'' ignored\n%!"
	    filename lineno key
    ) config
  in
  (match !init_file with
   | NoInitFile -> ()
   | DefaultInitFile ->
       let home = try Sys.getenv "HOME" with Not_found -> "/" in
       let filename = home // ".virt-toprc" in
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
	  print_endline "NB: If you want to monitor a local Xen hypervisor, you usually need to be root";
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
    | Invalid_argument "virConnectGetHostname not supported" -> "unknown" in

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
       if not !script_mode then (
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
  if not !script_mode then (
    initscr ();
    cbreak ();
    noecho ();
    nonl ();
    let stdscr = stdscr () in
    intrflush stdscr false;
    keypad stdscr true;
    ()
  );

  (* This tuple of static information is called 'setup' in other parts
   * of this program, and is passed to other functions such as redraw and
   * main_loop.  See virt_top_main.ml.
   *)
  (conn,
   !batch_mode, !script_mode, !csv_enabled, (* immutable modes *)
   node_info, hostname, libvirt_version (* info that doesn't change *)
  )

(* Show a percentage in 4 chars. *)
let show_percent percent =
  if percent <= 0. then " 0.0"
  else if percent <= 9.9 then sprintf " %1.1f" percent
  else if percent <= 99.9 then sprintf "%2.1f" percent
  else "100 "

(* Show an int64 option in 4 chars. *)
let rec show_int64_option = function
  | None -> "    "
  | Some n -> show_int64 n
(* Show an int64 in 4 chars. *)
and show_int64 = function
  | n when n < 0L -> "-!!!"
  | n when n <= 9999L ->
      sprintf "%4Ld" n
  | n when n /^ 1024L <= 999L ->
      sprintf "%3LdK" (n /^ 1024L)
  | n when n /^ 1_048_576L <= 999L ->
      sprintf "%3LdM" (n /^ 1_048_576L)
  | n when n /^ 1_073_741_824L <= 999L ->
      sprintf "%3LdG" (n /^ 1_073_741_824L)
  | _ -> ">!!!"

(* Format the total time (may be large!) in 9 chars. *)
let show_time ns =
  let secs_in_ns = 1_000_000_000L in
  let mins_in_ns = 60_000_000_000L in
  let hours_in_ns = 3_600_000_000_000L in

  let hours = ns /^ hours_in_ns in
  let ns = ns -^ (hours *^ hours_in_ns) in
  let mins = ns /^ mins_in_ns in
  let ns = ns -^ (mins *^ mins_in_ns) in
  let secs = ns /^ secs_in_ns in
  let ns = ns -^ (secs *^ secs_in_ns) in
  let pennies = ns /^ 10_000_000L in

  if hours < 12L then
    sprintf "%3Ld:%02Ld.%02Ld" (hours *^ 60L +^ mins) secs pennies
  else if hours <= 999L then
    sprintf "%3Ld:%02Ld:%02Ld" hours mins secs
  else (
    let days = hours /^ 24L in
    let hours = hours -^ (days *^ 24L) in
    sprintf "%3Ldd%02Ld:%02Ld" days hours mins
  )

(* Show a domain state (the 'S' column). *)
let show_state = function
  | D.InfoNoState -> '?'
  | D.InfoRunning -> 'R'
  | D.InfoBlocked -> 'S'
  | D.InfoPaused -> 'P'
  | D.InfoShutdown -> 'D'
  | D.InfoShutoff -> 'O'
  | D.InfoCrashed -> 'X'

(* Sum Domain.block_stats structures together.  Missing fields
 * get forced to 0.  Empty list returns all 0.
 *)
let zero_block_stats =
  { D.rd_req = 0L; rd_bytes = 0L; wr_req = 0L; wr_bytes = 0L; errs = 0L }
let add_block_stats bs1 bs2 =
  let add f1 f2 = if f1 >= 0L && f2 >= 0L then f1 +^ f2 else 0L in
  { D.rd_req = add bs1.D.rd_req   bs2.D.rd_req;
    rd_bytes = add bs1.D.rd_bytes bs2.D.rd_bytes;
    wr_req   = add bs1.D.wr_req   bs2.D.wr_req;
    wr_bytes = add bs1.D.wr_bytes bs2.D.wr_bytes;
    errs     = add bs1.D.errs     bs2.D.errs }
let sum_block_stats =
  List.fold_left add_block_stats zero_block_stats

(* Get the difference between two block_stats structures.  Missing data
 * forces the difference to -1.
 *)
let diff_block_stats curr prev =
  let sub f1 f2 = if f1 >= 0L && f2 >= 0L then f1 -^ f2 else -1L in
  { D.rd_req = sub curr.D.rd_req   prev.D.rd_req;
    rd_bytes = sub curr.D.rd_bytes prev.D.rd_bytes;
    wr_req   = sub curr.D.wr_req   prev.D.wr_req;
    wr_bytes = sub curr.D.wr_bytes prev.D.wr_bytes;
    errs     = sub curr.D.errs     prev.D.errs }

(* Sum Domain.interface_stats structures together.  Missing fields
 * get forced to 0.  Empty list returns all 0.
 *)
let zero_interface_stats =
  { D.rx_bytes = 0L; rx_packets = 0L; rx_errs = 0L; rx_drop = 0L;
    tx_bytes = 0L; tx_packets = 0L; tx_errs = 0L; tx_drop = 0L }
let add_interface_stats is1 is2 =
  let add f1 f2 = if f1 >= 0L && f2 >= 0L then f1 +^ f2 else 0L in
  { D.rx_bytes = add is1.D.rx_bytes   is2.D.rx_bytes;
    rx_packets = add is1.D.rx_packets is2.D.rx_packets;
    rx_errs    = add is1.D.rx_errs    is2.D.rx_errs;
    rx_drop    = add is1.D.rx_drop    is2.D.rx_drop;
    tx_bytes   = add is1.D.tx_bytes   is2.D.tx_bytes;
    tx_packets = add is1.D.tx_packets is2.D.tx_packets;
    tx_errs    = add is1.D.tx_errs    is2.D.tx_errs;
    tx_drop    = add is1.D.tx_drop    is2.D.tx_drop }
let sum_interface_stats =
  List.fold_left add_interface_stats zero_interface_stats

(* Get the difference between two interface_stats structures.
 * Missing data forces the difference to -1.
 *)
let diff_interface_stats curr prev =
  let sub f1 f2 = if f1 >= 0L && f2 >= 0L then f1 -^ f2 else -1L in
  { D.rx_bytes = sub curr.D.rx_bytes   prev.D.rx_bytes;
    rx_packets = sub curr.D.rx_packets prev.D.rx_packets;
    rx_errs    = sub curr.D.rx_errs    prev.D.rx_errs;
    rx_drop    = sub curr.D.rx_drop    prev.D.rx_drop;
    tx_bytes   = sub curr.D.tx_bytes   prev.D.tx_bytes;
    tx_packets = sub curr.D.tx_packets prev.D.tx_packets;
    tx_errs    = sub curr.D.tx_errs    prev.D.tx_errs;
    tx_drop    = sub curr.D.tx_drop    prev.D.tx_drop }

(* Update the display and sleep for given number of seconds. *)
let sleep n = refresh (); Unix.sleep n

(* The curses getstr/getnstr functions are just weird.
 * This helper function also enables echo temporarily.
 *)
let get_string maxlen =
  echo ();
  let str = String.create maxlen in
  let ok = getstr str in (* Safe because binding calls getnstr. *)
  noecho ();
  if not ok then ""
  else (
    (* Chop at first '\0'. *)
    try
      let i = String.index str '\000' in
      String.sub str 0 i
    with
      Not_found -> str (* it is full maxlen bytes *)
  )

(* Pad a string to the full width with spaces.  If too long, truncate. *)
let pad width str =
  let n = String.length str in
  if n = width then str
  else if n > width then String.sub str 0 width
  else (* if n < width then *) str ^ String.make (width-n) ' '

(* Line numbers. *)
let top_lineno = 0
let summary_lineno = 1 (* this takes 2 lines *)
let message_lineno = 3
let header_lineno = 4
let domains_lineno = 5

(* Print in the "message area". *)
let clear_msg () = move message_lineno 0; clrtoeol ()
let print_msg str = clear_msg (); mvaddstr message_lineno 0 str; ()

(* Intermediate "domain + stats" structure that we use to collect
 * everything we know about a domain within the collect function.
 *)
type rd_domain = Inactive | Active of rd_active
and rd_active = {
  rd_domid : int;			(* Domain ID. *)
  rd_dom : [`R] D.t;			(* Domain object. *)
  rd_info : D.info;			(* Domain CPU info now. *)
  rd_block_stats : (string * D.block_stats) list;
                                        (* Domain block stats now. *)
  rd_interface_stats : (string * D.interface_stats) list;
                                        (* Domain net stats now. *)
  rd_prev_info : D.info option;		(* Domain CPU info previously. *)
  rd_prev_block_stats : (string * D.block_stats) list;
                                        (* Domain block stats prev. *)
  rd_prev_interface_stats : (string * D.interface_stats) list;
                                        (* Domain interface stats prev. *)
  (* The following are since the last slice, or 0 if cannot be calculated: *)
  rd_cpu_time : float;			(* CPU time used in nanoseconds. *)
  rd_percent_cpu : float;		(* CPU time as percent of total. *)
  (* The following are since the last slice, or None if cannot be calc'd: *)
  rd_block_rd_reqs : int64 option;      (* Number of block device read rqs. *)
  rd_block_wr_reqs : int64 option;      (* Number of block device write rqs. *)
  rd_net_rx_bytes : int64 option;	(* Number of bytes received. *)
  rd_net_tx_bytes : int64 option;	(* Number of bytes transmitted. *)
}

(* Collect stats. *)
let collect, clear_pcpu_display_data =
  (* We cache the list of block devices and interfaces for each domain
   * here, so we don't need to reparse the XML each time.
   *)
  let devices = Hashtbl.create 13 in

  (* Function to get the list of block devices, network interfaces for
   * a particular domain.  Get it from the devices cache, and if not
   * there then parse the domain XML.
   *)
  let get_devices id dom =
    try Hashtbl.find devices id
    with Not_found ->
      let blkdevs, netifs = (!parse_device_xml) id dom in
      Hashtbl.replace devices id (blkdevs, netifs);
      blkdevs, netifs
  in

  (* We save the state of domains across redraws here, which allows us
   * to deduce %CPU usage from the running total.
   *)
  let last_info = Hashtbl.create 13 in
  let last_time = ref (Unix.gettimeofday ()) in

  (* Save vcpuinfo structures across redraws too (only for pCPU display). *)
  let last_vcpu_info = Hashtbl.create 13 in

  let clear_pcpu_display_data () =
    (* Clear out vcpu_info used by PCPUDisplay display_mode
     * when we switch back to TaskDisplay mode.
     *)
    Hashtbl.clear last_vcpu_info
  in

  let collect (conn, _, _, _, node_info, _, _) =
    (* Number of physical CPUs (some may be disabled). *)
    let nr_pcpus = C.maxcpus_of_node_info node_info in

    (* Get the current time. *)
    let time = Unix.gettimeofday () in
    let tm = Unix.localtime time in
    let printable_time =
      sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
    mvaddstr top_lineno 0 ("virt-top " ^ printable_time ^ " - ");

    (* What's the total CPU time elapsed since we were last called? (ns) *)
    let total_cpu_per_pcpu = 1_000_000_000. *. (time -. !last_time) in
    (* Avoid division by zero. *)
    let total_cpu_per_pcpu =
      if total_cpu_per_pcpu <= 0. then 1. else total_cpu_per_pcpu in
    let total_cpu = float node_info.C.cpus *. total_cpu_per_pcpu in

    (* Get the domains.  Match up with their last_info (if any). *)
    let doms =
      (* Active domains. *)
      let n = C.num_of_domains conn in
      let ids =
	if n > 0 then Array.to_list (C.list_domains conn n)
	else [] in
      let doms =
	List.filter_map (
	  fun id ->
	    try
	      let dom = D.lookup_by_id conn id in
	      let name = D.get_name dom in
	      let blkdevs, netifs = get_devices id dom in

	      (* Get current CPU, block and network stats. *)
	      let info = D.get_info dom in
	      let block_stats =
		try List.map (fun dev -> dev, D.block_stats dom dev) blkdevs
		with
		| Invalid_argument "virDomainBlockStats not supported"
		| Libvirt.Virterror _ -> [] in
	      let interface_stats =
		try List.map (fun dev -> dev, D.interface_stats dom dev) netifs
		with
		| Invalid_argument "virDomainInterfaceStats not supported"
		| Libvirt.Virterror _ -> [] in

	      let prev_info, prev_block_stats, prev_interface_stats =
		try
		  let prev_info, prev_block_stats, prev_interface_stats =
		    Hashtbl.find last_info id in
		  Some prev_info, prev_block_stats, prev_interface_stats
		with Not_found -> None, [], [] in

	      Some (name, Active {
		      rd_domid = id; rd_dom = dom; rd_info = info;
		      rd_block_stats = block_stats;
		      rd_interface_stats = interface_stats;
		      rd_prev_info = prev_info;
		      rd_prev_block_stats = prev_block_stats;
		      rd_prev_interface_stats = prev_interface_stats;
		      rd_cpu_time = 0.; rd_percent_cpu = 0.;
		      rd_block_rd_reqs = None; rd_block_wr_reqs = None;
		      rd_net_rx_bytes = None; rd_net_tx_bytes = None;
		    })
	    with
	      Libvirt.Virterror _ -> None (* ignore transient error *)
	) ids in

      (* Inactive domains. *)
      let doms_inactive =
	try
	  let n = C.num_of_defined_domains conn in
	  let names =
	    if n > 0 then Array.to_list (C.list_defined_domains conn n)
	    else [] in
	  List.map (fun name -> name, Inactive) names
	with
	  (* Ignore transient errors, in particular errors from
	   * num_of_defined_domains if it cannot contact xend.
	   *)
	| Libvirt.Virterror _ -> [] in

      doms @ doms_inactive in

    (* Calculate the CPU time (ns) and %CPU used by each domain. *)
    let doms =
      List.map (
	function
	(* We have previous CPU info from which to calculate it? *)
	| name, Active ({ rd_prev_info = Some prev_info } as rd) ->
	    let cpu_time =
	      Int64.to_float (rd.rd_info.D.cpu_time -^ prev_info.D.cpu_time) in
	    let percent_cpu = 100. *. cpu_time /. total_cpu in
	    let rd = { rd with
			 rd_cpu_time = cpu_time;
			 rd_percent_cpu = percent_cpu } in
	    name, Active rd
	(* For all other domains we can't calculate it, so leave as 0 *)
	| rd -> rd
      ) doms in

    (* Calculate the number of block device read/write requests across
     * all block devices attached to a domain.
     *)
    let doms =
      List.map (
	function
	(* Do we have stats from the previous slice? *)
	| name, Active ({ rd_prev_block_stats = ((_::_) as prev_block_stats) }
			  as rd) ->
	    let block_stats = rd.rd_block_stats in (* stats now *)

	    (* Add all the devices together.  Throw away device names. *)
	    let prev_block_stats =
	      sum_block_stats (List.map snd prev_block_stats) in
	    let block_stats =
	      sum_block_stats (List.map snd block_stats) in

	    (* Calculate increase in read & write requests. *)
	    let read_reqs =
	      block_stats.D.rd_req -^ prev_block_stats.D.rd_req in
	    let write_reqs =
	      block_stats.D.wr_req -^ prev_block_stats.D.wr_req in

	    let rd = { rd with
			 rd_block_rd_reqs = Some read_reqs;
			 rd_block_wr_reqs = Some write_reqs } in
	    name, Active rd
	(* For all other domains we can't calculate it, so leave as None. *)
	| rd -> rd
      ) doms in

    (* Calculate the same as above for network interfaces across
     * all network interfaces attached to a domain.
     *)
    let doms =
      List.map (
	function
	(* Do we have stats from the previous slice? *)
	| name, Active ({ rd_prev_interface_stats =
			      ((_::_) as prev_interface_stats) }
			  as rd) ->
	    let interface_stats = rd.rd_interface_stats in (* stats now *)

	    (* Add all the devices together.  Throw away device names. *)
	    let prev_interface_stats =
	      sum_interface_stats (List.map snd prev_interface_stats) in
	    let interface_stats =
	      sum_interface_stats (List.map snd interface_stats) in

	    (* Calculate increase in rx & tx bytes. *)
	    let rx_bytes =
	      interface_stats.D.rx_bytes -^ prev_interface_stats.D.rx_bytes in
	    let tx_bytes =
	      interface_stats.D.tx_bytes -^ prev_interface_stats.D.tx_bytes in

	    let rd = { rd with
			 rd_net_rx_bytes = Some rx_bytes;
			 rd_net_tx_bytes = Some tx_bytes } in
	    name, Active rd
	(* For all other domains we can't calculate it, so leave as None. *)
	| rd -> rd
      ) doms in

    (* Collect some extra information in PCPUDisplay display_mode. *)
    let pcpu_display =
      if !display_mode = PCPUDisplay then (
	(* Get the VCPU info and VCPU->PCPU mappings for active domains.
	 * Also cull some data we don't care about.
	 *)
	let doms = List.filter_map (
	  function
	  | (name, Active rd) ->
	      (try
		 let domid = rd.rd_domid in
		 let maplen = C.cpumaplen nr_pcpus in
		 let maxinfo = rd.rd_info.D.nr_virt_cpu in
		 let nr_vcpus, vcpu_infos, cpumaps =
		   D.get_vcpus rd.rd_dom maxinfo maplen in

		 (* Got previous vcpu_infos for this domain? *)
		 let prev_vcpu_infos =
		   try Some (Hashtbl.find last_vcpu_info domid)
		   with Not_found -> None in
		 (* Update last_vcpu_info. *)
		 Hashtbl.replace last_vcpu_info domid vcpu_infos;

		 (match prev_vcpu_infos with
		  | Some prev_vcpu_infos
		      when Array.length prev_vcpu_infos = Array.length vcpu_infos ->
		      Some (domid, name, nr_vcpus, vcpu_infos, prev_vcpu_infos,
			    cpumaps, maplen)
		  | _ -> None (* ignore missing / unequal length prev_vcpu_infos *)
		 );
	       with
		 Libvirt.Virterror _ -> None(* ignore transient libvirt errs *)
	      )
	  | (_, Inactive) -> None (* ignore inactive doms *)
	) doms in
	let nr_doms = List.length doms in

	(* Rearrange the data into a matrix.  Major axis (down) is
	 * pCPUs.  Minor axis (right) is domains.  At each node we store:
	 *  cpu_time (on this pCPU only, nanosecs),
	 *  average? (if set, then cpu_time is an average because the
	 *     vCPU is pinned to more than one pCPU)
	 *  running? (if set, we were instantaneously running on this pCPU)
	 *)
	let empty_node = (0L, false, false) in
	let pcpus = Array.make_matrix nr_pcpus nr_doms empty_node in

	List.iteri (
	  fun di (domid, name, nr_vcpus, vcpu_infos, prev_vcpu_infos,
		  cpumaps, maplen) ->
	    (* Which pCPUs can this dom run on? *)
	    for v = 0 to nr_vcpus-1 do
	      let pcpu = vcpu_infos.(v).D.cpu in (* instantaneous pCPU *)
	      let nr_poss_pcpus = ref 0 in (* how many pcpus can it run on? *)
	      for p = 0 to nr_pcpus-1 do
		(* vcpu v can reside on pcpu p *)
		if C.cpu_usable cpumaps maplen v p then
		  incr nr_poss_pcpus
	      done;
	      let nr_poss_pcpus = Int64.of_int !nr_poss_pcpus in
	      for p = 0 to nr_pcpus-1 do
		(* vcpu v can reside on pcpu p *)
		if C.cpu_usable cpumaps maplen v p then
		  let vcpu_time_on_pcpu =
		    vcpu_infos.(v).D.vcpu_time
		    -^ prev_vcpu_infos.(v).D.vcpu_time in
		  let vcpu_time_on_pcpu =
		    vcpu_time_on_pcpu /^ nr_poss_pcpus in
		  pcpus.(p).(di) <-
		    (vcpu_time_on_pcpu, nr_poss_pcpus > 1L, p = pcpu)
	      done
	    done
	) doms;

	(* Sum the CPU time used by each pCPU, for the %CPU column. *)
	let pcpus_cpu_time = Array.map (
	  fun row ->
	    let cpu_time = ref 0L in
	    for di = 0 to Array.length row-1 do
	      let t, _, _ = row.(di) in
	      cpu_time := !cpu_time +^ t
	    done;
	    Int64.to_float !cpu_time
	) pcpus in

	Some (doms, pcpus, pcpus_cpu_time)
      ) else
	None in

    (* Calculate totals. *)
    let totals = List.fold_left (
      fun (count, running, blocked, paused, shutdown, shutoff,
	   crashed, active, inactive,
	   total_cpu_time, total_memory, total_domU_memory) ->
	function
	| (name, Active rd) ->
	    let test state orig =
	      if rd.rd_info.D.state = state then orig+1 else orig
	    in
	    let running = test D.InfoRunning running in
	    let blocked = test D.InfoBlocked blocked in
	    let paused = test D.InfoPaused paused in
	    let shutdown = test D.InfoShutdown shutdown in
	    let shutoff = test D.InfoShutoff shutoff in
	    let crashed = test D.InfoCrashed crashed in

	    let total_cpu_time = total_cpu_time +. rd.rd_cpu_time in
	    let total_memory = total_memory +^ rd.rd_info.D.memory in
	    let total_domU_memory = total_domU_memory +^
	      if rd.rd_domid > 0 then rd.rd_info.D.memory else 0L in

	    (count+1, running, blocked, paused, shutdown, shutoff,
	     crashed, active+1, inactive,
	     total_cpu_time, total_memory, total_domU_memory)

	| (name, Inactive) -> (* inactive domain *)
	    (count+1, running, blocked, paused, shutdown, shutoff,
	     crashed, active, inactive+1,
	     total_cpu_time, total_memory, total_domU_memory)
    ) (0,0,0,0,0,0,0,0,0, 0.,0L,0L) doms in

    (* Update last_time, last_info. *)
    last_time := time;
    Hashtbl.clear last_info;
    List.iter (
      function
      | (_, Active rd) ->
	  let info = rd.rd_info, rd.rd_block_stats, rd.rd_interface_stats in
	  Hashtbl.add last_info rd.rd_domid info
      | _ -> ()
    ) doms;

    (doms,
     time, printable_time,
     nr_pcpus, total_cpu, total_cpu_per_pcpu,
     totals,
     pcpu_display)
  in

  collect, clear_pcpu_display_data

(* Redraw the display. *)
let redraw =
  (* Keep a historical list of %CPU usages. *)
  let historical_cpu = ref [] in
  let historical_cpu_last_time = ref (Unix.gettimeofday ()) in
  fun
  (_, _, _, _, node_info, _, _) (* setup *)
  (doms,
   time, printable_time,
   nr_pcpus, total_cpu, total_cpu_per_pcpu,
   totals,
   pcpu_display) (* state *) ->
    clear ();

    (* Get the screen/window size. *)
    let lines, cols = get_size () in

    (* Basic node_info. *)
    addstr (sprintf "%s %d/%dCPU %dMHz %LdMB "
	      node_info.C.model node_info.C.cpus nr_pcpus node_info.C.mhz
	      (node_info.C.memory /^ 1024L));
    (* Save the cursor position for when we come to draw the
     * historical CPU times (down in this function).
     *)
    let stdscr = stdscr () in
    let historical_cursor = getyx stdscr in

    (match !display_mode with
     | TaskDisplay -> (*---------- Showing domains ----------*)
	 (* Sort domains on current sort_order. *)
	 let doms =
	   let cmp =
	     match !sort_order with
	     | DomainName ->
		 (fun _ -> 0) (* fallthrough to default name compare *)
	     | Processor ->
		 (function
		  | Active rd1, Active rd2 ->
		      compare rd2.rd_percent_cpu rd1.rd_percent_cpu
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	     | Memory ->
		 (function
		  | Active { rd_info = info1 }, Active { rd_info = info2 } ->
		      compare info2.D.memory info1.D.memory
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	     | Time ->
		 (function
		  | Active { rd_info = info1 }, Active { rd_info = info2 } ->
		      compare info2.D.cpu_time info1.D.cpu_time
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	     | DomainID ->
		 (function
		  | Active { rd_domid = id1 }, Active { rd_domid = id2 } ->
		      compare id1 id2
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	     | NetRX ->
		 (function
		  | Active { rd_net_rx_bytes = r1 }, Active { rd_net_rx_bytes = r2 } ->
		      compare r2 r1
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	     | NetTX ->
		 (function
		  | Active { rd_net_tx_bytes = r1 }, Active { rd_net_tx_bytes = r2 } ->
		      compare r2 r1
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	     | BlockRdRq ->
		 (function
		  | Active { rd_block_rd_reqs = r1 }, Active { rd_block_rd_reqs = r2 } ->
		      compare r2 r1
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	     | BlockWrRq ->
		 (function
		  | Active { rd_block_wr_reqs = r1 }, Active { rd_block_wr_reqs = r2 } ->
		      compare r2 r1
		  | Active _, Inactive -> -1
		  | Inactive, Active _ -> 1
		  | Inactive, Inactive -> 0)
	   in
	   let cmp (name1, dom1) (name2, dom2) =
	     let r = cmp (dom1, dom2) in
	     if r <> 0 then r
	     else compare name1 name2
	   in
	   List.sort ~cmp doms in

	 (* Print domains. *)
	 attron A.reverse;
	 mvaddstr header_lineno 0
	   (pad cols "   ID S RDRQ WRRQ RXBY TXBY %CPU %MEM    TIME   NAME");
	 attroff A.reverse;

	 let rec loop lineno = function
	   | [] -> ()
	   | (name, Active rd) :: doms ->
	       if lineno < lines then (
		 let state = show_state rd.rd_info.D.state in
		 let rd_req = show_int64_option rd.rd_block_rd_reqs in
		 let wr_req = show_int64_option rd.rd_block_wr_reqs in
		 let rx_bytes = show_int64_option rd.rd_net_rx_bytes in
		 let tx_bytes = show_int64_option rd.rd_net_tx_bytes in
		 let percent_cpu = show_percent rd.rd_percent_cpu in
		 let percent_mem =
		   100L *^ rd.rd_info.D.memory /^ node_info.C.memory in
		 let percent_mem = Int64.to_float percent_mem in
		 let percent_mem = show_percent percent_mem in
		 let time = show_time rd.rd_info.D.cpu_time in

		 let line = sprintf "%5d %c %s %s %s %s %s %s %s %s"
		   rd.rd_domid state rd_req wr_req rx_bytes tx_bytes
		   percent_cpu percent_mem time name in
		 let line = pad cols line in
		 mvaddstr lineno 0 line;
		 loop (lineno+1) doms
	       )
	   | (name, Inactive) :: doms -> (* inactive domain *)
	       if lineno < lines then (
		 let line =
		   sprintf
		     "    -                                           (%s)"
		     name in
		 let line = pad cols line in
		 mvaddstr lineno 0 line;
		 loop (lineno+1) doms
	       )
	 in
	 loop domains_lineno doms

     | PCPUDisplay -> (*---------- Showing physical CPUs ----------*)
	 let doms, pcpus, pcpus_cpu_time =
	   match pcpu_display with
	   | Some p -> p
	   | None -> failwith "internal error: no pcpu_display data" in

	 (* Display the pCPUs. *)
	 let dom_names =
	   String.concat "" (
	     List.map (
	       fun (_, name, _, _, _, _, _) ->
		 let len = String.length name in
		 let width = max (len+1) 7 in
		 pad width name
	     ) doms
	   ) in
	 attron A.reverse;
	 mvaddstr header_lineno 0 (pad cols ("PHYCPU %CPU " ^ dom_names));
	 attroff A.reverse;

	 Array.iteri (
	   fun p row ->
	     mvaddstr (p+domains_lineno) 0 (sprintf "%4d   " p);
	     let cpu_time = pcpus_cpu_time.(p) in (* ns used on this CPU *)
	     let percent_cpu = 100. *. cpu_time /. total_cpu_per_pcpu in
	     addstr (show_percent percent_cpu);
	     addch 32;

	     List.iteri (
	       fun di (domid, name, _, _, _, _, _) ->
		 let t, is_average, is_running = pcpus.(p).(di) in
		 let len = String.length name in
		 let width = max (len+1) 7 in
		 let str =
		   if t <= 0L then ""
		   else (
		     let t = Int64.to_float t in
		     let percent = 100. *. t /. total_cpu_per_pcpu in
		     sprintf "%s%c%c " (show_percent percent)
		       (if is_average then '=' else ' ')
		       (if is_running then '#' else ' ')
		   ) in
		 addstr (pad width str);
		 ()
	     ) doms
	 ) pcpus;

     | NetDisplay -> (*---------- Showing network interfaces ----------*)
	 (* Only care about active domains. *)
	 let doms = List.filter_map (
	   function
	   | (name, Active rd) -> Some (name, rd)
	   | (_, Inactive) -> None
	 ) doms in

	 (* For each domain we have a list of network interfaces seen
	  * this slice, and seen in the previous slice, which we now
	  * match up to get a list of (domain, interface) for which
	  * we have current & previous knowledge.  (And ignore the rest).
	  *)
	 let devs =
	   List.map (
	     fun (name, rd) ->
	       List.filter_map (
		 fun (dev, stats) ->
		   try
		     (* Have prev slice stats for this device? *)
		     let prev_stats =
		       List.assoc dev rd.rd_prev_interface_stats in
		     Some (dev, name, rd, stats, prev_stats)
		   with Not_found -> None
	       ) rd.rd_interface_stats
	   ) doms in

	 (* Finally we have a list of:
	  * device name, domain name, rd_* stuff, curr stats, prev stats.
	  *)
	 let devs : (string * string * rd_active *
		       D.interface_stats * D.interface_stats) list =
	   List.flatten devs in

	 (* Difference curr slice & prev slice. *)
	 let devs = List.map (
	   fun (dev, name, rd, curr, prev) ->
	     dev, name, rd, diff_interface_stats curr prev
	 ) devs in

	 (* Sort by current sort order, but map some of the standard
	  * sort orders into ones which makes sense here.
	  *)
	 let devs =
	   let cmp =
	     match !sort_order with
	     | DomainName ->
		 (fun _ -> 0) (* fallthrough to default name compare *)
	     | DomainID ->
		 (fun (_, { rd_domid = id1 }, _, { rd_domid = id2 }) ->
		    compare id1 id2)
	     | Processor | Memory | Time | BlockRdRq | BlockWrRq
		   (* fallthrough to RXBY comparison. *)
	     | NetRX ->
		 (fun ({ D.rx_bytes = b1 }, _, { D.rx_bytes = b2 }, _) ->
		    compare b2 b1)
	     | NetTX ->
		 (fun ({ D.tx_bytes = b1 }, _, { D.tx_bytes = b2 }, _) ->
		    compare b2 b1)
	   in
	   let cmp (dev1, name1, rd1, stats1) (dev2, name2, rd2, stats2) =
	     let r = cmp (stats1, rd1, stats2, rd2) in
	     if r <> 0 then r
	     else compare (dev1, name1) (dev2, name2)
	   in
	   List.sort ~cmp devs in

	 (* Print the header for network devices. *)
	 attron A.reverse;
	 mvaddstr header_lineno 0
	   (pad cols "   ID S RXBY TXBY RXPK TXPK DOMAIN       INTERFACE");
	 attroff A.reverse;

	 (* Print domains and devices. *)
	 let rec loop lineno = function
	   | [] -> ()
	   | (dev, name, rd, stats) :: devs ->
	       if lineno < lines then (
		 let state = show_state rd.rd_info.D.state in
		 let rx_bytes =
		   if stats.D.rx_bytes >= 0L
		   then show_int64 stats.D.rx_bytes
		   else "    " in
		 let tx_bytes =
		   if stats.D.tx_bytes >= 0L
		   then show_int64 stats.D.tx_bytes
		   else "    " in
		 let rx_packets =
		   if stats.D.rx_packets >= 0L
		   then show_int64 stats.D.rx_packets
		   else "    " in
		 let tx_packets =
		   if stats.D.tx_packets >= 0L
		   then show_int64 stats.D.tx_packets
		   else "    " in

		 let line = sprintf "%5d %c %s %s %s %s %-12s %s"
		   rd.rd_domid state
		   rx_bytes tx_bytes
		   rx_packets tx_packets
		   (pad 12 name) dev in
		 let line = pad cols line in
		 mvaddstr lineno 0 line;
		 loop (lineno+1) devs
	       )
	 in
	 loop domains_lineno devs

     | BlockDisplay -> (*---------- Showing block devices ----------*)
	 (* Only care about active domains. *)
	 let doms = List.filter_map (
	   function
	   | (name, Active rd) -> Some (name, rd)
	   | (_, Inactive) -> None
	 ) doms in

	 (* For each domain we have a list of block devices seen
	  * this slice, and seen in the previous slice, which we now
	  * match up to get a list of (domain, device) for which
	  * we have current & previous knowledge.  (And ignore the rest).
	  *)
	 let devs =
	   List.map (
	     fun (name, rd) ->
	       List.filter_map (
		 fun (dev, stats) ->
		   try
		     (* Have prev slice stats for this device? *)
		     let prev_stats =
		       List.assoc dev rd.rd_prev_block_stats in
		     Some (dev, name, rd, stats, prev_stats)
		   with Not_found -> None
	       ) rd.rd_block_stats
	   ) doms in

	 (* Finally we have a list of:
	  * device name, domain name, rd_* stuff, curr stats, prev stats.
	  *)
	 let devs : (string * string * rd_active *
		       D.block_stats * D.block_stats) list =
	   List.flatten devs in

	 (* Difference curr slice & prev slice. *)
	 let devs = List.map (
	   fun (dev, name, rd, curr, prev) ->
	     dev, name, rd, diff_block_stats curr prev
	 ) devs in

	 (* Sort by current sort order, but map some of the standard
	  * sort orders into ones which makes sense here.
	  *)
	 let devs =
	   let cmp =
	     match !sort_order with
	     | DomainName ->
		 (fun _ -> 0) (* fallthrough to default name compare *)
	     | DomainID ->
		 (fun (_, { rd_domid = id1 }, _, { rd_domid = id2 }) ->
		    compare id1 id2)
	     | Processor | Memory | Time | NetRX | NetTX
		   (* fallthrough to RDRQ comparison. *)
	     | BlockRdRq ->
		 (fun ({ D.rd_req = b1 }, _, { D.rd_req = b2 }, _) ->
		    compare b2 b1)
	     | BlockWrRq ->
		 (fun ({ D.wr_req = b1 }, _, { D.wr_req = b2 }, _) ->
		    compare b2 b1)
	   in
	   let cmp (dev1, name1, rd1, stats1) (dev2, name2, rd2, stats2) =
	     let r = cmp (stats1, rd1, stats2, rd2) in
	     if r <> 0 then r
	     else compare (dev1, name1) (dev2, name2)
	   in
	   List.sort ~cmp devs in

	 (* Print the header for block devices. *)
	 attron A.reverse;
	 mvaddstr header_lineno 0
	   (pad cols "   ID S RDBY WRBY RDRQ WRRQ DOMAIN       DEVICE");
	 attroff A.reverse;

	 (* Print domains and devices. *)
	 let rec loop lineno = function
	   | [] -> ()
	   | (dev, name, rd, stats) :: devs ->
	       if lineno < lines then (
		 let state = show_state rd.rd_info.D.state in
		 let rd_bytes =
		   if stats.D.rd_bytes >= 0L
		   then show_int64 stats.D.rd_bytes
		   else "    " in
		 let wr_bytes =
		   if stats.D.wr_bytes >= 0L
		   then show_int64 stats.D.wr_bytes
		   else "    " in
		 let rd_req =
		   if stats.D.rd_req >= 0L
		   then show_int64 stats.D.rd_req
		   else "    " in
		 let wr_req =
		   if stats.D.wr_req >= 0L
		   then show_int64 stats.D.wr_req
		   else "    " in

		 let line = sprintf "%5d %c %s %s %s %s %-12s %s"
		   rd.rd_domid state
		   rd_bytes wr_bytes
		   rd_req wr_req
		   (pad 12 name) dev in
		 let line = pad cols line in
		 mvaddstr lineno 0 line;
		 loop (lineno+1) devs
	       )
	 in
	 loop domains_lineno devs
    ); (* end of display_mode conditional section *)

    let (count, running, blocked, paused, shutdown, shutoff,
	 crashed, active, inactive,
	 total_cpu_time, total_memory, total_domU_memory) = totals in

    mvaddstr summary_lineno 0
      (sprintf "%d domains, %d active, %d running, %d sleeping, %d paused, %d inactive D:%d O:%d X:%d"
	 count active running blocked paused inactive shutdown shutoff
	 crashed);

    (* Total %CPU used, and memory summary. *)
    let percent_cpu = 100. *. total_cpu_time /. total_cpu in
    mvaddstr (summary_lineno+1) 0
      (sprintf "CPU: %2.1f%%  Mem: %Ld MB (%Ld MB by guests)"
	 percent_cpu (total_memory /^ 1024L) (total_domU_memory /^ 1024L));

    (* Time to grab another historical %CPU for the list? *)
    if time >= !historical_cpu_last_time +. float !historical_cpu_delay
    then (
      historical_cpu := percent_cpu :: List.take 10 !historical_cpu;
      historical_cpu_last_time := time
    );

    (* Display historical CPU time. *)
    let () =
      let x, y = historical_cursor in (* Yes, it's a bug in ocaml-curses *)
      let maxwidth = cols - x in
      let line =
	String.concat " "
	  (List.map (sprintf "%2.1f%%") !historical_cpu) in
      let line = pad maxwidth line in
      mvaddstr y x line;
      () in

    move message_lineno 0; (* Park cursor in message area, as with top. *)
    refresh ();		   (* Refresh the display. *)
    ()

(* Write CSV header row. *)
let write_csv_header () =
  (!csv_write) (
    [ "Hostname"; "Time"; "Arch"; "Physical CPUs";
      "Count"; "Running"; "Blocked"; "Paused"; "Shutdown";
      "Shutoff"; "Crashed"; "Active"; "Inactive";
      "%CPU"; "Total memory (KB)"; "Total guest memory (KB)";
      "Total CPU time (ns)" ] @
      (* These fields are repeated for each domain: *)
    [ "Domain ID"; "Domain name"; ] @
    (if !csv_cpu then [ "CPU (ns)"; "%CPU"; ] else []) @
    (if !csv_block then [ "Block RDRQ"; "Block WRRQ"; ] else []) @
    (if !csv_net then [ "Net RXBY"; "Net TXBY" ] else [])
  )

(* Write summary data to CSV file. *)
let append_csv
    (_, _, _, _, node_info, hostname, _) (* setup *)
    (doms,
     _, printable_time,
     nr_pcpus, total_cpu, _,
     totals,
     _) (* state *) =

  (* The totals / summary fields. *)
  let (count, running, blocked, paused, shutdown, shutoff,
       crashed, active, inactive,
       total_cpu_time, total_memory, total_domU_memory) = totals in

  let percent_cpu = 100. *. total_cpu_time /. total_cpu in

  let summary_fields = [
    hostname; printable_time; node_info.C.model; string_of_int nr_pcpus;
    string_of_int count; string_of_int running; string_of_int blocked;
    string_of_int paused; string_of_int shutdown; string_of_int shutoff;
    string_of_int crashed; string_of_int active; string_of_int inactive;
    sprintf "%2.1f" percent_cpu;
    Int64.to_string total_memory; Int64.to_string total_domU_memory;
    Int64.to_string (Int64.of_float total_cpu_time)
  ] in

  (* The domains.
   *
   * Sort them by ID so that the list of relatively stable.  Ignore
   * inactive domains.
   *)
  let doms = List.filter_map (
    function
    | _, Inactive -> None		(* Ignore inactive domains. *)
    | name, Active rd -> Some (name, rd)
  ) doms in
  let cmp (_, { rd_domid = rd_domid1 }) (_, { rd_domid = rd_domid2 }) =
    compare rd_domid1 rd_domid2
  in
  let doms = List.sort ~cmp doms in

  let string_of_int64_option = Option.map_default Int64.to_string "" in

  let domain_fields = List.map (
    fun (domname, rd) ->
      [ string_of_int rd.rd_domid; domname ] @
	(if !csv_cpu then [
	   string_of_float rd.rd_cpu_time; string_of_float rd.rd_percent_cpu
	 ] else []) @
	(if !csv_block then [
	   string_of_int64_option rd.rd_block_rd_reqs;
	   string_of_int64_option rd.rd_block_wr_reqs;
	 ] else []) @
	(if !csv_net then [
	   string_of_int64_option rd.rd_net_rx_bytes;
	   string_of_int64_option rd.rd_net_tx_bytes;
	 ] else [])
  ) doms in
  let domain_fields = List.flatten domain_fields in

  (!csv_write) (summary_fields @ domain_fields)

(* Main loop. *)
let rec main_loop ((_, batch_mode, script_mode, csv_enabled, _, _, _)
		     as setup) =
  if csv_enabled then write_csv_header ();

  while not !quit do
    let state = collect setup in	        (* Collect stats. *)
    if not script_mode then redraw setup state; (* Redraw display. *)
    if csv_enabled then append_csv setup state; (* Update CSV file. *)

    (* Clear up unused virDomainPtr objects. *)
    Gc.compact ();

    if not batch_mode && not script_mode then
      get_key_press setup
    else (* Batch mode or script mode - just sleep, ignore keys. *)
      Unix.sleep (!delay / 1000);

    (* Max iterations? *)
    if !iterations >= 0 then (
      decr iterations;
      if !iterations = 0 then quit := true
    );
  done

and get_key_press setup =
  (* Read the next key, waiting up to !delay milliseconds. *)
  timeout !delay;
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
    else unknown_command k
  )

and change_delay () =
  print_msg (sprintf "Change delay from %.1f to: " (float !delay /. 1000.));
  let str = get_string 16 in
  (* Try to parse the number. *)
  let error =
    try
      let newdelay = float_of_string str in
      if newdelay <= 0. then (
	print_msg "Delay must be > 0"; true
      ) else (
	delay := int_of_float (newdelay *. 1000.); false
      )
    with
      Failure "float_of_string" ->
	print_msg "Not a valid number"; true in
  sleep (if error then 2 else 1)

and change_sort_order () =
  clear ();
  let lines, cols = get_size () in

  mvaddstr top_lineno 0 "Set sort order for main display";
  mvaddstr summary_lineno 0 "Type key or use up and down cursor keys.";

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
	 if not loop then sleep 1
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

(* Write an init file. *)
and write_init_file () =
  match !init_file with
  | NoInitFile -> ()			(* Do nothing if --no-init-file *)
  | DefaultInitFile ->
      let home = try Sys.getenv "HOME" with Not_found -> "/" in
      let filename = home // ".virt-toprc" in
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
    fp chan "# .virt-toprc virt-top configuration file\n";
    fp chan "# generated on %s by %s\n" printable_date_time username;
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
    fp chan "# To send debug and error messages to a file, uncomment next line\n";
    fp chan "#debug virt-top.out\n";
    nl ();
    fp chan "# Enable CSV output to the named file\n";
    fp chan "#csv virt-top.csv\n";
    nl ();
    fp chan "# To protect this file from being overwritten, uncomment next line\n";
    fp chan "#overwrite-init-file false\n";

    close_out chan;

    (* If the file exists, rename it as filename.old. *)
    (try Unix.rename filename (filename ^ ".old")
     with Unix.Unix_error _ -> ());

    (* Rename filename.new to filename. *)
    Unix.rename (filename ^ ".new") filename;

    print_msg (sprintf "Wrote settings to %s" filename); sleep 2
  with
  | Sys_error err -> print_msg "Error: %s"; sleep 2
  | Unix.Unix_error (err, fn, str) ->
      print_msg (sprintf "Error: %s %s %s" (Unix.error_message err) fn str);
      sleep 2

and show_help (_, _, _, _, _, hostname,
	       (libvirt_major, libvirt_minor, libvirt_release)) =
  clear ();

  (* Get the screen/window size. *)
  let lines, cols = get_size () in

  (* Banner at the top of the screen. *)
  let banner =
    sprintf "virt-top %s (libvirt %d.%d.%d) by Red Hat"
      Libvirt_version.version libvirt_major libvirt_minor libvirt_release in
  let banner = pad cols banner in
  attron A.reverse;
  mvaddstr 0 0 banner;
  attroff A.reverse;

  (* Status. *)
  mvaddstr 1 0
    (sprintf "Delay: %.1f secs; Batch: %s; Secure: %s; Sort: %s"
       (float !delay /. 1000.)
       (if !batch_mode then "On" else "Off")
       (if !secure_mode then "On" else "Off")
       (printable_sort_order !sort_order));
  mvaddstr 2 0
    (sprintf "Connect: %s; Hostname: %s"
       (match !uri with None -> "default" | Some s -> s)
       hostname);

  (* Misc keys on left. *)
  let banner = pad 38 "MAIN KEYS" in
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
    move lineno 10; addstr description; ()
  in
  key "space ^L" "Update display";
  key "q"        "Quit";
  key "d s"      "Set update interval";
  key "h"        "Help";

  (* Sort order. *)
  ignore (get_lineno ());
  let banner = pad 38 "SORTING" in
  attron A.reverse;
  mvaddstr (get_lineno ()) 1 banner;
  attroff A.reverse;

  key "P" "Sort by %CPU";
  key "M" "Sort by %MEM";
  key "T" "Sort by TIME";
  key "N" "Sort by ID";
  key "F" "Select sort field";

  (* Display modes on right. *)
  let banner = pad 39 "DISPLAY MODES" in
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
    move lineno 49; addstr description; ()
  in
  key "0" "Domains display";
  key "1" "Toggle physical CPUs";
  key "2" "Toggle network interfaces";
  key "3" "Toggle block devices";

  (* Update screen and wait for key press. *)
  mvaddstr (lines-1) 0
    "More help in virt-top(1) man page. Press any key to return.";
  refresh ();
  ignore (getch ())

and unknown_command k =
  print_msg "Unknown command - try 'h' for help";
  sleep 1
