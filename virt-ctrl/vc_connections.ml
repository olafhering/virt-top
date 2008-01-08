(* virt-ctrl: A graphical management tool.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
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

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

open Vc_helpers

(* List of currently open connections.  Actually it's a list of
 * (id, Libvirt.Connect.t) so that we can easily identify
 * connections by their unique ID.
 *)
let get_conns, add_conn, del_conn =
  let conns = ref [] in
  let id = ref 0 in
  let get_conns () = !conns in
  let add_conn conn =
    incr id; let id = !id in
    conns := (id, conn) :: !conns;
    id
  in
  let del_conn id =
    conns := List.filter (fun (id', _) -> id <> id') !conns
  in
  get_conns, add_conn, del_conn

(* The current state.  This is used so that we can see changes that
 * have happened and add or remove parts of the model.  (Previously
 * we used to recreate the whole model each time, but the problem
 * with that is we "forget" things like the selection).
 *)
type state = connection list
and connection = int (* connection ID *) * (active list * inactive list)
and active = int (* domain's ID *)
and inactive = string (* domain's name *)

(* The last "CPU time" seen for a domain, so we can calculate CPU % usage.
 * Hash of (connid, domid) -> cpu_time [int64].
 *)
let last_cpu_time = Hashtbl.create 13
let last_time = ref (Unix.gettimeofday ())

(* Store the node_info and hostname for each connection, fetched
 * once just after we connect since these don't normally change.
 * Hash of connid -> (C.node_info, hostname option, uri)
 *)
let static_conn_info = Hashtbl.create 13

type columns = string GTree.column * string GTree.column * string GTree.column * string GTree.column * string GTree.column * int GTree.column

let debug_repopulate = true

(* Populate the tree with the current list of connections, domains.
 * This function is called once per second.
 *)
let repopulate (tree : GTree.view) (model : GTree.tree_store)
    (col_name_id, col_domname, col_status, col_cpu, col_mem, col_id)
    state =
  let time_passed =
    let time_now = Unix.gettimeofday () in
    let time_passed = time_now -. !last_time in
    last_time := time_now;
    time_passed in

  (* Which connections have been added or removed? *)
  let conns = get_conns () in
  let added, _, removed =
    let old_conn_ids = List.map fst state
    and new_conn_ids = List.map fst conns in
    differences old_conn_ids new_conn_ids in

  (* Remove the subtrees for any connections which have gone. *)
  if debug_repopulate then List.iter (eprintf "-connection %d\n%!") removed;

  List.iter (
    fun conn_id ->
      filter_top_level_rows model
	(fun row -> conn_id <> model#get ~row ~column:col_id)
  ) removed;

  (* Add placeholder subtree for any new connections. *)
  if debug_repopulate then List.iter (eprintf "+connection %d\n%!") added;

  List.iter (
    fun conn_id ->
      let row = model#append () in
      (* Get the connection name, usually the hostname. *)
      let name =
	match Hashtbl.find static_conn_info conn_id with
	| (_, Some hostname, _) -> hostname
	| (_, None, _) -> sprintf "Conn #%d" conn_id in
      model#set ~row ~column:col_name_id name;
      model#set ~row ~column:col_id conn_id;
      (* Expand the new row. *)
      (* XXX This doesn't work, why? - Because we haven't create subrows yet.*)
      tree#expand_row (model#get_path row)
  ) added;

  let new_state =
    List.map (
      fun (conn_id, conn) ->
	(* Get the old list of active and inactive domains.  If this
	 * connection is newly created, start with empty lists.
	 *)
	let old_active, old_inactive =
	  try List.assoc conn_id state
	  with Not_found -> [], [] in

	(* Get the top level row in the model corresponding to this
	 * connection.
	 *)
	let parent =
	  try find_top_level_row model
	    (fun row -> conn_id = model#get ~row ~column:col_id)
	  with Not_found -> assert false (* Should never happen. *) in

	try
	  (* Number of CPUs available. *)
	  let node_info, _, _ = Hashtbl.find static_conn_info conn_id in
	  let nr_cpus = C.maxcpus_of_node_info node_info in

	  (* For this connection, get a current list of active domains (IDs) *)
	  let active =
	    let n = C.num_of_domains conn in
	    let doms = C.list_domains conn n in
	    Array.to_list doms in

	  (* Which active domains have been added or removed? *)
	  let added, _, removed = differences old_active active in

	  (* Remove any active domains which have disappeared. *)
	  if debug_repopulate then
	    List.iter (eprintf "-active %d\n%!") removed;

	  List.iter (
	    fun domid ->
	      filter_rows model
		(fun row -> domid <> model#get ~row ~column:col_id)
		(model#iter_children (Some parent))
	  ) removed;

	  (* Add any active domains which have appeared. *)
	  if debug_repopulate then
	    List.iter (eprintf "+active %d\n%!") added;

	  List.iter (
	    fun domid ->
	      let domname =
		try
		  let dom = D.lookup_by_id conn domid in
		  D.get_name dom
		with _ -> "" in (* Ignore any transient error. *)

	      let row = model#append ~parent () in
	      model#set ~row ~column:col_name_id (string_of_int domid);
	      model#set ~row ~column:col_domname domname;
	      model#set ~row ~column:col_id domid
	  ) added;

	  (* Get a current list of inactive domains (names). *)
	  let inactive =
	    let n = C.num_of_defined_domains conn in
	    let doms = C.list_defined_domains conn n in
	    Array.to_list doms in

	  (* Which inactive domains have been added or removed? *)
	  let added, _, removed = differences old_inactive inactive in

	  (* Remove any inactive domains which have disappeared. *)
	  if debug_repopulate then
	    List.iter (eprintf "-inactive %s\n%!") removed;

	  List.iter (
	    fun domname ->
	      filter_rows model
		(fun row ->
		   model#get ~row ~column:col_id <> -1 ||
		   model#get ~row ~column:col_domname <> domname)
		(model#iter_children (Some parent))
	  ) removed;

	  (* Add any inactive domains which have appeared. *)
	  if debug_repopulate then
	    List.iter (eprintf "+inactive %s\n%!") added;

	  List.iter (
	    fun domname ->
	      let row = model#append ~parent () in
	      model#set ~row ~column:col_name_id "";
	      model#set ~row ~column:col_domname domname;
	      model#set ~row ~column:col_status "inactive";
	      model#set ~row ~column:col_id (-1)
	  ) added;

	  (* Now iterate over all active domains and update their state,
	   * CPU and memory.
	   *)
	  iter_rows model (
	    fun row ->
	      let domid = model#get ~row ~column:col_id in
	      if domid >= 0 then ( (* active *)
		try
		  let dom = D.lookup_by_id conn domid in
		  let info = D.get_info dom in
		  let status = string_of_domain_state info.D.state in
		  model#set ~row ~column:col_status status;
		  let memory = sprintf "%Ld K" info.D.memory in
		  model#set ~row ~column:col_mem memory;

		  let ns_now = info.D.cpu_time in (* ns = nanoseconds *)
		  let ns_prev =
		    try
		      let ns = Hashtbl.find last_cpu_time (conn_id, domid) in
		      if ns > ns_now then 0L else ns (* Rebooted? *)
		    with Not_found -> 0L in
		  Hashtbl.replace last_cpu_time (conn_id, domid) ns_now;
		  let ns_now = Int64.to_float ns_now in
		  let ns_prev = Int64.to_float ns_prev in
		  let ns_used = ns_now -. ns_prev in
		  let ns_available = 1_000_000_000. *. float nr_cpus in
		  let cpu_percent =
		    100. *. (ns_used /. ns_available) /. time_passed in
		  let cpu_percent = sprintf "%.1f %%" cpu_percent in
		  model#set ~row ~column:col_cpu cpu_percent;

		with Libvirt.Virterror _ -> () (* Ignore any transient error *)
	      )
	  ) (model#iter_children (Some parent));

	  (* Return new state. *)
	  conn_id, (active, inactive)
	with
	(* Libvirt errors here are not really fatal.  They can happen
	 * if the state changes at the moment we read it.  If it does
	 * happen, just return the old state, and next time we come
	 * around to this connection it'll be fixed.
	 *)
	| Libvirt.Virterror err ->
	    prerr_endline (Libvirt.Virterror.to_string err);
	    conn_id, (old_active, old_inactive)
	| Failure msg ->
	    prerr_endline msg;
	    conn_id, (old_active, old_inactive)
    ) conns in

  (* Return the updated state. *)
  new_state

(* Make the treeview which displays the connections and domains. *)
let make_treeview ?packing () =
  let cols = new GTree.column_list in
  let col_name_id = cols#add Gobject.Data.string in
  let col_domname = cols#add Gobject.Data.string in
  let col_status = cols#add Gobject.Data.string in
  let col_cpu = cols#add Gobject.Data.string in
  let col_mem = cols#add Gobject.Data.string in
  (* Hidden column containing the connection ID or domain ID.  For
   * inactive domains, this contains -1 and col_domname is the name. *)
  let col_id = cols#add Gobject.Data.int in
  let model = GTree.tree_store cols in

  (* Column sorting functions. *)
  let make_sort_func_on column =
    fun (model : GTree.model) row1 row2 ->
      let col1 = model#get ~row:row1 ~column in
      let col2 = model#get ~row:row2 ~column in
      compare col1 col2
  in
  (*model#set_default_sort_func (make_sort_func_on col_domname);*)
  model#set_sort_func 0 (make_sort_func_on col_name_id);
  model#set_sort_func 1 (make_sort_func_on col_domname);
  model#set_sort_column_id 1 `ASCENDING;

  (* Make the GtkTreeView and attach column renderers to it. *)
  let tree = GTree.view ~model ~reorderable:false ?packing () in

  let append_visible_column title column sort =
    let renderer = GTree.cell_renderer_text [], ["text", column] in
    let view_col = GTree.view_column ~title ~renderer () in
    ignore (tree#append_column view_col);
    match sort with
    | None -> ()
    | Some (sort_indicator, sort_order, sort_column_id) ->
	view_col#set_sort_indicator sort_indicator;
	view_col#set_sort_order sort_order;
	view_col#set_sort_column_id sort_column_id
  in
  append_visible_column "ID" col_name_id (Some (false, `ASCENDING, 0));
  append_visible_column "Name" col_domname (Some (true, `ASCENDING, 1));
  append_visible_column "Status" col_status None;
  append_visible_column "CPU" col_cpu None;
  append_visible_column "Memory" col_mem None;

  let columns =
    col_name_id, col_domname, col_status, col_cpu, col_mem, col_id in
  let state = repopulate tree model columns [] in

  (tree, model, columns, state)

(* Callback function to open a connection.
 * This should be a lot more sophisticated. XXX
 *)
let open_connection () =
  let title = "Open connection to hypervisor" in
  let uri =
    GToolbox.input_string ~title ~text:"xen:///" ~ok:"Open" "Connection:" in
  match uri with
  | None -> ()
  | Some uri ->
      (* If this fails, let the exception escape and be printed
       * in the global exception handler.
       *)
      let conn = C.connect ~name:uri () in

      let node_info = C.get_node_info conn in
      let hostname =
	try Some (C.get_hostname conn)
	with
	| Libvirt.Not_supported "virConnectGetHostname"
	| Libvirt.Virterror _ -> None in

      (* Add it to our list of connections. *)
      let conn_id = add_conn conn in
      Hashtbl.add static_conn_info conn_id (node_info, hostname, uri)
