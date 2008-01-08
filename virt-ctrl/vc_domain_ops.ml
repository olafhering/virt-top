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

   Domain operations buttons.
*)

open Printf

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

(* Get the selected domain (if there is one) or return None. *)
let get_domain (tree : GTree.view) (model : GTree.tree_store)
    (columns : Vc_connections.columns) =
  let path, _ = tree#get_cursor () in
  match path with
  | None -> None			(* No row at all selected. *)
  | Some path ->
      let row = model#get_iter path in
      (* Visit parent to get the conn_id.
       * If this returns None, then it's a top-level row which is
       * selected (ie. a connection), so just ignore.
       *)
      match model#iter_parent row with
      | None -> None
      | Some parent ->
	  try
	    let (_, col_domname, _, _, _, col_id) = columns in
	    let conn_id = model#get ~row:parent ~column:col_id in
	    let conn =
	      List.assoc conn_id (Vc_connections.get_conns ()) in
	    let domid = model#get ~row ~column:col_id in
	    if domid = -1 then (	(* Inactive domain. *)
	      let domname = model#get ~row ~column:col_domname in
	      let dom = D.lookup_by_name conn domname in
	      let info = D.get_info dom in
	      Some (dom, info, -1)
	    ) else if domid > 0 then (	(* Active domU. *)
	      let dom = D.lookup_by_id conn domid in
	      let info = D.get_info dom in
	      Some (dom, info, domid)
	    ) else			(* Dom0 - ignore. *)
	      None
	  with
	    (* Domain or connection disappeared under us. *)
	  | Not_found -> None
	  | Failure msg ->
	      prerr_endline msg;
	      None
	  | Libvirt.Virterror err ->
	      prerr_endline (Libvirt.Virterror.to_string err);
	      None

let start_domain tree model columns () =
  match get_domain tree model columns with
  | None -> ()
  | Some (dom, _, domid) ->
      if domid = -1 then
	D.create dom

let pause_domain tree model columns () =
  match get_domain tree model columns with
  | None -> ()
  | Some (dom, info, domid) ->
      if domid >= 0 && info.D.state <> D.InfoPaused then
	D.suspend dom

let resume_domain tree model columns () =
  match get_domain tree model columns with
  | None -> ()
  | Some (dom, info, domid) ->
      if domid >= 0 && info.D.state = D.InfoPaused then
	D.resume dom

let shutdown_domain tree model columns () =
  match get_domain tree model columns with
  | None -> ()
  | Some (dom, info, domid) ->
      if domid >= 0 && info.D.state <> D.InfoShutdown then
	D.shutdown dom
