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

   This file contains any code which needs optional package OCaml-DBUS.
*)

(* There is *zero* documentation for this.  I examined a lot of code
 * to do this, and the following page was also very helpful:
 * http://www.amk.ca/diary/2007/04/rough_notes_python_and_dbus.html
 * See also the DBus API reference:
 * http://dbus.freedesktop.org/doc/dbus/api/html/index.html
 * See also Dan Berrange's Perl bindings:
 * http://search.cpan.org/src/DANBERR/Net-DBus-0.33.5/lib/Net/
 *
 * This code is a complicated state machine because that's what
 * D-Bus requires.  Enable debugging below to trace messages.
 *
 * It's also very unelegant and leaks memory.
 *
 * The code connects to D-Bus only the first time that the
 * connection dialog is opened, and thereafter it attaches itself
 * to the Gtk main loop, waiting for events.  It's probably not
 * safe if the avahi or dbus daemon restarts.
 *)

open Printf
open Virt_ctrl_gettext.Gettext
open DBus

let debug = true

let service = "_libvirt._tcp"

let rec print_msg msg =
  (match Message.get_type msg with
   | Message.Invalid ->
       eprintf "Invalid";
   | Message.Method_call ->
       eprintf "Method_call";
   | Message.Method_return ->
       eprintf "Method_return";
   | Message.Error ->
       eprintf "Error";
   | Message.Signal ->
       eprintf "Signal");

  let print_opt f name =
    match f msg with
    | None -> ()
    | Some value -> eprintf "\n\t%s=%S" name value
  in
  print_opt Message.get_member "member";
  print_opt Message.get_path "path";
  print_opt Message.get_interface "interface";
  print_opt Message.get_sender "sender";

  let fields = Message.get msg in
  eprintf "\n\t[";
  print_fields fields;
  eprintf "]\n%!";

and print_fields fields =
  eprintf "%s" (String.concat ", " (List.map string_of_ty fields))

(* Perform a synchronous call to an object method. *)
let call_method ~bus ~err ~name ~path ~interface ~methd args =
  (* Create the method_call message. *)
  let msg = Message.new_method_call name path interface methd in
  Message.append msg args;
  (* Send the message, get reply. *)
  let r = Connection.send_with_reply_and_block bus msg (-1) err in
  Message.get r

(* Services we've found.
 * This is a map from name -> URI.
 * XXX We just assume Xen at the moment.
 * XXX The same machine can appear on multiple interfaces, so this
 * isn't right.
 *)
let services : (string, string) Hashtbl.t = Hashtbl.create 13

(* Process a Found message, indicating that we've found and fully
 * resolved a new service.
 *)
let add_service bus err msg =
  (* match fields in the Found message from ServiceResolver. *)
  match Message.get msg with
  | Int32 _ ::				(* interface *)
      Int32 (*protocol*)_ ::		(* 0 = IPv4, 1=IPv6 *)
      String name ::			(* "Virtualization Host foo" *)
      String _ ::			(* "_libvirt._tcp" *)
      String _ ::			(* domain name *)
      String hostname ::		(* this is the hostname as a string *)
      Int32 _ ::			(* ? aprotocol *)
      String address ::			(* IP address as a string *)
      UInt16 (*port*)_ :: _ ->		(* port is set to 0 by libvirtd *)

      let hostname = if hostname <> "" then hostname else address in
      (*let protocol = if protocol = 1_l then IPv6 else IPv4 in*)

      (* XXX *)
      let uri = "xen://" ^ hostname ^ "/" in

      if debug then eprintf "adding %s %s\n%!" name uri;

      Hashtbl.replace services name uri

  | _ ->
      prerr_endline (s_ "warning: unexpected message contents of Found signal")

(* Process an ItemRemove message, indicating that a service has
 * gone away.
 *)
let remove_service bus err msg =
  (* match fields in the ItemRemove message from ServiceBrowser. *)
  match Message.get msg with
  | Int32 _ ::				(* interface *)
      Int32 _ ::			(* protocol *)
      String name :: _ ->		(* name *)
      if debug then eprintf "removing %s\n%!" name;
      Hashtbl.remove services name

  | _ ->
      prerr_endline
	(s_ "warning: unexpected message contents of ItemRemove signal")

(* A service has appeared on the network.  Resolve its IP address, etc. *)
let start_resolve_service bus err sb_path msg =
  (* match fields in the ItemNew message from ServiceBrowser. *)
  match Message.get msg with
  | ((Int32 _) as interface) ::
      ((Int32 _) as protocol) ::
      ((String _) as name) ::
      ((String _) as service) ::
      ((String _) as domain) :: _ ->
      (* Create a new ServiceResolver object which is used to resolve
       * the actual locations of network services found by the ServiceBrowser.
       *)
      let sr =
	call_method ~bus ~err
	  ~name:"org.freedesktop.Avahi"
	  ~path:"/"
	  ~interface:"org.freedesktop.Avahi.Server"
	  ~methd:"ServiceResolverNew"
	  [
	    interface;
	    protocol;
	    name;
	    service;
	    domain;
	    Int32 (-1_l);		(* AVAHI_PROTO_UNSPEC *)
	    UInt32 0_l;			(* flags *)
	  ] in
      let sr_path =
	match sr with
	| [ ObjectPath path ] -> path
	| _ -> assert false in

      if debug then eprintf "ServiceResolver path = %S\n%!" sr_path;

      (* Add a match rule so we see these all signals of interest. *)
      Bus.add_match bus
	(String.concat "," [
	   "type='signal'";
	   "sender='org.freedesktop.Avahi.ServiceResolver'";
	   "path='" ^ sr_path ^ "'";
	 ]) err;

      ()

  | _ ->
      prerr_endline
	(s_ "warning: unexpected message contents of ItemNew signal")

(* This is called when we get a message/signal.  Could be from the
 * (global) ServiceBrowser or any of the ServiceResolver objects.
 *)
let got_message bus err sb_path msg =
  if debug then print_msg msg;

  let typ = Message.get_type msg in
  let member = match Message.get_member msg with None -> "" | Some m -> m in
  let interface =
    match Message.get_interface msg with None -> "" | Some m -> m in

  if typ = Message.Signal then (
    match interface, member with
    | "org.freedesktop.Avahi.ServiceBrowser", "CacheExhausted" -> ()
    | "org.freedesktop.Avahi.ServiceBrowser", "AllForNow" -> ()
    | "org.freedesktop.Avahi.ServiceBrowser", "ItemNew" ->
	(* New service has appeared, start to resolve it. *)
	start_resolve_service bus err sb_path msg
    | "org.freedesktop.Avahi.ServiceResolver", "Found" ->
	(* Resolver has finished resolving the name of a previously
	 * appearing service.
	 *)
	add_service bus err msg
    | "org.freedesktop.Avahi.ServiceBrowser", "ItemRemove" ->
	(* Service has disappeared. *)
	remove_service bus err msg
    | "org.freedesktop.DBus", _ -> ()
    | interface, member ->
	let () =
	  eprintf (f_ "warning: ignored unknown message %s from %s\n%!")
	    member interface in
	()
  );
  true

(* Store the connection ((bus, err, io_id) tuple).  However don't bother
 * connecting to D-Bus at all until the user opens the connection
 * dialog for the first time.
 *)
let connection = ref None

(* Create global error and system bus object, and create the service browser. *)
let connect () =
  match !connection with
  | Some (bus, err, _) -> (bus, err, false)
  | None ->
      let err = Error.init () in
      let bus = Bus.get Bus.System err in
      if Error.is_set err then
	failwith (s_ "error set after getting System bus");

      (* Create a new ServiceBrowser object which emits a signal whenever
       * a new network service of the type specified is found on the network.
       *)
      let sb =
	call_method ~bus ~err
	  ~name:"org.freedesktop.Avahi"
	  ~path:"/"
	  ~interface:"org.freedesktop.Avahi.Server"
	  ~methd:"ServiceBrowserNew"
	  [
	    Int32 (-1_l);	        (* interface, -1=AVAHI_IF_UNSPEC *)
	    Int32 (-1_l);		(* AVAHI_PROTO_UNSPEC *)
	    String service;		(* service type *)
	    String "";			(* XXX call GetDomainName() *)
	    UInt32 0_l;			(* flags *)
	  ] in
      let sb_path =
	match sb with
	| [ ObjectPath path ] -> path
	| _ -> assert false in

      if debug then eprintf "ServiceBrowser path = %S\n%!" sb_path;

      (* Register a callback to accept the signals. *)
      (* XXX This leaks memory because it is never freed. *)
      Connection.add_filter bus (
	fun bus msg -> got_message bus err sb_path msg
      );

      (* Add a match rule so we see these all signals of interest. *)
      Bus.add_match bus
	(String.concat "," [
	   "type='signal'";
	   "sender='org.freedesktop.Avahi.ServiceBrowser'";
	   "path='" ^ sb_path ^ "'";
	 ]) err;

      (* This is called from the Gtk main loop whenever there is new
       * data to read on the D-Bus socket.
       *)
      let callback _ =
	if debug then eprintf "dbus callback\n%!";
	if Connection.read_write_dispatch bus 0 then true
	else (				(* Disconnected. *)
	  connection := None;
	  false
	)
      in

      (* Get the file descriptor and attach to the Gtk main loop. *)
      let fd = Connection.get_fd bus in
      let channel = GMain.Io.channel_of_descr fd in
      let io_id = GMain.Io.add_watch ~cond:[`IN] ~callback channel in

      connection := Some (bus, err, io_id);
      (bus, err, true)

(* This function is called by the connection dialog and is expected
 * to return a list of services we know about now.
 *)
let find_services () =
  let bus, err, just_connected = connect () in

  (* If we've just connected, wait briefly for the list to stablise. *)
  if just_connected then (
    let start_time = Unix.gettimeofday () in
    while Unix.gettimeofday () -. start_time < 0.5 do
      ignore (Connection.read_write_dispatch bus 500)
    done
  );

  (* Return the services we know about. *)
  Hashtbl.fold (fun k v vs -> (k, v) :: vs) services []

;;

Vc_connection_dlg.find_libvirtd_with_zeroconf := find_services
