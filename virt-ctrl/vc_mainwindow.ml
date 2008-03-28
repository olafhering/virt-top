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
open Virt_ctrl_gettext.Gettext

let title = s_ "Virtual Control"

let utf8_copyright = "\194\169"

let help_about () =
  let gtk_version =
    let gtk_major, gtk_minor, gtk_micro = GMain.Main.version in
    sprintf "%d.%d.%d" gtk_major gtk_minor gtk_micro in
  let virt_version = string_of_int (fst (Libvirt.get_version ())) in
  let title = "About " ^ title in
  let icon = GMisc.image () in
  icon#set_stock `DIALOG_INFO;
  icon#set_icon_size `DIALOG;
  GToolbox.message_box
    ~title
    ~icon
    (sprintf (f_ "Virtualization control tool (virt-ctrl) by
Richard W.M. Jones (rjones@redhat.com).

Copyright %s 2007-2008 Red Hat Inc.

Libvirt version: %s

Gtk toolkit version: %s") utf8_copyright virt_version gtk_version)

(* Catch any exception and throw up a dialog. *)
let () =
  (* A nicer exception printing function. *)
  let string_of_exn = function
    | Libvirt.Virterror err ->
	s_ "Virtualisation error" ^ ": " ^ (Libvirt.Virterror.to_string err)
    | Failure msg -> msg
    | exn -> Printexc.to_string exn
  in
  GtkSignal.user_handler :=
    fun exn ->
      let label = string_of_exn exn in
      prerr_endline label;
      let title = s_ "Error" in
      let icon = GMisc.image () in
      icon#set_stock `DIALOG_ERROR;
      icon#set_icon_size `DIALOG;
      GToolbox.message_box ~title ~icon label

let make
    ~start_domain ~pause_domain ~resume_domain ~shutdown_domain
    ~open_domain_details =
  (* Create the main window. *)
  let window = GWindow.window ~width:800 ~height:600 ~title () in
  let vbox = GPack.vbox ~packing:window#add () in

  (* Menu bar. *)
  let quit_item =
    let menubar = GMenu.menu_bar ~packing:vbox#pack () in
    let factory = new GMenu.factory menubar in
    let accel_group = factory#accel_group in
    let file_menu = factory#add_submenu (s_ "File") in
    let help_menu = factory#add_submenu (s_ "Help") in

    window#add_accel_group accel_group;

    (* File menu. *)
    let factory = new GMenu.factory file_menu ~accel_group in
    let open_item = factory#add_item (s_ "Open connection ...")
      ~key:GdkKeysyms._O in
    ignore (factory#add_separator ());
    let quit_item = factory#add_item (s_ "Quit") ~key:GdkKeysyms._Q in

    ignore (open_item#connect#activate
	      ~callback:(Vc_connection_dlg.open_connection window));

    (* Help menu. *)
    let factory = new GMenu.factory help_menu ~accel_group in
    let help_item = factory#add_item (s_ "Help") in
    let help_about_item = factory#add_item (s_ "About ...") in

    ignore (help_about_item#connect#activate ~callback:help_about);

    quit_item in

  (* The toolbar. *)
  let toolbar = GButton.toolbar ~packing:vbox#pack () in

  (* The treeview. *)
  let (tree, model, columns, initial_state) =
    Vc_connections.make_treeview
      ~packing:(vbox#pack ~expand:true ~fill:true) () in

  (* Add buttons to the toolbar (requires the treeview to
   * have been made above).
   *)
  let () =
    let connect_button_menu = GMenu.menu () in
    let connect_button =
      GButton.menu_tool_button
	~label:(s_ "Connect ...") ~stock:`CONNECT
	~menu:connect_button_menu
	~packing:toolbar#insert () in
    ignore (GButton.separator_tool_item ~packing:toolbar#insert ());
    let open_button =
      GButton.tool_button ~label:(s_ "Details") ~stock:`OPEN
	~packing:toolbar#insert () in
    ignore (GButton.separator_tool_item ~packing:toolbar#insert ());
    let start_button =
      GButton.tool_button ~label:(s_ "Start") ~stock:`ADD
	~packing:toolbar#insert () in
    let pause_button =
      GButton.tool_button ~label:(s_ "Pause") ~stock:`MEDIA_PAUSE
	~packing:toolbar#insert () in
    let resume_button =
      GButton.tool_button ~label:(s_ "Resume") ~stock:`MEDIA_PLAY
	~packing:toolbar#insert () in
    ignore (GButton.separator_tool_item ~packing:toolbar#insert ());
    let shutdown_button =
      GButton.tool_button ~label:(s_ "Shutdown") ~stock:`STOP
	~packing:toolbar#insert () in

    (* Set callbacks for the toolbar buttons. *)
    ignore (connect_button#connect#clicked
	      ~callback:(Vc_connection_dlg.open_connection window));
    ignore (open_button#connect#clicked
	      ~callback:(open_domain_details tree model columns));
    ignore (start_button#connect#clicked
	      ~callback:(start_domain tree model columns));
    ignore (pause_button#connect#clicked
	      ~callback:(pause_domain tree model columns));
    ignore (resume_button#connect#clicked
	      ~callback:(resume_domain tree model columns));
    ignore (shutdown_button#connect#clicked
	      ~callback:(shutdown_domain tree model columns));

    (* Set a menu on the connect menu-button. *)
    let () =
      let factory = new GMenu.factory connect_button_menu (*~accel_group*) in
      let local_xen = factory#add_item (s_ "Local Xen") in
      let local_qemu = factory#add_item (s_ "Local QEMU/KVM") in
      ignore (factory#add_separator ());
      let open_dialog = factory#add_item (s_ "Connect to ...") in
      ignore (local_xen#connect#activate
		~callback:Vc_connection_dlg.open_local_xen);
      ignore (local_qemu#connect#activate
		~callback:Vc_connection_dlg.open_local_qemu);
      ignore (open_dialog#connect#activate
		~callback:(Vc_connection_dlg.open_connection window)) in
    () in

  (* Make a timeout function which is called once per second. *)
  let state = ref initial_state in
  let callback () =
    (* Gc.compact is generally not safe in lablgtk programs, but
     * is explicitly allowed in timeouts (see lablgtk README).
     * This ensures memory is compacted regularly, but is also an
     * excellent way to catch memory bugs in the ocaml libvirt bindings.
     *)
    Gc.compact ();

    (* Ugh: Bug in lablgtk causes a segfault if a timeout raises an
     * exception.  Catch and print exceptions instead.
     *)
    (try state := Vc_connections.repopulate tree model columns !state
     with exn -> prerr_endline (Printexc.to_string exn));

    true
  in
  let timeout_id = GMain.Timeout.add ~ms:1000 ~callback in

  (* Quit. *)
  let quit _ =
    GMain.Timeout.remove timeout_id;
    GMain.quit ();
    false
  in

  ignore (window#connect#destroy ~callback:GMain.quit);
  ignore (window#event#connect#delete ~callback:quit);
  ignore (quit_item#connect#activate
	    ~callback:(fun () -> ignore (quit ()); ()));

  (* Display the window. *)
  window#show ()
