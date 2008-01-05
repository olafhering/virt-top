(* virt-manager-like graphical management tool.
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

let title = "Virtual Machine Manager"

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
    ("Virtual machine manager (OCaml version) by\n" ^
     "Richard W.M. Jones (rjones@redhat.com).\n\n" ^
     "Copyright " ^ utf8_copyright ^ " 2007 Red Hat Inc.\n\n" ^
     "Libvirt version: " ^ virt_version ^ "\n" ^
     "Gtk toolkit version: " ^ gtk_version)

(* Catch any exception and throw up a dialog. *)
let () =
  (* A nicer exception printing function. *)
  let string_of_exn = function
    | Libvirt.Virterror err ->
	"Virtualisation error: " ^ (Libvirt.Virterror.to_string err)
    | Failure msg -> msg
    | exn -> Printexc.to_string exn
  in
  GtkSignal.user_handler :=
    fun exn ->
      let label = string_of_exn exn in
      let title = "Error" in
      let icon = GMisc.image () in
      icon#set_stock `DIALOG_ERROR;
      icon#set_icon_size `DIALOG;
      GToolbox.message_box ~title ~icon label

let make ~open_connection
    ~start_domain ~pause_domain ~resume_domain ~shutdown_domain =
  (* Create the main window. *)
  let window = GWindow.window ~width:800 ~height:600 ~title () in
  let vbox = GPack.vbox ~packing:window#add () in

  (* Menu bar. *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let help_menu = factory#add_submenu "Help" in

  (* File menu. *)
  let factory = new GMenu.factory file_menu ~accel_group in
  let open_item = factory#add_item "Open connection ..."
    ~key:GdkKeysyms._O in
  ignore (factory#add_separator ());
  let quit_item = factory#add_item "Quit" ~key:GdkKeysyms._Q in

  ignore (open_item#connect#activate ~callback:open_connection);

  (* Help menu. *)
  let factory = new GMenu.factory help_menu ~accel_group in
  let help_item = factory#add_item "Help" in
  let help_about_item = factory#add_item "About ..." in

  ignore (help_about_item#connect#activate ~callback:help_about);

  (* The toolbar. *)
  let toolbar = GButton.toolbar ~packing:vbox#pack () in
  let connect_button =
    GButton.tool_button ~label:"Connect ..." ~stock:`CONNECT
      ~packing:toolbar#insert () in
  let start_button =
    GButton.tool_button ~label:"Start" ~stock:`ADD
      ~packing:toolbar#insert () in
  let pause_button =
    GButton.tool_button ~label:"Pause" ~stock:`MEDIA_PAUSE
      ~packing:toolbar#insert () in
  let resume_button =
    GButton.tool_button ~label:"Resume" ~stock:`MEDIA_PLAY
      ~packing:toolbar#insert () in
  let shutdown_button =
    GButton.tool_button ~label:"Shutdown" ~stock:`STOP
      ~packing:toolbar#insert () in
  ignore (connect_button#connect#clicked ~callback:open_connection);

  (* The treeview. *)
  let (tree, model, columns, initial_state) =
    Mlvirtmanager_connections.make_treeview
      ~packing:(vbox#pack ~expand:true ~fill:true) () in

  ignore (start_button#connect#clicked
	    ~callback:(start_domain tree model columns));
  ignore (pause_button#connect#clicked
	    ~callback:(pause_domain tree model columns));
  ignore (resume_button#connect#clicked
	    ~callback:(resume_domain tree model columns));
  ignore (shutdown_button#connect#clicked
	    ~callback:(shutdown_domain tree model columns));

  (* Make a timeout function which is called once per second. *)
  let state = ref initial_state in
  let callback () =
    state := Mlvirtmanager_connections.repopulate tree model columns !state;
    true
  in
  let timeout_id = GMain.Timeout.add ~ms:1000 ~callback in

  (* Quit. *)
  let quit _ =
    GMain.Timeout.remove timeout_id;
    GMain.Main.quit ();
    false
  in

  ignore (window#connect#destroy ~callback:GMain.quit);
  ignore (window#event#connect#delete ~callback:quit);
  ignore (quit_item#connect#activate
	    ~callback:(fun () -> ignore (quit ()); ()));

  window#add_accel_group accel_group;

  (* Display the window. *)
  window#show ()
