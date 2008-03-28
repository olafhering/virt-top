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

open Virt_ctrl_gettext.Gettext

type name = string
type uri = string
type service = name * uri

let local_xen_uri = "xen:///"
let local_qemu_uri = "qemu:///system"

(* Code in Vc_dbus overrides this, if that capability was compiled in. *)
let find_libvirtd_with_zeroconf = ref (fun () -> [])

(* Code in Vc_icons may override these with icons. *)
let icon_16x16_devices_computer_png = ref None
let icon_24x24_devices_computer_png = ref None
let icon_32x32_devices_computer_png = ref None
let icon_48x48_devices_computer_png = ref None

(* Open connection dialog. *)
let open_connection parent () =
  let title = s_ "Open connection to hypervisor" in
  let position = `CENTER_ON_PARENT in

  let dlg = GWindow.dialog ~title ~position ~parent
    ~modal:true ~width:450 () in

  (* We will enter the Gtk main loop recursively.  Wire up close and
   * other buttons to quit the recursive main loop.
   *)
  ignore (dlg#connect#destroy ~callback:GMain.quit);
  ignore (dlg#event#connect#delete
	    ~callback:(fun _ -> GMain.quit (); false));

  let uri = ref None in

  (* Pack the buttons into the dialog. *)
  let vbox = dlg#vbox in
  vbox#set_spacing 5;

  (* Local connections. *)
  let () =
    let frame =
      GBin.frame ~label:(s_ "This machine") ~packing:vbox#pack () in
    let hbox = GPack.hbox ~packing:frame#add () in
    hbox#set_spacing 20;
    ignore (
      let packing = hbox#pack in
      match !icon_24x24_devices_computer_png with
      | None -> GMisc.image ~stock:`DIRECTORY ~packing ()
      | Some pixbuf -> GMisc.image ~pixbuf ~packing ()
    );

    let vbox = GPack.vbox ~packing:hbox#pack () in
    vbox#set_spacing 5;

    let xen_button =
      GButton.button ~label:(s_ "Xen hypervisor")
	~packing:vbox#pack () in
    ignore (xen_button#connect#clicked
	      ~callback:(fun () ->
			   uri := Some local_xen_uri;
			   dlg#destroy ()));
    let qemu_button =
      GButton.button ~label:(s_ "QEMU or KVM")
	~packing:vbox#pack () in
    ignore (qemu_button#connect#clicked
	      ~callback:(fun () ->
			   uri := Some local_qemu_uri;
			   dlg#destroy ())) in

  (* Network connections. *)
  let () =
    let frame =
      GBin.frame ~label:(s_ "Local network")
	~packing:(vbox#pack ~expand:true) () in
    let hbox = GPack.hbox ~packing:frame#add () in
    hbox#set_spacing 20;
    ignore (GMisc.image ~stock:`NETWORK ~packing:hbox#pack ());

    let vbox = GPack.vbox ~packing:(hbox#pack ~expand:true) () in
    vbox#set_spacing 5;

    let cols = new GTree.column_list in
    (*let col_icon = cols#add Gobject.Data.string in*)
    let col_name = cols#add Gobject.Data.string in
    let model = GTree.list_store cols in

    let icons = GTree.icon_view
      ~selection_mode:`SINGLE ~model
      ~height:200
      ~packing:(vbox#pack ~expand:true ~fill:true) () in
    icons#set_border_width 4;

    (*icons#set_pixbuf_column col_icon;*)
    icons#set_text_column col_name;

    let refresh () =
      model#clear ();
      let services = !find_libvirtd_with_zeroconf () in

      (*let pixbuf = !icon_16x16_devices_computer_png in*)
      List.iter (
	fun (name, _) ->
	  let row = model#append () in
	  model#set ~row ~column:col_name name;
	  (*match pixbuf with
	    | None -> ()
	    | Some pixbuf -> model#set ~row ~column:col_icon pixbuf*)
      ) services
    in
    refresh ();

    let hbox = GPack.hbox ~packing:vbox#pack () in
    let refresh_button =
      GButton.button ~label:(s_ "Refresh")
	~stock:`REFRESH ~packing:hbox#pack () in
    let open_button =
      GButton.button ~label:(s_ "Open") ~packing:hbox#pack () in

    ignore (refresh_button#connect#clicked ~callback:refresh);

    (* Function callback when someone selects and hits Open. *)
    let callback () =
      match icons#get_selected_items with
      | [] -> () (* nothing selected *)
      | path :: _ ->
	  let row = model#get_iter path in
	  let name = model#get ~row ~column:col_name in
	  let services = !find_libvirtd_with_zeroconf () in
	  try
	    uri := Some (List.assoc name services);
	    dlg#destroy ()
	  with
	    Not_found -> () in

    ignore (open_button#connect#clicked ~callback) in

  (* Custom connections. *)
  let () =
    let frame =
      GBin.frame ~label:(s_ "URI connection") ~packing:vbox#pack () in
    let hbox = GPack.hbox ~packing:frame#add () in
    hbox#set_spacing 20;
    ignore (GMisc.image ~stock:`CONNECT ~packing:hbox#pack ());

    let hbox = GPack.hbox ~packing:(hbox#pack ~expand:true) () in
    let entry =
      GEdit.entry ~text:"xen://localhost/"
	~packing:(hbox#pack ~expand:true ~fill:true) () in
    let button =
      GButton.button ~label:(s_ "Open") ~packing:hbox#pack () in

    ignore (button#connect#clicked
	      ~callback:(fun () ->
			   uri := Some entry#text;
			   dlg#destroy ()));

    () in


  (* Just a cancel button in the action area. *)
  let cancel_button =
    GButton.button ~label:(s_ "Cancel")
      ~packing:dlg#action_area#pack () in
  ignore (cancel_button#connect#clicked
	    ~callback:(fun () ->
			 uri := None;
			 dlg#destroy ()));

  dlg#show ();

  (* Enter Gtk main loop recursively. *)
  GMain.main ();

  match !uri with
  | None -> ()
  | Some uri -> Vc_connections.open_connection uri

(* Callback from the Connect button drop-down menu. *)
let open_local_xen () =
  Vc_connections.open_connection local_xen_uri

let open_local_qemu () =
  Vc_connections.open_connection local_qemu_uri
