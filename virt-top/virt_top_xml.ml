(* 'top'-like tool for libvirt domains.
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

   This file contains all code which requires xml-light.
*)

open ExtList

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network ;;

Virt_top.parse_device_xml :=
fun id dom ->
  try
    let xml = D.get_xml_desc dom in
    let xml = Xml.parse_string xml in
    let devices =
      match xml with
      | Xml.Element ("domain", _, children) ->
	  let devices =
	    List.filter_map (
	      function
	      | Xml.Element ("devices", _, devices) -> Some devices
	      | _ -> None
	    ) children in
	  List.concat devices
      | _ ->
	  failwith "get_xml_desc didn't return <domain/>" in
    let rec target_dev_of = function
      | [] -> None
      | Xml.Element ("target", attrs, _) :: rest ->
	  (try Some (List.assoc "dev" attrs)
	   with Not_found -> target_dev_of rest)
      | _ :: rest -> target_dev_of rest
    in
    let blkdevs =
      List.filter_map (
	function
	| Xml.Element ("disk", _, children) -> target_dev_of children
	| _ -> None
      ) devices in
    let netifs =
      List.filter_map (
	function
	| Xml.Element ("interface", _, children) -> target_dev_of children
	| _ -> None
      ) devices in
    blkdevs, netifs
  with
  | Xml.Error _
  | Libvirt.Virterror _ -> [], [] (* ignore transient errs *)
