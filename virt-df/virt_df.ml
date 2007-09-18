(* 'df' command for virtual domains.
 * $Id$
 *)

open Printf
open ExtList

module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network

let uri = ref None

let () =
  (* Command line argument parsing. *)
  let set_uri = function "" -> uri := None | u -> uri := Some u in

  let argspec = Arg.align [
    "-c", Arg.String set_uri, "uri Connect to URI (default: Xen)";
    "--connect", Arg.String set_uri, "uri Connect to URI (default: Xen)";
  ] in

  let anon_fun str = raise (Arg.Bad (str ^ ": unknown parameter")) in
  let usage_msg = "virt-df : like 'df', shows disk space used in guests

SUMMARY
  virt-df [-options]

OPTIONS" in

  Arg.parse argspec anon_fun usage_msg

let xmls =
  (* Connect to the hypervisor. *)
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

  (* Get the list of active & inactive domains. *)
  let doms =
    let nr_active_doms = C.num_of_domains conn in
    let active_doms = Array.to_list (C.list_domains conn nr_active_doms) in
    let active_doms = List.map (D.lookup_by_id conn) active_doms in
    let nr_inactive_doms = C.num_of_defined_domains conn in
    let inactive_doms =
      Array.to_list (C.list_defined_domains conn nr_inactive_doms) in
    let inactive_doms = List.map (D.lookup_by_name conn) inactive_doms in
    active_doms @ inactive_doms in

  (* Get their XML. *)
  let xmls = List.map D.get_xml_desc doms in

  (* Parse the XML. *)
  let xmls = List.map Xml.parse_string xmls in

  (* Return just the XML documents - everything else will be closed
   * and freed including the connection to the hypervisor.
   *)
  xmls

(* Parse out the device XML to get the names of disks. *)
type domain = {
  dom_name : string;			(* Domain name. *)
  dom_id : int option;			(* Domain ID (if running). *)
  dom_disks : disk list;		(* Domain disks. *)
}
and disk = {
  d_type : string option;		(* The <disk type=...> *)
  d_device : string option;		(* The <disk device=...> *)
  d_source : string option;		(* The <source file=... or dev> *)
  d_target : string option;		(* The <target dev=...> *)
}

let doms : domain list =
  (* Grr.. Need to use a library which has XPATH support (or cduce). *)
  List.map (
    fun xml ->
      let nodes, domain_attrs =
	match xml with
	| Xml.Element ("domain", attrs, children) -> children, attrs
	| _ -> failwith "get_xml_desc didn't return <domain/>" in

      let domid =
	try Some (int_of_string (List.assoc "id" domain_attrs))
	with Not_found -> None in

      let rec loop = function
	| [] ->
	    failwith "get_xml_desc returned no <name> node in XML"
	| Xml.Element ("name", _, [Xml.PCData name]) :: _ -> name
	| Xml.Element ("name", _, _) :: _ ->
	    failwith "get_xml_desc returned strange <name> node"
	| _ :: rest -> loop rest
      in
      let name = loop nodes in

      let devices =
	let devices =
	  List.filter_map (
	    function
	    | Xml.Element ("devices", _, devices) -> Some devices
	    | _ -> None
	  ) nodes in
	List.concat devices in

      let rec target_dev_of = function
	| [] -> None
	| Xml.Element ("target", attrs, _) :: rest ->
	    (try Some (List.assoc "dev" attrs)
	     with Not_found -> target_dev_of rest)
	| _ :: rest -> target_dev_of rest
      in

      let rec source_file_of = function
	| [] -> None
	| Xml.Element ("source", attrs, _) :: rest ->
	    (try Some (List.assoc "file" attrs)
	     with Not_found -> source_file_of rest)
	| _ :: rest -> source_file_of rest
      in

      let rec source_dev_of = function
	| [] -> None
	| Xml.Element ("source", attrs, _) :: rest ->
	    (try Some (List.assoc "dev" attrs)
	     with Not_found -> source_dev_of rest)
	| _ :: rest -> source_dev_of rest
      in

      let disks =
	List.filter_map (
	  function
	  | Xml.Element ("disk", attrs, children) ->
	      let typ =
		try Some (List.assoc "type" attrs)
		with Not_found -> None in
	      let device =
		try Some (List.assoc "device" attrs)
		with Not_found -> None in
	      let source =
		match source_file_of children with
		| (Some _) as source -> source
		| None -> source_dev_of children in
	      let target = target_dev_of children in

	      Some {
		d_type = typ; d_device = device;
		d_source = source; d_target = target
	      }
	  | _ -> None
	) devices in

      { dom_name = name; dom_id = domid; dom_disks = disks }
  ) xmls

(* Print the domains / devices. *)
let () =
  List.iter (
    fun { dom_name = dom_name; dom_disks = dom_disks } ->
      printf "%s:\n" dom_name;
      List.iter (
	function
	| { d_source = Some source; d_target = Some target } ->
	    printf "\t%s -> %s\n" source target
	| { d_type = None; d_device = Some "cdrom";
	    d_source = None; d_target = Some target } ->
	    printf "\t[CD] -> %s\n" target
	| _ ->
	    printf "\t(device omitted, missing <source> or <target> in XML\n";
      ) dom_disks
  ) doms
