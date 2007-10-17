(* OCaml bindings for libvirt.
   (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
   http://libvirt.org/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*)

type uuid = string

type xml = string

external get_version : ?driver:string -> unit -> int * int = "ocaml_libvirt_get_version"

let uuid_length = 16
let uuid_string_length = 36

(* http://caml.inria.fr/pub/ml-archives/caml-list/2004/07/80683af867cce6bf8fff273973f70c95.en.html *)
type rw = [`R|`W]
type ro = [`R]

module Connect =
struct
  type 'rw t

  type node_info = {
    model : string;
    memory : int64;
    cpus : int;
    mhz : int;
    nodes : int;
    sockets : int;
    cores : int;
    threads : int;
  }

  external connect : ?name:string -> unit -> rw t = "ocaml_libvirt_connect_open"
  external connect_readonly : ?name:string -> unit -> ro t = "ocaml_libvirt_connect_open_readonly"
  external close : [>`R] t -> unit = "ocaml_libvirt_connect_close"
  external get_type : [>`R] t -> string = "ocaml_libvirt_connect_get_type"
  external get_version : [>`R] t -> int = "ocaml_libvirt_connect_get_version"
  external get_hostname : [>`R] t -> string = "ocaml_libvirt_connect_get_hostname"
  external get_uri : [>`R] t -> string = "ocaml_libvirt_connect_get_uri"
  external get_max_vcpus : [>`R] t -> ?type_:string -> unit -> int = "ocaml_libvirt_connect_get_max_vcpus"
  external list_domains : [>`R] t -> int -> int array = "ocaml_libvirt_connect_list_domains"
  external num_of_domains : [>`R] t -> int = "ocaml_libvirt_connect_num_of_domains"
  external get_capabilities : [>`R] t -> string = "ocaml_libvirt_connect_get_capabilities"
  external num_of_defined_domains : [>`R] t -> int = "ocaml_libvirt_connect_num_of_defined_domains"
  external list_defined_domains : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_defined_domains"
  external num_of_networks : [>`R] t -> int = "ocaml_libvirt_connect_num_of_networks"
  external list_networks : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_networks"
  external num_of_defined_networks : [>`R] t -> int = "ocaml_libvirt_connect_num_of_defined_networks"
  external list_defined_networks : [>`R] t -> int -> string array = "ocaml_libvirt_connect_list_defined_networks"
  external get_node_info : [>`R] t -> node_info = "ocaml_libvirt_connect_get_node_info"

  (* See VIR_NODEINFO_MAXCPUS macro defined in <libvirt.h>. *)
  let maxcpus_of_node_info { nodes = nodes; sockets = sockets;
			     cores = cores; threads = threads } =
    nodes * sockets * cores * threads

  (* See VIR_CPU_MAPLEN macro defined in <libvirt.h>. *)
  let cpumaplen nr_cpus =
    (nr_cpus + 7) / 8

  (* See VIR_USE_CPU, VIR_UNUSE_CPU, VIR_CPU_USABLE macros defined in <libvirt.h>. *)
  let use_cpu cpumap cpu =
    cpumap.[cpu/8] <-
      Char.chr (Char.code cpumap.[cpu/8] lor (1 lsl (cpu mod 8)))
  let unuse_cpu cpumap cpu =
    cpumap.[cpu/8] <-
      Char.chr (Char.code cpumap.[cpu/8] land (lnot (1 lsl (cpu mod 8))))
  let cpu_usable cpumaps maplen vcpu cpu =
    Char.code cpumaps.[vcpu*maplen + cpu/8] land (1 lsl (cpu mod 8)) <> 0

  external const : [>`R] t -> ro t = "%identity"
end

module Domain =
struct
  type 'rw dom
  type 'rw t = 'rw dom * 'rw Connect.t

  type state =
    | InfoNoState | InfoRunning | InfoBlocked | InfoPaused
    | InfoShutdown | InfoShutoff | InfoCrashed

  type info = {
    state : state;
    max_mem : int64;
    memory : int64;
    nr_virt_cpu : int;
    cpu_time : int64;
  }

  type vcpu_state = VcpuOffline | VcpuRunning | VcpuBlocked

  type vcpu_info = {
    number : int;
    vcpu_state : vcpu_state;
    vcpu_time : int64;
    cpu : int;
  }

  type sched_param = string * sched_param_value
  and sched_param_value =
    | SchedFieldInt32 of int32 | SchedFieldUInt32 of int32
    | SchedFieldInt64 of int64 | SchedFieldUInt64 of int64
    | SchedFieldFloat of float | SchedFieldBool of bool

  type migrate_flag = Live

  type block_stats = {
    rd_req : int64;
    rd_bytes : int64;
    wr_req : int64;
    wr_bytes : int64;
    errs : int64;
  }

  type interface_stats = {
    rx_bytes : int64;
    rx_packets : int64;
    rx_errs : int64;
    rx_drop : int64;
    tx_bytes : int64;
    tx_packets : int64;
    tx_errs : int64;
    tx_drop : int64;
  }

  external create_linux : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_domain_create_linux"
  external lookup_by_id : 'a Connect.t -> int -> 'a t = "ocaml_libvirt_domain_lookup_by_id"
  external lookup_by_uuid : 'a Connect.t -> uuid -> 'a t = "ocaml_libvirt_domain_lookup_by_uuid"
  external lookup_by_uuid_string : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_domain_lookup_by_uuid_string"
  external lookup_by_name : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_domain_lookup_by_name"
  external destroy : [>`W] t -> unit = "ocaml_libvirt_domain_destroy"
  external free : [>`R] t -> unit = "ocaml_libvirt_domain_free"
  external suspend : [>`W] t -> unit = "ocaml_libvirt_domain_suspend"
  external resume : [>`W] t -> unit = "ocaml_libvirt_domain_resume"
  external save : [>`W] t -> string -> unit = "ocaml_libvirt_domain_save"
  external restore : [>`W] Connect.t -> string -> unit = "ocaml_libvirt_domain_restore"
  external core_dump : [>`W] t -> string -> unit = "ocaml_libvirt_domain_core_dump"
  external shutdown : [>`W] t -> unit = "ocaml_libvirt_domain_shutdown"
  external reboot : [>`W] t -> unit = "ocaml_libvirt_domain_reboot"
  external get_name : [>`R] t -> string = "ocaml_libvirt_domain_get_name"
  external get_uuid : [>`R] t -> uuid = "ocaml_libvirt_domain_get_uuid"
  external get_uuid_string : [>`R] t -> string = "ocaml_libvirt_domain_get_uuid_string"
  external get_id : [>`R] t -> int = "ocaml_libvirt_domain_get_id"
  external get_os_type : [>`R] t -> string = "ocaml_libvirt_domain_get_os_type"
  external get_max_memory : [>`R] t -> int64 = "ocaml_libvirt_domain_get_max_memory"
  external set_max_memory : [>`W] t -> int64 -> unit = "ocaml_libvirt_domain_set_max_memory"
  external set_memory : [>`W] t -> int64 -> unit = "ocaml_libvirt_domain_set_memory"
  external get_info : [>`R] t -> info = "ocaml_libvirt_domain_get_info"
  external get_xml_desc : [>`R] t -> xml = "ocaml_libvirt_domain_get_xml_desc"
  external get_scheduler_type : [>`R] t -> string * int = "ocaml_libvirt_domain_get_scheduler_type"
  external get_scheduler_parameters : [>`R] t -> int -> sched_param array = "ocaml_libvirt_domain_get_scheduler_parameters"
  external set_scheduler_parameters : [>`W] t -> sched_param array -> unit = "ocaml_libvirt_domain_set_scheduler_parameters"
  external define_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_domain_define_xml"
  external undefine : [>`W] t -> unit = "ocaml_libvirt_domain_undefine"
  external create : [>`W] t -> unit = "ocaml_libvirt_domain_create"
  external get_autostart : [>`R] t -> bool = "ocaml_libvirt_domain_get_autostart"
  external set_autostart : [>`W] t -> bool -> unit = "ocaml_libvirt_domain_set_autostart"
  external set_vcpus : [>`W] t -> int -> unit = "ocaml_libvirt_domain_set_vcpus"
  external pin_vcpu : [>`W] t -> int -> string -> unit = "ocaml_libvirt_domain_pin_vcpu"
  external get_vcpus : [>`R] t -> int -> int -> int * vcpu_info array * string = "ocaml_libvirt_domain_get_vcpus"
  external get_max_vcpus : [>`R] t -> int = "ocaml_libvirt_domain_get_max_vcpus"
  external attach_device : [>`W] t -> xml -> unit = "ocaml_libvirt_domain_attach_device"
  external detach_device : [>`W] t -> xml -> unit = "ocaml_libvirt_domain_detach_device"
  external migrate : [>`W] t -> [>`W] Connect.t -> migrate_flag list -> ?dname:string -> ?uri:string -> ?bandwidth:int -> unit -> rw t = "ocaml_libvirt_domain_migrate_bytecode" "ocaml_libvirt_domain_migrate_native"
  external block_stats : [>`R] t -> string -> block_stats = "ocaml_libvirt_domain_block_stats"
  external interface_stats : [>`R] t -> string -> interface_stats = "ocaml_libvirt_domain_interface_stats"

  external const : [>`R] t -> ro t = "%identity"
end

module Network =
struct
  type 'rw net
  type 'rw t = 'rw net * 'rw Connect.t

  external lookup_by_name : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_network_lookup_by_name"
  external lookup_by_uuid : 'a Connect.t -> uuid -> 'a t = "ocaml_libvirt_network_lookup_by_uuid"
  external lookup_by_uuid_string : 'a Connect.t -> string -> 'a t = "ocaml_libvirt_network_lookup_by_uuid_string"
  external create_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_network_create_xml"
  external define_xml : [>`W] Connect.t -> xml -> rw t = "ocaml_libvirt_network_define_xml"
  external undefine : [>`W] t -> unit = "ocaml_libvirt_network_undefine"
  external create : [>`W] t -> unit = "ocaml_libvirt_network_create"
  external destroy : [>`W] t -> unit = "ocaml_libvirt_network_destroy"
  external free : [>`R] t -> unit = "ocaml_libvirt_network_free"
  external get_name : [>`R] t -> string = "ocaml_libvirt_network_get_name"
  external get_uuid : [>`R] t -> uuid = "ocaml_libvirt_network_get_uuid"
  external get_uuid_string : [>`R] t -> string = "ocaml_libvirt_network_get_uuid_string"
  external get_xml_desc : [>`R] t -> xml = "ocaml_libvirt_network_get_xml_desc"
  external get_bridge_name : [>`R] t -> string = "ocaml_libvirt_network_get_bridge_name"
  external get_autostart : [>`R] t -> bool = "ocaml_libvirt_network_get_autostart"
  external set_autostart : [>`W] t -> bool -> unit = "ocaml_libvirt_network_set_autostart"

  external const : [>`R] t -> ro t = "%identity"
end

module Virterror =
struct
  type code =
    | VIR_ERR_OK
    | VIR_ERR_INTERNAL_ERROR
    | VIR_ERR_NO_MEMORY
    | VIR_ERR_NO_SUPPORT
    | VIR_ERR_UNKNOWN_HOST
    | VIR_ERR_NO_CONNECT
    | VIR_ERR_INVALID_CONN
    | VIR_ERR_INVALID_DOMAIN
    | VIR_ERR_INVALID_ARG
    | VIR_ERR_OPERATION_FAILED
    | VIR_ERR_GET_FAILED
    | VIR_ERR_POST_FAILED
    | VIR_ERR_HTTP_ERROR
    | VIR_ERR_SEXPR_SERIAL
    | VIR_ERR_NO_XEN
    | VIR_ERR_XEN_CALL
    | VIR_ERR_OS_TYPE
    | VIR_ERR_NO_KERNEL
    | VIR_ERR_NO_ROOT
    | VIR_ERR_NO_SOURCE
    | VIR_ERR_NO_TARGET
    | VIR_ERR_NO_NAME
    | VIR_ERR_NO_OS
    | VIR_ERR_NO_DEVICE
    | VIR_ERR_NO_XENSTORE
    | VIR_ERR_DRIVER_FULL
    | VIR_ERR_CALL_FAILED
    | VIR_ERR_XML_ERROR
    | VIR_ERR_DOM_EXIST
    | VIR_ERR_OPERATION_DENIED
    | VIR_ERR_OPEN_FAILED
    | VIR_ERR_READ_FAILED
    | VIR_ERR_PARSE_FAILED
    | VIR_ERR_CONF_SYNTAX
    | VIR_ERR_WRITE_FAILED
    | VIR_ERR_XML_DETAIL
    | VIR_ERR_INVALID_NETWORK
    | VIR_ERR_NETWORK_EXIST
    | VIR_ERR_SYSTEM_ERROR
    | VIR_ERR_RPC
    | VIR_ERR_GNUTLS_ERROR
    | VIR_WAR_NO_NETWORK
    | VIR_ERR_NO_DOMAIN
    | VIR_ERR_NO_NETWORK

  let string_of_code = function
    | VIR_ERR_OK -> "VIR_ERR_OK"
    | VIR_ERR_INTERNAL_ERROR -> "VIR_ERR_INTERNAL_ERROR"
    | VIR_ERR_NO_MEMORY -> "VIR_ERR_NO_MEMORY"
    | VIR_ERR_NO_SUPPORT -> "VIR_ERR_NO_SUPPORT"
    | VIR_ERR_UNKNOWN_HOST -> "VIR_ERR_UNKNOWN_HOST"
    | VIR_ERR_NO_CONNECT -> "VIR_ERR_NO_CONNECT"
    | VIR_ERR_INVALID_CONN -> "VIR_ERR_INVALID_CONN"
    | VIR_ERR_INVALID_DOMAIN -> "VIR_ERR_INVALID_DOMAIN"
    | VIR_ERR_INVALID_ARG -> "VIR_ERR_INVALID_ARG"
    | VIR_ERR_OPERATION_FAILED -> "VIR_ERR_OPERATION_FAILED"
    | VIR_ERR_GET_FAILED -> "VIR_ERR_GET_FAILED"
    | VIR_ERR_POST_FAILED -> "VIR_ERR_POST_FAILED"
    | VIR_ERR_HTTP_ERROR -> "VIR_ERR_HTTP_ERROR"
    | VIR_ERR_SEXPR_SERIAL -> "VIR_ERR_SEXPR_SERIAL"
    | VIR_ERR_NO_XEN -> "VIR_ERR_NO_XEN"
    | VIR_ERR_XEN_CALL -> "VIR_ERR_XEN_CALL"
    | VIR_ERR_OS_TYPE -> "VIR_ERR_OS_TYPE"
    | VIR_ERR_NO_KERNEL -> "VIR_ERR_NO_KERNEL"
    | VIR_ERR_NO_ROOT -> "VIR_ERR_NO_ROOT"
    | VIR_ERR_NO_SOURCE -> "VIR_ERR_NO_SOURCE"
    | VIR_ERR_NO_TARGET -> "VIR_ERR_NO_TARGET"
    | VIR_ERR_NO_NAME -> "VIR_ERR_NO_NAME"
    | VIR_ERR_NO_OS -> "VIR_ERR_NO_OS"
    | VIR_ERR_NO_DEVICE -> "VIR_ERR_NO_DEVICE"
    | VIR_ERR_NO_XENSTORE -> "VIR_ERR_NO_XENSTORE"
    | VIR_ERR_DRIVER_FULL -> "VIR_ERR_DRIVER_FULL"
    | VIR_ERR_CALL_FAILED -> "VIR_ERR_CALL_FAILED"
    | VIR_ERR_XML_ERROR -> "VIR_ERR_XML_ERROR"
    | VIR_ERR_DOM_EXIST -> "VIR_ERR_DOM_EXIST"
    | VIR_ERR_OPERATION_DENIED -> "VIR_ERR_OPERATION_DENIED"
    | VIR_ERR_OPEN_FAILED -> "VIR_ERR_OPEN_FAILED"
    | VIR_ERR_READ_FAILED -> "VIR_ERR_READ_FAILED"
    | VIR_ERR_PARSE_FAILED -> "VIR_ERR_PARSE_FAILED"
    | VIR_ERR_CONF_SYNTAX -> "VIR_ERR_CONF_SYNTAX"
    | VIR_ERR_WRITE_FAILED -> "VIR_ERR_WRITE_FAILED"
    | VIR_ERR_XML_DETAIL -> "VIR_ERR_XML_DETAIL"
    | VIR_ERR_INVALID_NETWORK -> "VIR_ERR_INVALID_NETWORK"
    | VIR_ERR_NETWORK_EXIST -> "VIR_ERR_NETWORK_EXIST"
    | VIR_ERR_SYSTEM_ERROR -> "VIR_ERR_SYSTEM_ERROR"
    | VIR_ERR_RPC -> "VIR_ERR_RPC"
    | VIR_ERR_GNUTLS_ERROR -> "VIR_ERR_GNUTLS_ERROR"
    | VIR_WAR_NO_NETWORK -> "VIR_WAR_NO_NETWORK"
    | VIR_ERR_NO_DOMAIN -> "VIR_ERR_NO_DOMAIN"
    | VIR_ERR_NO_NETWORK -> "VIR_ERR_NO_NETWORK"

  type level =
    | VIR_ERR_NONE
    | VIR_ERR_WARNING
    | VIR_ERR_ERROR

  let string_of_level = function
    | VIR_ERR_NONE -> "VIR_ERR_NONE"
    | VIR_ERR_WARNING -> "VIR_ERR_WARNING"
    | VIR_ERR_ERROR -> "VIR_ERR_ERROR"

  type domain =
    | VIR_FROM_NONE
    | VIR_FROM_XEN
    | VIR_FROM_XEND
    | VIR_FROM_XENSTORE
    | VIR_FROM_SEXPR
    | VIR_FROM_XML
    | VIR_FROM_DOM
    | VIR_FROM_RPC
    | VIR_FROM_PROXY
    | VIR_FROM_CONF
    | VIR_FROM_QEMU
    | VIR_FROM_NET
    | VIR_FROM_TEST
    | VIR_FROM_REMOTE

  let string_of_domain = function
    | VIR_FROM_NONE -> "VIR_FROM_NONE"
    | VIR_FROM_XEN -> "VIR_FROM_XEN"
    | VIR_FROM_XEND -> "VIR_FROM_XEND"
    | VIR_FROM_XENSTORE -> "VIR_FROM_XENSTORE"
    | VIR_FROM_SEXPR -> "VIR_FROM_SEXPR"
    | VIR_FROM_XML -> "VIR_FROM_XML"
    | VIR_FROM_DOM -> "VIR_FROM_DOM"
    | VIR_FROM_RPC -> "VIR_FROM_RPC"
    | VIR_FROM_PROXY -> "VIR_FROM_PROXY"
    | VIR_FROM_CONF -> "VIR_FROM_CONF"
    | VIR_FROM_QEMU -> "VIR_FROM_QEMU"
    | VIR_FROM_NET -> "VIR_FROM_NET"
    | VIR_FROM_TEST -> "VIR_FROM_TEST"
    | VIR_FROM_REMOTE -> "VIR_FROM_REMOTE"

  type t = {
    code : code;
    domain : domain;
    message : string option;
    level : level;
    conn : ro Connect.t option;
    dom : ro Domain.t option;
    str1 : string option;
    str2 : string option;
    str3 : string option;
    int1 : int32;
    int2 : int32;
    net : ro Network.t option;
  }

  let to_string { code = code; domain = domain; message = message } =
    let buf = Buffer.create 128 in
    Buffer.add_string buf "libvirt: ";
    Buffer.add_string buf (string_of_code code);
    Buffer.add_string buf ": ";
    Buffer.add_string buf (string_of_domain domain);
    Buffer.add_string buf ": ";
    (match message with Some msg -> Buffer.add_string buf msg | None -> ());
    Buffer.contents buf

  external get_last_error : unit -> t option = "ocaml_libvirt_virterror_get_last_error"
  external get_last_conn_error : [>`R] Connect.t -> t option = "ocaml_libvirt_virterror_get_last_conn_error"
  external reset_last_error : unit -> unit = "ocaml_libvirt_virterror_reset_last_error"
  external reset_last_conn_error : [>`R] Connect.t -> unit = "ocaml_libvirt_virterror_reset_last_conn_error"

  let no_error () =
    { code = VIR_ERR_OK; domain = VIR_FROM_NONE; message = None;
      level = VIR_ERR_NONE; conn = None; dom = None;
      str1 = None; str2 = None; str3 = None;
      int1 = 0_l; int2 = 0_l; net = None }
end

exception Virterror of Virterror.t

(* Initialization. *)
external c_init : unit -> unit = "ocaml_libvirt_init"
let () =
  Callback.register_exception
    "ocaml_libvirt_virterror" (Virterror (Virterror.no_error ()));
  c_init ()
