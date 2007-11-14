(** OCaml bindings for libvirt.
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
(** This is a "raw" UUID, ie. a packed string of bytes. *)

type xml = string
(** Type of XML (an uninterpreted string of bytes).  Use PXP, expat,
    xml-light, etc. if you want to do anything useful with the XML.
*)

val get_version : ?driver:string -> unit -> int * int
  (** [get_version ()] returns the library version in the first part
      of the tuple, and [0] in the second part.

      [get_version ~driver ()] returns the library version in the first
      part of the tuple, and the version of the driver called [driver]
      in the second part.

      The version numbers are encoded as
      1,000,000 * major + 1,000 * minor + release.
  *)

val uuid_length : int
  (** Length of packed UUIDs. *)

val uuid_string_length : int
  (** Length of UUID strings. *)

(* These phantom types are used to ensure the type-safety of read-only
 * versus read-write connections.  For more information see:
 * http://caml.inria.fr/pub/ml-archives/caml-list/2004/07/80683af867cce6bf8fff273973f70c95.en.html
 *)
type rw = [`R|`W]
type ro = [`R]

module Connect :
sig
  type 'rw t
    (** Connection.  Read-only connections have type [ro Connect.t] and
	read-write connections have type [rw Connect.t].
      *)

  type node_info = {
    model : string;			(** CPU model *)
    memory : int64;			(** memory size in kilobytes *)
    cpus : int;				(** number of active CPUs *)
    mhz : int;				(** expected CPU frequency *)
    nodes : int;			(** number of NUMA nodes (1 = UMA) *)
    sockets : int;			(** number of CPU sockets per node *)
    cores : int;			(** number of cores per socket *)
    threads : int;			(** number of threads per core *)
  }

  val connect : ?name:string -> unit -> rw t
  val connect_readonly : ?name:string -> unit -> ro t
    (** [connect ~name ()] connects to the hypervisor with URI [name].

	[connect ()] connects to the default hypervisor.

	[connect_readonly] is the same but connects in read-only mode.
    *)

  val close : [>`R] t -> unit
    (** [close conn] closes and frees the connection object in memory.

	The connection is automatically closed if it is garbage
	collected.  This function just forces it to be closed
	and freed right away.
    *)

  val get_type : [>`R] t -> string
  val get_version : [>`R] t -> int
  val get_hostname : [>`R] t -> string
  val get_uri : [>`R] t -> string
  val get_max_vcpus : [>`R] t -> ?type_:string -> unit -> int
  val list_domains : [>`R] t -> int -> int array
  val num_of_domains : [>`R] t -> int
  val get_capabilities : [>`R] t -> string
  val num_of_defined_domains : [>`R] t -> int
  val list_defined_domains : [>`R] t -> int -> string array
  val num_of_networks : [>`R] t -> int
  val list_networks : [>`R] t -> int -> string array
  val num_of_defined_networks : [>`R] t -> int
  val list_defined_networks : [>`R] t -> int -> string array

    (* The name of this function is inconsistent, but the inconsistency
     * is really in libvirt itself.
     *)
  val get_node_info : [>`R] t -> node_info

  val node_get_free_memory : [> `R] t -> int64
    (**
       [node_get_free_memory conn]
       returns the amount of free memory (not allocated to any guest)
       in the machine.
    *)

  val node_get_cells_free_memory : [> `R] t -> int -> int -> int64 array
    (**
       [node_get_cells_free_memory conn start max]
       returns the amount of free memory on each NUMA cell in kilobytes.
       [start] is the first cell for which we return free memory.
       [max] is the maximum number of cells for which we return free memory.
       Returns an array of up to [max] entries in length.
    *)

  val maxcpus_of_node_info : node_info -> int
    (** Calculate the total number of CPUs supported (but not necessarily
	active) in the host.
    *)

  val cpumaplen : int -> int
    (** Calculate the length (in bytes) required to store the complete
	CPU map between a single virtual and all physical CPUs of a domain.
    *)

  val use_cpu : string -> int -> unit
    (** [use_cpu cpumap cpu] marks [cpu] as usable in [cpumap]. *)
  val unuse_cpu : string -> int -> unit
    (** [unuse_cpu cpumap cpu] marks [cpu] as not usable in [cpumap]. *)
  val cpu_usable : string -> int -> int -> int -> bool
    (** [cpu_usable cpumaps maplen vcpu cpu] checks returns true iff the
	[cpu] is usable by [vcpu]. *)

  external const : [>`R] t -> ro t = "%identity"
    (** [const conn] turns a read/write connection into a read-only
	connection.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with connections.  [Connect.t] is the
      connection object.
  *)

module Domain :
sig
  type 'rw t
    (** Domain handle.  Read-only handles have type [ro Domain.t] and
	read-write handles have type [rw Domain.t].
    *)

  type state =
    | InfoNoState | InfoRunning | InfoBlocked | InfoPaused
    | InfoShutdown | InfoShutoff | InfoCrashed

  type info = {
    state : state;			(** running state *)
    max_mem : int64;			(** maximum memory in kilobytes *)
    memory : int64;			(** memory used in kilobytes *)
    nr_virt_cpu : int;			(** number of virtual CPUs *)
    cpu_time : int64;			(** CPU time used in nanoseconds *)
  }

  type vcpu_state = VcpuOffline | VcpuRunning | VcpuBlocked

  type vcpu_info = {
    number : int;			(** virtual CPU number *)
    vcpu_state : vcpu_state;		(** state *)
    vcpu_time : int64;			(** CPU time used in nanoseconds *)
    cpu : int;				(** real CPU number, -1 if offline *)
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

  val create_linux : [>`W] Connect.t -> xml -> rw t
  val lookup_by_id : 'a Connect.t -> int -> 'a t
  val lookup_by_uuid : 'a Connect.t -> uuid -> 'a t
  val lookup_by_uuid_string : 'a Connect.t -> string -> 'a t
  val lookup_by_name : 'a Connect.t -> string -> 'a t
  val destroy : [>`W] t -> unit
  val free : [>`R] t -> unit
    (** [free domain] frees the domain object in memory.

	The domain object is automatically freed if it is garbage
	collected.  This function just forces it to be freed right
	away.
    *)

  val suspend : [>`W] t -> unit
  val resume : [>`W] t -> unit
  val save : [>`W] t -> string -> unit
  val restore : [>`W] Connect.t -> string -> unit
  val core_dump : [>`W] t -> string -> unit
  val shutdown : [>`W] t -> unit
  val reboot : [>`W] t -> unit
  val get_name : [>`R] t -> string
  val get_uuid : [>`R] t -> uuid
  val get_uuid_string : [>`R] t -> string
  val get_id : [>`R] t -> int
    (** [getid dom] returns the ID of the domain.

	Do not call this on a defined but not running domain.  Those
	domains don't have IDs, and you'll get an error here.
    *)

  val get_os_type : [>`R] t -> string
  val get_max_memory : [>`R] t -> int64
  val set_max_memory : [>`W] t -> int64 -> unit
  val set_memory : [>`W] t -> int64 -> unit
  val get_info : [>`R] t -> info
  val get_xml_desc : [>`R] t -> xml
  val get_scheduler_type : [>`R] t -> string * int
  val get_scheduler_parameters : [>`R] t -> int -> sched_param array
  val set_scheduler_parameters : [>`W] t -> sched_param array -> unit
  val define_xml : [>`W] Connect.t -> xml -> rw t
  val undefine : [>`W] t -> unit
  val create : [>`W] t -> unit
  val get_autostart : [>`R] t -> bool
  val set_autostart : [>`W] t -> bool -> unit
  val set_vcpus : [>`W] t -> int -> unit
  val pin_vcpu : [>`W] t -> int -> string -> unit
  val get_vcpus : [>`R] t -> int -> int -> int * vcpu_info array * string
  val get_max_vcpus : [>`R] t -> int
  val attach_device : [>`W] t -> xml -> unit
  val detach_device : [>`W] t -> xml -> unit

  val migrate : [>`W] t -> [>`W] Connect.t -> migrate_flag list ->
    ?dname:string -> ?uri:string -> ?bandwidth:int -> unit -> rw t

  val block_stats : [>`R] t -> string -> block_stats
  val interface_stats : [>`R] t -> string -> interface_stats

  external const : [>`R] t -> ro t = "%identity"
    (** [const dom] turns a read/write domain handle into a read-only
	domain handle.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with domains.  [Domain.t] is the
      domain object.
  *)

module Network : 
sig
  type 'rw t
    (** Network handle.  Read-only handles have type [ro Network.t] and
	read-write handles have type [rw Network.t].
    *)

  val lookup_by_name : 'a Connect.t -> string -> 'a t
  val lookup_by_uuid : 'a Connect.t -> uuid -> 'a t
  val lookup_by_uuid_string : 'a Connect.t -> string -> 'a t
  val create_xml : [>`W] Connect.t -> xml -> rw t
  val define_xml : [>`W] Connect.t -> xml -> rw t
  val undefine : [>`W] t -> unit
  val create : [>`W] t -> unit
  val destroy : [>`W] t -> unit
  val free : [>`R] t -> unit
    (** [free network] frees the network object in memory.

	The network object is automatically freed if it is garbage
	collected.  This function just forces it to be freed right
	away.
    *)

  val get_name : [>`R] t -> string
  val get_uuid : [>`R] t -> uuid
  val get_uuid_string : [>`R] t -> string
  val get_xml_desc : [>`R] t -> xml
  val get_bridge_name : [>`R] t -> string
  val get_autostart : [>`R] t -> bool
  val set_autostart : [>`W] t -> bool -> unit

  external const : [>`R] t -> ro t = "%identity"
    (** [const network] turns a read/write network handle into a read-only
	network handle.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with networks.  [Network.t] is the
      network object.
  *)

module Virterror :
sig
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
	(** See [<libvirt/virterror.h>] for meaning of these codes. *)

  val string_of_code : code -> string

  type level =
    | VIR_ERR_NONE
    | VIR_ERR_WARNING
    | VIR_ERR_ERROR
	(** No error, a warning or an error. *)

  val string_of_level : level -> string

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
	(** Subsystem / driver which produced the error. *)

  val string_of_domain : domain -> string

  type t = {
    code : code;			(** Error code. *)
    domain : domain;			(** Origin of the error. *)
    message : string option;		(** Human-readable message. *)
    level : level;			(** Error or warning. *)
    conn : ro Connect.t option;		(** Associated connection. *)
    dom : ro Domain.t option;		(** Associated domain. *)
    str1 : string option;		(** Informational string. *)
    str2 : string option;		(** Informational string. *)
    str3 : string option;		(** Informational string. *)
    int1 : int32;			(** Informational integer. *)
    int2 : int32;			(** Informational integer. *)
    net : ro Network.t option;		(** Associated network. *)
  }
    (** An error object. *)

  val to_string : t -> string
    (** Turn the exception into a printable string. *)

  val get_last_error : unit -> t option
  val get_last_conn_error : [>`R] Connect.t -> t option
    (** Get the last error at a global or connection level.

	Normally you do not need to use these functions because
	the library automatically turns errors into exceptions.
    *)

  val reset_last_error : unit -> unit
  val reset_last_conn_error : [>`R] Connect.t -> unit
    (** Reset the error at a global or connection level.

	Normally you do not need to use these functions.
    *)

  val no_error : unit -> t
    (** Creates an empty error message.

	Normally you do not need to use this function.
    *)
end
  (** Module dealing with errors. *)

exception Virterror of Virterror.t
(** This exception can be raised by any library function that detects
    an error.  To get a printable error message, call
    {!Virterror.to_string} on the content of this exception.

    Note that functions may also raise
    [Invalid_argument "virFoo not supported"]
    (where virFoo is the libvirt function name) if a function is
    not supported at either compile or runtime.  This applies to
    any libvirt function added after version 0.2.1.
    See also [http://libvirt.org/hvsupport.html]
*)

