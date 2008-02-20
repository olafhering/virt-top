(** OCaml bindings for libvirt. *)
(* (C) Copyright 2007 Richard W.M. Jones, Red Hat Inc.
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

(**
   {2 Introduction and examples}

   This is a set of bindings for writing OCaml programs to
   manage virtual machines through {{:http://libvirt.org/}libvirt}.

   {3 Using libvirt interactively}

   Using the interactive toplevel:

{v
$ ocaml -I +libvirt
        Objective Caml version 3.10.0

# #load "unix.cma";;
# #load "mllibvirt.cma";;
# let name = "test:///default";;
val name : string = "test:///default"
# let conn = Libvirt.Connect.connect_readonly ~name () ;;
val conn : Libvirt.ro Libvirt.Connect.t = <abstr>
# Libvirt.Connect.get_node_info conn;;
  : Libvirt.Connect.node_info =
{Libvirt.Connect.model = "i686"; Libvirt.Connect.memory = 3145728L;
 Libvirt.Connect.cpus = 16; Libvirt.Connect.mhz = 1400;
 Libvirt.Connect.nodes = 2; Libvirt.Connect.sockets = 2;
 Libvirt.Connect.cores = 2; Libvirt.Connect.threads = 2}
v}

   {3 Compiling libvirt programs}

   This command compiles a program to native code:

{v
ocamlopt -I +libvirt mllibvirt.cmxa list_domains.ml -o list_domains
v}

   {3 Example: Connect to the hypervisor}

   The main modules are {!Libvirt.Connect}, {!Libvirt.Domain} and
   {!Libvirt.Network} corresponding respectively to the
   {{:http://libvirt.org/html/libvirt-libvirt.html}virConnect*, virDomain* and virNetwork* functions from libvirt}.
   For brevity I usually rename these modules like this:

{v
module C = Libvirt.Connect
module D = Libvirt.Domain
module N = Libvirt.Network
v}

   To get a connection handle, assuming a Xen hypervisor:

{v
let name = "xen:///"
let conn = C.connect_readonly ~name ()
v}

   {3 Example: List running domains}

{v
open Printf

let n = C.num_of_domains conn in
let ids = C.list_domains conn n in
let domains = Array.map (D.lookup_by_id conn) ids in
Array.iter (
  fun dom ->
    printf "%8d %s\n%!" (D.get_id dom) (D.get_name dom)
) domains;
v}

   {3 Example: List inactive domains}

{v
let n = C.num_of_defined_domains conn in
let names = C.list_defined_domains conn n in
Array.iter (
  fun name ->
    printf "inactive %s\n%!" name
) names;
v}

   {3 Example: Print node info}

{v
let node_info = C.get_node_info conn in
printf "model = %s\n" node_info.C.model;
printf "memory = %Ld K\n" node_info.C.memory;
printf "cpus = %d\n" node_info.C.cpus;
printf "mhz = %d\n" node_info.C.mhz;
printf "nodes = %d\n" node_info.C.nodes;
printf "sockets = %d\n" node_info.C.sockets;
printf "cores = %d\n" node_info.C.cores;
printf "threads = %d\n%!" node_info.C.threads;

let hostname = C.get_hostname conn in
printf "hostname = %s\n%!" hostname;

let uri = C.get_uri conn in
printf "uri = %s\n%!" uri
v}

*)


(** {2 Programming issues}

    {3 General safety issues}

    Memory allocation / automatic garbage collection of all libvirt
    objects should be completely safe (except in the specific
    virterror case noted below).  If you find any safety issues or if your
    pure OCaml program ever segfaults, please contact the author.

    You can force a libvirt object to be freed early by calling
    the [close] function on the object.  This shouldn't affect
    the safety of garbage collection and should only be used when
    you want to explicitly free memory.  Note that explicitly
    closing a connection object does nothing if there are still
    unclosed domain or network objects referencing it.

    Note that even though you hold open (eg) a domain object, that
    doesn't mean that the domain (virtual machine) actually exists.
    The domain could have been shut down or deleted by another user.
    Thus domain objects can through odd exceptions at any time.
    This is just the nature of virtualisation.

    Virterror has a specific design error which means that the
    objects embedded in a virterror exception message are only
    valid as long as the connection handle is still open.  This
    is a design flaw in the C code of libvirt and we cannot fix
    or work around it in the OCaml bindings.

    {3 Backwards and forwards compatibility}

    OCaml-libvirt is backwards and forwards compatible with
    any libvirt >= 0.2.1.  One consequence of this is that
    your program can dynamically link to a {i newer} version of
    libvirt than it was compiled with, and it should still
    work.

    When we link to an older version of libvirt.so, there may
    be missing functions.  If ocaml-libvirt was compiled with
    gcc, then these are turned into OCaml {!Libvirt.Not_supported}
    exceptions.

    We don't support libvirt < 0.2.1, and never will so don't ask us.

    {3 Threads}

    You can issue multiple concurrent libvirt requests in
    different threads.  However you must follow this rule:
    Each thread must have its own separate libvirt connection, {i or}
    you must implement your own mutex scheme to ensure that no
    two threads can ever make concurrent calls using the same
    libvirt connection.

    (Note that multithreaded code is not well tested.  If you find
    bugs please report them.)

    {3 Initialisation}

    Libvirt requires all callers to call virInitialize before
    using the library.  This is done automatically for you by
    these bindings when the program starts up, and we believe
    that the way this is done is safe.

    {2 Reference}
*)

type uuid = string
    (** This is a "raw" UUID, ie. a packed string of bytes. *)

type xml = string
    (** Type of XML (an uninterpreted string of bytes).  Use PXP, expat,
	xml-light, etc. if you want to do anything useful with the XML.
    *)

type filename = string
    (** A filename. *)

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

type rw = [`R|`W]
type ro = [`R]
    (** These
	{{:http://caml.inria.fr/pub/ml-archives/caml-list/2004/07/80683af867cce6bf8fff273973f70c95.en.html}phantom types}
	are used to ensure the type-safety of read-only
	versus read-write connections.

	All connection/domain/etc. objects are marked with
	a phantom read-write or read-only type, and trying to
	pass a read-only object into a function which could
	mutate the object will cause a compile time error.

	Each module provides a function like {!Libvirt.Connect.const}
	to demote a read-write object into a read-only object.  The
	opposite operation is, of course, not allowed.

	If you want to handle both read-write and read-only
	connections at runtime, use a variant similar to this:
{v
type conn_t =
    | No_connection
    | Read_only of Libvirt.ro Libvirt.Connect.t
    | Read_write of Libvirt.rw Libvirt.Connect.t
v}
	See also the source of [mlvirsh].
    *)

type ('a, 'b) job_t
(** Forward definition of {!Job.t} to avoid recursive module dependencies. *)

(** {3 Connections} *)

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
    (** Returns the name of the driver (hypervisor). *)

  val get_version : [>`R] t -> int
    (** Returns the driver version
	[major * 1_000_000 + minor * 1000 + release]
    *)
  val get_hostname : [>`R] t -> string
    (** Returns the hostname of the physical server. *)
  val get_uri : [>`R] t -> string
    (** Returns the canonical connection URI. *)
  val get_max_vcpus : [>`R] t -> ?type_:string -> unit -> int
    (** Returns the maximum number of virtual CPUs
	supported by a guest VM of a particular type. *)
  val list_domains : [>`R] t -> int -> int array
    (** [list_domains conn max] returns the running domain IDs,
	up to a maximum of [max] entries.
	Call {!num_of_domains} first to get a value for [max].
    *)
  val num_of_domains : [>`R] t -> int
    (** Returns the number of running domains. *)
  val get_capabilities : [>`R] t -> xml
    (** Returns the hypervisor capabilities (as XML). *)
  val num_of_defined_domains : [>`R] t -> int
    (** Returns the number of inactive (shutdown) domains. *)
  val list_defined_domains : [>`R] t -> int -> string array
    (** [list_defined_domains conn max]
	returns the names of the inactive domains, up to
	a maximum of [max] entries.
	Call {!num_of_defined_domains} first to get a value for [max].
    *)
  val num_of_networks : [>`R] t -> int
    (** Returns the number of networks. *)
  val list_networks : [>`R] t -> int -> string array
    (** [list_networks conn max]
	returns the names of the networks, up to a maximum
	of [max] entries.
	Call {!num_of_networks} first to get a value for [max].
    *)
  val num_of_defined_networks : [>`R] t -> int
    (** Returns the number of inactive networks. *)
  val list_defined_networks : [>`R] t -> int -> string array
    (** [list_defined_networks conn max]
	returns the names of the inactive networks, up to a maximum
	of [max] entries.
	Call {!num_of_defined_networks} first to get a value for [max].
    *)

  val num_of_pools : [>`R] t -> int
    (** Returns the number of storage pools. *)
  val list_pools : [>`R] t -> int -> string array
    (** Return list of storage pools. *)
  val num_of_defined_pools : [>`R] t -> int
    (** Returns the number of storage pools. *)
  val list_defined_pools : [>`R] t -> int -> string array
    (** Return list of storage pools. *)

    (* The name of this function is inconsistent, but the inconsistency
     * is really in libvirt itself.
     *)
  val get_node_info : [>`R] t -> node_info
    (** Return information about the physical server. *)

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
      connection object. *)

(** {3 Domains} *)

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
    (** Create a new guest domain (not necessarily a Linux one)
	from the given XML.
    *)
  val create_linux_job : [>`W] Connect.t -> xml -> ([`Domain], rw) job_t
    (** Asynchronous domain creation. *)
  val lookup_by_id : 'a Connect.t -> int -> 'a t
    (** Lookup a domain by ID. *)
  val lookup_by_uuid : 'a Connect.t -> uuid -> 'a t
    (** Lookup a domain by UUID.  This uses the packed byte array UUID. *)
  val lookup_by_uuid_string : 'a Connect.t -> string -> 'a t
    (** Lookup a domain by (string) UUID. *)
  val lookup_by_name : 'a Connect.t -> string -> 'a t
    (** Lookup a domain by name. *)
  val destroy : [>`W] t -> unit
    (** Abruptly destroy a domain. *)
  val free : [>`R] t -> unit
    (** [free domain] frees the domain object in memory.

	The domain object is automatically freed if it is garbage
	collected.  This function just forces it to be freed right
	away.
    *)

  val suspend : [>`W] t -> unit
    (** Suspend a domain. *)
  val resume : [>`W] t -> unit
    (** Resume a domain. *)
  val save : [>`W] t -> filename -> unit
    (** Suspend a domain, then save it to the file. *)
  val save_job : [>`W] t -> filename -> ([`Domain_nocreate], rw) job_t
    (** Asynchronous domain suspend. *)
  val restore : [>`W] Connect.t -> filename -> unit
    (** Restore a domain from a file. *)
  val restore_job : [>`W] Connect.t -> filename -> ([`Domain_nocreate], rw) job_t
    (** Asynchronous domain restore. *)
  val core_dump : [>`W] t -> filename -> unit
    (** Force a domain to core dump to the named file. *)
  val core_dump_job : [>`W] t -> filename -> ([`Domain_nocreate], rw) job_t
    (** Asynchronous core dump. *)
  val shutdown : [>`W] t -> unit
    (** Shutdown a domain. *)
  val reboot : [>`W] t -> unit
    (** Reboot a domain. *)
  val get_name : [>`R] t -> string
    (** Get the domain name. *)
  val get_uuid : [>`R] t -> uuid
    (** Get the domain UUID (as a packed byte array). *)
  val get_uuid_string : [>`R] t -> string
    (** Get the domain UUID (as a printable string). *)
  val get_id : [>`R] t -> int
    (** [getid dom] returns the ID of the domain.

	Do not call this on a defined but not running domain.  Those
	domains don't have IDs, and you'll get an error here.
    *)

  val get_os_type : [>`R] t -> string
    (** Get the operating system type. *)
  val get_max_memory : [>`R] t -> int64
    (** Get the maximum memory allocation. *)
  val set_max_memory : [>`W] t -> int64 -> unit
    (** Set the maximum memory allocation. *)
  val set_memory : [>`W] t -> int64 -> unit
    (** Set the normal memory allocation. *)
  val get_info : [>`R] t -> info
    (** Get information about a domain. *)
  val get_xml_desc : [>`R] t -> xml
    (** Get the XML description of a domain. *)
  val get_scheduler_type : [>`R] t -> string * int
    (** Get the scheduler type. *)
  val get_scheduler_parameters : [>`R] t -> int -> sched_param array
    (** Get the array of scheduler parameters. *)
  val set_scheduler_parameters : [>`W] t -> sched_param array -> unit
    (** Set the array of scheduler parameters. *)
  val define_xml : [>`W] Connect.t -> xml -> rw t
    (** Define a new domain (but don't start it up) from the XML. *)
  val undefine : [>`W] t -> unit
    (** Undefine a domain - removes its configuration. *)
  val create : [>`W] t -> unit
    (** Launch a defined (inactive) domain. *)
  val create_job : [>`W] t -> ([`Domain_nocreate], rw) job_t
    (** Asynchronous launch domain. *)
  val get_autostart : [>`R] t -> bool
    (** Get the autostart flag for a domain. *)
  val set_autostart : [>`W] t -> bool -> unit
    (** Set the autostart flag for a domain. *)
  val set_vcpus : [>`W] t -> int -> unit
    (** Change the number of vCPUs available to a domain. *)
  val pin_vcpu : [>`W] t -> int -> string -> unit
    (** [pin_vcpu dom vcpu bitmap] pins a domain vCPU to a bitmap of physical
	CPUs.  See the libvirt documentation for details of the
	layout of the bitmap. *)
  val get_vcpus : [>`R] t -> int -> int -> int * vcpu_info array * string
    (** [get_vcpus dom maxinfo maplen] returns the pinning information
	for a domain.  See the libvirt documentation for details
	of the array and bitmap returned from this function.
    *)
  val get_max_vcpus : [>`R] t -> int
    (** Returns the maximum number of vCPUs supported for this domain. *)
  val attach_device : [>`W] t -> xml -> unit
    (** Attach a device (described by the device XML) to a domain. *)
  val detach_device : [>`W] t -> xml -> unit
    (** Detach a device (described by the device XML) from a domain. *)

  val migrate : [>`W] t -> [>`W] Connect.t -> migrate_flag list ->
    ?dname:string -> ?uri:string -> ?bandwidth:int -> unit -> rw t
    (** [migrate dom dconn flags ()] migrates a domain to a
	destination host described by [dconn].

	The optional flag [?dname] is used to rename the domain.

	The optional flag [?uri] is used to route the migration.

	The optional flag [?bandwidth] is used to limit the bandwidth
	used for migration (in Mbps). *)

  val block_stats : [>`R] t -> string -> block_stats
    (** Returns block device stats. *)
  val interface_stats : [>`R] t -> string -> interface_stats
    (** Returns network interface stats. *)

  external const : [>`R] t -> ro t = "%identity"
    (** [const dom] turns a read/write domain handle into a read-only
	domain handle.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with domains.  [Domain.t] is the
      domain object. *)

(** {3 Networks} *)

module Network : 
sig
  type 'rw t
    (** Network handle.  Read-only handles have type [ro Network.t] and
	read-write handles have type [rw Network.t].
    *)

  val lookup_by_name : 'a Connect.t -> string -> 'a t
    (** Lookup a network by name. *)
  val lookup_by_uuid : 'a Connect.t -> uuid -> 'a t
    (** Lookup a network by (packed) UUID. *)
  val lookup_by_uuid_string : 'a Connect.t -> string -> 'a t
    (** Lookup a network by UUID string. *)
  val create_xml : [>`W] Connect.t -> xml -> rw t
    (** Create a network. *)
  val create_xml_job : [>`W] Connect.t -> xml -> ([`Network], rw) job_t
    (** Asynchronous create network. *)
  val define_xml : [>`W] Connect.t -> xml -> rw t
    (** Define but don't activate a network. *)
  val undefine : [>`W] t -> unit
    (** Undefine configuration of a network. *)
  val create : [>`W] t -> unit
    (** Start up a defined (inactive) network. *)
  val create_job : [>`W] t -> ([`Network_nocreate], rw) job_t
    (** Asynchronous start network. *)
  val destroy : [>`W] t -> unit
    (** Destroy a network. *)
  val free : [>`R] t -> unit
    (** [free network] frees the network object in memory.

	The network object is automatically freed if it is garbage
	collected.  This function just forces it to be freed right
	away.
    *)

  val get_name : [>`R] t -> string
    (** Get network name. *)
  val get_uuid : [>`R] t -> uuid
    (** Get network packed UUID. *)
  val get_uuid_string : [>`R] t -> string
    (** Get network UUID as a printable string. *)
  val get_xml_desc : [>`R] t -> xml
    (** Get XML description of a network. *)
  val get_bridge_name : [>`R] t -> string
    (** Get bridge device name of a network. *)
  val get_autostart : [>`R] t -> bool
    (** Get the autostart flag for a network. *)
  val set_autostart : [>`W] t -> bool -> unit
    (** Set the autostart flag for a network. *)

  external const : [>`R] t -> ro t = "%identity"
    (** [const network] turns a read/write network handle into a read-only
	network handle.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with networks.  [Network.t] is the
      network object. *)

(** {3 Storage pools} *)

module Pool :
sig
  type 'rw t
    (** Storage pool handle. *)

  type pool_state = Inactive | Building | Running | Degraded
    (** State of the storage pool. *)

  type pool_build_flags = New | Repair | Resize
    (** Flags for creating a storage pool. *)

  type pool_delete_flags = Normal | Zeroed
    (** Flags for deleting a storage pool. *)

  type pool_info = {
    state : pool_state;			(** Pool state. *)
    capacity : int64;			(** Logical size in bytes. *)
    allocation : int64;			(** Currently allocated in bytes. *)
    available : int64;			(** Remaining free space bytes. *)
  }

  val lookup_by_name : 'a Connect.t -> string -> 'a t
  val lookup_by_uuid : 'a Connect.t -> uuid -> 'a t
  val lookup_by_uuid_string : 'a Connect.t -> string -> 'a t
    (** Look up a storage pool by name, UUID or UUID string. *)

  val create_xml : [>`W] Connect.t -> xml -> rw t
    (** Create a storage pool. *)
  val define_xml : [>`W] Connect.t -> xml -> rw t
    (** Define but don't activate a storage pool. *)
  val build : [>`W] t -> pool_build_flags -> unit
    (** Build a storage pool. *)
  val undefine : [>`W] t -> unit
    (** Undefine configuration of a storage pool. *)
  val create : [>`W] t -> unit
    (** Start up a defined (inactive) storage pool. *)
  val destroy : [>`W] t -> unit
    (** Destroy a storage pool. *)
  val delete : [>`W] t -> unit
    (** Delete a storage pool. *)
  val free : [>`R] t -> unit
    (** Free a storage pool object in memory.

	The storage pool object is automatically freed if it is garbage
	collected.  This function just forces it to be freed right
	away.
    *)
  val refresh : [`R] t -> unit
    (** Refresh the list of volumes in the storage pool. *)

  val get_name : [`R] t -> string
    (** Name of the pool. *)
  val get_uuid : [`R] t -> uuid
    (** Get the UUID (as a packed byte array). *)
  val get_uuid_string : [`R] t -> string
    (** Get the UUID (as a printable string). *)
  val get_info : [`R] t -> pool_info
    (** Get information about the pool. *)
  val get_xml_desc : [`R] t -> xml
    (** Get the XML description. *)
  val get_autostart : [`R] t -> bool
    (** Get the autostart flag for the storage pool. *)
  val set_autostart : [`W] t -> bool -> unit
    (** Set the autostart flag for the storage pool. *)

  val num_of_volumes : [`R] t -> int
    (** Returns the number of storage volumes within the storage pool. *)
  val list_volumes : [`R] t -> int -> string array
    (** Return list of storage volumes. *)

  external const : [>`R] t -> ro t = "%identity"
    (** [const conn] turns a read/write storage pool into a read-only
	pool.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with storage pools. *)

(** {3 Storage volumes} *)

module Volume :
sig
  type 'rw t
    (** Storage volume handle. *)

  type vol_type = File | Block
    (** Type of a storage volume. *)

  type vol_delete_flags = Normal | Zeroed
    (** Flags for deleting a storage volume. *)

  type vol_info = {
    typ : vol_type;			(** Type of storage volume. *)
    capacity : int64;			(** Logical size in bytes. *)
    allocation : int64;			(** Currently allocated in bytes. *)
  }

  val lookup_by_name : 'a Pool.t -> string -> 'a t
  val lookup_by_key : 'a Connect.t -> string -> 'a t
  val lookup_by_path : 'a Connect.t -> string -> 'a t
    (** Look up a storage volume by name, key or path volume. *)

  val pool_of_volume : 'a t -> 'a Pool.t
    (** Get the storage pool containing this volume. *)

  val get_name : [`R] t -> string
    (** Name of the volume. *)
  val get_key : [`R] t -> string
    (** Key of the volume. *)
  val get_path : [`R] t -> string
    (** Path of the volume. *)
  val get_info : [`R] t -> vol_info
    (** Get information about the storage volume. *)
  val get_xml_desc : [`R] t -> xml
    (** Get the XML description. *)

  val create_xml : [`W] Pool.t -> xml -> unit
    (** Create a storage volume. *)
  val delete : [`W] t -> unit
    (** Delete a storage volume. *)
  val free : [>`R] t -> unit
    (** Free a storage volume object in memory.

	The storage volume object is automatically freed if it is garbage
	collected.  This function just forces it to be freed right
	away.
    *)

  external const : [>`R] t -> ro t = "%identity"
    (** [const conn] turns a read/write storage volume into a read-only
	volume.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with storage volumes. *)

(** {3 Jobs and asynchronous processing} *)

module Job :
sig
  type ('jobclass, 'rw) t = ('jobclass, 'rw) job_t
    (** A background asynchronous job.

        Jobs represent a pending operation such as domain creation.
	The possible types for a job are:

{v
(`Domain, `W) Job.t            Job creating a new domain
(`Domain_nocreate, `W) Job.t   Job acting on an existing domain
(`Network, `W) Job.t           Job creating a new network
(`Network_nocreate, `W) Job.t  Job acting on an existing network
v}
      *)

  type job_type = Bounded | Unbounded
    (** A Bounded job is one where we can estimate time to completion. *)

  type job_state = Running | Complete | Failed | Cancelled
    (** State of the job. *)

  type job_info = {
    typ : job_type;			(** Job type (Bounded, Unbounded) *)
    state : job_state;			(** Job state (Running, etc.) *)
    running_time : int;			(** Actual running time (seconds) *)
    (** The following fields are only available in Bounded jobs: *)
    remaining_time : int;		(** Estimated time left (seconds) *)
    percent_complete : int		(** Estimated percent complete *)
  }

  val get_info : ('a,'b) t -> job_info
    (** Get information and status about the job. *)

  val get_domain : ([`Domain], 'a) t -> 'a Domain.t
    (** Get the completed domain from a job.

        You should only call it on a job in state Complete. *)

  val get_network : ([`Network], 'a) t -> 'a Network.t
    (** Get the completed network from a job.

        You should only call it on a job in state Complete. *)

  val cancel : ('a,'b) t -> unit
    (** Cancel a job. *)

  val free : ('a, [>`R]) t -> unit
    (** Free a job object in memory.

	The job object is automatically freed if it is garbage
	collected.  This function just forces it to be freed right
	away.
    *)

  external const : ('a, [>`R]) t -> ('a, ro) t = "%identity"
    (** [const conn] turns a read/write job into a read-only
	job.  Note that the opposite operation is impossible.
      *)
end
  (** Module dealing with asynchronous jobs. *)

(** {3 Error handling and exceptions} *)

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
    | VIR_ERR_INVALID_MAC
    | VIR_ERR_AUTH_FAILED
    | VIR_ERR_INVALID_STORAGE_POOL
    | VIR_ERR_INVALID_STORAGE_VOL
    | VIR_WAR_NO_STORAGE
    | VIR_ERR_NO_STORAGE_POOL
    | VIR_ERR_NO_STORAGE_VOL
	(* ^^ NB: If you add a variant you MUST edit
	   libvirt_c_epilogue.c:MAX_VIR_* *)
    | VIR_ERR_UNKNOWN of int
	(** See [<libvirt/virterror.h>] for meaning of these codes. *)

  val string_of_code : code -> string

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
    | VIR_FROM_OPENVZ
    | VIR_FROM_XENXM
    | VIR_FROM_STATS_LINUX
    | VIR_FROM_STORAGE
	(* ^^ NB: If you add a variant you MUST edit
	   libvirt_c_epilogue.c: MAX_VIR_* *)
    | VIR_FROM_UNKNOWN of int
	(** Subsystem / driver which produced the error. *)

  val string_of_domain : domain -> string

  type level =
    | VIR_ERR_NONE
    | VIR_ERR_WARNING
    | VIR_ERR_ERROR
	(* ^^ NB: If you add a variant you MUST edit libvirt_c.c: MAX_VIR_* *)
    | VIR_ERR_UNKNOWN_LEVEL of int
	(** No error, a warning or an error. *)

  val string_of_level : level -> string

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
*)

exception Not_supported of string
(**
    Functions may raise
    [Not_supported "virFoo"]
    (where [virFoo] is the libvirt function name) if a function is
    not supported at either compile or run time.  This applies to
    any libvirt function added after version 0.2.1.

    See also {{:http://libvirt.org/hvsupport.html}http://libvirt.org/hvsupport.html}
*)

