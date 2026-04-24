(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
 * Copyright (c) 2018-2019 Hannes Mehnert <hannes@mehnert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Network devices

    [Mirage_net] defines the signature for MirageOS network devices.

    {e Release %%VERSION%% } *)

module Net : sig
  type error = [ `Invalid_length | `Disconnected ]
  (** The type for IO operation errors *)

  val pp_error: error Fmt.t
  (** [pp_error] pretty-print network errors. *)
end

type stats = {
  mutable rx_bytes: int64;
  mutable rx_pkts: int32;
  mutable tx_bytes: int64;
  mutable tx_pkts: int32;
}
(** The type for frame statistics to track the usage of the device. *)

(** {2 Networking} *)

(** A network interface that serves Ethernet frames. *)
module type S = sig

  type error = private [> Net.error]
  (** The type for network interface errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type t
  (** The type representing the internal state of the network device. *)

  val disconnect: t -> unit Lwt.t
  (** Disconnect from the network device. While this might take some time to
      complete, it can never result in an error. *)

  val write: t -> size:int -> (Cstruct.t -> int) -> (unit, error) result Lwt.t
  (** [write net ~size fill] allocates a buffer of length [size], where [size]
     must not exceed the interface maximum packet size ({!mtu} plus Ethernet
     header). The allocated buffer is zeroed and passed to the [fill] function
     which returns the payload length, which may not exceed the length of the
     buffer. When [fill] returns, a sub buffer is put on the wire: the allocated
     buffer from index 0 to the returned length. *)

  val listen: t -> header_size:int -> (Cstruct.t -> unit Lwt.t) -> (unit, error) result Lwt.t
  (** [listen ~header_size net fn] waits for a [packet] with size at most
     [header_size + mtu] on the network device. When a [packet] is received, an
     asynchronous task is created in which [fn packet] is called. The ownership
     of [packet] is transferred to [fn].  The function can be stopped by calling
     {!disconnect}. *)

  val mac: t -> Macaddr.t
  (** [mac net] is the MAC address of [net]. *)

  val mtu: t -> int
  (** [mtu net] is the Maximum Transmission Unit of [net]. This excludes the
     Ethernet header. *)

  val get_stats_counters: t -> stats
  (** Obtain the most recent snapshot of the interface statistics. *)

  val reset_stats_counters: t -> unit
  (** Reset the statistics associated with this interface to their
      defaults. *)

end

module Stats : sig
  val create: unit -> stats
  (** [create ()] returns a fresh set of zeroed counters *)

  val rx: stats -> int64 -> unit
  (** [rx t size] records that we received a packet of length [size] *)

  val tx: stats -> int64 -> unit
  (** [tx t size] records that we transmitted a packet of length [size] *)

  val reset: stats -> unit
  (** [reset t] resets all packet counters in [t] to 0 *)
end

(** the type of packets, currently a {!type:Cstruct.t}, 
    could be {!type:bytes} in the future *)
type packet = Cstruct.t

(** Packet memory usage tracker *)
module PacketQueue : sig
    type 'a t
    (** memory usage tracker *)

    type input = [`Input]
    (** tracker for packet inputs *)

    type output = [`Output]
    (** tracker for packet outputs *)

    type promise = [`Promise]
    (** tracker for memory used by promises *)

    type device = [ input | output | promise ]
    (** the type for network device memory trackers *)

    val get_used_bytes: 'a t -> int
    (** [get_used_bytes t] is the amount of bytes used by this queue
        and all its children. *)

    val get_limit_bytes: 'a t -> int
    (** [get_limit_bytes t] gets the effective limit of [t] in bytes.

        The effective limit is the smallest limit of this queue and its parents.
    *)

    val set_limit_bytes: 'a t -> int -> unit
    (** [set_limit_bytes t bytes] sets the limit of [t] to [bytes] *)

    val get_free_bytes: 'a t -> int
    (** [get_free_bytes t] is the effective amount of bytes available in this queue.

        The effective amount of free bytes is the minimum between the free bytes in this queue
        and all its parents.

        Can be negative if already exceeded.
    *)

    val make_input: ?parent:[<device >`Input ] t -> size_in_bytes:int -> unit -> [> input] t
    (** [make_input ?parent ~size_in_bytes] tracker for packets received.
        These are all the same size,
        because packet views still hold the large original packet allocated
     *)
    
    val on_input: input t -> packet -> unit
    (** [on_packet t packet] tracks the memory usage of [packet] on [t] until finalised.
        Ignores the size of the packet and uses that from [t].
    *)

    val on_output: output t -> packet -> unit
    (** [on_packet t packet] tracks the memory usage of [packet] on [t] until finalised.
        Uses the actual packet size from [packet].
    *)

    val make_output: ?parent:[< device > `Output] t -> unit -> [> output] t
    (** [make_output ?parent ()] tracker for packets to be sent,
        these could be of different size, except when sending back
        a packet that we received unchanged.
    *)

    val make_promise: ?parent:[< device > `Promise] t -> unit -> [> promise] t
    (** [make_promise ?parent ()] tracker for promises associated with
        processing packets.
    *)

    val on_promise: promise t -> size_in_bytes:int -> 'a Lwt.t -> 'a Lwt.t
    (** [on_promise t ~size_in_bytes promise] increments the memory usage by 
        [size_in_bytes] during the execution of [promise].
        When [promise] terminates or is abandoned the memory usage is decremented.
    *)

    val make_device: ?parent:[< device] t -> unit -> [> device] t
    (** [make_device ?parent ()] is a network device. *)

    val global: _ t
    (** global memory usage for the entire network stack *)
end
