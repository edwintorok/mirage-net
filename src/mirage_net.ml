(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
 * Copyright (c) 2018      Hannes Mehnert <hannes@mehnert.org>
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

module Net = struct
  type error = [ `Invalid_length | `Disconnected ]
  let pp_error ppf = function
    | `Invalid_length -> Fmt.string ppf "invalid length (exceeds size)"
    | `Disconnected -> Fmt.string ppf "network device is disconnected"
end

type stats = {
  mutable rx_bytes: int64;
  mutable rx_pkts: int32;
  mutable tx_bytes: int64;
  mutable tx_pkts: int32;
}

module Stats = struct
  let create () = { rx_pkts=0l; rx_bytes=0L; tx_pkts=0l; tx_bytes=0L }

  let rx t size =
    t.rx_pkts <- Int32.succ t.rx_pkts;
    t.rx_bytes <- Int64.add t.rx_bytes size

  let tx t size =
    t.tx_pkts <- Int32.succ t.tx_pkts;
    t.tx_bytes <- Int64.add t.tx_bytes size

  let reset t =
    t.rx_bytes <- 0L;
    t.rx_pkts  <- 0l;
    t.tx_bytes <- 0L;
    t.tx_pkts  <- 0l
end

module type S = sig
  type error = private [> Net.error ]
  val pp_error: error Fmt.t
  type t
  val disconnect : t -> unit Lwt.t
  val write: t -> size:int -> (Cstruct.t -> int) -> (unit, error) result Lwt.t
  val listen: t -> header_size:int -> (Cstruct.t -> unit Lwt.t) -> (unit, error) result Lwt.t
  val mac: t -> Macaddr.t
  val mtu: t -> int
  val get_stats_counters: t -> stats
  val reset_stats_counters: t -> unit
end

type packet = Cstruct.t

module PacketQueue = struct
  type 'a t =
  { mutable used_bytes: int
  ; mutable limit_bytes: int
  ; size_in_bytes: int
  ; parent: 'a t option
  ; untrack_packet: unit -> unit
  }

  type input = [`Input]

  type output = [`Output]

  type promise = [`Promise]

  type device = [input|output|promise]

  let get_used_bytes t = t.used_bytes

  let rec min_parents f t acc =
    match t with
    | None -> acc
    | Some t ->
        (min_parents[@tailcall]) f t.parent Int.(min acc @@ f t)

  let min_parents f t =
    min_parents f t.parent (f t)
  
  let limit_bytes t = t.limit_bytes

  let get_limit_bytes t = min_parents limit_bytes t

  let set_limit_bytes t limit_bytes =
    t.limit_bytes <- limit_bytes

  let free_bytes t = t.limit_bytes - t.used_bytes

  let get_free_bytes t = min_parents free_bytes t

  let rec update t size_in_bytes =
    t.used_bytes <- t.used_bytes + size_in_bytes;
    match t.parent with
    | None -> ()
    | Some t -> (update[@tailcall]) t size_in_bytes

  let make ?parent ~size_in_bytes () =
    let rec t =
    { parent
    ; size_in_bytes
    ; used_bytes = 0
    ; limit_bytes = max_int
    ; untrack_packet
    }
    and untrack_packet () = update t ~-size_in_bytes
    in t

  let global = make ~size_in_bytes:0 ()

  let make_input ?(parent : [<device > `Input] t option) ~size_in_bytes () : [> input] t =
    (make ?parent ~size_in_bytes () :> [> input] t)

  let make_device ?parent () =
    (make ?parent ~size_in_bytes:0 () :> [> device] t)

  let make_output ?parent () =
    (make ?parent ~size_in_bytes:0 () :> [> output] t)

  let make_promise ?parent () =
    (make ?parent ~size_in_bytes:0 () :> [> promise] t)

  let on_input t packet =
    update t t.size_in_bytes;
    Gc.finalise_last t.untrack_packet packet

  let untrack_packet t packet =
    update t (-Cstruct.length packet)

  let on_output t packet =
    let size_in_bytes = Cstruct.length packet in
    update t size_in_bytes;
    Gc.finalise (untrack_packet t) packet

  let on_promise' t ~size_in_bytes =
      let untrack () = update t ~-size_in_bytes in
      (* [untrack] must not have [promise] in scope, see {!val:Gc.finalise} *)
      fun promise ->
        update t size_in_bytes;
        Lwt.on_termination promise untrack;
        Gc.finalise_last untrack promise;
        promise

  let on_promise t ~size_in_bytes promise =
    if Lwt.is_sleeping promise then
      (on_promise'[@tailcall]) t ~size_in_bytes promise
    else promise
end
