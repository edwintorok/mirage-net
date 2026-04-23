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

module Mem = struct
  type t =
  { mutable bytes: int
  ; mutable limit_bytes: int
  }

  let free_bytes t = t.limit_bytes - t.bytes

  let region = { bytes = 0; limit_bytes = max_int }

  let track_sleeping ~size_in_bytes =
    let tracked = ref true in
    let untrack _ =
      if !tracked then (
        region.bytes <- region.bytes - size_in_bytes;
        tracked := false
      )
    in
    (* see {!val:Gc.finalise}, the closure must not capture the value [promise]. *)
    fun promise ->
      Lwt.on_termination promise untrack;
      Gc.finalise untrack promise

  let track_lwt ~size_in_bytes promise =
    region.bytes <- region.bytes + size_in_bytes;
    if Lwt.is_sleeping promise then
      track_sleeping ~size_in_bytes promise
    else
      region.bytes <- region.bytes - size_in_bytes;
    promise

  let heap = { bytes = 0; limit_bytes = max_int }
  
  let word_size_in_bytes = Sys.word_size / 8

  let round_up ~multiple n =
    ((n + multiple - 1) / multiple) * multiple

  let round_up_ctrl ctrl words =
    match ctrl.Gc.major_heap_increment with
    | 0 ->
        (* OCaml 5.x *)
        round_up ~multiple:4096 words
    | n when n <= 1000 ->
        (* could also be calculated with logarithms, but this is simpler *)
        let rec loop heap_size words =
          if words <= 0 then heap_size
          else
            let grow = heap_size * n / 100 in
            (loop[@tailcall]) (heap_size + grow) (words - grow)
        in
        let heap_size = Gc.(quick_stat ()).heap_words in
        loop heap_size words - heap_size
    | multiple ->
        round_up ~multiple words

  let set_free_bytes bytes =
    let stat = Gc.quick_stat ()
    and ctrl = Gc.get () in
    (* See https://sqlite.org/malloc.html#_mathematical_guarantees_against_memory_allocation_failures.
       Although the OCaml GC can move values, it only does so after a minor heap collection,
       so we still need to take fragmentation into account.

       In the minor heap allocations are between 2 and 256 words, thus a 4.5 multiplier should be safe
       based on the linked formula.
     *)

    (* on OCaml 5 we don't have this statistic, so we fall back to always calculating fragmentation *)
    let fragmentable = max 0 (ctrl.Gc.minor_heap_size - stat.Gc.largest_free) in
    let rest = ctrl.Gc.minor_heap_size - fragmentable in
    let required_free_words = rest + fragmentable * 9/2 in
    let needed_free_words = max 0 (required_free_words - stat.Gc.free_words)
      |> round_up_ctrl ctrl in

    let delta = max 1514 (bytes * 5/38 - needed_free_words * word_size_in_bytes) in
    heap.limit_bytes <- heap.bytes + delta;
    region.limit_bytes <- region.bytes + delta

  let untrack_packet packet =
    heap.bytes <- heap.bytes - Cstruct.length packet

  let track packet =
    let delta_bytes = Cstruct.length packet in
    heap.bytes <- heap.bytes + delta_bytes;
    Gc.finalise untrack_packet packet
end
