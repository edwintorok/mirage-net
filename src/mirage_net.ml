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
  let word_size_bytes = Sys.word_size / 8

  let overhead = 11 * word_size_bytes

  let size_of packet =
    Cstruct.length packet + overhead

  module Heap = struct
    let bytes = ref 0

    let get_bytes () = !bytes

    let update ~delta_bytes =
      bytes := !bytes + delta_bytes

    let untrack packet =
      update ~delta_bytes:(-size_of packet)

    let track packet =
      let delta_bytes = size_of packet in
      update ~delta_bytes;
      (* track the underlying bigarray, and not the [Cstruct].
        When [Cstruct] views are created the original [Cstruct] can get finalised,
        but it'll point to the same underlying bigarray *)
      Gc.finalise untrack packet

    (* cannot be max_int: it overflows *)
    let max_heap_words = ref (max_int / word_size_bytes)

    let set_free_bytes bytes =
      let stat = Gc.quick_stat ()
      and ctrl = Gc.get () in
      (* See https://sqlite.org/malloc.html#_mathematical_guarantees_against_memory_allocation_failures.
         In the minor heap each allocation is between 2 and 256 words,
         the maximum ratio is 128, so we need ~4.5x memory available in the major heap to account for fragmentation.
         (The GC could move values, but it'll only do that after it has moved values from the minor heap to the major heap,
          so we need to take fragmentation into account to avoid fatal errors from the minor GC)

         TODO: this assumes an ideal allocator, the size class based one might have higher maximum fragmentation.
       *)
      max_heap_words := stat.heap_words + bytes / word_size_bytes - ctrl.minor_heap_size * 9 / 2

    let get_free_bytes stat =
      (!max_heap_words - stat.Gc.heap_words + stat.Gc.free_words - get_bytes ()) * word_size_bytes
  end

  module Region = struct
    let bytes = ref 0
    let get_bytes () = !bytes
    let update ~delta_bytes =
      bytes := !bytes + delta_bytes

    let limit_bytes = ref max_int
    
    let get_limit_bytes () = !limit_bytes
    
    let set_limit_bytes bytes =
      limit_bytes := bytes

    let free_bytes () = get_limit_bytes () - get_bytes ()

    let track_promise ~delta_bytes =
      let tracked = ref true in
      let untrack () =
        if !tracked then begin
          update ~delta_bytes;
          tracked := false
        end
      in
      (* see {!val:Gc.finalise}, the closure must not capture the promise,
        or we'd keep the value alive forever *)
      fun promise ->
        Lwt.on_termination promise untrack;
        (* if the promise is abandoned we still need to update our memory usage *)
        Gc.finalise_last untrack promise

    let track ~delta_bytes handler packet =
      update ~delta_bytes;
      let delta_bytes = -delta_bytes in
      let t = handler packet in
      if Lwt.is_sleeping t then
        (* this allocates, only call it when it is actually not terminated yet *)
        track_promise ~delta_bytes t
      else
        update ~delta_bytes;
      t
  end

  let track handler packet =
    let delta_bytes = size_of packet in
    Heap.track packet;
    Region.track ~delta_bytes handler packet
end
