open Mirage_net.Mem

let test_region_update delta_bytes () =
  let old = Region.get_bytes () in
  Region.update ~delta_bytes;
  let actual = Region.get_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes"
    ~actual ~expected:(old + delta_bytes))

let test_limit_update_free ~limit delta_bytes () =
  Region.set_limit_bytes limit;
  let actual = Region.get_limit_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.get_limit_bytes"
    ~actual ~expected:limit);

  (* test that updating beyond the limit doesn't fail *)
  let old = Region.get_bytes () in
  let n = (limit - old) / delta_bytes in
  for _ = 1 to n do
    Region.update ~delta_bytes
  done;
  Region.update ~delta_bytes;
  let actual = Region.get_bytes ()
  and expected = old + (n+1) * delta_bytes
  in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes"
    ~actual ~expected);

  let expected = limit - expected in
  let actual = Region.free_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.free_bytes"
      ~actual ~expected);

  let delta_bytes = -delta_bytes in
  Region.update ~delta_bytes;
  Region.update ~delta_bytes;
  let actual = Region.free_bytes () in
  let expected = expected - 2 * delta_bytes in
  Alcotest.V1.(check' int ~msg:"Region.free_bytes"
      ~actual ~expected)

let test_heap_update delta_bytes () =
  let old = Heap.get_bytes () in
  Heap.update ~delta_bytes;
  let actual = Heap.get_bytes () in
  Alcotest.V1.(check' int ~msg:"heap.get_bytes"
    ~actual ~expected:(old + delta_bytes))

let test_free_bytes () =
  let free = Heap.get_free_bytes (Gc.quick_stat ()) in
  if free <= 0 then
    Alcotest.failf "There should be some free memory on startup, got: %d" free

let global = ref [||]

let packet = 1514

let alloc n =
  Array.init n (fun _ ->
    let p = Cstruct.create packet in
    Heap.track p;
    p)

let overhead = Obj.reachable_words (Obj.repr Cstruct.empty) * Sys.word_size / 8

let test_free_bytes_alloc () =
  Gc.full_major ();
  Gc.compact ();
  let stat0 = Gc.quick_stat () in
  let free0 = Heap.get_free_bytes stat0 in
  let used0 = Heap.get_bytes () in
  let packets = 100 in
  global := alloc packets;
  Gc.full_major ();
  Gc.compact ();
  let stat1 = Gc.quick_stat () in
  let free1 = Heap.get_free_bytes stat1 in
  let used1 = Heap.get_bytes () in

  let expected = packets * (packet + overhead ) in

  Alcotest.V1.(check' int ~msg:"Heap track used" ~actual:(used1 - used0) ~expected);

  let actual = free0 - free1
  and expected = (stat1.heap_words - stat0.heap_words + stat0.free_words - stat1.free_words
    + expected
  ) * Sys.word_size / 8 in
  Alcotest.V1.(check' int ~msg:"free memory change" ~actual ~expected);
  Alcotest.V1.(check' int ~msg:"free delta" ~actual ~expected)

let test_heap_track () =
  global := [||];
  Gc.full_major ();
  Gc.compact ();
  let packets = 10 in

  let used0 = Heap.get_bytes () in
  global := alloc packets;
  let used1 = Heap.get_bytes () in

  let actual = used1 - used0
  and expected = packets * (packet + overhead) in
  Alcotest.V1.(check' int ~msg:"Heap.get_bytes" ~actual ~expected);

  global := [||];
  Gc.full_major ();
  let used2 = Heap.get_bytes () in
  Alcotest.V1.(check' int ~msg:"Heap.track+get_bytes" ~actual:used2 ~expected:used0)

let test_track _ () =
  let open Lwt.Syntax in
  Gc.full_major ();
  Gc.compact ();
  let used00 = Region.get_bytes () in
  let ( _ : Cstruct.t Lwt.t) = Cstruct.create packet |> track Lwt.return in
  let used01 = Region.get_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes01" ~actual:used01 ~expected:used00);

  let sleep _packet = Lwt.pause () in
  let used10 = Region.get_bytes () in
  let res = Cstruct.create packet |> track sleep in
  let used11 = Region.get_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes11" ~actual:used11 ~expected:(used10 + packet + overhead));
  let* () = res in

  (* on_termination handlers do not run immediately, wait for next cycle *)
  let+ () = Lwt.pause () in

  let used12 = Region.get_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes12" ~actual:used12 ~expected:used10);

  Gc.full_major ();
  Gc.compact ();
  let used13 = Region.get_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes13" ~actual:used13 ~expected:used10)

let ignore_lwt (_ : _ Lwt.t) = ()

let test_track_abandon _ () =
  Gc.full_major ();
  Gc.compact ();
  let used00 = Region.get_bytes () in
  let sleep _packet = fst (Lwt.wait ()) in
  let () = Cstruct.create packet |> track sleep |> ignore_lwt in
  Gc.full_major ();
  let used01 = Region.get_bytes () in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes01" ~actual:used01 ~expected:used00);
  Lwt.return_unit

let () =
  Alcotest_lwt.V1.(run "mem"
  [ "region",
    [ test_case_sync "update+" `Quick @@ test_region_update 8
    ; test_case_sync "update-" `Quick @@ test_region_update ~-8
    ; test_case_sync "limit_update_free" `Quick @@ test_limit_update_free ~limit:30 8
    ]
  ; "heap",
    [test_case_sync "update+" `Quick @@ test_heap_update 8
    ;test_case_sync "update-" `Quick @@ test_heap_update 8
    ;test_case_sync "free_bytes_startup" `Quick @@ test_free_bytes
    ;test_case_sync "free_bytes_alloc" `Quick @@ test_free_bytes_alloc
    ;test_case "track" `Quick test_track
    ;test_case "track_abandon" `Quick test_track_abandon
    ]
  ]) |> Lwt_main.run
