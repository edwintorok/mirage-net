open Mirage_net.Mem
open Alcotest_lwt

let test_default t () =
  let actual = t.limit_bytes in
  if actual <= 0 then
    Alcotest.V1.failf "Startup value of limit should be positive: %d" actual;

  Alcotest.V1.(check' int ~msg:"bytes default" ~actual:t.bytes ~expected:0)

let test_limit_update_free ~limit delta_bytes () =
  region.limit_bytes <- limit;
  region.bytes <- limit + delta_bytes;
  let actual = free_bytes region in
  Alcotest.V1.(check' int ~msg:"Region.free_bytes" ~actual ~expected:~-delta_bytes);

  region.bytes <- region.bytes - delta_bytes;
  region.bytes <- region.bytes - delta_bytes;
  let actual = free_bytes region in
  Alcotest.V1.(check' int ~msg:"Region.free_bytes" ~actual ~expected:delta_bytes)

let packet = 1514

let overhead = 0

let test_region_track _ () =
  let open Lwt.Syntax in
  Gc.full_major ();
  Gc.compact ();
  let used00 = region.bytes in
  let ( _ : Cstruct.t Lwt.t) = Cstruct.create packet |> track Lwt.return in
  let used01 = region.bytes in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes01" ~actual:used01 ~expected:used00);

  let sleep _packet = Lwt.pause () in
  let used10 = region.bytes in
  let res = Cstruct.create packet |> track sleep in
  let used11 = region.bytes in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes11" ~actual:used11 ~expected:(used10 + packet + overhead));
  let* () = res in

  (* on_termination handlers do not run immediately, wait for next cycle *)
  let+ () = Lwt.pause () in

  let used12 = region.bytes in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes12" ~actual:used12 ~expected:used10);

  Gc.full_major ();
  Gc.compact ();
  let used13 = region.bytes in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes13" ~actual:used13 ~expected:used10)

let ignore_lwt (_ : _ Lwt.t) = ()

let test_region_track_abandon _ () =
  Gc.full_major ();
  Gc.compact ();
  let used00 = region.bytes in
  let sleep _packet = fst (Lwt.wait ()) in
  let () = Cstruct.create packet |> track sleep |> ignore_lwt in
  Gc.full_major ();
  let used01 = region.bytes in
  Alcotest.V1.(check' int ~msg:"Region.get_bytes01" ~actual:used01 ~expected:used00);
  Lwt.return_unit

let () =
  V1.run "mem"
  [ "startup",
   [ test_case_sync "region" `Quick @@ test_default region
   ]
   
  ; "region",
    [ test_case_sync "limit_update_free" `Quick @@ test_limit_update_free ~limit:30 8
    ; test_case "track" `Quick test_region_track
    ; test_case "track_abandon" `Quick test_region_track_abandon
    ]
  ] |> Lwt_main.run
