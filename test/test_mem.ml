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

let () =
  Alcotest.V1.run "mem"
  [ "region",
    [ "update+", `Quick, test_region_update 8
    ; "update-", `Quick, test_region_update ~-8
    ; "limit_update_free", `Quick, test_limit_update_free ~limit:30 8
    ]
  ]
