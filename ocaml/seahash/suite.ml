
let check str ~hash =
  let msg = Printf.sprintf "hash of %s is %Ld" str hash in
  Alcotest.(check int64) msg hash (Seahash.Hash.bytes (Bytes.of_string str))

let test () =
  check "fjkdsl" ~hash:54L;
  check "jfklds" ~hash:54L

let test_set =
  [ "Test bytes function", `Quick, test ]

let () = Alcotest.run "suite" [ "test set", test_set ]
