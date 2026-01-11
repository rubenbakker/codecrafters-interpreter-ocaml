open! Base

let%expect_test "Reader.peek" =
  let reader = Reader.create_from_string "Hallo World!" in
  Stdlib.Printf.printf "First peek: %c\n"
    (reader |> Reader.peek |> Option.value_exn);
  Stdlib.Printf.printf "Second peek: %c\n"
    (reader |> Reader.advance |> Reader.peek |> Option.value_exn);
  [%expect {|
    First peek: H
    Second peek: a
    |}]

let%expect_test "Reader.matches_next" =
  let reader = Reader.create_from_string "Hallo World!" in
  Stdlib.Printf.printf "matches 'a': %b\n" (reader |> Reader.matches_next 'a');
  Stdlib.Printf.printf "matches 'x': %b\n" (reader |> Reader.matches_next 'x');
  [%expect {|
    matches 'a': true
    matches 'x': false
    |}]
