open! Base

let%expect_test "unary expression" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.BANG; lexeme = "!"; line = 1 };
      { token_type = Tokens.TRUE; lexeme = "true"; line = 1 };
    ]
  in
  let res = Parser.parse tokens in
  Result.ok res |> Option.value_exn |> Interpreter.evaluate
  |> Interpreter.sexp_of_value_t |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect {| (BooleanValue false) |}]
