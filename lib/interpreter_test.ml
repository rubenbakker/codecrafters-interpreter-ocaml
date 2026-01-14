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

let%expect_test "plus numbers" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.NUMBER 5.0; lexeme = "5.0"; line = 1 };
      { token_type = Tokens.PLUS; lexeme = "+"; line = 1 };
      { token_type = Tokens.NUMBER 6.0; lexeme = "6.0"; line = 1 };
    ]
  in
  let res = Parser.parse tokens in
  Result.ok res |> Option.value_exn |> Interpreter.evaluate
  |> Interpreter.value_to_string |> Stdlib.print_endline;
  [%expect {| 11 |}]

let%expect_test "plus strings" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.STRING "hello"; lexeme = "hello"; line = 1 };
      { token_type = Tokens.PLUS; lexeme = "+"; line = 1 };
      { token_type = Tokens.STRING "world"; lexeme = "world"; line = 1 };
    ]
  in
  let res = Parser.parse tokens in
  Result.ok res |> Option.value_exn |> Interpreter.evaluate
  |> Interpreter.value_to_string |> Stdlib.print_endline;
  [%expect {| helloworld |}]
