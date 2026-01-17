open! Base

let%expect_test "unary expression" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.BANG; lexeme = "!"; line = 1 };
      { token_type = Tokens.TRUE; lexeme = "true"; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn
  |> Interpreter.evaluate (Interpreter.create_environment None)
  |> Result.ok |> Option.value_exn |> Interpreter.sexp_of_value_t
  |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect {| (BooleanValue false) |}]

let%expect_test "plus numbers" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.NUMBER 5.0; lexeme = "5.0"; line = 1 };
      { token_type = Tokens.PLUS; lexeme = "+"; line = 1 };
      { token_type = Tokens.NUMBER 6.0; lexeme = "6.0"; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn
  |> Interpreter.evaluate (Interpreter.create_environment None)
  |> Result.ok |> Option.value_exn |> Interpreter.value_to_string
  |> Stdlib.print_endline;
  [%expect {| 11 |}]

let%expect_test "plus strings" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.STRING "hello"; lexeme = "hello"; line = 1 };
      { token_type = Tokens.PLUS; lexeme = "+"; line = 1 };
      { token_type = Tokens.STRING "world"; lexeme = "world"; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn
  |> Interpreter.evaluate (Interpreter.create_environment None)
  |> Result.ok |> Option.value_exn |> Interpreter.value_to_string
  |> Stdlib.print_endline;
  [%expect {| helloworld |}]

let%expect_test "minus strings should report error" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.STRING "hello"; lexeme = "hello"; line = 1 };
      { token_type = Tokens.MINUS; lexeme = "-"; line = 1 };
      { token_type = Tokens.STRING "world"; lexeme = "world"; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn
  |> Interpreter.evaluate (Interpreter.create_environment None)
  |> Result.error |> Option.value_exn |> Interpreter.error_to_string
  |> Stdlib.print_endline;
  [%expect {|
    Operands must be numbers
    [line 1]
    |}]

let%expect_test "print statements" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.PRINT; lexeme = "print"; line = 1 };
      { token_type = Tokens.STRING "hello"; lexeme = "hello"; line = 1 };
      { token_type = Tokens.SEMICOLON; lexeme = ";"; line = 1 };
      { token_type = Tokens.PRINT; lexeme = "print"; line = 2 };
      { token_type = Tokens.STRING "world"; lexeme = "world"; line = 2 };
      { token_type = Tokens.SEMICOLON; lexeme = ";"; line = 2 };
      { token_type = Tokens.EOF; lexeme = ""; line = 2 };
    ]
  in
  let program = Parser.parse_program tokens |> Result.ok |> Option.value_exn in
  Interpreter.run_program program (Interpreter.create_environment None);
  [%expect {|
    hello
    world
    |}]
