open! Base

let%expect_test "simple unary" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.BANG; lexeme = "!"; line = 1 };
      { token_type = Tokens.TRUE; lexeme = "true"; line = 1 };
      { token_type = Tokens.SEMICOLON; lexeme = ";"; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn |> Ast.sexp_of_t |> Sexp.to_string_hum
  |> Stdlib.print_endline;
  [%expect
    {|
    (Unary ((token_type BANG) (lexeme !) (line 1))
     (Literal (LiteralBoolean true)))
    |}]

let%expect_test "double unary" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.BANG; lexeme = "!"; line = 1 };
      { token_type = Tokens.BANG; lexeme = "!"; line = 1 };
      { token_type = Tokens.TRUE; lexeme = "true"; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn |> Ast.sexp_of_t |> Sexp.to_string_hum
  |> Stdlib.print_endline;
  [%expect
    {|
    (Unary ((token_type BANG) (lexeme !) (line 1))
     (Unary ((token_type BANG) (lexeme !) (line 1))
      (Literal (LiteralBoolean true))))
    |}]

let%expect_test "minus number" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.MINUS; lexeme = "-"; line = 1 };
      { token_type = Tokens.NUMBER 5.0; lexeme = "5.0"; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn |> Ast.sexp_of_t |> Sexp.to_string_hum
  |> Stdlib.print_endline;
  [%expect
    {| (Unary ((token_type MINUS) (lexeme -) (line 1)) (Literal (LiteralNumber 5))) |}]

let%expect_test "unary with grouping" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.MINUS; lexeme = "-"; line = 1 };
      { token_type = Tokens.LEFT_PAREN; lexeme = "("; line = 1 };
      { token_type = Tokens.NUMBER 5.0; lexeme = "5.0"; line = 1 };
      { token_type = Tokens.RIGHT_PAREN; lexeme = ")"; line = 1 };
      { token_type = Tokens.EOF; lexeme = ""; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn |> Ast.sexp_of_t |> Sexp.to_string_hum
  |> Stdlib.print_endline;
  [%expect
    {|
    (Unary ((token_type MINUS) (lexeme -) (line 1))
     (Grouping (Literal (LiteralNumber 5))))
    |}]

let%expect_test "parse arithmetic" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.NUMBER 24.0; lexeme = "24.0"; line = 1 };
      { token_type = Tokens.STAR; lexeme = "*"; line = 1 };
      { token_type = Tokens.NUMBER 78.0; lexeme = "78.0"; line = 1 };
      { token_type = Tokens.SLASH; lexeme = "/"; line = 1 };
      { token_type = Tokens.NUMBER 27.0; lexeme = "27.0"; line = 1 };
      { token_type = Tokens.EOF; lexeme = ""; line = 1 };
    ]
  in
  let res = Parser.parse_expression tokens in
  Result.ok res |> Option.value_exn |> Ast.to_string |> Stdlib.print_endline;
  [%expect {| (/ (* 24.0 78.0) 27.0) |}]

let%expect_test "parse statement with expression" =
  Scanner.scan "print 5 + 2;"
  |> Scanner.get_tokens |> Parser.parse_program |> Result.ok |> Option.value_exn
  |> Ast.program_to_string |> Stdlib.print_endline;
  [%expect {| (PRINT (+ 5.0 2.0)) |}]
