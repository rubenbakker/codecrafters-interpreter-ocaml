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

let%expect_test "variable" =
  Scanner.scan "var n = 5 + 2; print n;"
  |> Scanner.get_tokens |> Parser.parse_program |> Result.ok |> Option.value_exn
  |> Ast.program_to_string |> Stdlib.print_endline;
  [%expect {|
    (VAR n = (+ 5.0 2.0))
    (PRINT (VAR n))
    |}]

let%expect_test "variable scope" =
  (match
     Scanner.scan "var n = 5 + 2; { var n = 7; print n; } print n;"
     |> Scanner.get_tokens |> Parser.parse_program
   with
  | Ok r -> Ast.program_to_string r |> Stdlib.print_endline
  | Error e -> Parser.format_error e |> Stdlib.print_endline);

  [%expect
    {|
    (VAR n = (+ 5.0 2.0))
    (BLOCK
    (VAR n = 7.0)
    (PRINT (VAR n)))
    (PRINT (VAR n))
    |}]

let%expect_test "assignments" =
  (match
     Scanner.scan "var n = 5; n = p = 7; print n = 3; print p;"
     |> Scanner.get_tokens |> Parser.parse_program
   with
  | Ok r -> Ast.program_to_string r |> Stdlib.print_endline
  | Error e -> Parser.format_error e |> Stdlib.print_endline);

  [%expect
    {|
    (VAR n = 5.0)
    ((ASSIGN n (ASSIGN p 7.0)))

    (PRINT (ASSIGN n 3.0))

    (PRINT (VAR p))
    |}]

let%expect_test "if assignment" =
  (match
     Scanner.scan
       {|
// This program should print a different string
// based on the value of age
var stage = "unknown";
print "1";
var age = 45;
if (age < 18) { stage = "child"; }
if (age >= 18) { stage = "adult"; }
print stage;
print "2 after stage";
var isAdult = age >= 18;
if (isAdult) { print "eligible for voting"; }
if (!isAdult) { print "not eligible for voting"; }
      |}
     |> Scanner.get_tokens |> Parser.parse_program
   with
  | Ok r ->
      Ast.sexp_of_program_t r |> Sexp.to_string_hum |> Stdlib.print_endline
  | Error e -> Parser.format_error e |> Stdlib.print_endline);

  [%expect {|
    ((VarStmt stage (Literal (LiteralString unknown)))
     (PrintStmt (Literal (LiteralString 1)))
     (VarStmt age (Literal (LiteralNumber 45)))
     (IfStmt
      (Binary (Variable age) ((token_type LESS) (lexeme <) (line 7))
       (Literal (LiteralNumber 18)))
      (Block
       ((ExprStmt
         (Assign ((token_type EQUAL) (lexeme stage) (line 7))
          (Literal (LiteralString child))))))
      ())
     (IfStmt
      (Binary (Variable age) ((token_type GREATER_EQUAL) (lexeme >=) (line 8))
       (Literal (LiteralNumber 18)))
      (Block
       ((ExprStmt
         (Assign ((token_type EQUAL) (lexeme stage) (line 8))
          (Literal (LiteralString adult))))))
      ())
     (PrintStmt (Variable stage))
     (PrintStmt (Literal (LiteralString "2 after stage")))
     (VarStmt isAdult
      (Binary (Variable age) ((token_type GREATER_EQUAL) (lexeme >=) (line 11))
       (Literal (LiteralNumber 18))))
     (IfStmt (Variable isAdult)
      (Block ((PrintStmt (Literal (LiteralString "eligible for voting"))))) ())
     (IfStmt (Unary ((token_type BANG) (lexeme !) (line 13)) (Variable isAdult))
      (Block ((PrintStmt (Literal (LiteralString "not eligible for voting")))))
      ()))
    |}]
