open! Base

let%expect_test "Simple binary" =
  let ast =
    Ast.Binary
      ( Ast.Literal (Ast.LiteralString "Hallo"),
        { token_type = Tokens.BANG_EQUAL; lexeme = "!="; line = 1 },
        Ast.Literal (Ast.LiteralString "Test") )
  in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect
    {|
    (Binary (Literal (LiteralString Hallo))
     ((token_type BANG_EQUAL) (lexeme !=) (line 1))
     (Literal (LiteralString Test)))
    |}]

let%expect_test "Nested binary" =
  let ast =
    Ast.Binary
      ( Ast.Literal (Ast.LiteralNumber 5.0),
        { token_type = Tokens.STAR; lexeme = "*"; line = 1 },
        Ast.Binary
          ( Ast.Literal (Ast.LiteralNumber 7.0),
            { token_type = Tokens.STAR; lexeme = "*"; line = 1 },
            Ast.Literal (Ast.LiteralNumber 2.0) ) )
  in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect
    {|
    (Binary (Literal (LiteralNumber 5)) ((token_type STAR) (lexeme *) (line 1))
     (Binary (Literal (LiteralNumber 7)) ((token_type STAR) (lexeme *) (line 1))
      (Literal (LiteralNumber 2))))
    |}]

let%expect_test "to_string" =
  let ast =
    Ast.Binary
      ( Ast.Literal (Ast.LiteralNumber 5.0),
        { token_type = Tokens.STAR; lexeme = "*"; line = 1 },
        Ast.Binary
          ( Ast.Literal (Ast.LiteralNumber 7.0),
            { token_type = Tokens.STAR; lexeme = "*"; line = 1 },
            Ast.Literal (Ast.LiteralNumber 2.0) ) )
  in
  Ast.to_string ast |> Stdlib.print_endline;
  [%expect {|
    (* 5.0 (* 7.0 2.0))
    |}]
