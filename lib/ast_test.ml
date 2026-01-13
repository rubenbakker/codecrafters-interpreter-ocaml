open! Base

let%expect_test "Simple binary" =
  let ast =
    Ast.Binary
      ( Ast.Literal (Ast.LiteralString "Hallo"),
        Tokens.BANG_EQUAL,
        Ast.Literal (Ast.LiteralString "Test") )
  in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect
    {|
    (Binary (Literal (LiteralString Hallo)) BANG_EQUAL
     (Literal (LiteralString Test)))
    |}]

let%expect_test "Nested binary" =
  let ast =
    Ast.Binary
      ( Ast.Literal (Ast.LiteralNumber 5.0),
        Tokens.STAR,
        Ast.Binary
          ( Ast.Literal (Ast.LiteralNumber 7.0),
            Tokens.STAR,
            Ast.Literal (Ast.LiteralNumber 2.0) ) )
  in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect
    {|
    (Binary (Literal (LiteralNumber 5)) STAR
     (Binary (Literal (LiteralNumber 7)) STAR (Literal (LiteralNumber 2))))
    |}]

let%expect_test "to_string" =
  let ast =
    Ast.Binary
      ( Ast.Literal (Ast.LiteralNumber 5.0),
        Tokens.STAR,
        Ast.Binary
          ( Ast.Literal (Ast.LiteralNumber 7.0),
            Tokens.STAR,
            Ast.Literal (Ast.LiteralNumber 2.0) ) )
  in
  Ast.to_string ast |> Stdlib.print_endline;
  [%expect {|
    (5.0 * (7.0 * 2.0))
    |}]
