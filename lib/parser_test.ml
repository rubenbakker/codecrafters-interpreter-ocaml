open! Base

let%expect_test "simple unary" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.BANG; lexeme = "!" };
      { token_type = Tokens.TRUE; lexeme = "true" };
    ]
  in
  let ast, _rest = Parser.expression tokens in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect {| (Unary BANG (Literal (LiteralBoolean true))) |}]

let%expect_test "double unary" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.BANG; lexeme = "!" };
      { token_type = Tokens.BANG; lexeme = "!" };
      { token_type = Tokens.TRUE; lexeme = "true" };
    ]
  in
  let ast, _rest = Parser.expression tokens in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect {| (Unary BANG (Unary BANG (Literal (LiteralBoolean true)))) |}]

let%expect_test "minus number" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.MINUS; lexeme = "-" };
      { token_type = Tokens.NUMBER 5.0; lexeme = "5.0" };
    ]
  in
  let ast, _rest = Parser.expression tokens in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect {| (Unary MINUS (Literal (LiteralNumber 5))) |}]

let%expect_test "unary with grouping" =
  let tokens : Tokens.t list =
    [
      { token_type = Tokens.MINUS; lexeme = "-" };
      { token_type = Tokens.LEFT_PAREN; lexeme = "(" };
      { token_type = Tokens.NUMBER 5.0; lexeme = "5.0" };
      { token_type = Tokens.RIGHT_PAREN; lexeme = ")" };
    ]
  in
  let ast, _rest = Parser.expression tokens in
  Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline;
  [%expect {| (Unary MINUS (Grouping (Literal (LiteralNumber 5)))) |}]
