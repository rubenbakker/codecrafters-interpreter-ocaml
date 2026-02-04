open Base

let%expect_test "block var is in list" =
  (match
     Scanner.scan
       {|
        // This variable is used in the function `f` below.
        var variable = "global";

        {
          var x = 15;
          fun f(fa) {
            print fa;
            print variable;
            print x;
            {
              x = 17;
            }
          }

          f("hallo"); // this should print "global"

          // This variable declaration shouldn't affect
          // the usage in `f` above.
          var variable = "local";

          f("hello"); // this should still print "global"
          print variable;
        }
      |}
     |> Scanner.get_tokens |> Parser.parse_program
   with
  | Ok r -> (
      match Resolver.analyze_program r with
      | Ok p ->
          p |> Ast.sexp_of_program_t |> Sexp.to_string_hum
          |> Stdlib.print_endline
      | Error _ -> Stdlib.print_endline "error")
  | Error e -> Stdlib.print_endline (Parser.format_error e));
  [%expect
    {|
    ((VarStmt variable (Literal (LiteralString global)))
     (Block
      ((VarStmt x (Literal (LiteralNumber 15)))
       (Function ((token_type IDENTIFIER) (lexeme f) (line 7))
        (((token_type IDENTIFIER) (lexeme fa) (line 7)))
        ((PrintStmt
          (Variable fa ((token_type IDENTIFIER) (lexeme fa) (line 8)) (0)))
         (PrintStmt
          (Variable variable ((token_type IDENTIFIER) (lexeme variable) (line 9))
           (2)))
         (PrintStmt
          (Variable x ((token_type IDENTIFIER) (lexeme x) (line 10)) (1)))
         (Block
          ((ExprStmt
            (Assign ((token_type EQUAL) (lexeme x) (line 12))
             (Literal (LiteralNumber 17)) (2)))))))
       (ExprStmt
        (Call (Variable f ((token_type IDENTIFIER) (lexeme f) (line 16)) (0))
         ((Literal (LiteralString hallo)))
         ((token_type LEFT_PAREN) (lexeme "(") (line 16))))
       (VarStmt variable (Literal (LiteralString local)))
       (ExprStmt
        (Call (Variable f ((token_type IDENTIFIER) (lexeme f) (line 22)) (0))
         ((Literal (LiteralString hello)))
         ((token_type LEFT_PAREN) (lexeme "(") (line 22))))
       (PrintStmt
        (Variable variable ((token_type IDENTIFIER) (lexeme variable) (line 23))
         (0))))))
    |}]
