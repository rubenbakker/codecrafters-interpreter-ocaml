open Base

let%expect_test "global var isn't not in list" =
  (match
     Scanner.scan
       {|
        // This variable is used in the function `f` below.
        var variable = "global";

        {
          fun f() {
            print variable;
          }

          f(); // this should print "global"

          // This variable declaration shouldn't affect
          // the usage in `f` above.
          var variable = "local";

          f(); // this should still print "global"
        }
      |}
     |> Scanner.get_tokens |> Parser.parse_program
   with
  | Ok r -> (
      match Analyzer.analyze_program_tl r with
      | Ok r ->
          Analyzer.sexp_of_tl r |> Sexp.to_string_hum |> Stdlib.print_endline
      | Error e -> Stdlib.print_endline e)
  | Error e -> Stdlib.print_endline (Parser.format_error e));
  [%expect {| (((Variable f) 0) ((Variable f) 0)) |}]

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
      match Analyzer.analyze_program_tl r with
      | Ok r ->
          Analyzer.sexp_of_tl r |> Sexp.to_string_hum |> Stdlib.print_endline
      | Error e -> Stdlib.print_endline e)
  | Error e -> Stdlib.print_endline (Parser.format_error e));
  [%expect {|
    (((Variable variable) 0) ((Variable f) 0) ((Variable f) 0)
     ((Literal (LiteralNumber 17)) 2) ((Variable x) 1) ((Variable fa) 0))
    |}]
