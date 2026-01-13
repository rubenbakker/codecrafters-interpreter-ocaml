open Base
open Lib

let tokenize_command filename =
  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  let token_results = Scanner.scan file_contents in
  let errors = token_results |> Scanner.get_errors in

  errors |> List.map ~f:(fun error -> Stdlib.prerr_endline error) |> ignore;

  token_results |> Scanner.get_tokens |> Tokens.print_tokens;
  match errors with [] -> 0 | _ -> 65

let parse_command filename =
  let file_contents = In_channel.with_open_text filename In_channel.input_all in
  let token_results = Scanner.scan file_contents in
  let errors = token_results |> Scanner.get_errors in

  match errors with
  | [] ->
      token_results |> Scanner.get_tokens |> Parser.parse |> Ast.to_string
      |> Stdlib.print_endline;
      0
  | _ ->
      errors |> List.map ~f:(fun error -> Stdlib.prerr_endline error) |> ignore;

      token_results |> Scanner.get_tokens |> Tokens.print_tokens;
      65

let () =
  if Array.length Stdlib.Sys.argv < 3 then (
    Stdlib.Printf.eprintf
      "Usage: ./your_program.sh (tokenize|parse) <filename>\n";
    Stdlib.exit 1);

  let command = Stdlib.Sys.argv.(1) in
  let filename = Stdlib.Sys.argv.(2) in

  let exit_code =
    match command with
    | "tokenize" -> tokenize_command filename
    | "parse" -> parse_command filename
    | _ ->
        Stdlib.Printf.eprintf "Unknown command: %s\n" command;
        1
  in
  Stdlib.flush Stdlib.stderr;
  Stdlib.flush Stdlib.stdout;
  Stdlib.exit exit_code
