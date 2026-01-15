open Base
open Lib

let tokenize_command filename =
  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  let token_results = Scanner.scan file_contents in
  let errors = token_results |> Scanner.get_errors in

  errors |> List.map ~f:(fun error -> Stdlib.prerr_endline error) |> ignore;

  token_results |> Scanner.get_tokens |> Tokens.print_tokens;
  match errors with [] -> 0 | _ -> 65

let parse_expression filename ok_fn =
  let file_contents = In_channel.with_open_text filename In_channel.input_all in
  let token_results = Scanner.scan file_contents in
  let errors = token_results |> Scanner.get_errors in

  match errors with
  | [] -> (
      match token_results |> Scanner.get_tokens |> Parser.parse_expression with
      | Ok ast -> ok_fn ast
      | Error error ->
          Stdlib.prerr_endline (Parser.format_error error);
          65)
  | _ ->
      errors |> List.map ~f:(fun error -> Stdlib.prerr_endline error) |> ignore;
      65

let parse_program filename ok_fn =
  let file_contents = In_channel.with_open_text filename In_channel.input_all in
  let token_results = Scanner.scan file_contents in
  let errors = token_results |> Scanner.get_errors in

  match errors with
  | [] -> (
      match token_results |> Scanner.get_tokens |> Parser.parse_program with
      | Ok program -> ok_fn program
      | Error error ->
          Stdlib.prerr_endline (Parser.format_error error);
          65)
  | _ ->
      errors |> List.map ~f:(fun error -> Stdlib.prerr_endline error) |> ignore;
      65

let parse_command filename =
  parse_expression filename (fun ast ->
      Ast.to_string ast |> Stdlib.print_endline;
      0)

let evaluate_command filename =
  parse_expression filename (fun ast ->
      match Interpreter.evaluate ast with
      | Ok ast ->
          Interpreter.value_to_string ast |> Stdlib.print_endline;
          0
      | Error error ->
          Interpreter.error_to_string error |> Stdlib.prerr_endline;
          70)

let run_command filename =
  parse_program filename (fun program ->
      match Interpreter.run program with
      | Ok _ -> 0
      | Error error ->
          Interpreter.error_to_string error |> Stdlib.prerr_endline;
          65)

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
    | "evaluate" -> evaluate_command filename
    | "run" -> run_command filename
    | _ ->
        Stdlib.Printf.eprintf "Unknown command: %s\n" command;
        1
  in
  Stdlib.flush Stdlib.stderr;
  Stdlib.flush Stdlib.stdout;
  Stdlib.exit exit_code
