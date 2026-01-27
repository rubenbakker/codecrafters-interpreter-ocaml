open Base

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

let run_command filename =
  parse_program filename (fun program ->
      match Interpreter.run program with
      | Ok _ -> 0
      | Error error ->
          Interpreter.error_to_string error |> Stdlib.prerr_endline;
          70)
