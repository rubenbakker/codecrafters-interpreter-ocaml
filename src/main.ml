open Base
open Lib

let () =
  if Array.length Stdlib.Sys.argv < 3 then (
    Stdlib.Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    Stdlib.exit 1);

  let command = Stdlib.Sys.argv.(1) in
  let filename = Stdlib.Sys.argv.(2) in

  if String.(command <> "tokenize") then (
    Stdlib.Printf.eprintf "Unknown command: %s\n" command;
    Stdlib.exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  let token_results = Scanner.scan file_contents in
  let errors = token_results |> Scanner.get_errors in

  errors |> List.map ~f:(fun error -> Stdlib.prerr_endline error) |> ignore;

  token_results |> Scanner.get_tokens |> Tokens.print_tokens;

  Stdlib.flush Stdlib.stderr;
  Stdlib.flush Stdlib.stdout;
  let exit_code = match errors with [] -> 0 | _ -> 65 in
  Stdlib.exit exit_code
