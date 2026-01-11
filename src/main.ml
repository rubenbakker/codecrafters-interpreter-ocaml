open Base

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

  let open Lib.Scanner in
  char_list_of_string file_contents |> parse |> print_tokens;

  Stdlib.flush Stdlib.stdout
