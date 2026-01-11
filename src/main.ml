open Base

type token = LEFT_PAREN | RIGHT_PAREN | EOF

let char_list_of_string str = List.init (String.length str) ~f:(String.get str)

let rec parse_rec (chars : char list) (acc : token list) : token list =
  match chars with
  | [] -> EOF :: acc
  | char :: rest -> (
      match char with
      | '(' -> parse_rec rest (LEFT_PAREN :: acc)
      | ')' -> parse_rec rest (RIGHT_PAREN :: acc)
      | _ -> parse_rec rest acc)

let parse (chars : char list) : token list = parse_rec chars []

let token_name (x : token) : string =
  match x with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | EOF -> "EOF"

let lexeme_for_token (x : token) : string =
  match x with LEFT_PAREN -> "(" | RIGHT_PAREN -> ")" | EOF -> ""

let print_tokens (tokens : token list) =
  List.map
    ~f:(fun token ->
      Stdlib.Printf.printf "%s %s null\n" (token_name token)
        (lexeme_for_token token))
    tokens
  |> ignore

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

  char_list_of_string file_contents |> parse |> print_tokens;
  Stdlib.flush Stdlib.stdout
