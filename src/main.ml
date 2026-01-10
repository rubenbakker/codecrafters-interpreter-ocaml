open Stdlib

type token = LeftParen | RightParen | Eof

let char_list_of_string str = List.init (String.length str) (String.get str)

let rec parse (chars : char list) : token list =
  match chars with
  | [] -> [ Eof ]
  | char :: rest -> (
      match char with
      | '(' -> LeftParen :: parse rest
      | ')' -> RightParen :: parse rest
      | _ -> parse rest)

let token_name (x : token) : string =
  match x with
  | LeftParen -> "LEFT_PAREN"
  | RightParen -> "RIGHT_PAREN"
  | Eof -> "EOF"

let lexeme_for_token (x : token) : string =
  match x with LeftParen -> "(" | RightParen -> ")" | Eof -> ""

let print_tokens (tokens : token list) =
  List.map
    (fun token ->
      Printf.printf "%s %s null\n" (token_name token) (lexeme_for_token token))
    tokens
  |> ignore

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  char_list_of_string file_contents |> parse |> print_tokens;
  flush stdout
