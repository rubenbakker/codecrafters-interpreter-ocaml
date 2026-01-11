open Base

type token_type = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | EOF

let char_list_of_string str = List.init (String.length str) ~f:(String.get str)

let rec parse_rec (chars : char list) (acc : token_type list) : token_type list
    =
  match chars with
  | [] -> EOF :: acc
  | char :: rest -> (
      match char with
      | '(' -> parse_rec rest (LEFT_PAREN :: acc)
      | ')' -> parse_rec rest (RIGHT_PAREN :: acc)
      | '{' -> parse_rec rest (LEFT_BRACE :: acc)
      | '}' -> parse_rec rest (RIGHT_BRACE :: acc)
      | _ -> parse_rec rest acc)

let parse (chars : char list) : token_type list = parse_rec chars [] |> List.rev

let token_name (x : token_type) : string =
  match x with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | EOF -> "EOF"

let lexeme_for_token (x : token_type) : string =
  match x with
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | EOF -> ""

let print_tokens (tokens : token_type list) =
  List.map
    ~f:(fun token ->
      Stdlib.Printf.printf "%s %s null\n" (token_name token)
        (lexeme_for_token token))
    tokens
  |> ignore
