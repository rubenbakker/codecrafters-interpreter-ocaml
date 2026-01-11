open Base

type token_type = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | EOF
type t = { type_ : token_type; lexeme : string }

let char_list_of_string str = List.init (String.length str) ~f:(String.get str)

let rec parse_rec (chars : char list) (acc : t list) : t list =
  match chars with
  | [] -> { type_ = EOF; lexeme = "" } :: acc
  | char :: rest -> (
      match char with
      | '(' -> parse_rec rest ({ type_ = LEFT_PAREN; lexeme = "(" } :: acc)
      | ')' -> parse_rec rest ({ type_ = RIGHT_PAREN; lexeme = ")" } :: acc)
      | '{' -> parse_rec rest ({ type_ = LEFT_BRACE; lexeme = "{" } :: acc)
      | '}' -> parse_rec rest ({ type_ = RIGHT_BRACE; lexeme = "}" } :: acc)
      | _ -> parse_rec rest acc)

let parse (chars : char list) : t list = parse_rec chars [] |> List.rev

let token_name (x : token_type) : string =
  match x with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | EOF -> "EOF"

let t_to_string token =
  Stdlib.Printf.sprintf "%s %s null\n" (token_name token.type_) token.lexeme

let print_tokens (tokens : t list) =
  List.map ~f:(fun token -> Stdlib.print_endline (t_to_string token)) tokens
  |> ignore
