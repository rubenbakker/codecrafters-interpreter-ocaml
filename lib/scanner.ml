open Base

type token_type =
  | BANG
  | BANG_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | MINUS
  | STAR
  | COMMA
  | DOT
  | SEMICOLON
  | SLASH
  | PLUS
  | EQUAL
  | EQUAL_EQUAL
  | EOF

type t = { token_type : token_type; lexeme : string }
type token_result_t = (t, string) Result.t

let char_list_of_string str = List.init (String.length str) ~f:(String.get str)

let rec parse_rec (chars : char list) (acc : token_result_t list) (line : int) :
    token_result_t list =
  match chars with
  | [] -> Ok { token_type = EOF; lexeme = "" } :: acc
  | '!' :: '=' :: rest ->
      parse_rec rest (Ok { token_type = BANG_EQUAL; lexeme = "!=" } :: acc) line
  | '!' :: rest ->
      parse_rec rest (Ok { token_type = BANG; lexeme = "!" } :: acc) line
  | '=' :: '=' :: rest ->
      parse_rec rest
        (Ok { token_type = EQUAL_EQUAL; lexeme = "==" } :: acc)
        line
  | '=' :: rest ->
      parse_rec rest (Ok { token_type = EQUAL; lexeme = "=" } :: acc) line
  | '<' :: '=' :: rest ->
      parse_rec rest (Ok { token_type = LESS_EQUAL; lexeme = "<=" } :: acc) line
  | '<' :: rest ->
      parse_rec rest (Ok { token_type = LESS; lexeme = "<" } :: acc) line
  | '>' :: '=' :: rest ->
      parse_rec rest
        (Ok { token_type = GREATER_EQUAL; lexeme = ">=" } :: acc)
        line
  | '>' :: rest ->
      parse_rec rest (Ok { token_type = GREATER; lexeme = ">" } :: acc) line
  | '(' :: rest ->
      parse_rec rest (Ok { token_type = LEFT_PAREN; lexeme = "(" } :: acc) line
  | ')' :: rest ->
      parse_rec rest (Ok { token_type = RIGHT_PAREN; lexeme = ")" } :: acc) line
  | '{' :: rest ->
      parse_rec rest (Ok { token_type = LEFT_BRACE; lexeme = "{" } :: acc) line
  | '}' :: rest ->
      parse_rec rest (Ok { token_type = RIGHT_BRACE; lexeme = "}" } :: acc) line
  | '*' :: rest ->
      parse_rec rest (Ok { token_type = STAR; lexeme = "*" } :: acc) line
  | ',' :: rest ->
      parse_rec rest (Ok { token_type = COMMA; lexeme = "," } :: acc) line
  | '.' :: rest ->
      parse_rec rest (Ok { token_type = DOT; lexeme = "." } :: acc) line
  | ';' :: rest ->
      parse_rec rest (Ok { token_type = SEMICOLON; lexeme = ";" } :: acc) line
  | '/' :: rest ->
      parse_rec rest (Ok { token_type = SLASH; lexeme = "/" } :: acc) line
  | '+' :: rest ->
      parse_rec rest (Ok { token_type = PLUS; lexeme = "+" } :: acc) line
  | '-' :: rest ->
      parse_rec rest (Ok { token_type = MINUS; lexeme = "-" } :: acc) line
  | '\n' :: rest -> parse_rec rest acc (line + 1)
  | (_ as v) :: rest ->
      parse_rec rest
        (Error
           (Stdlib.Printf.sprintf "[line %d] Error: Unexpected character: %c"
              line v)
        :: acc)
        line

let parse (chars : char list) : token_result_t list =
  parse_rec chars [] 1 |> List.rev

let get_errors (tokens : token_result_t list) : string list =
  List.filter_map
    ~f:(fun tr -> match tr with Ok _ -> None | Error err -> Some err)
    tokens

let get_tokens (tokens : token_result_t list) : t list =
  List.filter_map
    ~f:(fun tr -> match tr with Ok token -> Some token | Error _ -> None)
    tokens

let token_name (x : token_type) : string =
  match x with
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | STAR -> "STAR"
  | COMMA -> "COMMA"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | DOT -> "DOT"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | EOF -> "EOF"

let t_to_string token =
  Stdlib.Printf.sprintf "%s %s null" (token_name token.token_type) token.lexeme

let print_tokens (tokens : t list) =
  List.map ~f:(fun token -> Stdlib.print_endline (t_to_string token)) tokens
  |> ignore
