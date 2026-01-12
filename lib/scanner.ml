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
  | STRING of string
  | NUMBER of float
  | INDENTIFIER of string
  | EOF

type t = { token_type : token_type; lexeme : string }
type token_result_t = (t, string) Result.t

let char_is_alphanum char = Char.is_alphanum char || Char.(char = '_')
let char_is_alpha char = Char.is_alpha char || Char.(char = '_')
let char_list_of_string str = List.init (String.length str) ~f:(String.get str)

let rec skip_comment chars =
  match chars with
  | [] -> chars
  | '\n' :: rest -> rest
  | _ :: rest -> skip_comment rest

let number_token_of_chars chars =
  let str = chars |> List.rev |> String.of_char_list in
  { token_type = NUMBER (Float.of_string str); lexeme = str }

let rec scan_number (chars : char list) (num_chars : char list) :
    char list * token_result_t =
  match chars with
  | [] -> (chars, Ok (number_token_of_chars num_chars))
  | ('.' as v) :: rest -> scan_number rest (v :: num_chars)
  | (_ as v) :: rest when Char.is_digit v -> scan_number rest (v :: num_chars)
  | _ -> (chars, Ok (number_token_of_chars num_chars))

let rec scan_string chars str line =
  match chars with
  | [] ->
      ( chars,
        Error
          (Stdlib.Printf.sprintf "[line %d] Error: Unterminated string." line),
        line )
  | '\"' :: rest ->
      let str = '\"' :: str |> List.rev |> String.of_char_list in
      ( rest,
        Ok
          {
            token_type =
              STRING (String.sub ~pos:1 ~len:(String.length str - 2) str);
            lexeme = str;
          },
        line )
  | '\n' :: rest -> scan_string rest ('\n' :: str) (line + 1)
  | (_ as v) :: rest -> scan_string rest (v :: str) line

let token_for_identifier identifier_chars : t =
  let str = List.rev identifier_chars |> String.of_char_list in
  { token_type = INDENTIFIER str; lexeme = str }

let rec scan_identifier chars identifier =
  match chars with
  | [] -> (chars, Ok (token_for_identifier identifier))
  | (_ as v) :: rest when char_is_alphanum v ->
      scan_identifier rest (v :: identifier)
  | _ -> (chars, Ok (token_for_identifier identifier))

let rec parse_rec (chars : char list) (acc : token_result_t list) (line : int) :
    token_result_t list =
  match chars with
  | [] -> Ok { token_type = EOF; lexeme = "" } :: acc
  | '\"' :: rest ->
      let rest, string_token_result, line = scan_string rest [ '\"' ] line in
      parse_rec rest (string_token_result :: acc) line
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
  | '/' :: '/' :: rest -> parse_rec (skip_comment rest) acc (line + 1)
  | '/' :: rest ->
      parse_rec rest (Ok { token_type = SLASH; lexeme = "/" } :: acc) line
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
  | '+' :: rest ->
      parse_rec rest (Ok { token_type = PLUS; lexeme = "+" } :: acc) line
  | '-' :: rest ->
      parse_rec rest (Ok { token_type = MINUS; lexeme = "-" } :: acc) line
  | '\n' :: rest -> parse_rec rest acc (line + 1)
  | '\t' :: rest -> parse_rec rest acc line
  | ' ' :: rest -> parse_rec rest acc line
  | (_ as v) :: rest when char_is_alpha v ->
      let rest, token_result = scan_identifier rest [ v ] in
      parse_rec rest (token_result :: acc) line
  | (_ as v) :: rest when Char.is_digit v ->
      let rest, token_result = scan_number rest [ v ] in
      parse_rec rest (token_result :: acc) line
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
  | STRING _ -> "STRING"
  | NUMBER _ -> "NUMBER"
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
  | INDENTIFIER _ -> "INDENTIFIER"
  | EOF -> "EOF"

let literal token_type =
  match token_type with
  | STRING value -> Some value
  | NUMBER value -> (
      let value = Float.to_string value in
      let value = String.rstrip ~drop:(fun ch -> Char.(ch = '0')) value in
      match Stdlib.String.ends_with ~suffix:"." value with
      | true -> Some (String.append value "0")
      | false -> Some value)
  | _ -> None

let t_to_string token =
  let literal_value =
    match literal token.token_type with Some value -> value | None -> "null"
  in
  Stdlib.Printf.sprintf "%s %s %s"
    (token_name token.token_type)
    token.lexeme literal_value

let print_tokens (tokens : t list) =
  List.map ~f:(fun token -> Stdlib.print_endline (t_to_string token)) tokens
  |> ignore
