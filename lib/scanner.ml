open Base
open Tokens

type token_result_t = (t, string) Result.t

let char_is_alphanum char = Char.is_alphanum char || Char.(char = '_')
let char_is_alpha char = Char.is_alpha char || Char.(char = '_')
let char_list_of_string str = List.init (String.length str) ~f:(String.get str)

let rec skip_comment chars =
  match chars with
  | [] -> chars
  | '\n' :: rest -> rest
  | _ :: rest -> skip_comment rest

let number_token_of_chars chars line =
  let str = chars |> List.rev |> String.of_char_list in
  { token_type = NUMBER (Float.of_string str); lexeme = str; line }

let rec scan_number (chars : char list) (num_chars : char list) (line : int) :
    char list * token_result_t =
  match chars with
  | [] -> (chars, Ok (number_token_of_chars num_chars line))
  | ('.' as v) :: rest -> scan_number rest (v :: num_chars) line
  | (_ as v) :: rest when Char.is_digit v ->
      scan_number rest (v :: num_chars) line
  | _ -> (chars, Ok (number_token_of_chars num_chars line))

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
            line;
          },
        line )
  | '\n' :: rest -> scan_string rest ('\n' :: str) (line + 1)
  | (_ as v) :: rest -> scan_string rest (v :: str) line

let token_for_identifier identifier_chars line : t =
  let str = List.rev identifier_chars |> String.of_char_list in
  let token_type =
    match str with
    | "and" -> AND
    | "class" -> CLASS
    | "else" -> ELSE
    | "false" -> FALSE
    | "for" -> FOR
    | "fun" -> FUN
    | "if" -> IF
    | "nil" -> NIL
    | "or" -> OR
    | "print" -> PRINT
    | "return" -> RETURN
    | "super" -> SUPER
    | "this" -> THIS
    | "true" -> TRUE
    | "var" -> VAR
    | "while" -> WHILE
    | _ -> IDENTIFIER
  in
  { token_type; lexeme = str; line }

let rec scan_identifier chars identifier line =
  match chars with
  | [] -> (chars, Ok (token_for_identifier identifier line))
  | (_ as v) :: rest when char_is_alphanum v ->
      scan_identifier rest (v :: identifier) line
  | _ -> (chars, Ok (token_for_identifier identifier line))

let rec scan_rec (chars : char list) (acc : token_result_t list) (line : int) :
    token_result_t list =
  match chars with
  | [] -> Ok { token_type = EOF; lexeme = ""; line } :: acc
  | '\"' :: rest ->
      let rest, string_token_result, line = scan_string rest [ '\"' ] line in
      scan_rec rest (string_token_result :: acc) line
  | '!' :: '=' :: rest ->
      scan_rec rest
        (Ok { token_type = BANG_EQUAL; lexeme = "!="; line } :: acc)
        line
  | '!' :: rest ->
      scan_rec rest (Ok { token_type = BANG; lexeme = "!"; line } :: acc) line
  | '=' :: '=' :: rest ->
      scan_rec rest
        (Ok { token_type = EQUAL_EQUAL; lexeme = "=="; line } :: acc)
        line
  | '=' :: rest ->
      scan_rec rest (Ok { token_type = EQUAL; lexeme = "="; line } :: acc) line
  | '<' :: '=' :: rest ->
      scan_rec rest
        (Ok { token_type = LESS_EQUAL; lexeme = "<="; line } :: acc)
        line
  | '<' :: rest ->
      scan_rec rest (Ok { token_type = LESS; lexeme = "<"; line } :: acc) line
  | '>' :: '=' :: rest ->
      scan_rec rest
        (Ok { token_type = GREATER_EQUAL; lexeme = ">="; line } :: acc)
        line
  | '>' :: rest ->
      scan_rec rest
        (Ok { token_type = GREATER; lexeme = ">"; line } :: acc)
        line
  | '/' :: '/' :: rest -> scan_rec (skip_comment rest) acc (line + 1)
  | '/' :: rest ->
      scan_rec rest (Ok { token_type = SLASH; lexeme = "/"; line } :: acc) line
  | '(' :: rest ->
      scan_rec rest
        (Ok { token_type = LEFT_PAREN; lexeme = "("; line } :: acc)
        line
  | ')' :: rest ->
      scan_rec rest
        (Ok { token_type = RIGHT_PAREN; lexeme = ")"; line } :: acc)
        line
  | '{' :: rest ->
      scan_rec rest
        (Ok { token_type = LEFT_BRACE; lexeme = "{"; line } :: acc)
        line
  | '}' :: rest ->
      scan_rec rest
        (Ok { token_type = RIGHT_BRACE; lexeme = "}"; line } :: acc)
        line
  | '*' :: rest ->
      scan_rec rest (Ok { token_type = STAR; lexeme = "*"; line } :: acc) line
  | ',' :: rest ->
      scan_rec rest (Ok { token_type = COMMA; lexeme = ","; line } :: acc) line
  | '.' :: rest ->
      scan_rec rest (Ok { token_type = DOT; lexeme = "."; line } :: acc) line
  | ';' :: rest ->
      scan_rec rest
        (Ok { token_type = SEMICOLON; lexeme = ";"; line } :: acc)
        line
  | '+' :: rest ->
      scan_rec rest (Ok { token_type = PLUS; lexeme = "+"; line } :: acc) line
  | '-' :: rest ->
      scan_rec rest (Ok { token_type = MINUS; lexeme = "-"; line } :: acc) line
  | '\n' :: rest -> scan_rec rest acc (line + 1)
  | '\t' :: rest -> scan_rec rest acc line
  | ' ' :: rest -> scan_rec rest acc line
  | (_ as v) :: rest when char_is_alpha v ->
      let rest, token_result = scan_identifier rest [ v ] line in
      scan_rec rest (token_result :: acc) line
  | (_ as v) :: rest when Char.is_digit v ->
      let rest, token_result = scan_number rest [ v ] line in
      scan_rec rest (token_result :: acc) line
  | (_ as v) :: rest ->
      scan_rec rest
        (Error
           (Stdlib.Printf.sprintf "[line %d] Error: Unexpected character: %c"
              line v)
        :: acc)
        line

let scan str : token_result_t list =
  scan_rec (char_list_of_string str) [] 1 |> List.rev

let get_errors (tokens : token_result_t list) : string list =
  List.filter_map
    ~f:(fun tr -> match tr with Ok _ -> None | Error err -> Some err)
    tokens

let get_tokens (tokens : token_result_t list) : t list =
  List.filter_map
    ~f:(fun tr -> match tr with Ok token -> Some token | Error _ -> None)
    tokens
