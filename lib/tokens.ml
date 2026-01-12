open! Base

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
  | IDENTIFIER
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FOR
  | FUN
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

type t = { token_type : token_type; lexeme : string }

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
  | IDENTIFIER -> "IDENTIFIER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FOR -> "FOR"
  | FUN -> "FUN"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
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

let to_string token =
  let literal_value =
    match literal token.token_type with Some value -> value | None -> "null"
  in
  Stdlib.Printf.sprintf "%s %s %s"
    (token_name token.token_type)
    token.lexeme literal_value

let print_tokens tokens =
  List.map ~f:(fun token -> Stdlib.print_endline (to_string token)) tokens
  |> ignore
