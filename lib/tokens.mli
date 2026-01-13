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
[@@deriving compare, equal, sexp]

type t = { token_type : token_type; lexeme : string }
[@@deriving compare, equal, sexp]

val token_name : t -> string
val literal : t -> string option
val to_string : t -> string
val print_tokens : t list -> unit
val number_to_string : float -> string
