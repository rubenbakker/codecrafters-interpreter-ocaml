open Base

type literal_t =
  | LiteralNumber of Float.t
  | LiteralString of String.t
  | LiteralBoolean of Bool.t
  | LiteralNil
[@@deriving compare, equal, sexp]

type t =
  | Binary of t * Tokens.t * t
  | Grouping of t
  | Literal of literal_t
  | Unary of Tokens.t * t
[@@deriving compare, equal, sexp]

type stmt_t = PrintStmt of t | ExprStmt of t [@@deriving compare, equal, sexp]
type program_t = stmt_t list [@@deriving compare, equal, sexp]

let token_type_to_string token_type =
  match token_type with
  | Tokens.BANG -> "!"
  | Tokens.BANG_EQUAL -> "!="
  | Tokens.LESS -> "<"
  | Tokens.LESS_EQUAL -> "<="
  | Tokens.GREATER -> ">"
  | Tokens.GREATER_EQUAL -> ">="
  | Tokens.LEFT_PAREN -> "("
  | Tokens.RIGHT_PAREN -> ")"
  | Tokens.LEFT_BRACE -> "{"
  | Tokens.RIGHT_BRACE -> "}"
  | Tokens.MINUS -> "-"
  | Tokens.STAR -> "*"
  | Tokens.COMMA -> ","
  | Tokens.DOT -> "."
  | Tokens.SEMICOLON -> ";"
  | Tokens.SLASH -> "/"
  | Tokens.PLUS -> "+"
  | Tokens.EQUAL -> "="
  | Tokens.EQUAL_EQUAL -> "=="
  | Tokens.STRING _ -> ""
  | Tokens.NUMBER _ -> ""
  | Tokens.IDENTIFIER -> ""
  | Tokens.AND -> "&&"
  | Tokens.CLASS -> "class"
  | Tokens.ELSE -> "else"
  | Tokens.FALSE -> "false"
  | Tokens.FOR -> "for"
  | Tokens.FUN -> "fun"
  | Tokens.IF -> "if"
  | Tokens.NIL -> "nil"
  | Tokens.OR -> "or"
  | Tokens.PRINT -> "print"
  | Tokens.RETURN -> "return"
  | Tokens.SUPER -> "super"
  | Tokens.THIS -> "this"
  | Tokens.TRUE -> "true"
  | Tokens.VAR -> "var"
  | Tokens.WHILE -> "while"
  | Tokens.EOF -> ""

let literal_to_string literal =
  match literal with
  | LiteralNumber value -> Tokens.number_to_string value
  | LiteralBoolean value -> (
      match value with true -> "true" | false -> "false")
  | LiteralNil -> "nil"
  | LiteralString value -> value

let rec to_string (ast : t) : string =
  match ast with
  | Binary (left_expr, operator, right_expr) ->
      Stdlib.Printf.sprintf "(%s %s %s)"
        (token_type_to_string operator.token_type)
        (to_string left_expr) (to_string right_expr)
  | Grouping expr -> Stdlib.Printf.sprintf "(group %s)" (to_string expr)
  | Literal value -> literal_to_string value
  | Unary (operator, expr) ->
      Stdlib.Printf.sprintf "(%s %s)"
        (token_type_to_string operator.token_type)
        (to_string expr)

let program_to_string (program : program_t) : string =
  List.map
    ~f:(fun stmt ->
      match stmt with
      | PrintStmt expr -> Stdlib.Printf.sprintf "(PRINT %s)\n" (to_string expr)
      | ExprStmt expr -> Stdlib.Printf.sprintf "(%s)\n" (to_string expr))
    program
  |> String.concat_lines
