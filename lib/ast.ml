open Base

type literal_t =
  | LiteralNumber of Float.t
  | LiteralString of String.t
  | LiteralBoolean of Bool.t
  | LiteralNil
[@@deriving compare, equal, sexp, hash]

type t =
  | Call of t * t list * Tokens.t
  | Binary of t * Tokens.t * t
  | Logical of t * Tokens.t * t
  | Assign of Tokens.t * t
  | Grouping of t
  | Literal of literal_t
  | Variable of string
  | Unary of Tokens.t * t
[@@deriving compare, equal, sexp, hash]

type stmt_t =
  | Block of stmt_t list
  | Function of Tokens.t * Tokens.t list * stmt_t list
  | IfStmt of t * stmt_t * stmt_t option
  | WhileStmt of t * stmt_t
  | ForStmt of stmt_t option * t * stmt_t
  | PrintStmt of t
  | ReturnStmt of t
  | VarStmt of string * t
  | ExprStmt of t
[@@deriving compare, equal, sexp, hash]

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
  | Call (callee, _, _) ->
      Stdlib.Printf.sprintf "(CALLEE %s)" (to_string callee)
  | Assign (name, expr) ->
      Stdlib.Printf.sprintf "(ASSIGN %s %s)" name.lexeme (to_string expr)
  | Grouping expr -> Stdlib.Printf.sprintf "(group %s)" (to_string expr)
  | Literal value -> literal_to_string value
  | Variable name -> Stdlib.Printf.sprintf "(VAR %s)" name
  | Logical (left_expr, operator, right_expr) ->
      Stdlib.Printf.sprintf "(LOGIC %s %s %s)" (to_string left_expr)
        (token_type_to_string operator.token_type)
        (to_string right_expr)
  | Unary (operator, expr) ->
      Stdlib.Printf.sprintf "(%s %s)"
        (token_type_to_string operator.token_type)
        (to_string expr)

let rec program_to_string (program : program_t) : string =
  List.map
    ~f:(fun stmt ->
      match stmt with
      | Block stmts ->
          Stdlib.Printf.sprintf "(BLOCK\n%s)"
            (String.rstrip (program_to_string stmts))
      | Function (name, args, body) ->
          Stdlib.Printf.sprintf "(FUNCTION %s (%s)\n%s)" (Tokens.to_string name)
            (args
            |> List.map ~f:(fun a -> Tokens.to_string a)
            |> String.concat ~sep:", ")
            (String.rstrip (program_to_string body))
      | IfStmt (cond, when_branch, else_branch) ->
          let else_str =
            match else_branch with
            | Some else_branch ->
                Stdlib.Printf.sprintf "(ELSE %s)"
                  (program_to_string [ else_branch ])
            | None -> ""
          in
          Stdlib.Printf.sprintf "(IF (%s) %s%s)" (to_string cond)
            (program_to_string [ when_branch ])
            else_str
      | WhileStmt (cond, body) ->
          Stdlib.Printf.sprintf "(WHILE %s\n%s)" (to_string cond)
            (program_to_string [ body ])
      | ForStmt (init, cond, body) ->
          Stdlib.Printf.sprintf "(FOR %s %s %s)"
            (match init with
            | Some init -> program_to_string [ init ]
            | None -> "nil")
            (to_string cond)
            (program_to_string [ body ])
      | VarStmt (name, init_expr) ->
          Stdlib.Printf.sprintf "(VAR %s = %s)" name (to_string init_expr)
      | PrintStmt expr -> Stdlib.Printf.sprintf "(PRINT %s)\n" (to_string expr)
      | ReturnStmt expr ->
          Stdlib.Printf.sprintf "(RETURN %s)\n" (to_string expr)
      | ExprStmt expr -> Stdlib.Printf.sprintf "(%s)\n" (to_string expr))
    program
  |> String.concat_lines
