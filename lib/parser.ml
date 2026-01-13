open! Base

exception Invalid_state of string * string

let rec expression (tokens : Tokens.t list) = equality tokens None

and equality (tokens : Tokens.t list) (ast : Ast.t option) :
    Ast.t * Tokens.t list =
  let left_expr, rest = comparison tokens None in
  match rest with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.EQUAL
         || Tokens.equal_token_type operator.token_type Tokens.EQUAL_EQUAL ->
      let right_expr, rest = comparison rest None in
      equality rest
        (Some (Ast.Binary (left_expr, operator.token_type, right_expr)))
  | _ -> ( match ast with Some ast -> (ast, rest) | None -> (left_expr, rest))

and comparison (tokens : Tokens.t list) (ast : Ast.t option) :
    Ast.t * Tokens.t list =
  let left_expr, rest = term tokens None in
  match rest with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.GREATER
         || Tokens.equal_token_type operator.token_type Tokens.GREATER_EQUAL
         || Tokens.equal_token_type operator.token_type Tokens.LESS
         || Tokens.equal_token_type operator.token_type Tokens.LESS_EQUAL ->
      let right_expr, rest = term rest None in
      comparison rest
        (Some (Ast.Binary (left_expr, operator.token_type, right_expr)))
  | _ -> ( match ast with Some ast -> (ast, rest) | None -> (left_expr, rest))

and term (tokens : Tokens.t list) (ast : Ast.t option) : Ast.t * Tokens.t list =
  let left_expr, rest = factor tokens in
  match rest with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.MINUS
         || Tokens.equal_token_type operator.token_type Tokens.PLUS ->
      let right_expr, rest = factor rest in
      term rest (Some (Ast.Binary (left_expr, operator.token_type, right_expr)))
  | _ -> ( match ast with Some ast -> (ast, rest) | None -> (left_expr, rest))

and factor (tokens : Tokens.t list) : Ast.t * Tokens.t list =
  let left_expr, rest = unary tokens in
  match factor_right rest left_expr with
  | Some result -> result
  | None -> (left_expr, rest)

and factor_right (tokens : Tokens.t list) (expr : Ast.t) :
    (Ast.t * Tokens.t list) option =
  match tokens with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.STAR
         || Tokens.equal_token_type operator.token_type Tokens.SLASH -> (
      let right_expr, rest = unary rest in
      let ast = Ast.Binary (expr, operator.token_type, right_expr) in
      match factor_right rest ast with
      | Some (ast, rest) -> Some (ast, rest)
      | None -> Some (ast, rest))
  | _ -> None

and unary (tokens : Tokens.t list) : Ast.t * Tokens.t list =
  match tokens with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.BANG
         || Tokens.equal_token_type operator.token_type Tokens.MINUS ->
      let right_ast, rest = unary rest in
      (Ast.Unary (operator.token_type, right_ast), rest)
  | _ -> primary tokens

and primary (tokens : Tokens.t list) : Ast.t * Tokens.t list =
  match tokens with
  | { token_type = Tokens.TRUE; _ } :: rest ->
      (Ast.Literal (Ast.LiteralBoolean true), rest)
  | { token_type = Tokens.FALSE; _ } :: rest ->
      (Ast.Literal (Ast.LiteralBoolean false), rest)
  | { token_type = Tokens.NIL; _ } :: rest -> (Ast.Literal Ast.LiteralNil, rest)
  | { token_type = Tokens.NUMBER value; _ } :: rest ->
      (Ast.Literal (Ast.LiteralNumber value), rest)
  | { token_type = Tokens.STRING value; _ } :: rest ->
      (Ast.Literal (Ast.LiteralString value), rest)
  | { token_type = Tokens.LEFT_PAREN; _ } :: rest -> (
      let ast, (rest : Tokens.t list) = expression rest in
      match rest with
      | { token_type = Tokens.RIGHT_PAREN; _ } :: rest ->
          (Ast.Grouping ast, rest)
      | _ ->
          raise
            (Invalid_state
               ( "missing right paren",
                 List.hd_exn tokens |> Tokens.sexp_of_t |> Sexp.to_string_hum ))
      )
  | { token_type = Tokens.EOF; _ } :: rest -> (Ast.Literal Ast.LiteralNil, rest)
  | _ ->
      raise
        (Invalid_state
           ( "Unknown primary token",
             List.hd_exn tokens |> Tokens.sexp_of_t |> Sexp.to_string_hum ))

let parse (tokens : Tokens.t list) =
  let ast, _ = expression tokens in
  ast
