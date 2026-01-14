open! Base

type parse_error = { token : Tokens.t; message : string }

exception Parse_exn of parse_error

let rec expression (tokens : Tokens.t list) = equality tokens

and equality (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, expr = comparison tokens in
  match equality_right rest expr with
  | Some result -> result
  | None -> (rest, expr)

and equality_right (tokens : Tokens.t list) (expr : Ast.t) :
    (Tokens.t list * Ast.t) option =
  match tokens with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.EQUAL
         || Tokens.equal_token_type operator.token_type Tokens.BANG_EQUAL
         || Tokens.equal_token_type operator.token_type Tokens.EQUAL_EQUAL -> (
      let rest, right_expr = comparison rest in
      let ast = Ast.Binary (expr, operator.token_type, right_expr) in
      match equality_right rest ast with
      | Some result -> Some result
      | None -> Some (rest, ast))
  | _ -> None

and comparison (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, expr = term tokens in
  match comparison_right rest expr with
  | Some result -> result
  | None -> (rest, expr)

and comparison_right (tokens : Tokens.t list) (expr : Ast.t) :
    (Tokens.t list * Ast.t) option =
  match tokens with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.GREATER
         || Tokens.equal_token_type operator.token_type Tokens.GREATER_EQUAL
         || Tokens.equal_token_type operator.token_type Tokens.LESS
         || Tokens.equal_token_type operator.token_type Tokens.LESS_EQUAL -> (
      let rest, right_expr = term rest in
      let ast = Ast.Binary (expr, operator.token_type, right_expr) in
      match comparison_right rest ast with
      | Some result -> Some result
      | None -> Some (rest, ast))
  | _ -> None

and term (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, expr = factor tokens in
  match term_right rest expr with Some result -> result | None -> (rest, expr)

and term_right (tokens : Tokens.t list) (expr : Ast.t) :
    (Tokens.t list * Ast.t) option =
  match tokens with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.MINUS
         || Tokens.equal_token_type operator.token_type Tokens.PLUS -> (
      let rest, right_expr = factor rest in
      let ast = Ast.Binary (expr, operator.token_type, right_expr) in
      match term_right rest ast with
      | Some result -> Some result
      | None -> Some (rest, ast))
  | _ -> None

and factor (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, expr = unary tokens in
  match factor_right rest expr with
  | Some result -> result
  | None -> (rest, expr)

and factor_right (tokens : Tokens.t list) (expr : Ast.t) :
    (Tokens.t list * Ast.t) option =
  match tokens with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.STAR
         || Tokens.equal_token_type operator.token_type Tokens.SLASH -> (
      let rest, right_expr = unary rest in
      let ast = Ast.Binary (expr, operator.token_type, right_expr) in
      match factor_right rest ast with
      | Some result -> Some result
      | None -> Some (rest, ast))
  | _ -> None

and unary (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  match tokens with
  | operator :: rest
    when Tokens.equal_token_type operator.token_type Tokens.BANG
         || Tokens.equal_token_type operator.token_type Tokens.MINUS ->
      let rest, right_expr = unary rest in
      (rest, Ast.Unary (operator.token_type, right_expr))
  | _ -> primary tokens

and primary (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  match tokens with
  | { token_type = Tokens.TRUE; _ } :: rest ->
      (rest, Ast.Literal (Ast.LiteralBoolean true))
  | { token_type = Tokens.FALSE; _ } :: rest ->
      (rest, Ast.Literal (Ast.LiteralBoolean false))
  | { token_type = Tokens.NIL; _ } :: rest -> (rest, Ast.Literal Ast.LiteralNil)
  | { token_type = Tokens.NUMBER value; _ } :: rest ->
      (rest, Ast.Literal (Ast.LiteralNumber value))
  | { token_type = Tokens.STRING value; _ } :: rest ->
      (rest, Ast.Literal (Ast.LiteralString value))
  | { token_type = Tokens.LEFT_PAREN; _ } :: rest -> (
      let (rest : Tokens.t list), ast = expression rest in
      match rest with
      | { token_type = Tokens.RIGHT_PAREN; _ } :: rest ->
          (rest, Ast.Grouping ast)
      | _ ->
          raise
            (Parse_exn
               {
                 token = List.hd_exn rest;
                 message = "Expect ')' after expression.";
               }))
  | { token_type = Tokens.EOF; _ } :: rest -> (rest, Ast.Literal Ast.LiteralNil)
  | _ ->
      raise
        (Parse_exn
           { token = List.hd_exn tokens; message = "Expect expression." })

let parse (tokens : Tokens.t list) : (Ast.t, parse_error) Result.t =
  try
    let _, ast = expression tokens in
    Ok ast
  with Parse_exn error -> Error error

let format_error ({ token; message } : parse_error) : string =
  let pos =
    match token.token_type with
    | Tokens.EOF -> "at end"
    | _ -> Stdlib.Printf.sprintf "at '%s'" token.lexeme
  in
  Stdlib.Printf.sprintf "[Line %d] Error %s: %s" token.line pos message
