open! Base

type parse_error = { token : Tokens.t; message : string }

exception Parse_exn of parse_error
exception Todo_exn of string option

let todo msg = raise (Todo_exn msg)

let consume_token (tokens : Tokens.t list) ~(tt : Tokens.token_type)
    ~error:(message : string) : Tokens.t list =
  match tokens with
  | { token_type; _ } :: rest when Tokens.equal_token_type tt token_type -> rest
  | _ -> raise (Parse_exn { token = List.hd_exn tokens; message })

let rec program (tokens : Tokens.t list) : (Ast.program_t, parse_error) Result.t
    =
  try
    let prg = declaration tokens [] in
    Ok prg
  with Parse_exn error -> Error error

and declaration (tokens : Tokens.t list) (acc : Ast.program_t) : Ast.program_t =
  match tokens with
  | [ { token_type = Tokens.EOF; _ } ] -> List.rev acc
  | { token_type = Tokens.VAR; _ }
    :: { token_type = Tokens.IDENTIFIER; lexeme = name; _ }
    :: rest -> (
      match rest with
      | { token_type = Tokens.EQUAL; _ } :: rest ->
          let rest, expr = expression rest in
          let rest =
            consume_token rest ~tt:Tokens.SEMICOLON
              ~error:"Expect ';' after expression."
          in
          declaration rest (Ast.VarStmt (name, expr) :: acc)
      | _ ->
          let rest =
            consume_token rest ~tt:Tokens.SEMICOLON
              ~error:"Expect ';' after expression."
          in
          statement rest (Ast.VarStmt (name, Ast.Literal Ast.LiteralNil) :: acc)
      )
  | rest -> statement rest acc

and statement (tokens : Tokens.t list) (acc : Ast.program_t) : Ast.program_t =
  match tokens with
  | [ { token_type = Tokens.EOF; _ } ] -> List.rev acc
  | { token_type = Tokens.PRINT; _ } :: rest ->
      let rest, expr = expression rest in
      let rest =
        consume_token rest ~tt:Tokens.SEMICOLON
          ~error:"Expect ';' after expression."
      in
      statement rest (Ast.PrintStmt expr :: acc)
  | rest ->
      let rest, expr = expression rest in
      let rest =
        consume_token rest ~tt:Tokens.SEMICOLON
          ~error:"Expect ';' after expression."
      in
      statement rest (Ast.ExprStmt expr :: acc)

and expression (tokens : Tokens.t list) = equality tokens

and equality (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, expr = comparison tokens in
  match equality_right rest expr with
  | Some result -> result
  | None -> (rest, expr)

and equality_right (tokens : Tokens.t list) (expr : Ast.t) :
    (Tokens.t list * Ast.t) option =
  match tokens with
  | operator :: rest
    when Tokens.matches_any operator
           [ Tokens.EQUAL; Tokens.BANG_EQUAL; Tokens.EQUAL_EQUAL ] -> (
      let rest, right_expr = comparison rest in
      let ast = Ast.Binary (expr, operator, right_expr) in
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
    when Tokens.matches_any operator
           [
             Tokens.GREATER;
             Tokens.GREATER_EQUAL;
             Tokens.LESS;
             Tokens.LESS_EQUAL;
           ] -> (
      let rest, right_expr = term rest in
      let ast = Ast.Binary (expr, operator, right_expr) in
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
    when Tokens.matches_any operator [ Tokens.MINUS; Tokens.PLUS ] -> (
      let rest, right_expr = factor rest in
      let ast = Ast.Binary (expr, operator, right_expr) in
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
    when Tokens.matches_any operator [ Tokens.STAR; Tokens.SLASH ] -> (
      let rest, right_expr = unary rest in
      let ast = Ast.Binary (expr, operator, right_expr) in
      match factor_right rest ast with
      | Some result -> Some result
      | None -> Some (rest, ast))
  | _ -> None

and unary (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  match tokens with
  | operator :: rest
    when Tokens.matches_any operator [ Tokens.BANG; Tokens.MINUS ] ->
      let rest, right_expr = unary rest in
      (rest, Ast.Unary (operator, right_expr))
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
  | { token_type = Tokens.IDENTIFIER; lexeme = name; _ } :: rest ->
      (rest, Ast.Variable name)
  | { token_type = Tokens.LEFT_PAREN; _ } :: rest ->
      let (rest : Tokens.t list), ast = expression rest in
      let rest =
        consume_token rest ~tt:Tokens.RIGHT_PAREN
          ~error:"Expect ')' after expression."
      in
      (rest, Ast.Grouping ast)
  | { token_type = Tokens.EOF; _ } :: rest -> (rest, Ast.Literal Ast.LiteralNil)
  | _ ->
      raise
        (Parse_exn
           { token = List.hd_exn tokens; message = "Expect expression." })

let parse_program (tokens : Tokens.t list) :
    (Ast.program_t, parse_error) Result.t =
  program tokens

let parse_expression (tokens : Tokens.t list) : (Ast.t, parse_error) Result.t =
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
