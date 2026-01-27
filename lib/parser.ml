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
    let _, prg = declarations tokens [] in
    Ok prg
  with Parse_exn error -> Error error

and declarations (tokens : Tokens.t list) (acc : Ast.program_t) :
    Tokens.t list * Ast.program_t =
  match tokens with
  | [ { token_type = Tokens.EOF; _ } ] -> ([], List.rev acc)
  | _ ->
      let rest, stmt = declaration tokens in
      declarations rest (stmt :: acc)

and declaration (tokens : Tokens.t list) : Tokens.t list * Ast.stmt_t =
  match tokens with
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
          (rest, Ast.VarStmt (name, expr))
      | _ ->
          let rest =
            consume_token rest ~tt:Tokens.SEMICOLON
              ~error:"Expect ';' after expression."
          in
          (rest, Ast.VarStmt (name, Ast.Literal Ast.LiteralNil)))
  | rest -> statement rest

and statement (tokens : Tokens.t list) : Tokens.t list * Ast.stmt_t =
  match tokens with
  | { token_type = Tokens.LEFT_BRACE; _ } :: rest -> block rest []
  | { token_type = Tokens.IF; _ }
    :: { token_type = Tokens.LEFT_PAREN; _ }
    :: rest ->
      if_stmt rest
  | ({ token_type = Tokens.IF; _ } as token) :: _ ->
      raise (Parse_exn { token; message = "Expect '(' after 'if'." })
  | { token_type = Tokens.WHILE; _ }
    :: { token_type = Tokens.LEFT_PAREN; _ }
    :: rest ->
      while_stmt rest
  | ({ token_type = Tokens.WHILE; _ } as token) :: _ ->
      raise (Parse_exn { token; message = "Expect '(' after 'while'." })
  | { token_type = Tokens.FOR; _ }
    :: { token_type = Tokens.LEFT_PAREN; _ }
    :: rest ->
      for_stmt rest
  | { token_type = Tokens.PRINT; _ } :: rest ->
      let rest, expr = expression rest in
      let rest =
        consume_token rest ~tt:Tokens.SEMICOLON
          ~error:"Expect ';' after expression."
      in
      (rest, Ast.PrintStmt expr)
  | rest ->
      let rest, expr = expression rest in
      let rest =
        consume_token rest ~tt:Tokens.SEMICOLON
          ~error:"Expect ';' after expression."
      in
      (rest, Ast.ExprStmt expr)

and if_stmt (tokens : Tokens.t list) : Tokens.t list * Ast.stmt_t =
  let rest, cond = expression tokens in
  let rest =
    consume_token rest ~tt:Tokens.RIGHT_PAREN
      ~error:"Expect ')' after if condition."
  in
  let rest, when_branch = statement rest in
  let rest, else_branch =
    match rest with
    | { token_type = Tokens.ELSE; _ } :: rest ->
        let rest, stmt = statement rest in
        (rest, Some stmt)
    | _ -> (rest, None)
  in
  (rest, Ast.IfStmt (cond, when_branch, else_branch))

and while_stmt (tokens : Tokens.t list) : Tokens.t list * Ast.stmt_t =
  let rest, cond = expression tokens in
  let rest =
    consume_token rest ~tt:Tokens.RIGHT_PAREN
      ~error:"Expect ')' after while condition."
  in
  let rest, body = statement rest in
  (rest, Ast.WhileStmt (cond, body))

and for_stmt (tokens : Tokens.t list) : Tokens.t list * Ast.stmt_t =
  let rest, init =
    match tokens with
    | { token_type = Tokens.SEMICOLON; _ } :: rest -> (rest, None)
    | _ ->
        let rest, ast = declaration tokens in
        (rest, Some ast)
  in
  let rest, cond =
    match rest with
    | { token_type = Tokens.SEMICOLON; _ } :: rest ->
        (rest, Ast.Literal (Ast.LiteralBoolean true))
    | _ ->
        let rest, ast = expression rest in
        let rest =
          consume_token rest ~tt:Tokens.SEMICOLON
            ~error:"Expect ')' after for condition."
        in
        (rest, ast)
  in
  let rest, increment =
    match rest with
    | { token_type = Tokens.RIGHT_PAREN; _ } :: rest -> (rest, None)
    | _ ->
        let rest, ast = expression rest in
        let rest =
          consume_token rest ~tt:Tokens.RIGHT_PAREN
            ~error:"Expect ')' after for increment."
        in
        (rest, Some ast)
  in
  let rest, body = statement rest in
  let body =
    match increment with
    | Some increment -> Ast.Block [ body; Ast.ExprStmt increment ]
    | None -> body
  in
  (rest, Ast.ForStmt (init, cond, body))

and block (tokens : Tokens.t list) (acc : Ast.program_t) :
    Tokens.t list * Ast.stmt_t =
  match tokens with
  | { token_type = Tokens.RIGHT_BRACE; _ } :: rest ->
      (rest, Ast.Block (List.rev acc))
  | [ ({ token_type = Tokens.EOF; _ } as token) ] ->
      raise (Parse_exn { token; message = "Expect '}' after expression." })
  | _ ->
      let rest, stmt = declaration tokens in
      block rest (stmt :: acc)

and expression (tokens : Tokens.t list) = assignment tokens

and assignment (tokens : Tokens.t list) =
  let rest, expr = logic_or tokens in
  match rest with
  | ({ token_type = Tokens.EQUAL; _ } as token) :: rest -> (
      let rest, value = assignment rest in
      match expr with
      | Ast.Variable name ->
          (rest, Ast.Assign ({ token with lexeme = name }, value))
      | _ -> raise (Parse_exn { token; message = "Invalid assignment target." })
      )
  | _ -> (rest, expr)

and logic_or (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, left_expr = logic_and tokens in
  match rest with
  | ({ token_type = Tokens.OR; _ } as operator) :: rest ->
      let rest, right_expr = logic_or rest in
      (rest, Ast.Logical (left_expr, operator, right_expr))
  | _ -> (rest, left_expr)

and logic_and (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, left_expr = equality tokens in
  match rest with
  | ({ token_type = Tokens.AND; _ } as operator) :: rest ->
      let rest, right_expr = logic_and rest in
      (rest, Ast.Logical (left_expr, operator, right_expr))
  | _ -> (rest, left_expr)

and equality (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, expr = comparison tokens in
  match equality_right rest expr with
  | Some result -> result
  | None -> (rest, expr)

and equality_right (tokens : Tokens.t list) (expr : Ast.t) :
    (Tokens.t list * Ast.t) option =
  match tokens with
  | operator :: rest
    when Tokens.matches_any operator [ Tokens.BANG_EQUAL; Tokens.EQUAL_EQUAL ]
    -> (
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
