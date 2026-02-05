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
  | { token_type = Tokens.CLASS; _ }
    :: ({ token_type = Tokens.IDENTIFIER; _ } as name_token)
    :: rest ->
      class_stmt name_token rest
  | { token_type = Tokens.FUN; _ }
    :: ({ token_type = Tokens.IDENTIFIER; _ } as name_token)
    :: rest ->
      function_stmt name_token rest
  | ({ token_type = Tokens.FUN; _ } as token) :: _ ->
      raise (Parse_exn { token; message = "Expect function name." })
  | { token_type = Tokens.VAR; _ }
    :: { token_type = Tokens.IDENTIFIER; lexeme = name; _ }
    :: rest -> (
      match rest with
      | ({ token_type = Tokens.EQUAL; _ } as token) :: rest ->
          let rest, expr = expression rest in
          let rest =
            consume_token rest ~tt:Tokens.SEMICOLON
              ~error:"Expect ';' after expression."
          in
          (rest, Ast.VarStmt (name, expr, token))
      | _ ->
          let rest =
            consume_token rest ~tt:Tokens.SEMICOLON
              ~error:"Expect ';' after expression."
          in
          ( rest,
            Ast.VarStmt (name, Ast.Literal Ast.LiteralNil, List.hd_exn rest) ))
  | rest -> statement rest

and class_stmt (name_token : Tokens.t) (tokens : Tokens.t list) :
    Tokens.t list * Ast.stmt_t =
  match tokens with
  | { token_type = Tokens.LEFT_BRACE; _ }
    :: { token_type = Tokens.RIGHT_BRACE; _ }
    :: rest ->
      (rest, ClassStmt (name_token, []))
  | { token_type = Tokens.LEFT_BRACE; _ } :: rest ->
      let rest, methods = class_body rest [] in
      (rest, ClassStmt (name_token, methods))
  | _ ->
      raise
        (Parse_exn
           { token = name_token; message = "Expect '{' after class name" })

and class_body (tokens : Tokens.t list) (acc : Ast.stmt_t list) :
    Tokens.t list * Ast.stmt_t list =
  match tokens with
  | { token_type = Tokens.RIGHT_BRACE; _ } :: rest -> (rest, List.rev acc)
  | ({ token_type = Tokens.EOF; _ } as token) :: _ ->
      raise (Parse_exn { token; message = "Expect '}' after class." })
  | ({ token_type = Tokens.IDENTIFIER; _ } as name_token) :: rest ->
      let rest, func = function_stmt name_token rest in
      class_body rest (func :: acc)
  | _ ->
      raise
        (Parse_exn
           {
             token = List.hd_exn tokens;
             message = "Unexpected token in class.";
           })

and function_stmt (name_token : Tokens.t) (tokens : Tokens.t list) :
    Tokens.t list * Ast.stmt_t =
  match tokens with
  | { token_type = Tokens.LEFT_PAREN; _ } :: rest -> (
      let rest, args = function_args rest [] in
      let rest =
        consume_token rest ~tt:Tokens.LEFT_BRACE
          ~error:"Expect '{' after function argument list"
      in
      let rest, body = block rest [] in
      match body with
      | Block stmts -> (rest, Ast.Function (name_token, args, stmts))
      | _ ->
          raise
            (Parse_exn
               { token = name_token; message = "Couldn't parse function body." })
      )
  | _ ->
      raise
        (Parse_exn
           { token = name_token; message = "Expect '(' after function name." })

and function_args (tokens : Tokens.t list) (acc : Tokens.t list) :
    Tokens.t list * Tokens.t list =
  match tokens with
  | { token_type = Tokens.RIGHT_PAREN; _ } :: rest -> (rest, List.rev acc)
  | ({ token_type = Tokens.IDENTIFIER; _ } as token)
    :: { token_type = Tokens.RIGHT_PAREN; _ }
    :: rest ->
      (rest, List.rev (token :: acc))
  | ({ token_type = Tokens.IDENTIFIER; _ } as token)
    :: { token_type = Tokens.COMMA; _ }
    :: rest ->
      function_args rest (token :: acc)
  | _ ->
      raise
        (Parse_exn
           {
             token = List.hd_exn tokens;
             message = "Expected ')' after function arg list";
           })

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
  | ({ token_type = Tokens.RETURN; _ } as token)
    :: { token_type = Tokens.SEMICOLON; _ }
    :: rest ->
      (rest, Ast.ReturnStmt (Ast.Literal Ast.LiteralNil, token))
  | ({ token_type = Tokens.RETURN; _ } as token) :: rest ->
      let rest, expr = expression rest in
      let rest =
        consume_token rest ~tt:Tokens.SEMICOLON
          ~error:"Expect ';' after expression."
      in
      (rest, Ast.ReturnStmt (expr, token))
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
  ( rest,
    match init with
    | None -> Ast.WhileStmt (cond, body)
    | Some init -> Ast.Block [ init; Ast.WhileStmt (cond, body) ] )

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
      | Ast.Variable (name, _, _) ->
          (rest, Ast.Assign ({ token with lexeme = name }, value, ref None))
      | Ast.GetProperty (instance, name_token) ->
          (rest, Ast.SetProperty (instance, name_token, value))
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
  | _ -> call tokens

and call (tokens : Tokens.t list) : Tokens.t list * Ast.t =
  let rest, expr = primary tokens in
  call_only rest expr

and call_only (tokens : Tokens.t list) (callee : Ast.t) : Tokens.t list * Ast.t
    =
  match tokens with
  | { token_type = Tokens.DOT; _ }
    :: ({ token_type = Tokens.IDENTIFIER; _ } as token_name)
    :: rest ->
      call_only rest (Ast.GetProperty (callee, token_name))
  | ({ token_type = Tokens.LEFT_PAREN; _ } as token) :: rest ->
      let rest, args = call_args rest [] in
      call_only rest (Ast.Call (callee, args, token))
  | _ -> (tokens, callee)

and call_args (tokens : Tokens.t list) (acc : Ast.t list) :
    Tokens.t list * Ast.t list =
  match tokens with
  | { token_type = Tokens.RIGHT_PAREN; _ } :: rest -> (rest, [])
  | _ -> (
      let rest, expr = expression tokens in
      match rest with
      | { token_type = Tokens.COMMA; _ } :: rest -> call_args rest (expr :: acc)
      | { token_type = Tokens.RIGHT_PAREN; _ } :: rest ->
          (rest, List.rev (expr :: acc))
      | _ ->
          raise
            (Parse_exn
               {
                 token = List.hd_exn tokens;
                 message = "Expected ')' after function arg list";
               }))

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
  | ({ token_type = Tokens.IDENTIFIER; lexeme = name; _ } as token) :: rest ->
      (rest, Ast.Variable (name, token, ref None))
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
