open! Base

type environment_vars = (string, value_t) Hashtbl.t
and environment = { parent : environment option; vars : environment_vars }

and value_t =
  | BooleanValue of bool
  | NumberValue of float
  | StringValue of string
  | NativeFunctionValue of string * int
  | FunctionValue of Tokens.t * Tokens.t list * Ast.stmt_t list
  | NilValue

type runtime_error = { token : Tokens.t; message : string }

exception Runtime_exn of runtime_error
exception Notimplemented
exception Return_exn of value_t

let create_environment (parent : environment option) : environment =
  { parent; vars = Hashtbl.create (module String) }

let define_var ~(name : string) ~(value : value_t) (env : environment) =
  Hashtbl.set env.vars ~key:name ~data:value

let rec get_var_ ~(token : Tokens.t) (env : environment option) : value_t =
  let name = token.lexeme in
  match env with
  | None ->
      raise
        (Runtime_exn
           {
             token;
             message = Stdlib.Printf.sprintf "Undefined variable '%s'" name;
           })
  | Some env -> (
      match Hashtbl.find env.vars token.lexeme with
      | None -> get_var_ ~token env.parent
      | Some v -> v)

let get_var ~(token : Tokens.t) (env : environment) : value_t =
  get_var_ ~token (Some env)

let rec assign_var_ ~(token : Tokens.t) ~(value : value_t)
    (env : environment option) =
  let name = token.lexeme in
  match env with
  | None ->
      raise
        (Runtime_exn
           {
             token;
             message = Stdlib.Printf.sprintf "Undefined variable '%s'" name;
           })
  | Some env -> (
      match Hashtbl.find env.vars token.lexeme with
      | None -> assign_var_ ~token ~value env.parent
      | Some _ ->
          Hashtbl.set env.vars ~key:token.lexeme ~data:value;
          value)

let assign_var ~(token : Tokens.t) ~(value : value_t) (env : environment) =
  assign_var_ ~token ~value (Some env)

let rec global_env env =
  match env.parent with None -> env | Some env -> global_env env

let is_truthy value =
  match value with
  | BooleanValue b -> b
  | NumberValue _ -> true
  | StringValue _ -> true
  | NativeFunctionValue _ -> true
  | FunctionValue (_, _, _) -> true
  | NilValue -> false

let is_equal left right =
  match (left, right) with
  | BooleanValue left, BooleanValue right -> Bool.(left = right)
  | NumberValue left, NumberValue right -> Float.(left = right)
  | StringValue left, StringValue right -> String.(left = right)
  | NilValue, NilValue -> true
  | _, _ -> false

let error_to_string error =
  Stdlib.Printf.sprintf "%s\n[line %d]" error.message error.token.line

let number_to_string value =
  let value = Float.to_string value in
  let value = String.rstrip ~drop:(fun ch -> Char.(ch = '0')) value in
  match Stdlib.String.ends_with ~suffix:"." value with
  | true -> String.sub ~pos:0 ~len:(String.length value - 1) value
  | false -> value

let value_to_string value =
  match value with
  | NativeFunctionValue (name, _) -> Stdlib.Printf.sprintf "<fn %s>" name
  | FunctionValue (name, _, _) -> Stdlib.Printf.sprintf "<fn %s>" name.lexeme
  | BooleanValue b -> if b then "true" else "false"
  | NumberValue n -> number_to_string n
  | StringValue s -> s
  | NilValue -> "nil"

let rec run_program (program : Ast.program_t) (env : environment) : unit =
  match program with
  | [] -> ()
  | stmt :: rest ->
      statement stmt env;
      run_program rest env

and statement (stmt : Ast.stmt_t) (env : environment) : unit =
  match stmt with
  | Ast.Block stmts -> run_program stmts (create_environment (Some env))
  | Ast.Function (name, args, body) -> define_function name args body env
  | Ast.ReturnStmt expr -> return expr env
  | Ast.PrintStmt expr ->
      expression expr env |> value_to_string |> Stdlib.print_endline
  | Ast.VarStmt (name, expr) ->
      define_var ~name ~value:(expression expr env) env
  | Ast.ExprStmt expr -> expression expr env |> value_to_string |> ignore
  | Ast.IfStmt (cond, when_stmt, else_stmt) -> (
      match expression cond env |> is_truthy with
      | true -> statement when_stmt env
      | false -> (
          match else_stmt with
          | Some else_stmt -> statement else_stmt env
          | None -> ()))
  | Ast.WhileStmt (cond, body) -> perform_while cond body env
  | Ast.ForStmt (init, cond, body) -> perform_for init cond body env

and perform_while cond body env =
  match expression cond env |> is_truthy with
  | true ->
      statement body env;
      perform_while cond body env
  | false -> ()

and perform_for init cond body env =
  (match init with Some init -> statement init env | None -> ());
  match expression cond env |> is_truthy with
  | true ->
      statement body env;
      perform_for None cond body env
  | false -> ()

and return expr env =
  let return_value = expression expr env in
  raise (Return_exn return_value)

and define_function name args body env =
  define_var ~name:name.lexeme ~value:(FunctionValue (name, args, body)) env

and expression (ast : Ast.t) (env : environment) : value_t =
  match ast with
  | Ast.Assign (var, expr) ->
      assign_var ~token:var ~value:(expression expr env) env
  | Ast.Unary (token_type, expr) -> unary token_type expr env
  | Ast.Call (callee, args, token) -> call callee args token env
  | Ast.Literal value -> literal value
  | Ast.Grouping ast -> expression ast env
  | Ast.Binary (left_expr, token_type, right_expr) ->
      binary left_expr token_type right_expr env
  | Ast.Logical (left_expr, token_type, right_expr) ->
      logical left_expr token_type right_expr env
  | Ast.Variable name ->
      let token : Tokens.t =
        { token_type = Tokens.IDENTIFIER; lexeme = name; line = 99 }
      in
      get_var ~token env

and logical left_expr token right_expr env =
  let left = expression left_expr env in
  let left_truthy = is_truthy left in
  match token.token_type with
  | Tokens.OR when left_truthy -> left
  | Tokens.AND when not left_truthy -> left
  | _ -> expression right_expr env

and binary left_expr token right_expr env =
  let left = expression left_expr env and right = expression right_expr env in
  match (token.token_type, left, right) with
  | Tokens.EQUAL_EQUAL, left, right -> BooleanValue (is_equal left right)
  | Tokens.BANG_EQUAL, left, right -> BooleanValue (not (is_equal left right))
  | Tokens.GREATER, NumberValue left, NumberValue right ->
      BooleanValue Float.(left > right)
  | Tokens.GREATER_EQUAL, NumberValue left, NumberValue right ->
      BooleanValue Float.(left >= right)
  | Tokens.LESS, NumberValue left, NumberValue right ->
      BooleanValue Float.(left < right)
  | Tokens.LESS_EQUAL, NumberValue left, NumberValue right ->
      BooleanValue Float.(left <= right)
  | Tokens.SLASH, NumberValue left, NumberValue right ->
      NumberValue (left /. right)
  | Tokens.STAR, NumberValue left, NumberValue right ->
      NumberValue (left *. right)
  | Tokens.MINUS, NumberValue left, NumberValue right ->
      NumberValue (left -. right)
  | Tokens.PLUS, NumberValue left, NumberValue right ->
      NumberValue (left +. right)
  | Tokens.PLUS, StringValue left, StringValue right ->
      StringValue (left ^ right)
  | _, _, _ ->
      raise (Runtime_exn { token; message = "Operands must be numbers" })

and call callee args token env : value_t =
  match expression callee env with
  | NativeFunctionValue ("clock", arity) ->
      if arity = List.length args then NumberValue (Unix.time ())
      else
        raise
          (Runtime_exn
             {
               token;
               message =
                 Stdlib.Printf.sprintf "Expected %d arguments but got %d" arity
                   (List.length args);
             })
  | NativeFunctionValue (name, _) ->
      raise
        (Runtime_exn
           {
             token;
             message = Stdlib.Printf.sprintf "Unimplemented function %s" name;
           })
  | FunctionValue (_fun_name, fun_args, fun_body) ->
      call_function fun_args fun_body args token env
  | _ ->
      raise
        (Runtime_exn { token; message = "Can only call functions and classes." })

and call_function fun_args fun_body args token env =
  let global_env = global_env env in
  let func_enc = create_environment (Some global_env) in
  let open List.Or_unequal_lengths in
  match List.zip fun_args args with
  | Ok arg_pairs -> (
      List.map
        ~f:(fun (name_token, expr) ->
          define_var ~name:name_token.lexeme ~value:(expression expr env)
            func_enc)
        arg_pairs
      |> ignore;
      try
        run_program fun_body func_enc;
        NilValue
      with Return_exn value -> value)
  | Unequal_lengths ->
      raise
        (Runtime_exn
           {
             token;
             message =
               Stdlib.Printf.sprintf "Expected %d arguments but got %d"
                 (List.length fun_args) (List.length args);
           })

and unary token expr env : value_t =
  let right_value = expression expr env in
  match (token.token_type, right_value) with
  | Tokens.MINUS, NumberValue n -> NumberValue (n *. -1.0)
  | Tokens.MINUS, _ ->
      raise (Runtime_exn { token; message = "Operands must be numbers" })
  | Tokens.BANG, _ -> BooleanValue (is_truthy right_value |> not)
  | _ -> raise Notimplemented

and literal literal : value_t =
  match literal with
  | Ast.LiteralNumber n -> NumberValue n
  | Ast.LiteralBoolean b -> BooleanValue b
  | Ast.LiteralNil -> NilValue
  | Ast.LiteralString s -> StringValue s

let evaluate (env : environment) (ast : Ast.t) :
    (value_t, runtime_error) Result.t =
  try Ok (expression ast env)
  with Runtime_exn runtime_error -> Error runtime_error

let define_native_funcs env =
  define_var ~name:"clock" ~value:(NativeFunctionValue ("clock", 0)) env;
  env

let run (program : Ast.program_t) : (unit, runtime_error) Result.t =
  try Ok (create_environment None |> define_native_funcs |> run_program program)
  with Runtime_exn runtime_error -> Error runtime_error
