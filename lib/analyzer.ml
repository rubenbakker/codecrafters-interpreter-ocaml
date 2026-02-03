open! Base

type error_t = { token : Tokens.t; message : string } [@@deriving sexp]
type error_list_t = error_t list [@@deriving sexp]
type t = Ast.t * int [@@deriving compare, equal, sexp]

type var_t = Declared of string | Defined of string
[@@deriving compare, equal, sexp]

type vars_t = (string, var_t) Hashtbl.t
type scope_t = { vars : vars_t; parent : scope_t option }

let create_scope ?(parent : scope_t option = None) () =
  { vars = Hashtbl.create (module String); parent }

let declare_var (scope : scope_t) (name : string) =
  if Option.is_some scope.parent then
    Hashtbl.set scope.vars ~key:name ~data:(Declared name)
  else ()

let define_var (scope : scope_t) (name : string) =
  if Option.is_some scope.parent then
    Hashtbl.set scope.vars ~key:name ~data:(Defined name)
  else ()

let error_to_string (error : error_t) =
  Stdlib.Printf.sprintf "[line %d] %s" error.token.line error.message

let rec resolve_local_var (scope : scope_t) (name : string) (token : Tokens.t)
    (distance : int) : (int option, error_t) Result.t =
  match Hashtbl.find scope.vars name with
  | None -> (
      match scope.parent with
      | None -> Ok None
      | Some parent -> resolve_local_var parent name token (distance + 1))
  | Some var -> (
      match var with
      | Declared _ ->
          Error
            {
              token;
              message =
                Stdlib.Printf.sprintf
                  "Error at '%s': Can't read local variable in its own \
                   initializer."
                  name;
            }
      | Defined _ -> Ok (Some distance))

let resolve_var (scope : scope_t) (name : string) (token : Tokens.t) :
    (int option, error_t) Result.t =
  resolve_local_var scope name token 0

let rec statements (stmts : Ast.program_t) (scope : scope_t)
    (acc : error_list_t) : error_list_t =
  match stmts with
  | [] -> List.rev acc
  | stmt :: rest -> statement stmt scope acc |> statements rest scope

and statement (stmt : Ast.stmt_t) (scope : scope_t) (acc : error_list_t) :
    error_list_t =
  match stmt with
  | Ast.Block stmts ->
      statements stmts (create_scope ~parent:(Some scope) ()) acc
  | Ast.Function (fun_name, args, body) ->
      define_var scope fun_name.lexeme;
      let scope = create_scope ~parent:(Some scope) () in
      List.map args ~f:(fun arg_token -> define_var scope arg_token.lexeme)
      |> ignore;
      statements body scope acc
  | Ast.IfStmt (cond, branch, else_branch) -> (
      let acc = expression cond scope acc in
      let acc = statement branch scope acc in
      match else_branch with
      | Some else_branch -> statement else_branch scope acc
      | None -> acc)
  | Ast.WhileStmt (cond, body) ->
      expression cond scope acc |> statement body scope
  | Ast.PrintStmt stmt -> expression stmt scope acc
  | Ast.ReturnStmt expr -> expression expr scope acc
  | Ast.VarStmt (name, expr) ->
      declare_var scope name;
      let result = expression expr scope acc in
      define_var scope name;
      result
  | Ast.ExprStmt expr -> expression expr scope acc

and expression (expr : Ast.t) (scope : scope_t) (acc : error_list_t) :
    error_list_t =
  match expr with
  | Ast.Call (callee, args, _) ->
      let arg_defs =
        List.map args ~f:(fun expr -> expression expr scope []) |> List.concat
      in
      expression callee scope (List.concat [ acc; arg_defs ])
  | Ast.Binary (expr1, _, expr2) ->
      expression expr1 scope acc |> expression expr2 scope
  | Ast.Logical (expr1, _, expr2) ->
      expression expr1 scope acc |> expression expr2 scope
  | Ast.Assign (name_token, expr, distance) ->
      let acc =
        match resolve_var scope name_token.lexeme name_token with
        | Ok d ->
            distance := d;
            acc
        | Error err -> err :: acc
      in
      expression expr scope acc
  | Ast.Grouping expr -> expression expr scope acc
  | Ast.Literal _ -> acc
  | Ast.Variable (name, token, distance) -> (
      match resolve_var scope name token with
      | Ok d ->
          distance := d;
          acc
      | Error err -> err :: acc)
  | Ast.Unary (_, expr) -> expression expr scope acc

(* let analyze_program_tl (program : Ast.program_t) : (t list, string) Result.t = *)
(*   Ok (statements program (create_scope ()) []) *)
(**)
let analyze_program (program : Ast.program_t) :
    (Ast.program_t, error_list_t) Result.t =
  let errors = statements program (create_scope ()) [] in
  match errors with [] -> Ok program | _ -> Error errors
