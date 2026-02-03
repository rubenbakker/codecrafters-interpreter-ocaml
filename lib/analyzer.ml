open! Base

type t = Ast.t * int [@@deriving compare, equal, sexp]
type tl = t list [@@deriving compare, equal, sexp]

type var_t = Declared of string | Defined of string
[@@deriving compare, equal, sexp]

type vars_t = (string, var_t) Hashtbl.t
type scope_t = { vars : vars_t; parent : scope_t option }

let create_scope ?(parent : scope_t option = None) () =
  { vars = Hashtbl.create (module String); parent }

let declare_var (scope : scope_t) (name : string) =
  Hashtbl.set scope.vars ~key:name ~data:(Declared name)

let define_var (scope : scope_t) (name : string) =
  Hashtbl.set scope.vars ~key:name ~data:(Defined name)

let rec resolve_local_var (scope : scope_t) (name : string) (distance : int) :
    int option =
  match Hashtbl.find scope.vars name with
  | Some _ -> Some distance
  | None -> (
      match scope.parent with
      | None -> None
      | Some parent -> resolve_local_var parent name (distance + 1))

let resolve_var (scope : scope_t) (name : string) : int option =
  resolve_local_var scope name 0

let rec statements (stmts : Ast.program_t) (scope : scope_t) (acc : tl) =
  match stmts with
  | [] -> List.rev acc
  | stmt :: rest -> statement stmt scope acc |> statements rest scope

and statement (stmt : Ast.stmt_t) (scope : scope_t) (acc : tl) : tl =
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
      define_var scope name;
      expression expr scope acc
  | Ast.ExprStmt expr -> expression expr scope acc

and expression (expr : Ast.t) (scope : scope_t) (acc : tl) : tl =
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
      distance := resolve_var scope name_token.lexeme;
      expression expr scope acc
  | Ast.Grouping expr -> expression expr scope acc
  | Ast.Literal _ -> acc
  | Ast.Variable (name, _, distance) ->
      distance := resolve_var scope name;
      acc
  | Ast.Unary (_, expr) -> expression expr scope acc

let analyze_program_tl (program : Ast.program_t) : (t list, string) Result.t =
  Ok (statements program (create_scope ()) [])

let analyze_program (program : Ast.program_t) : (unit, string) Result.t =
  statements program (create_scope ()) [] |> ignore;
  Ok ()
