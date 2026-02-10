open! Base

type error_t = { token : Tokens.t; message : string } [@@deriving sexp]
type error_list_t = error_t list [@@deriving sexp]
type t = Ast.t * int [@@deriving compare, equal, sexp]

type var_t = Declared of string | Defined of string
[@@deriving compare, equal, sexp]

type vars_t = (string, var_t) Hashtbl.t

type scope_type_t = Inherit | Function | Initializer
[@@deriving equal, compare]

type class_type_t = InheritClassType | InsideClass [@@deriving equal, compare]

type scope_t = {
  vars : vars_t;
  parent : scope_t option;
  scope_type : scope_type_t;
  class_type : class_type_t;
}

let create_scope ?(parent : scope_t option = None)
    ?(scope_type : scope_type_t = Inherit)
    ?(class_type : class_type_t = InheritClassType) () =
  let scope_type =
    match (scope_type, parent) with
    | Inherit, Some parent -> parent.scope_type
    | _, _ -> scope_type
  in
  let class_type =
    match (class_type, parent) with
    | InheritClassType, Some parent -> parent.class_type
    | _, _ -> class_type
  in
  { vars = Hashtbl.create (module String); parent; scope_type; class_type }

let declare_var (scope : scope_t) (name : string) (token : Tokens.t) =
  if Option.is_some scope.parent then
    match Hashtbl.find scope.vars name with
    | None -> Ok (Hashtbl.set scope.vars ~key:name ~data:(Declared name))
    | Some _ ->
        Error
          {
            token;
            message =
              Stdlib.Printf.sprintf
                "Error at '%s': Already a variable with this name in this \
                 scope."
                name;
          }
  else Ok ()

let define_var (scope : scope_t) (name : string) : unit =
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
      let acc =
        match declare_var scope fun_name.lexeme fun_name with
        | Ok _ -> acc
        | Error error -> error :: acc
      in
      define_var scope fun_name.lexeme;
      let function_type =
        if
          equal_class_type_t scope.class_type InsideClass
          && String.(fun_name.lexeme = "init")
        then Initializer
        else Function
      in
      let scope =
        create_scope ~scope_type:function_type ~parent:(Some scope) ()
      in
      let arg_errors =
        List.map args ~f:(fun arg_token ->
            match declare_var scope arg_token.lexeme arg_token with
            | Ok () ->
                define_var scope arg_token.lexeme;
                None
            | Error error -> Some error)
        |> List.filter_opt |> List.rev
      in
      statements body scope (List.concat [ arg_errors; acc ])
  | Ast.ClassStmt (name_token, superclass, methods) ->
      let rec resolve_methods methods scope acc =
        match methods with
        | [] -> acc
        | m :: rest ->
            let errors = statement m scope acc in
            resolve_methods rest scope (List.concat [ errors; acc ])
      in
      let acc =
        match declare_var scope name_token.lexeme name_token with
        | Ok _ ->
            let class_scope, acc =
              match superclass with
              | Some superclass -> (
                  if String.(name_token.lexeme = superclass.token.lexeme) then
                    ( scope,
                      {
                        token = name_token;
                        message =
                          Stdlib.Printf.sprintf
                            "Error at '%s'. A class can't inherit from itself."
                            name_token.lexeme;
                      }
                      :: acc )
                  else
                    match
                      resolve_var scope superclass.token.lexeme superclass.token
                    with
                    | Ok distance ->
                        superclass.distance := distance;
                        let scope =
                          create_scope ~parent:(Some scope)
                            ~class_type:InsideClass ()
                        in
                        define_var scope "super";
                        (scope, acc)
                    | Error err -> (scope, err :: acc))
              | None -> (scope, acc)
            in
            define_var scope name_token.lexeme;
            let class_scope =
              create_scope ~parent:(Some class_scope) ~class_type:InsideClass ()
            in
            define_var class_scope "this";
            resolve_methods methods class_scope acc
        | Error error -> error :: acc
      in
      acc
  | Ast.IfStmt (cond, branch, else_branch) -> (
      let acc = expression cond scope acc in
      let acc = statement branch scope acc in
      match else_branch with
      | Some else_branch -> statement else_branch scope acc
      | None -> acc)
  | Ast.WhileStmt (cond, body) ->
      expression cond scope acc |> statement body scope
  | Ast.PrintStmt stmt -> expression stmt scope acc
  | Ast.ReturnStmt (expr, token) -> (
      let acc =
        match scope.scope_type with
        | Function -> acc
        | Initializer ->
            if Option.is_none expr then acc
            else
              { token; message = "Can't return a value from an initializer." }
              :: acc
        | Inherit ->
            {
              token;
              message = "Error at 'return': Can't return from top-level code.";
            }
            :: acc
      in
      match expr with None -> acc | Some expr -> expression expr scope acc)
  | Ast.VarStmt (name, expr, token) ->
      let acc =
        match declare_var scope name token with
        | Ok () -> acc
        | Error error -> error :: acc
      in
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
  | Ast.GetProperty (instance, _) -> expression instance scope acc
  | Ast.SetProperty (instance, _, expr) ->
      expression instance scope acc |> expression expr scope
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
  | Ast.This (token, distance) -> (
      match scope.class_type with
      | InsideClass -> (
          match resolve_var scope token.lexeme token with
          | Ok d ->
              distance := d;
              acc
          | Error err -> err :: acc)
      | InheritClassType ->
          { token; message = "Can't use 'this' outside a class." } :: acc)
  | Ast.Super (token, _, distance) -> (
      match scope.class_type with
      | InsideClass -> (
          match resolve_var scope token.lexeme token with
          | Ok d ->
              distance := d;
              acc
          | Error err -> err :: acc)
      | InheritClassType ->
          { token; message = "Can't use 'super' outside a class." } :: acc)
  | Ast.Unary (_, expr) -> expression expr scope acc

let analyze_program (program : Ast.program_t) :
    (Ast.program_t, error_list_t) Result.t =
  let errors = statements program (create_scope ()) [] in
  match errors with [] -> Ok program | _ -> Error errors
