open! Base

type value_t =
  | BooleanValue of bool
  | NumberValue of float
  | StringValue of string
  | NilValue
[@@deriving sexp, compare, equal]

exception Interpreter_exn of string
exception Notimplemented

let with_boolean_value value fn =
  match value with
  | BooleanValue b -> BooleanValue (fn b)
  | _ -> raise (Interpreter_exn "Not a boolean!")

let with_number_value value fn =
  match value with
  | NumberValue n -> NumberValue (fn n)
  | _ -> raise (Interpreter_exn "Not a number!")

let rec evaluate (ast : Ast.t) : value_t =
  match ast with
  | Ast.Unary (token_type, expr) -> unary token_type expr
  | Ast.Literal value -> literal value
  | _ -> raise Notimplemented

and unary token_type expr : value_t =
  let right_value = evaluate expr in
  match token_type with
  | Tokens.BANG -> with_boolean_value right_value (fun b -> not b)
  | Tokens.MINUS -> with_number_value right_value (fun n -> n *. -1.0)
  | _ -> raise Notimplemented

and literal literal : value_t =
  match literal with
  | Ast.LiteralNumber n -> NumberValue n
  | Ast.LiteralBoolean b -> BooleanValue b
  | Ast.LiteralNil -> NilValue
  | Ast.LiteralString s -> StringValue s

let value_to_string value =
  match value with
  | BooleanValue b -> if b then "true" else "false"
  | NumberValue n -> Tokens.number_to_string n
  | StringValue s -> s
  | NilValue -> "nil"
