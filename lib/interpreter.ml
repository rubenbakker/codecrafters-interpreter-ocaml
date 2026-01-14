open! Base

type value_t =
  | BooleanValue of bool
  | NumberValue of float
  | StringValue of string
  | NilValue
[@@deriving sexp, compare, equal]

type runtime_error = { token : Tokens.t; message : string }

exception Runtime_exn of runtime_error
exception Notimplemented

let is_truthy value =
  match value with
  | BooleanValue b -> b
  | NumberValue _ -> true
  | StringValue _ -> true
  | NilValue -> false

let is_equal left right =
  match (left, right) with
  | BooleanValue left, BooleanValue right -> Bool.(left = right)
  | NumberValue left, NumberValue right -> Float.(left = right)
  | StringValue left, StringValue right -> String.(left = right)
  | NilValue, NilValue -> true
  | _, _ -> false

let rec evaluate (ast : Ast.t) : value_t =
  match ast with
  | Ast.Unary (token_type, expr) -> unary token_type expr
  | Ast.Literal value -> literal value
  | Ast.Grouping ast -> evaluate ast
  | Ast.Binary (left_expr, token_type, right_expr) ->
      binary left_expr token_type right_expr

and binary left_expr token right_expr =
  let left = evaluate left_expr and right = evaluate right_expr in
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

and unary token expr : value_t =
  let right_value = evaluate expr in
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

let number_to_string value =
  let value = Float.to_string value in
  let value = String.rstrip ~drop:(fun ch -> Char.(ch = '0')) value in
  match Stdlib.String.ends_with ~suffix:"." value with
  | true -> String.sub ~pos:0 ~len:(String.length value - 1) value
  | false -> value

let value_to_string value =
  match value with
  | BooleanValue b -> if b then "true" else "false"
  | NumberValue n -> number_to_string n
  | StringValue s -> s
  | NilValue -> "nil"

let error_to_string error =
  Stdlib.Printf.sprintf "%s\n[line %d]" error.message error.token.line

let run (ast : Ast.t) : (value_t, runtime_error) Result.t =
  try Ok (evaluate ast) with Runtime_exn runtime_error -> Error runtime_error
