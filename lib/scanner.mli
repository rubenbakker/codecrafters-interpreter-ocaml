type token_result_t = (Tokens.t, string) Result.t

val scan : string -> token_result_t list
val get_tokens : token_result_t list -> Tokens.t list
val get_errors : token_result_t list -> string list
