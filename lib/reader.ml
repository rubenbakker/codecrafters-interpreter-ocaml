open! Base

type 'a t = { items : 'a list; remaining : 'a list; line_number : int }

let create items = { items; remaining = items; line_number = 1 }

let create_from_string (str : string) : 'a t =
  create (List.init (String.length str) ~f:(String.get str))

let at_end reader = List.is_empty reader.remaining

let advance reader =
  match reader.remaining with
  | [] -> reader
  | _ :: rest -> { reader with remaining = rest }

let peek reader = List.hd reader.remaining

let matches_next item reader =
  match reader.remaining with
  | [] -> false
  | [ _ ] -> false
  | _ :: next :: _ -> Poly.(next = item)
