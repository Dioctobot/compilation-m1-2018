(** Un terme a le type ['a t] ssi il s'évalue en une
    valeur de type 'a. *)
type eval_to_int = int
type eval_to_bool = bool

type 'a t =
  | Int : int -> eval_to_int t
  | Add : eval_to_int t * eval_to_int t -> eval_to_int t
  | If  : eval_to_bool t * 'something t * 'something t ->
          'something t
  | Eq  : 'something t * 'something t -> eval_to_bool t

type 'a value = 'a

exception IllTyped

let rec eval
: type something. something t -> something value
= function
  | Int x ->
     x
  | Add (e1, e2) ->
     eval e1 + eval e2
  | If (c, e1, e2) ->
     if eval c then eval e1 else eval e2
  | Eq (e1, e2) ->
     (eval e1 = eval e2)
