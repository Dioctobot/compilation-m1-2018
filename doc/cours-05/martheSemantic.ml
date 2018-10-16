type t =
  | Int of int
  | Add of t * t
  | Var of string
  | Let of string * t * t

(** [fresh ()] renvoie un nom de variable qui n'a jamais été
    utilisé. *)
let fresh =
  let x = ref 0 in
  fun () -> incr x; "_" ^ string_of_int !x

(** [rename x y t] remplace toutes les occurrences libres de x
    par la variable y qui est fraiche. *)
let rec rename x y = function
  | Int n -> Int n
  | Add (t1, t2) -> Add (rename x y t1, rename x y t2)
  | Var z when z = x -> Var y
  | Var z -> Var z
  | Let (z, t1, t2) ->
     Let (z, rename x y t1,
          if z = x then t2 else rename x y t2)

(** [subst x u t] remplace toutes les occurrences libres de x
     par u dans t. *)
let rec subst x u = function
  | Int n -> Int n
  | Add (t1, t2) -> Add (subst x u t1, subst x u t2)
  | Var y when y = x -> u
  | Var y -> Var y
  | Let (y, t1, t2) ->
     let z = fresh () in
     let t2 = rename y z t2 in
     Let (z, subst x u t1, subst x u t2)

(** [eval t] tel que [t -> eval t] si t n'est pas une valeur. *)
exception EndOfTheWorld
let rec eval t =
  if is_value t then
    t
  else
    match t with
    | Add (Int m, Int n) -> Int (m + n)
    | Add (Int n, t) -> Add (Int n, eval t)
    | Add (t1, t2)   -> Add (eval t1, t2)
    | Int _ -> assert false (* By is_value *)
    | Var x -> raise EndOfTheWorld
    | Let (x, e1, e2) -> subst x e1 e2 (* β-reduction *)
and is_value = function
  | Int _ -> true
  | _ -> false

module Env : sig
  type t
  val empty : t
  val bind : string -> t -> t
  val is_bound : string -> t -> bool
end = struct
  type t = string list
  let empty = []
  let bind x env = x :: env
  let is_bound x env = List.mem x env
end

exception TypeError
let rec check : Env.t -> t -> unit = fun env -> function
  | Int _ -> ()
  | Add (t1, t2) -> check env t1; check env t2
  | Var x -> if not (Env.is_bound x env) then raise TypeError
  | Let (x, t1, t2) -> check env t1; check (Env.bind x env) t2

let rec big_eval = function
  | Int n -> Int n
  | Var x -> assert false
  | Add (t1, t2) -> begin
      match big_eval t1, big_eval t2 with
      | Int n, Int m -> Int (n + m)
      | _, _ -> assert false
    end
  | Let (x, t1, t2) -> big_eval (subst x (big_eval t1) t2)
