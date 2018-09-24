(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixPrettyPrinter

type typing_environment = unit

let initial_typing_environment () = ()

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast : typing_environment =
  ()

let print_typing_environment env =
  "You know nothing, John Snow."
