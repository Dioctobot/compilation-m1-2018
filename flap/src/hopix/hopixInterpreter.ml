open HopixAST

(** A runtime environment contains all the information necessary
    to evaluate a program. *)
type runtime = unit

(** In the interactive loop, we will display some observable
    feedback about the evaluation. *)
type observable = unit

(** The evaluation starts with an initial runtime. *)
let initial_runtime : unit -> runtime =
  fun () -> ()

(** [evaluate runtime p] executes the program [p] and
    produces a new runtime as well as an observation
      of this runtime. *)
let evaluate : runtime -> program -> runtime * observable =
  fun () _ -> ((), ())

(** [print_observable o] returns a human-readable
    representation of an observable. *)
let print_observable : runtime -> observable -> string =
  fun () () -> "Nothing for the moment"
