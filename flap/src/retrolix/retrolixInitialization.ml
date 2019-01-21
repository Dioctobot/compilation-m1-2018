(** Register some compilers that have Retrolix as a target or source language. *)
let initialize () =
  Languages.register (module Retrolix)
